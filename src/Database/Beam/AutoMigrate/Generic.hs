{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Database.Beam.AutoMigrate.Generic where

import Data.Bifunctor
import Data.Kind
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Proxy
import qualified Data.Set as S
import qualified Data.Text as T
import Database.Beam.AutoMigrate.Annotated
import Database.Beam.AutoMigrate.Compat
import Database.Beam.AutoMigrate.Types
import Database.Beam.AutoMigrate.Util (canonicalizeForeignKeyPairs, foreignKeyName, generatedIdentifier, pkFieldNames, sequenceEither)
import Database.Beam.AutoMigrate.Validity (Qualified (..), Reason (..), ValidationFailed (..))
import Database.Beam.Schema (PrimaryKey, TableEntity)
import qualified Database.Beam.Schema as Beam
import Database.Beam.Schema.Tables (Beamable (..), dbEntityDescriptor, dbEntityName)
import GHC.Generics
import GHC.TypeLits
import Lens.Micro ((^.))

--
--- Machinery to derive a 'Schema' from a 'DatabaseSettings'.
--

class GSchema be db (anns :: [Annotation]) (x :: * -> *) where
  gSchema :: AnnotatedDatabaseSettings be db -> Proxy anns -> x p -> Either [ValidationFailed] Schema

-- Table-specific classes

class GTables be db (anns :: [Annotation]) (x :: * -> *) where
  gTables :: AnnotatedDatabaseSettings be db -> Proxy anns -> x p -> Either [ValidationFailed] (Tables, Sequences)

class GTableEntry (be :: *) (db :: DatabaseKind) (anns :: [Annotation]) (tableFound :: Bool) (x :: * -> *) where
  gTableEntries ::
    AnnotatedDatabaseSettings be db ->
    Proxy anns ->
    Proxy tableFound ->
    x p ->
    Either [ValidationFailed] ([(TableName, Table)], Sequences)

-- Enumerations-specific classes

class GEnums be db x where
  gEnums :: AnnotatedDatabaseSettings be db -> x p -> Either [ValidationFailed] Enumerations

-- Column-specific classes

-- | Type-level witness of whether or not we have to generate an extra \"SEQUENCE\" in case the table
--  field is a 'SqlSerial'.
data GenSequencesForSerial
  = GenSequences
  | NoGenSequences

class GColumns (genSeqs :: GenSequencesForSerial) (x :: * -> *) where
  gColumns :: Proxy genSeqs -> TableName -> x p -> Either [ValidationFailed] (Columns, Sequences)

class GTableConstraintColumns be db x where
  gTableConstraintsColumns :: AnnotatedDatabaseSettings be db -> TableName -> x p -> Either [ValidationFailed] (S.Set TableConstraint)

--
-- Combining independent results
--

-- | Preserve errors from both independent results when combining them.
bothEither :: Either [e] a -> Either [e] b -> Either [e] (a, b)
bothEither (Left e1) (Left e2) = Left (e1 <> e2)
bothEither (Left e1) _ = Left e1
bothEither _ (Left e2) = Left e2
bothEither (Right a) (Right b) = Right (a, b)

-- | Prevent duplicate table or sequence names from discarding definitions during a map merge.
combineTables :: Tables -> Sequences -> Tables -> Sequences -> Either [ValidationFailed] (Tables, Sequences)
combineTables tablesA seqsA tablesB seqsB =
  if null errs then Right (M.union tablesA tablesB, M.union seqsA seqsB) else Left errs
  where
    errs =
      [InvalidTableName tName (DuplicateTableName tName) | tName <- M.keys (M.intersection tablesA tablesB)]
        <> [InvalidSequence sName (DuplicateSequenceName sName) | sName <- M.keys (M.intersection seqsA seqsB)]

-- | Reject duplicate table entries before converting the entries to a map.
combineEntries ::
  [(TableName, Table)] ->
  Sequences ->
  [(TableName, Table)] ->
  Sequences ->
  Either [ValidationFailed] ([(TableName, Table)], Sequences)
combineEntries entriesA seqsA entriesB seqsB =
  if null errs then Right (entriesA <> entriesB, M.union seqsA seqsB) else Left errs
  where
    names = map fst (entriesA <> entriesB)
    seqNames = M.keys seqsA <> M.keys seqsB
    errs =
      [ InvalidTableName tName (DuplicateTableName tName)
      | tName <- L.nub names,
        length (filter (== tName) names) > 1
      ]
        <> [ InvalidSequence sName (DuplicateSequenceName sName)
           | sName <- L.nub seqNames,
             length (filter (== sName) seqNames) > 1
           ]

-- | Reject duplicate columns and sequences before merging their maps.
combineColumns :: TableName -> (Columns, Sequences) -> (Columns, Sequences) -> Either [ValidationFailed] (Columns, Sequences)
combineColumns tName (colsA, seqsA) (colsB, seqsB) =
  if null errs then Right (M.union colsA colsB, M.union seqsA seqsB) else Left errs
  where
    errs =
      [InvalidColumn (Qualified tName cName) (DuplicateColumnName tName cName) | cName <- M.keys (M.intersection colsA colsB)]
        <> [InvalidSequence sName (DuplicateSequenceName sName) | sName <- M.keys (M.intersection seqsA seqsB)]

-- | Preserve identical enum definitions and report every conflicting pair with the same name.
mergeEnumMaps :: Enumerations -> Enumerations -> Either [ValidationFailed] Enumerations
mergeEnumMaps enumsA enumsB =
  if null errs then Right (M.union enumsA enumsB) else Left errs
  where
    errs =
      [ InvalidEnum eName (ConflictingEnumDefinition eName existing incoming)
      | (eName, incoming) <- M.toList enumsB,
        Just existing <- [M.lookup eName enumsA],
        existing /= incoming
      ]

--
-- Deriving information about 'Schema's
--

instance GSchema be db anns x => GSchema be db anns (D1 f x) where
  gSchema db p (M1 x) = gSchema db p x

instance
  ( Constructor f,
    GTables be db anns x,
    GEnums be db x
  ) =>
  GSchema be db anns (C1 f x)
  where
  gSchema db p (M1 x) =
    bothEither (gTables db p x) (gEnums db x)
      >>= \((tables, sequences), enumerations) ->
        pure
          Schema
            { schemaTables = tables,
              schemaEnumerations = enumerations,
              schemaSequences = sequences
            }

--
-- Deriving information about 'Enums'.
--

instance GEnums be db x => GEnums be db (D1 f x) where
  gEnums db (M1 x) = gEnums db x

instance GEnums be db x => GEnums be db (C1 f x) where
  gEnums db (M1 x) = gEnums db x

instance (GEnums be db a, GEnums be db b) => GEnums be db (a :*: b) where
  gEnums db (a :*: b) =
    bothEither (gEnums db a) (gEnums db b) >>= uncurry mergeEnumMaps

instance
  ( IsAnnotatedDatabaseEntity be (TableEntity tbl),
    Beam.Table tbl,
    GEnums be db (Rep (TableSchema tbl)),
    Generic (TableSchema tbl)
  ) =>
  GEnums be db (S1 f (K1 R (AnnotatedDatabaseEntity be db (TableEntity tbl))))
  where
  gEnums db (M1 (K1 annEntity)) =
    gEnums db (from (dbAnnotatedSchema (annEntity ^. annotatedDescriptor)))

instance
  {-# OVERLAPS #-}
  (GEnums be db (Rep (sub f)), Generic (sub f)) =>
  GEnums be db (S1 m (K1 R (sub f)))
  where
  gEnums db (M1 (K1 e)) = gEnums db (from e)

instance HasColumnType ty => GEnums be db (S1 f (K1 R (TableFieldSchema tbl ty))) where
  gEnums _ (M1 (K1 _)) = Right (defaultEnums (Proxy @ty))

-- primary-key-wrapped types do not yield any enumerations.

instance GEnums be db (S1 f (K1 R (PrimaryKey tbl1 (TableFieldSchema tbl2)))) where
  gEnums _ (M1 (K1 _)) = Right mempty

instance GEnums be db (S1 f (K1 R (PrimaryKey tbl1 (g (TableFieldSchema tbl2))))) where
  gEnums _ (M1 (K1 _)) = Right mempty

--
-- Deriving information about 'Table's.
--

instance GTableEntry be db anns 'False (S1 f x) => GTables be db anns (S1 f x) where
  gTables db p x = do
    (tbls, sqs) <- gTableEntries db p (Proxy @'False) x
    pure (M.fromList tbls, sqs)

instance GTableEntry be db anns tableFound x => GTableEntry be db anns tableFound (S1 f x) where
  gTableEntries db p1 p2 (M1 x) = gTableEntries db p1 p2 x

instance (GTables be db anns a, GTables be db anns b) => GTables be db anns (a :*: b) where
  gTables db p (a :*: b) =
    bothEither (gTables db p a) (gTables db p b)
      >>= \((ta, sa), (tb, sb)) -> combineTables ta sa tb sb

mkTableEntryNoFkDiscovery ::
  ( GColumns 'GenSequences (Rep (TableSchema tbl)),
    Generic (TableSchema tbl),
    Beam.Table tbl
  ) =>
  AnnotatedDatabaseEntity be db (TableEntity tbl) ->
  Either [ValidationFailed] ((TableName, Table), Sequences)
mkTableEntryNoFkDiscovery annEntity = do
  let entity = annEntity ^. deannotate
      tName = entity ^. dbEntityDescriptor . dbEntityName
      tableName' = TableName tName
      pDesc = annEntity ^. annotatedDescriptor
      pkCols = pkFieldNames entity
      pkColSet = S.fromList pkCols
      pkName =
        generatedIdentifier
          (tName <> "_pkey")
          (["primary-key", tName] <> map columnName pkCols)
      pks = if S.null pkColSet then mempty else S.singleton (PrimaryKey pkName pkColSet)
  (columns, seqs) <- gColumns (Proxy @'GenSequences) tableName' (from (dbAnnotatedSchema pDesc))
  manualCons <- sequenceEither (map (resolvePendingForeignKey tableName') (dbAnnotatedForeignKeys pDesc))
  let annotatedCons = dbAnnotatedConstraints pDesc
      allCons = S.toList annotatedCons <> manualCons
      duplicateErrors =
        [ InvalidTableConstraint c (DuplicateConstraintName tableName' n)
        | c <- allCons,
          let n = constraintNameOf c,
          length (filter ((== n) . constraintNameOf) allCons) > 1
        ]
  if null duplicateErrors
    then
      Right
        ( (tableName', Table (pks <> annotatedCons <> S.fromList manualCons) columns),
          seqs
        )
    else Left duplicateErrors

mkTableEntryFkDiscovery ::
  ( GColumns 'GenSequences (Rep (TableSchema tbl)),
    Generic (TableSchema tbl),
    Beam.Table tbl,
    GTableConstraintColumns be db (Rep (TableSchema tbl))
  ) =>
  AnnotatedDatabaseSettings be db ->
  AnnotatedDatabaseEntity be db (TableEntity tbl) ->
  Either [ValidationFailed] ((TableName, Table), Sequences)
mkTableEntryFkDiscovery db annEntity = do
  ((tName, table), seqs) <- mkTableEntryNoFkDiscovery annEntity
  inferred <-
    gTableConstraintsColumns
      db
      tName
      (from (dbAnnotatedSchema (annEntity ^. annotatedDescriptor)))
  let explicitLocalSets =
        S.fromList (map foreignKeyLocalColumns (filter isForeignKey (S.toList (tableConstraints table))))
      keptInferred =
        S.filter (\c -> not (isForeignKey c && foreignKeyLocalColumns c `S.member` explicitLocalSets)) inferred
  pure ((tName, table {tableConstraints = keptInferred <> tableConstraints table}), seqs)

-- | Reject unequal column counts and repeated columns before pairing a manual foreign key.
resolvePendingForeignKey :: TableName -> PendingForeignKey -> Either [ValidationFailed] TableConstraint
resolvePendingForeignKey localTable pfk =
  let target = pfkTargetTable pfk
      locals = pfkLocalColumns pfk
      remotes = pfkRemoteColumns pfk
   in if null locals || null remotes
        then invalid target (EmptyKey localTable "FOREIGN KEY")
        else
          if length locals /= length remotes
            then invalid target (ForeignKeyArityMismatch localTable target (length locals) (length remotes))
            else case NE.nonEmpty (zip locals remotes) of
              Nothing -> invalid target (EmptyKey localTable "FOREIGN KEY")
              Just pairs
                | length (L.nub locals) /= length locals || length (L.nub remotes) /= length remotes ->
                  invalid target (DuplicateForeignKeyColumns localTable (foreignKeyName localTable pairs target))
                | otherwise ->
                  Right
                    ( ForeignKey
                        (foreignKeyName localTable pairs target)
                        target
                        (canonicalizeForeignKeyPairs pairs)
                        (pfkOnDelete pfk)
                        (pfkOnUpdate pfk)
                    )
  where
    invalid tName reason = Left [InvalidForeignKeyDeclaration localTable tName reason]

isForeignKey :: TableConstraint -> Bool
isForeignKey = \case
  ForeignKey {} -> True
  _ -> False

foreignKeyLocalColumns :: TableConstraint -> S.Set ColumnName
foreignKeyLocalColumns = \case
  ForeignKey _ _ pairs _ _ -> S.fromList (map fst (NE.toList pairs))
  _ -> S.empty

constraintNameOf :: TableConstraint -> T.Text
constraintNameOf = \case
  PrimaryKey n _ -> n
  Unique n _ -> n
  ForeignKey n _ _ _ _ -> n

--
-- Automatic FK-discovery algorithm starts here
--
-- The general idea is to carry around a (tableFound :: Bool) type-level witness to be used as we uncons
-- the type-level list. If we find a match, we toggle-off the FK-discovery algorithm, otherwise we don't.

instance
  GTableEntry be db xs (TestTableEqual tbl tbl') (K1 R (AnnotatedDatabaseEntity be db (TableEntity tbl))) =>
  GTableEntry be db (UserDefinedFk tbl' ': xs) 'False (K1 R (AnnotatedDatabaseEntity be db (TableEntity tbl)))
  where
  gTableEntries db _ _ x@(K1 _) =
    gTableEntries db (Proxy @xs) (Proxy @(TestTableEqual tbl tbl')) x

instance
  GTableEntry be db xs 'True (K1 R (AnnotatedDatabaseEntity be db (TableEntity tbl))) =>
  GTableEntry be db (UserDefinedFk tbl' ': xs) 'True (K1 R (AnnotatedDatabaseEntity be db (TableEntity tbl)))
  where
  gTableEntries db _ _ x@(K1 _) =
    gTableEntries db (Proxy @xs) (Proxy @'True) x

-- At this point we explored the full list and the previous equality check yielded 'True, which means a
-- match was found. We disable the FK-discovery algorithm.

instance
  ( IsAnnotatedDatabaseEntity be (TableEntity tbl),
    GColumns 'GenSequences (Rep (TableSchema tbl)),
    Generic (TableSchema tbl),
    Beam.Table tbl
  ) =>
  GTableEntry be db '[] 'True (K1 R (AnnotatedDatabaseEntity be db (TableEntity tbl)))
  where
  gTableEntries _ _ _ (K1 annEntity) = fmap (first (: [])) (mkTableEntryNoFkDiscovery annEntity)

-- At this point we explored the full list and the previous equality check yielded 'False, so we kickoff the
-- automatic FK-discovery algorithm.

instance
  ( IsAnnotatedDatabaseEntity be (TableEntity tbl),
    GColumns 'GenSequences (Rep (TableSchema tbl)),
    Generic (TableSchema tbl),
    Beam.Table tbl,
    GTableConstraintColumns be db (Rep (TableSchema tbl))
  ) =>
  GTableEntry be db '[] 'False (K1 R (AnnotatedDatabaseEntity be db (TableEntity tbl)))
  where
  gTableEntries db Proxy Proxy (K1 annEntity) = fmap (first (: [])) (mkTableEntryFkDiscovery db annEntity)

-- sub-db support for GTableEntry

instance
  ( Generic (innerDB (AnnotatedDatabaseEntity be outerDB)),
    Beam.Database be innerDB,
    GTableEntry be outerDB xs found (Rep (innerDB (AnnotatedDatabaseEntity be outerDB)))
  ) =>
  GTableEntry be outerDB xs found (K1 R (innerDB (AnnotatedDatabaseEntity be outerDB)))
  where
  gTableEntries outerDB p1 p2 (K1 innerDB) =
    gTableEntries outerDB p1 p2 (from innerDB)

instance GTableEntry be outerDB xs found x => GTableEntry be outerDB xs found (D1 f x) where
  gTableEntries outerDB p1 p2 (M1 x) = gTableEntries outerDB p1 p2 x

instance GTableEntry be outerDB xs found x => GTableEntry be outerDB xs found (C1 f x) where
  gTableEntries outerDB p1 p2 (M1 x) = gTableEntries outerDB p1 p2 x

instance
  (GTableEntry be outerDB xs found a, GTableEntry be outerDB xs found b) =>
  GTableEntry be outerDB xs found (a :*: b)
  where
  gTableEntries outerDB p1 p2 (a :*: b) =
    bothEither
      (gTableEntries outerDB p1 p2 a)
      (gTableEntries outerDB p1 p2 b)
      >>= \((la, sa), (lb, sb)) -> combineEntries la sa lb sb

-- end of sub-db support for GTableEntry

instance GColumns gseq x => GColumns gseq (D1 f x) where
  gColumns p t (M1 x) = gColumns p t x

instance GTableConstraintColumns be db x => GTableConstraintColumns be db (D1 f x) where
  gTableConstraintsColumns db tbl (M1 x) = gTableConstraintsColumns db tbl x

instance GColumns gseq x => GColumns gseq (C1 f x) where
  gColumns p t (M1 x) = gColumns p t x

instance GTableConstraintColumns be db x => GTableConstraintColumns be db (C1 f x) where
  gTableConstraintsColumns db tbl (M1 x) = gTableConstraintsColumns db tbl x

instance (GColumns p a, GColumns p b) => GColumns p (a :*: b) where
  gColumns p t (a :*: b) =
    bothEither (gColumns p t a) (gColumns p t b)
      >>= \((ca, sa), (cb, sb)) -> combineColumns t (ca, sa) (cb, sb)

instance (GTableConstraintColumns be db a, GTableConstraintColumns be db b) => GTableConstraintColumns be db (a :*: b) where
  gTableConstraintsColumns db tbl (a :*: b) =
    bothEither
      (gTableConstraintsColumns db tbl a)
      (gTableConstraintsColumns db tbl b)
      >>= \(ca, cb) -> Right (S.union ca cb)

--
-- Column entries
--

instance HasCompanionSequence ty => GColumns 'GenSequences (S1 m (K1 R (TableFieldSchema tbl ty))) where
  gColumns Proxy t (M1 (K1 (TableFieldSchema name (FieldSchema ty constr)))) =
    case hasCompanionSequence (Proxy @ty) t name of
      Nothing ->
        Right (M.singleton name (Column ty constr), mempty)
      Just (sq, extraDefault) ->
        Right (M.singleton name (Column ty (S.insert extraDefault constr)), uncurry M.singleton sq)

instance GColumns 'NoGenSequences (S1 m (K1 R (TableFieldSchema tbl ty))) where
  gColumns Proxy _ (M1 (K1 (TableFieldSchema name (FieldSchema ty constr)))) =
    Right (M.singleton name (Column ty constr), mempty)

instance
  {-# OVERLAPS #-}
  ( GColumns p (Rep (sub f)),
    Generic (sub f)
  ) =>
  GColumns p (S1 m (K1 R (sub f)))
  where
  gColumns p t (M1 (K1 e)) = gColumns p t (from e)

-- If a PrimaryKey is referenced in another table, we do not want to recreate the \"SEQUENCE\" and
-- \"nextval\" default constraint associated with the original field.
instance
  ( GColumns 'NoGenSequences (Rep (PrimaryKey tbl f)),
    Generic (PrimaryKey tbl f),
    Beamable (PrimaryKey tbl)
  ) =>
  GColumns x (S1 m (K1 R (PrimaryKey tbl f)))
  where
  gColumns _ t (M1 (K1 e)) = gColumns (Proxy @NoGenSequences) t (from e)

instance GTableConstraintColumns be db (S1 m (K1 R (TableFieldSchema tbl ty))) where
  gTableConstraintsColumns _db _tbl (M1 (K1 _)) = Right S.empty

instance
  {-# OVERLAPS #-}
  ( Generic (AnnotatedDatabaseSettings be db),
    Generic (sub f),
    GColumns 'GenSequences (Rep (sub f)),
    GTableConstraintColumns be db (Rep (sub f))
  ) =>
  GTableConstraintColumns be db (S1 m (K1 R (sub f)))
  where
  gTableConstraintsColumns db tname (M1 (K1 e)) =
    gTableConstraintsColumns db tname (from e)

instance
  ( Generic (AnnotatedDatabaseSettings be db),
    Generic (PrimaryKey tbl f),
    GOrderedColumnNames (Rep (PrimaryKey tbl f)),
    GTableLookupSettings sel tbl (Rep (AnnotatedDatabaseSettings be db)),
    m ~ MetaSel sel su ss ds
  ) =>
  GTableConstraintColumns be db (S1 m (K1 R (PrimaryKey tbl f)))
  where
  gTableConstraintsColumns db localTable (M1 (K1 e)) =
    let localCols = map ColumnName (gOrderedColumnNames (from e))
        (reftname, remoteCols) = gTableLookupSettings (Proxy @sel) (Proxy @tbl) (from db)
        invalid reason = Left [InvalidForeignKeyDeclaration localTable reftname reason]
     in if null localCols || null remoteCols
          then invalid (EmptyKey localTable "FOREIGN KEY")
          else
            if length localCols /= length remoteCols
              then invalid (ForeignKeyArityMismatch localTable reftname (length localCols) (length remoteCols))
              else case NE.nonEmpty (zip localCols remoteCols) of
                Nothing -> invalid (EmptyKey localTable "FOREIGN KEY")
                Just pairs
                  | length (L.nub localCols) /= length localCols
                      || length (L.nub remoteCols) /= length remoteCols ->
                    invalid (DuplicateForeignKeyColumns localTable (foreignKeyName localTable pairs reftname))
                  | otherwise ->
                    Right
                      ( S.singleton
                          ( ForeignKey
                              (foreignKeyName localTable pairs reftname)
                              reftname
                              (canonicalizeForeignKeyPairs pairs)
                              NoAction
                              NoAction
                          )
                      )

-- We want a type class for the table lookup, because we want to return a
-- value-level table name based on the database settings!

-- | Lookup a table by type in the given DB settings.
--
-- The selector name is only provided for error messages.
--
-- Only returns if the table type is unique.
-- Returns the table name and the column names of its primary key.
class GTableLookupSettings (sel :: Maybe Symbol) (tbl :: TableKind) x where
  gTableLookupSettings :: Proxy sel -> Proxy tbl -> x p -> (TableName, [ColumnName])

-- | Helper class that takes an additional continuation parameter 'k'.
--
-- We treat 'k' as a type-level stack with 'U1' being the empty stack and
-- ':*:' used right-associatively to place items onto the stack.
--
-- The reason we do not use a type-level list here is that we also need
-- a term-level representation of the continuation, and we already have
-- suitable inhabitants for 'U1' and ':*:'.
class GTableLookupTables (sel :: Maybe Symbol) (tbl :: TableKind) (x :: Type -> Type) (k :: Type -> Type) where
  gTableLookupTables :: Proxy sel -> Proxy tbl -> x p -> k p -> (TableName, [ColumnName])

-- | We use this function to continue searching once we've already found
-- a match, and to abort if we find a second match.
class GTableLookupTablesExpectFail (sel :: Maybe Symbol) (tbl :: TableKind) (x :: Type -> Type) (k :: Type -> Type) where
  gTableLookupTablesExpectFail :: Proxy sel -> Proxy tbl -> (TableName, [ColumnName]) -> x p -> k p -> (TableName, [ColumnName])

instance
  (GTableLookupSettings sel tbl x) =>
  GTableLookupSettings sel tbl (D1 f x)
  where
  gTableLookupSettings sel tbl (M1 x) = gTableLookupSettings sel tbl x

instance
  (GTableLookupTables sel tbl x U1) =>
  GTableLookupSettings sel tbl (C1 f x)
  where
  gTableLookupSettings sel tbl (M1 x) = gTableLookupTables sel tbl x U1

instance
  (GTableLookupTables sel tbl x k) =>
  GTableLookupTables sel tbl (S1 f x) k
  where
  gTableLookupTables sel tbl (M1 x) = gTableLookupTables sel tbl x

instance
  ( GTableLookupTables sel tbl a (b :*: k)
  ) =>
  GTableLookupTables sel tbl (a :*: b) k
  where
  gTableLookupTables sel tbl (a :*: b) k = gTableLookupTables sel tbl a (b :*: k)

instance
  (GTableLookupTablesExpectFail sel tbl x k) =>
  GTableLookupTablesExpectFail sel tbl (S1 f x) k
  where
  gTableLookupTablesExpectFail sel tbl r (M1 x) = gTableLookupTablesExpectFail sel tbl r x

instance
  ( GTableLookupTablesExpectFail sel tbl a (b :*: k)
  ) =>
  GTableLookupTablesExpectFail sel tbl (a :*: b) k
  where
  gTableLookupTablesExpectFail sel tbl r (a :*: b) k = gTableLookupTablesExpectFail sel tbl r a (b :*: k)

instance
  ( GTableLookupTable (TestTableEqual tbl tbl') sel tbl k,
    Beamable tbl',
    Beam.Table tbl'
  ) =>
  GTableLookupTables sel tbl (K1 R (AnnotatedDatabaseEntity be db (TableEntity tbl'))) k
  where
  gTableLookupTables sel tbl (K1 annEntity) k =
    let entity = annEntity ^. deannotate
        tname = entity ^. dbEntityDescriptor . dbEntityName
        cnames = pkFieldNames entity
     in gTableLookupTable (Proxy @(TestTableEqual tbl tbl')) sel tbl (TableName tname, cnames) k

-- sub-db support for GTableLookupTables

instance
  ( GTableLookupTables sel tbl (Rep (innerDB (AnnotatedDatabaseEntity be outerDB))) k,
    Beam.Database be innerDB,
    Generic (innerDB (AnnotatedDatabaseEntity be outerDB))
  ) =>
  GTableLookupTables sel tbl (K1 R (innerDB (AnnotatedDatabaseEntity be outerDB))) k
  where
  gTableLookupTables sel tbl (K1 subDB) k =
    gTableLookupTables sel tbl (from subDB) k

instance GTableLookupTables sel tbl x k => GTableLookupTables sel tbl (D1 f x) k where
  gTableLookupTables sel tbl (M1 x) k = gTableLookupTables sel tbl x k

instance GTableLookupTables sel tbl x k => GTableLookupTables sel tbl (C1 f x) k where
  gTableLookupTables sel tbl (M1 x) k = gTableLookupTables sel tbl x k

-- end of sub-db support for GTableLookupTables

instance
  ( GTableLookupTableExpectFail (TestTableEqual tbl tbl') sel tbl k,
    Beamable tbl'
  ) =>
  GTableLookupTablesExpectFail sel tbl (K1 R (AnnotatedDatabaseEntity be db (TableEntity tbl'))) k
  where
  gTableLookupTablesExpectFail sel tbl r (K1 _entity) =
    gTableLookupTableExpectFail (Proxy @(TestTableEqual tbl tbl')) sel tbl r

-- sub-db support for GTableLookupTablesExpectFail

instance
  ( GTableLookupTablesExpectFail sel tbl (Rep (innerDb (AnnotatedDatabaseEntity be outerDb))) k,
    Generic (innerDb (AnnotatedDatabaseEntity be outerDb)),
    Beam.Database be innerDb
  ) =>
  GTableLookupTablesExpectFail sel tbl (K1 R (innerDb (AnnotatedDatabaseEntity be outerDb))) k
  where
  gTableLookupTablesExpectFail sel tbl r (K1 subDB) =
    gTableLookupTablesExpectFail sel tbl r (from subDB)

instance
  ( GTableLookupTablesExpectFail sel tbl x k
  ) =>
  GTableLookupTablesExpectFail sel tbl (D1 f x) k
  where
  gTableLookupTablesExpectFail sel tbl r (M1 x) =
    gTableLookupTablesExpectFail sel tbl r x

instance
  ( GTableLookupTablesExpectFail sel tbl x k
  ) =>
  GTableLookupTablesExpectFail sel tbl (C1 f x) k
  where
  gTableLookupTablesExpectFail sel tbl r (M1 x) =
    gTableLookupTablesExpectFail sel tbl r x

-- end of sub-db support for GTableLookupTablesExpectFail

type family TestTableEqual (tbl1 :: TableKind) (tbl2 :: TableKind) :: Bool where
  TestTableEqual tbl tbl = True
  TestTableEqual _ _ = False

class GTableLookupTable (b :: Bool) (sel :: Maybe Symbol) (tbl :: TableKind) (k :: Type -> Type) where
  gTableLookupTable :: Proxy b -> Proxy sel -> Proxy tbl -> (TableName, [ColumnName]) -> k p -> (TableName, [ColumnName])

class GTableLookupTableExpectFail (b :: Bool) (sel :: Maybe Symbol) (tbl :: TableKind) (k :: Type -> Type) where
  gTableLookupTableExpectFail :: Proxy b -> Proxy sel -> Proxy tbl -> (TableName, [ColumnName]) -> k p -> (TableName, [ColumnName])

instance GTableLookupTable True sel tbl U1 where
  gTableLookupTable _ _ _ r _ = r

type LookupAmbiguous (sel :: Maybe Symbol) (tbl :: TableKind) =
  Text "Could not derive foreign key constraint for " :<>: ShowField sel :<>: Text ","
    :$$: Text "because there are several tables of type `" :<>: ShowType tbl :<>: Text "' in the schema."
    :$$: Text "In this scenario you have to manually disable the FK-discovery algorithm for all the tables "
    :$$: Text "hit by such ambiguity, for example by creating your Schema via: "
    :$$: Text ""
    :$$: Text "fromAnnotatedDbSettings annotatedDB (Proxy @'[ 'UserDefinedFk TBL1, 'UserDefinedFk TBL2, .. ])"
    :$$: Text ""
    :$$: Text "Where `TBL1..n` are types referencing " :<>: ShowType tbl :<>: Text " in the schema."
    :$$: Text "Once done that, you can explicitly provide manual FKs for the tables by using `foreignKeyOnPk` "
    :$$: Text "when annotating your `DatabaseSettings`."

type LookupFailed (sel :: Maybe Symbol) (tbl :: TableKind) =
  Text "Could not derive foreign key constraint for " :<>: ShowField sel :<>: Text ","
    :$$: Text "because there are no tables of type `" :<>: ShowType tbl :<>: Text "' in the schema."
    :$$: Text "This might be because this particular " :<>: ShowField sel :<>: Text " doesn't appear anywhere "
    :$$: Text "in your database, so the FK-discovery mechanism doesn't know how to reach it. Please note that "
    :$$: Text "in presence of nested databases there might be more than one field with the same name referencing "
    :$$: Text "different things. You might want to check you are adding or targeting the correct one."

type family ShowField (sel :: Maybe Symbol) :: ErrorMessage where
  ShowField Nothing = Text "unnamed field"
  ShowField (Just sel) = Text "field `" :<>: Text sel :<>: Text "'"

instance TypeError (LookupAmbiguous sel tbl) => GTableLookupTableExpectFail True sel tbl k where
  gTableLookupTableExpectFail _ _ _ _ _ = error "impossible"

instance (GTableLookupTablesExpectFail sel tbl k ks) => GTableLookupTable True sel tbl (k :*: ks) where
  gTableLookupTable _ sel tbl r (k :*: ks) = gTableLookupTablesExpectFail sel tbl r k ks

instance TypeError (LookupFailed sel tbl) => GTableLookupTable False sel tbl U1 where
  gTableLookupTable _ _ _ _ = error "impossible"

instance GTableLookupTableExpectFail False sel tbl U1 where
  gTableLookupTableExpectFail _ _ _ r _ = r

instance (GTableLookupTablesExpectFail sel tbl k ks) => GTableLookupTableExpectFail False sel tbl (k :*: ks) where
  gTableLookupTableExpectFail _ sel tbl r (k :*: ks) = gTableLookupTablesExpectFail sel tbl r k ks

instance GTableLookupTables sel tbl k ks => GTableLookupTable False sel tbl (k :*: ks) where
  gTableLookupTable _ sel tbl _ (k :*: ks) =
    gTableLookupTables sel tbl k ks

-- | Preserve field correspondence when pairing foreign key columns.
-- Collect names in structural order before storing complete pairs in a set.
class GOrderedColumnNames (x :: * -> *) where
  gOrderedColumnNames :: x p -> [T.Text]

instance GOrderedColumnNames x => GOrderedColumnNames (D1 f x) where
  gOrderedColumnNames (M1 x) = gOrderedColumnNames x

instance GOrderedColumnNames x => GOrderedColumnNames (C1 f x) where
  gOrderedColumnNames (M1 x) = gOrderedColumnNames x

instance (GOrderedColumnNames a, GOrderedColumnNames b) => GOrderedColumnNames (a :*: b) where
  gOrderedColumnNames (a :*: b) = gOrderedColumnNames a <> gOrderedColumnNames b

instance GOrderedColumnNames U1 where
  gOrderedColumnNames U1 = []

instance GOrderedColumnNames (S1 m (K1 R (TableFieldSchema tbl ty))) where
  gOrderedColumnNames (M1 (K1 (TableFieldSchema name _))) = [columnName name]

instance
  ( Generic (g (TableFieldSchema tbl2)),
    GOrderedColumnNames (Rep (g (TableFieldSchema tbl2)))
  ) =>
  GOrderedColumnNames (S1 m (K1 R (g (TableFieldSchema tbl2))))
  where
  gOrderedColumnNames (M1 (K1 e)) = gOrderedColumnNames (from e)

instance
  ( Generic (PrimaryKey tbl1 (Beam.Nullable (TableFieldSchema tbl2))),
    GOrderedColumnNames (Rep (PrimaryKey tbl1 (Beam.Nullable (TableFieldSchema tbl2))))
  ) =>
  GOrderedColumnNames (S1 m (K1 R (PrimaryKey tbl1 (Beam.Nullable (TableFieldSchema tbl2)))))
  where
  gOrderedColumnNames (M1 (K1 e)) = gOrderedColumnNames (from e)
