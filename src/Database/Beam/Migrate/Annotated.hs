{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module Database.Beam.Migrate.Annotated where

import           Data.Kind

import           Data.Proxy
import qualified Lens.Micro as Lens
import           Lens.Micro                               ( SimpleGetter, (^.) )
import           GHC.Generics                            as Generic
import qualified Data.Text as T
import           Data.Text                                ( Text )
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Monoid (Endo(..))

import qualified Database.Beam                           as Beam
import           Database.Beam.Schema.Tables              ( PrimaryKey )
import           Database.Beam.Migrate.Types
import           Database.Beam.Migrate.Compat
import           Database.Beam.Migrate.Util
import           Database.Beam.Schema.Tables              ( IsDatabaseEntity
                                                          , DatabaseEntityDescriptor
                                                          , TableEntity
                                                          , DatabaseEntity
                                                          , DatabaseEntityDefaultRequirements
                                                          , DatabaseEntityRegularRequirements
                                                          , dbEntityDescriptor
                                                          , dbEntityName
                                                          , dbTableSettings
                                                          , FieldModification(..)
                                                          , EntityModification(..)
                                                          )

--
-- Annotating a 'DatabaseSettings' with meta information.
--

-- | NOTE(adn) Unfortunately we cannot reuse the stock 'zipTables' from 'beam-core', because it works by
-- supplying a rank-2 function with 'IsDatabaseEntity' and 'DatabaseEntityRegularRequirements' as witnesses,
-- we we need the annotated counterparts instead.
-- This function can be written without the need of a typeclass, but alas it requires the /unexported/
-- 'GZipDatabase' from 'beam-core', so we had to re-implement this ourselves for now.
zipTables :: ( Generic (db f), Generic (db g), Generic (db h)
             , Monad m
             , GZipDatabase be f g h
                            (Rep (db f)) (Rep (db g)) (Rep (db h)) ) =>
             Proxy be ->
             (forall tbl. (IsAnnotatedDatabaseEntity be tbl, AnnotatedDatabaseEntityRegularRequirements be tbl) => f tbl -> g tbl -> m (h tbl)) ->
             db f -> db g -> m (db h)
    -- We need the pattern type signature on 'combine' to get around a type checking bug in GHC 8.0.1. In future releases,
    -- we will switch to the standard forall.
zipTables be combine (f :: db f) (g :: db g) =
      refl $ \h ->
        to <$> gZipDatabase (Proxy @f, Proxy @g, h, be) combine (from f) (from g)
      where
        -- For GHC 8.0.1 renamer bug
        refl :: (Proxy h -> m (db h)) -> m (db h)
        refl fn = fn Proxy

-- See above on why this has been re-implemented.
class GZipDatabase be f g h x y z where
  gZipDatabase :: Monad m =>
                  (Proxy f, Proxy g, Proxy h, Proxy be)
               -> (forall tbl. (IsAnnotatedDatabaseEntity be tbl, AnnotatedDatabaseEntityRegularRequirements be tbl) => f tbl -> g tbl -> m (h tbl))
               -> x () -> y () -> m (z ())
instance GZipDatabase be f g h x y z =>
  GZipDatabase be f g h (M1 a b x) (M1 a b y) (M1 a b z) where
  gZipDatabase p combine ~(M1 f) ~(M1 g) = M1 <$> gZipDatabase p combine f g
instance ( GZipDatabase be f g h ax ay az
         , GZipDatabase be f g h bx by bz ) =>
  GZipDatabase be f g h (ax :*: bx) (ay :*: by) (az :*: bz) where
  gZipDatabase p combine ~(ax :*: bx) ~(ay :*: by) =
    do a <- gZipDatabase p combine ax ay
       b <- gZipDatabase p combine bx by
       pure (a :*: b)
instance (IsAnnotatedDatabaseEntity be tbl, AnnotatedDatabaseEntityRegularRequirements be tbl) =>
  GZipDatabase be f g h (K1 Generic.R (f tbl)) (K1 Generic.R (g tbl)) (K1 Generic.R (h tbl)) where

  gZipDatabase _ combine ~(K1 x) ~(K1 y) =
    K1 <$> combine x y

--
-- An annotated Database settings.
--

type AnnotatedDatabaseSettings be db = db (AnnotatedDatabaseEntity be db)

data AnnotatedDatabaseEntity be (db :: (* -> *) -> *) entityType where
  AnnotatedDatabaseEntity :: (IsAnnotatedDatabaseEntity be entityType, IsDatabaseEntity be entityType)
                          => AnnotatedDatabaseEntityDescriptor be entityType
                          -> DatabaseEntity be db entityType
                          -> AnnotatedDatabaseEntity be db entityType

class IsDatabaseEntity be entityType => IsAnnotatedDatabaseEntity be entityType where
  data AnnotatedDatabaseEntityDescriptor be entityType :: *
  type AnnotatedDatabaseEntityDefaultRequirements be entityType :: Constraint
  type AnnotatedDatabaseEntityRegularRequirements be entityType :: Constraint

  dbAnnotatedEntityAuto :: AnnotatedDatabaseEntityRegularRequirements be entityType 
                        => DatabaseEntityDescriptor be entityType
                        -> AnnotatedDatabaseEntityDescriptor be entityType

instance IsDatabaseEntity be (TableEntity tbl)
         => IsAnnotatedDatabaseEntity be (TableEntity tbl) where
  data AnnotatedDatabaseEntityDescriptor be (TableEntity tbl) where
    AnnotatedDatabaseTable
      :: Beam.Table tbl =>
       { dbAnnotatedSchema      :: TableSchema tbl 
       , dbAnnotatedConstraints :: Set TableConstraint
       }
      -> AnnotatedDatabaseEntityDescriptor be (TableEntity tbl)
  type AnnotatedDatabaseEntityDefaultRequirements be (TableEntity tbl) =
      (DatabaseEntityDefaultRequirements be (TableEntity tbl))
  type AnnotatedDatabaseEntityRegularRequirements be (TableEntity tbl) =
      (DatabaseEntityRegularRequirements be (TableEntity tbl)
      , GDefaultTableSchema (Rep (TableSchema tbl) ()) (Rep (Beam.TableSettings tbl) ())
      , Generic (TableSchema tbl)
      , Generic (Beam.TableSettings tbl)
      )

  dbAnnotatedEntityAuto edesc = AnnotatedDatabaseTable (defaultTableSchema . dbTableSettings $ edesc) mempty

-- | A 'SimpleGetter' to get a plain 'DatabaseEntityDescriptor' from an 'AnnotatedDatabaseEntity'.
lowerEntityDescriptor :: SimpleGetter (AnnotatedDatabaseEntity be db entityType) (DatabaseEntityDescriptor be entityType)
lowerEntityDescriptor = Lens.to (\(AnnotatedDatabaseEntity _ e) -> e ^. dbEntityDescriptor)

annotatedDescriptor :: SimpleGetter (AnnotatedDatabaseEntity be db entityType) (AnnotatedDatabaseEntityDescriptor be entityType)
annotatedDescriptor = Lens.to (\(AnnotatedDatabaseEntity e _) -> e)

deannotate :: SimpleGetter (AnnotatedDatabaseEntity be db entityType) (DatabaseEntity be db entityType)
deannotate = Lens.to (\(AnnotatedDatabaseEntity _ e) -> e)

-- | A table schema.
type TableSchema tbl =
    tbl (TableFieldSchema tbl)

-- | A schema for a field within a given table
data TableFieldSchema (tbl :: (* -> *) -> *) ty where
    TableFieldSchema 
      :: 
      { tableFieldName :: Text
      , tableFieldSchema :: FieldSchema ty } 
      -> TableFieldSchema tbl ty

data FieldSchema ty where
  FieldSchema :: ColumnType
              -> Set ColumnConstraint
              -> FieldSchema ty

deriving instance Show (FieldSchema ty)

--
-- Deriving a 'TableSchema'.
--

class GDefaultTableSchema x y where
    gDefTblSchema :: Proxy x -> y -> x

instance GDefaultTableSchema (x p) (y p) => GDefaultTableSchema (D1 f x p) (D1 f y p) where
    gDefTblSchema (Proxy :: Proxy (D1 f x p)) (M1 y) = 
        M1 $ gDefTblSchema (Proxy :: Proxy (x p)) y

instance GDefaultTableSchema (x p) (y p) => GDefaultTableSchema (C1 f x p) (C1 f y p) where
    gDefTblSchema (Proxy :: Proxy (C1 f x p)) (M1 y) = 
        M1 $ gDefTblSchema (Proxy :: Proxy (x p)) y

instance (GDefaultTableSchema (a p) (c p), GDefaultTableSchema (b p) (d p)) 
    => GDefaultTableSchema ((a :*: b) p) ((c :*: d) p) where
    gDefTblSchema (Proxy :: Proxy ((a :*: b) p)) (c :*: d) = 
        gDefTblSchema (Proxy :: Proxy (a p)) c :*: 
        gDefTblSchema (Proxy :: Proxy (b p)) d

instance ( SchemaConstraint (Beam.TableField tbl ty) ~ ColumnConstraint
         , HasSchemaConstraints (Beam.TableField tbl ty), HasDefaultSqlDataType ty
         )
    => GDefaultTableSchema (S1 f (K1 Generic.R (TableFieldSchema tbl ty)) p)
                           (S1 f (K1 Generic.R (Beam.TableField tbl ty)) p) where
    gDefTblSchema (_ :: Proxy (S1 f (K1 Generic.R (TableFieldSchema tbl ty)) p)) (M1 (K1 fName)) = M1 (K1 s)
      where s = TableFieldSchema (fName ^. Beam.fieldName) defaultFieldSchema
            defaultFieldSchema = FieldSchema (defaultSqlDataType (Proxy @ty) False)
                                             (schemaConstraints (Proxy @(Beam.TableField tbl ty)))

-- | Instance for 'PrimaryKey's going from one table into another.
instance ( Generic (PrimaryKey tbl1 (Beam.TableField tbl2))
         , Generic (PrimaryKey tbl1 (TableFieldSchema tbl2))
         , GDefaultTableSchema (Rep (PrimaryKey tbl1 (TableFieldSchema tbl2)) ())
                               (Rep (PrimaryKey tbl1 (Beam.TableField tbl2)) ())
         )
    => GDefaultTableSchema (S1 f (K1 Generic.R (PrimaryKey tbl1 (TableFieldSchema tbl2))) ())
                           (S1 f (K1 Generic.R (PrimaryKey tbl1 (Beam.TableField tbl2))) ()) where
    gDefTblSchema (_ :: Proxy (S1 f (K1 Generic.R (PrimaryKey tbl1 (TableFieldSchema tbl2))) ())) (M1 (K1 fName)) = 
      M1 (K1 $ to' $ gDefTblSchema Proxy (from' fName))

-- | Instance for things like 'Nullable (TableFieldSchema tbl)'.
instance ( Generic (PrimaryKey tbl1 (g (Beam.TableField tbl2)))
         , Generic (PrimaryKey tbl1 (g (TableFieldSchema tbl2)))
         , GDefaultTableSchema (Rep (PrimaryKey tbl1 (g (TableFieldSchema tbl2))) ())
                               (Rep (PrimaryKey tbl1 (g (Beam.TableField tbl2))) ())
         )
    => GDefaultTableSchema (S1 f (K1 Generic.R (PrimaryKey tbl1 (g (TableFieldSchema tbl2)))) p)
                           (S1 f (K1 Generic.R (PrimaryKey tbl1 (g (Beam.TableField tbl2)))) p) where
    gDefTblSchema (_ :: Proxy (S1 f (K1 Generic.R (PrimaryKey tbl1 (g (TableFieldSchema tbl2)))) p)) (M1 (K1 fName)) = 
      M1 (K1 $ to' $ gDefTblSchema Proxy (from' fName))

defaultTableSchema :: forall tbl. 
                   ( GDefaultTableSchema (Rep (TableSchema tbl) ()) (Rep (Beam.TableSettings tbl) ())
                   , Generic (TableSchema tbl)
                   , Generic (Beam.TableSettings tbl)
                   )
                   => Beam.TableSettings tbl 
                   -> TableSchema tbl
defaultTableSchema tSettings = 
    to $ gDefTblSchema (Proxy :: Proxy (Rep (TableSchema tbl) ())) (from' tSettings)

from' :: Generic a => a -> Rep a ()
from' = from

to' :: Generic a => Rep a () -> a
to' = to

--
-- Annotating 'Table's and 'Field's after the default 'AnnotatedDatabaseSettings' has been instantiated.
--


annotateTableFields :: tbl (FieldModification (TableFieldSchema tbl)) 
                    -> EntityModification (AnnotatedDatabaseEntity be db) be (TableEntity tbl)
annotateTableFields modFields = 
    EntityModification (Endo (\(AnnotatedDatabaseEntity tbl@(AnnotatedDatabaseTable {}) e) 
      -> AnnotatedDatabaseEntity (tbl { 
       dbAnnotatedSchema = Beam.withTableModification modFields (dbAnnotatedSchema tbl) 
                            }) e))

--
-- Specifying default values
--

defaultsTo :: Show ty => ty -> FieldModification (TableFieldSchema tbl) (Maybe ty)
defaultsTo tyVal = FieldModification $ \old -> 
    case tableFieldSchema old of 
      FieldSchema ty c -> old { 
          -- Postgres converts the default values all lowercase, so we need to abide to this format.
          tableFieldSchema = FieldSchema ty $ S.singleton (Default $ T.toLower $ T.pack $ show tyVal) <> c 
        }

--
-- Specifying uniqness constrainst
--

data UniqueConstraint (tbl :: ((* -> *) -> *)) where
  U   :: (tbl (Beam.TableField tbl) -> Beam.Columnar Beam.Identity (Beam.TableField tbl ty)) 
      -> UniqueConstraint tbl
  UPK :: Beam.Beamable (PrimaryKey tbl') 
      => (tbl (Beam.TableField tbl) -> PrimaryKey tbl' (Beam.TableField tbl)) 
      -> UniqueConstraint tbl

uniqueFields :: [UniqueConstraint tbl]
             -> EntityModification (AnnotatedDatabaseEntity be db) be (TableEntity tbl)
uniqueFields us =
    EntityModification (Endo (\(AnnotatedDatabaseEntity tbl@(AnnotatedDatabaseTable {}) e) 
      -> AnnotatedDatabaseEntity (tbl { 
       dbAnnotatedConstraints = 
           let cols = concatMap (\case
                 (U f)   -> [ColumnName $ (f (tableSettings e) ^. Beam.fieldName)]
                 (UPK f) -> fieldAsColumnNames $ f (tableSettings e)
                                       ) us
               tName   = e ^. dbEntityDescriptor . dbEntityName
               conname = T.intercalate "_" (tName : map columnName cols) <> "_ukey"
           in S.insert (Unique conname (S.fromList cols)) (dbAnnotatedConstraints tbl)
                            }) e))

--
-- Specifying FK constrainst
--

data ForeignKeyConstraint (tbl :: ((* -> *) -> *)) (tbl' :: ((* -> *) -> *)) where
  References :: Beam.Beamable (PrimaryKey tbl')
             => ( tbl (Beam.TableField tbl)   -> PrimaryKey tbl' (Beam.TableField tbl)) 
             -> ( tbl' (Beam.TableField tbl') -> Beam.Columnar Beam.Identity (Beam.TableField tbl' ty))
             -> ForeignKeyConstraint tbl tbl'
  --FPK :: Beam.Beamable (PrimaryKey tbl') 
  --    => DatabaseEntity be db (TableEntity tbl')
  --    -> (tbl' (Beam.TableField tbl') -> PrimaryKey tbl' (Beam.TableField tbl')) 
  --    -> ForeignKeyConstraint tbl be db

foreignKeyOn :: Beam.Beamable tbl' 
             => DatabaseEntity be db (TableEntity tbl')
             -> [ForeignKeyConstraint tbl tbl']
             -> ReferenceAction
             -- ^ On Delete
             -> ReferenceAction
             -- ^ On Update
             -> EntityModification (AnnotatedDatabaseEntity be db) be (TableEntity tbl)
foreignKeyOn externalEntity us onDelete onUpdate =
    EntityModification (Endo (\(AnnotatedDatabaseEntity tbl@(AnnotatedDatabaseTable {}) e) 
      -> AnnotatedDatabaseEntity (tbl { 
       dbAnnotatedConstraints = 
           let colPairs = concatMap (\case
                 (References ours theirs) -> zipWith (,)
                     (fieldAsColumnNames (ours   (tableSettings e)))
                     [ColumnName (theirs (tableSettings externalEntity) ^. Beam.fieldName)]
                     ) us
               tName   = externalEntity ^. dbEntityDescriptor . dbEntityName
               conname = T.intercalate "_" (tName : map (columnName . snd) colPairs) <> "_fkey"
           in S.insert (ForeignKey conname (TableName tName) (S.fromList colPairs) onDelete onUpdate) 
                       (dbAnnotatedConstraints tbl)
                            }) e))
