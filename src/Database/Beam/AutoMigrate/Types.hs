{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Beam.AutoMigrate.Types where

import Control.DeepSeq
import Control.Applicative
import Control.Exception
import Data.ByteString.Lazy (ByteString)
import Data.Default.Class (Default(..))
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.String
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Typeable
import Database.Beam.Backend.SQL (BeamSqlBackendSyntax)
import qualified Database.Beam.Backend.SQL.AST as AST
import Database.Beam.Postgres (Pg, Postgres)
import qualified Database.Beam.Postgres.Syntax as Syntax
import GHC.Generics hiding (to)
import Control.Lens (Lens', lens, to, _Right)
import Control.Lens (preview, set)

import Control.Lens.TH

--
-- Types (sketched)
--

data Schema = Schema
  { schemaTables :: Tables,
    schemaEnumerations :: Enumerations,
    schemaSequences :: Sequences
  }
  deriving (Show, Eq, Generic)

instance NFData Schema

--
-- Enumerations
--

type Enumerations = Map EnumerationName Enumeration

newtype EnumerationName = EnumerationName
  { enumName :: Text
  }
  deriving (Show, Eq, Ord, Generic)

newtype Enumeration = Enumeration
  { enumValues :: [Text]
  }
  deriving (Show, Eq, Ord, Generic)

instance NFData EnumerationName

instance NFData Enumeration

--
-- Sequences
--

type Sequences = Map SequenceName (Maybe Sequence)

newtype SequenceName = SequenceName
  { seqName :: Text
  }
  deriving (Show, Eq, Ord, Generic)

-- For now this type is isomorphic to unit as we don't need to support anything other than plain
-- sequences.
data Sequence = Sequence
  { seqTable :: TableName,
    seqColumn :: ColumnName
  }
  deriving (Show, Eq, Ord, Generic)

instance NFData SequenceName

instance NFData Sequence

mkSequenceName :: TableName -> ColumnName -> SequenceName
mkSequenceName tname cname = SequenceName (tableName tname <> "___" <> columnName cname <> "___seq")

--
-- Tables
--

type Tables = Map TableName Table

newtype TableName = TableName
  { tableName :: Text
  }
  deriving (Show, Eq, Ord, NFData, Generic)

data TableConstraints = TableConstraints
  { primaryKeyConstraint :: Maybe (PrimaryKeyConstraint, UniqueConstraintOptions)
  , foreignKeyConstraints :: Map ForeignKey ForeignKeyConstraintOptions
  , uniqueConstraints :: Map Unique UniqueConstraintOptions
  } deriving (Eq, Show, Generic)

instance Semigroup TableConstraints where
  TableConstraints pk1 fk1 u1 <> TableConstraints pk2 fk2 u2 = TableConstraints
    (pk1 <> pk2)
    (Map.unionWith (<>) fk1 fk2)
    (Map.unionWith (<>) u1 u2)

instance NFData TableConstraints

instance IsString TableName where
  fromString = TableName . T.pack

data Table = Table
  { tableConstraints :: TableConstraints,
    tableColumns :: Columns
  }
  deriving (Eq, Show, Generic)

instance NFData Table

type Columns = Map ColumnName Column

newtype ColumnName = ColumnName
  { columnName :: Text
  }
  deriving (Show, Eq, Ord, NFData, Generic)

instance IsString ColumnName where
  fromString = ColumnName . T.pack

data NullableConstraint = Null | NotNull
  deriving (Show, Eq, Generic)

instance NFData NullableConstraint

data ColumnConstraints = ColumnConstraints
  { columnNullable :: NullableConstraint,
    columnDefault :: Maybe DefaultConstraint
  } deriving (Show, Eq, Generic)

instance NFData ColumnConstraints

data Column = Column
  { columnType :: ColumnType,
    columnConstraints :: ColumnConstraints
  }
  deriving (Show, Eq, Generic)

-- Manual instance as 'AST.DataType' doesn't derive 'NFData'.
instance NFData Column where
  rnf c = columnConstraints c `deepseq` ()

-- | Basic types for columns. We piggyback on 'beam-core' SQL types for now. Albeit they are a bit more
-- specialised (i.e, SQL specific), we are less subject from their and our representation to diverge.
data ColumnType
  = -- | Standard SQL types.
    SqlStdType AST.DataType
  | -- | Postgres specific types.
    PgSpecificType PgDataType
  | -- | An enumeration implemented with text-based encoding.
    DbEnumeration EnumerationName Enumeration
  deriving (Show, Eq, Generic)

data PgDataType
  = PgJson
  | PgJsonB
  | PgRangeInt4
  | PgRangeInt8
  | PgRangeNum
  | PgRangeTs
  | PgRangeTsTz
  | PgRangeDate
  | PgUuid
  | PgEnumeration EnumerationName
  | PgOid

deriving instance Show PgDataType

deriving instance Eq PgDataType

deriving instance Generic PgDataType

-- Newtype wrapper to be able to derive appropriate 'HasDefaultSqlDataType' for /Postgres/ enum types.
newtype PgEnum a
  = PgEnum a
  deriving (Show, Eq, Typeable, Enum, Bounded, Generic)

-- Newtype wrapper to be able to derive appropriate 'HasDefaultSqlDataType' for /textual/ enum types.
newtype DbEnum a
  = DbEnum a
  deriving (Show, Eq, Typeable, Enum, Bounded, Generic)

newtype ConstraintName = ConstraintName { unConsraintName :: Text }
  deriving (IsString, Eq, Ord, Show, NFData)

data PrimaryKeyConstraint = PrimaryKey
      -- | This set of 'Column's identifies the Table's 'PrimaryKey'.
      (Set ColumnName)
  deriving (Eq, Ord, Show, Generic)


instance Semigroup PrimaryKeyConstraint where
  PrimaryKey c1 <> PrimaryKey c2 = PrimaryKey (c1 <> c2)

instance NFData PrimaryKeyConstraint

  -- | This set of 'Column's identifies a Table's 'ForeignKey'. This is usually found in the 'tableConstraints'
    -- of the table where the foreign key is actually defined (in terms of 'REFERENCES').
    -- The set stores a (fk_column, pk_column) correspondence.
data ForeignKey = ForeignKey
  TableName
  (Set (ColumnName, ColumnName))
  deriving (Eq, Ord, Show, Generic)

instance NFData ForeignKey

data ForeignKeyConstraintOptions = ForeignKeyConstraintOptions
  { foreignKeyConstraintName :: Maybe ConstraintName -- ^ The Maybe indicates not that the constraint might not have a name, but that we aren't obligated it to name it immediately.
  , onDelete :: ReferenceAction {- onDelete -}
  , onUpdate :: ReferenceAction {- onUpdate -}
  }
  deriving (Eq, Ord, Show, Generic)

instance Default ForeignKeyConstraintOptions where
  def = ForeignKeyConstraintOptions Nothing NoAction NoAction

instance Semigroup ForeignKeyConstraintOptions where
  ForeignKeyConstraintOptions n1 d1 u1 <> ForeignKeyConstraintOptions n2 d2 u2 = ForeignKeyConstraintOptions
    (n1 <|> n2)
    (const d1 d2)
    (const u1 u2)

instance NFData ForeignKeyConstraintOptions

data Unique = Unique (Set ColumnName)
  deriving (Show, Eq, Ord, Generic)

instance NFData Unique

data UniqueConstraintOptions = UniqueConstraintOptions
  { uniqueConstraintName :: Maybe ConstraintName -- ^ The Maybe indicates not that the constraint might not have a name, but that we aren't obligated it to name it immediately.
  }
  deriving (Eq, Ord, Show, Generic)

-- | suitable default options which allows postgres to choose a constraint name
instance Default UniqueConstraintOptions where
  def = UniqueConstraintOptions Nothing

instance Semigroup UniqueConstraintOptions where
  UniqueConstraintOptions n1 <> UniqueConstraintOptions n2 = UniqueConstraintOptions
    (n1 <|> n2)

instance NFData UniqueConstraintOptions

-- | we will treat default constraint expressions specially in the case that a column is associated with a sequence.
--
-- We only ever represent two kinds of sequence, either the sequence is owned by a column which has a default of nextval(sequence); or else the sequence is not owned, and must have a name.
data DefaultConstraint
  = DefaultExpr Text
  | Autoincrement (Maybe SequenceName)
  deriving (Show, Eq, Ord, Generic)

instance NFData DefaultConstraint

data ReferenceAction
  = NoAction
  | Restrict
  | Cascade
  | SetNull
  | SetDefault
  deriving (Show, Eq, Ord, Generic)

instance NFData ReferenceAction

--
-- Modifying the 'Schema'
--

-- | A possible list of edits on a 'Schema'.
data EditAction
  = EditAction_Manual ManualEditAction
  | EditAction_Automatic AutomaticEditAction
  deriving (Show, Eq)

data ManualEditAction
  = ColumnRenamed TableName ColumnName {- old name -} ColumnName {- new name -}
  deriving (Show, Eq)

data AutomaticEditAction
  = TableAdded TableName Table
  | TableRemoved TableName


  | PrimaryKeyAdded TableName PrimaryKeyConstraint UniqueConstraintOptions
  | UniqueConstraintAdded TableName Unique UniqueConstraintOptions
  | ForeignKeyAdded TableName ForeignKey ForeignKeyConstraintOptions

  | TableConstraintRemoved TableName ConstraintName
  | RenameConstraint TableName
    ConstraintName {- old name -}
    ConstraintName {- new name -}

  | ColumnAdded TableName ColumnName Column
  | ColumnRemoved TableName ColumnName
  | ColumnTypeChanged TableName ColumnName ColumnType {- old type -} ColumnType {- new type -}
  | ColumnNullableChanged TableName ColumnName NullableConstraint {- is nullable -}
  | ColumnDefaultChanged TableName ColumnName (Maybe DefaultConstraint)
  | EnumTypeAdded EnumerationName Enumeration
  | EnumTypeRemoved EnumerationName
  | EnumTypeValueAdded EnumerationName Text {- added value -} InsertionOrder Text {- insertion point -}
  | SequenceAdded SequenceName (Maybe Sequence)
  | SequenceRemoved SequenceName
  | SequenceRenamed
    SequenceName {- old name -}
    SequenceName {- new name -}
  | SequenceSetOwner SequenceName (Maybe Sequence)
  deriving (Show, Eq)

-- | Safety rating for a given edit.
--
-- "Safety" is defined as some 'EditAction' that might cause data loss.
data EditSafety
  = Safe
  | PotentiallySlow
  | Unsafe
  deriving (Show, Eq, Ord)

defaultEditSafety :: AutomaticEditAction -> EditSafety
defaultEditSafety = \case
  SequenceRenamed {} -> Safe
  SequenceSetOwner {} -> Safe
  TableAdded {} -> Safe
  TableRemoved {} -> Unsafe
  PrimaryKeyAdded {} -> Safe
  ForeignKeyAdded {} -> Safe
  UniqueConstraintAdded {} -> Safe
  TableConstraintRemoved {} -> Safe
  RenameConstraint {} -> Safe
  ColumnAdded {} -> Safe
  ColumnRemoved {} -> Unsafe
  ColumnTypeChanged {} -> Unsafe
  ColumnNullableChanged {} -> Safe
  ColumnDefaultChanged {} -> Safe
  EnumTypeAdded {} -> Safe
  EnumTypeRemoved {} -> Unsafe
  EnumTypeValueAdded {} -> Safe
  SequenceAdded {} -> Safe
  SequenceRemoved {} -> Unsafe

data EditCondition = EditCondition
  { _editCondition_query :: BeamSqlBackendSyntax Postgres,
    _editCondition_check :: Pg EditSafety
  }

prettyEditConditionQuery :: EditCondition -> ByteString
prettyEditConditionQuery = Syntax.pgRenderSyntaxScript . Syntax.fromPgCommand . _editCondition_query

instance Eq EditCondition where
  ec1 == ec2 = prettyEditConditionQuery ec1 == prettyEditConditionQuery ec2

instance Show EditCondition where
  show ec =
    unwords
      [ "EditConditon {",
        "_editCondition_query = PgCommand {",
        "pgCommandType = ",
        show $ Syntax.pgCommandType $ _editCondition_query ec,
        "fromPgCommand = ",
        toS $ prettyEditConditionQuery ec,
        "},",
        "_editCondition_check = <check function>",
        "}"
      ]

data Edit = Edit
  { _editAction :: EditAction,
    _editCondition :: Either EditCondition EditSafety
  }
  deriving (Show, Eq)

editAction :: Lens' Edit EditAction
editAction = lens _editAction (\(Edit _ ec) ea -> Edit ea ec)

editCondition :: Lens' Edit (Either EditCondition EditSafety)
editCondition = lens _editCondition (\(Edit ea _) ec -> Edit ea ec)

editSafetyIs :: EditSafety -> Edit -> Bool
editSafetyIs s = fromMaybe False . preview (editCondition . _Right . to (== s))

mkEditWith :: (AutomaticEditAction -> EditSafety) -> AutomaticEditAction -> Edit
mkEditWith isSafe e = Edit (EditAction_Automatic e) (Right $ isSafe e)

defMkEdit :: AutomaticEditAction -> Edit
defMkEdit = mkEditWith defaultEditSafety

data InsertionOrder
  = Before
  | After
  deriving (Show, Eq, Generic)

instance NFData InsertionOrder

instance NFData EditAction where
  rnf (EditAction_Automatic ea) = rnf ea
  rnf (EditAction_Manual ea) = rnf ea

instance NFData ManualEditAction where
  rnf (ColumnRenamed tName oldName newName) = tName `deepseq` oldName `deepseq` newName `deepseq` ()

-- Manual instance as 'AST.DataType' doesn't derive 'NFData'.
instance NFData AutomaticEditAction where
  rnf (TableAdded tName tbl) = tName `deepseq` tbl `deepseq` ()
  rnf (TableRemoved tName) = rnf tName
  rnf (UniqueConstraintAdded tName tCon tOpts) = tName `deepseq` tCon `deepseq` tOpts `deepseq` ()
  rnf (ForeignKeyAdded tName tCon tOpts) = tName `deepseq` tCon `deepseq` tOpts `deepseq` ()
  rnf (PrimaryKeyAdded tName tCon tOpts) = tName `deepseq` tCon `deepseq` tOpts `deepseq` ()
  rnf (ColumnAdded tName cName col) = tName `deepseq` cName `deepseq` col `deepseq` ()
  rnf (ColumnRemoved tName colName) = tName `deepseq` colName `deepseq` ()
  rnf (ColumnTypeChanged tName colName c1 c2) = c1 `seq` c2 `seq` tName `deepseq` colName `deepseq` ()
  rnf (ColumnNullableChanged tName cName cCon) = tName `deepseq` cName `deepseq` cCon `deepseq` ()
  rnf (ColumnDefaultChanged tName colName cCon) = tName `deepseq` colName `deepseq` cCon `deepseq` ()
  rnf (EnumTypeAdded eName enum) = eName `deepseq` enum `deepseq` ()
  rnf (EnumTypeRemoved eName) = eName `deepseq` ()
  rnf (EnumTypeValueAdded eName inserted order insertionPoint) =
    eName `deepseq` inserted `deepseq` order `deepseq` insertionPoint `deepseq` ()
  rnf (SequenceAdded sName s) = sName `deepseq` s `deepseq` ()
  rnf (SequenceRemoved sName) = sName `deepseq` ()
  rnf (TableConstraintRemoved tName cName) = tName `deepseq` cName `deepseq` ()
  rnf (RenameConstraint tName cName cName') = tName `deepseq` cName `deepseq` cName' `deepseq` ()
  rnf (SequenceRenamed sName sName') = sName `deepseq` sName' `deepseq` ()
  rnf (SequenceSetOwner sName sOwner) = sName `deepseq` sOwner `deepseq` ()

-- | A possible enumerations of the reasons why a 'diff' operation might not work.
data DiffError
  = -- | The diff couldn't be completed. TODO(adn) We need extra information
    -- we can later on reify into the raw SQL queries users can try to run
    -- themselves.
    AutomaticDiffNotPossible
  | -- | Postgres doesn't support removing values from an enum.
    ValuesRemovedFromEnum EnumerationName [Text]
  deriving (Show, Generic, Eq)

instance Exception DiffError

instance NFData DiffError

--
-- Utility functions
--

noSchema :: Schema
noSchema = Schema mempty mempty mempty

noTableConstraints :: TableConstraints
noTableConstraints = TableConstraints
  { primaryKeyConstraint = Nothing
  , foreignKeyConstraints = Map.empty
  , uniqueConstraints = Map.empty
  }

noColumnConstraints :: ColumnConstraints
noColumnConstraints = ColumnConstraints Null Nothing


fmap concat $ traverse (makeLensesWith (set lensField (mappingNamer $ pure . ('_':)) defaultFieldRules))
  [ ''Schema
  , ''Sequence
  , ''TableConstraints
  , ''Table
  , ''Column
  , ''ColumnConstraints
  ]
