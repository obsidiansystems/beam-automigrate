{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Database.Beam.AutoMigrate.Types where

import Control.DeepSeq
import Control.Exception
import Data.ByteString.Lazy (ByteString)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.String
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Database.Beam.Backend.SQL (BeamSqlBackendSyntax)
import qualified Database.Beam.Backend.SQL.AST as AST
import Database.Beam.Postgres (Pg, Postgres)
import qualified Database.Beam.Postgres.Syntax as Syntax
import GHC.Generics hiding (to)
import Lens.Micro (Lens', lens, to, _Right)
import Lens.Micro.Extras (preview)

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

type Sequences = Map SequenceName Sequence

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

parseSequenceName :: SequenceName -> Maybe (TableName, ColumnName)
parseSequenceName (SequenceName sName) = case T.splitOn "___" sName of
  [tName, cName, "seq"] -> Just (TableName tName, ColumnName cName)
  _ -> Nothing

--
-- Tables
--

type Tables = Map TableName Table

newtype TableName = TableName
  { tableName :: Text
  }
  deriving (Show, Eq, Ord, NFData, Generic)

instance IsString TableName where
  fromString = TableName . T.pack

data Table = Table
  { tableConstraints :: Set TableConstraint,
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

data Column = Column
  { columnType :: ColumnType,
    columnConstraints :: Set ColumnConstraint
  }
  deriving (Show, Eq, Generic)

-- Manual instance as 'AST.DataType' doesn't derive 'NFData'.
instance NFData Column where
  rnf c = rnf (columnConstraints c)

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
  | PgPoint
  | PgLine
  | PgLineSegment
  | PgBox
  | PgPath
  | PgPolygon
  | PgCircle

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

instance Semigroup Table where
  (Table c1 t1) <> (Table c2 t2) = Table (c1 <> c2) (t1 <> t2)

instance Monoid Table where
  mempty = Table mempty mempty

type ConstraintName = Text

data TableConstraint
  = -- | This set of 'Column's identifies the Table's 'PrimaryKey'.
    PrimaryKey ConstraintName (Set ColumnName)
  | -- | This set of 'Column's identifies a Table's 'ForeignKey'. This is usually found in the 'tableConstraints'
    -- of the table where the foreign key is actually defined (in terms of 'REFERENCES').
    -- The set stores a (fk_column, pk_column) correspondence.
    ForeignKey ConstraintName TableName (Set (ColumnName, ColumnName)) ReferenceAction {- onDelete -} ReferenceAction {- onUpdate -}
  | Unique ConstraintName (Set ColumnName)
  deriving (Show, Eq, Ord, Generic)

instance NFData TableConstraint

data ColumnConstraint
  = NotNull
  | Default Text {- the actual default -}
  deriving (Show, Eq, Ord, Generic)

instance NFData ColumnConstraint

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
  = TableAdded TableName Table
  | TableRemoved TableName
  | TableConstraintAdded TableName TableConstraint
  | TableConstraintRemoved TableName TableConstraint
  | ColumnAdded TableName ColumnName Column
  | ColumnRemoved TableName ColumnName
  | ColumnTypeChanged TableName ColumnName ColumnType {- old type -} ColumnType {- new type -}
  | ColumnConstraintAdded TableName ColumnName ColumnConstraint
  | ColumnConstraintRemoved TableName ColumnName ColumnConstraint
  | EnumTypeAdded EnumerationName Enumeration
  | EnumTypeRemoved EnumerationName
  | EnumTypeValueAdded EnumerationName Text {- added value -} InsertionOrder Text {- insertion point -}
  | SequenceAdded SequenceName Sequence
  | SequenceRemoved SequenceName
  deriving (Show, Eq)

-- | Safety rating for a given edit.
--
-- "Safety" is defined as some 'EditAction' that might cause data loss.
data EditSafety
  = Safe
  | PotentiallySlow
  | Unsafe
  deriving (Show, Eq, Ord)

defaultEditSafety :: EditAction -> EditSafety
defaultEditSafety = \case
  TableAdded {} -> Safe
  TableRemoved {} -> Unsafe
  TableConstraintAdded {} -> Safe
  TableConstraintRemoved {} -> Safe
  ColumnAdded {} -> Safe
  ColumnRemoved {} -> Unsafe
  ColumnTypeChanged {} -> Unsafe
  ColumnConstraintAdded {} -> Safe
  ColumnConstraintRemoved {} -> Safe
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

mkEditWith :: (EditAction -> EditSafety) -> EditAction -> Edit
mkEditWith isSafe e = Edit e (Right $ isSafe e)

defMkEdit :: EditAction -> Edit
defMkEdit = mkEditWith defaultEditSafety

data InsertionOrder
  = Before
  | After
  deriving (Show, Eq, Generic)

instance NFData InsertionOrder

-- Manual instance as 'AST.DataType' doesn't derive 'NFData'.
instance NFData EditAction where
  rnf (TableAdded tName tbl) = tName `deepseq` tbl `deepseq` ()
  rnf (TableRemoved tName) = rnf tName
  rnf (TableConstraintAdded tName tCon) = tName `deepseq` tCon `deepseq` ()
  rnf (TableConstraintRemoved tName tCon) = tName `deepseq` tCon `deepseq` ()
  rnf (ColumnAdded tName cName col) = tName `deepseq` cName `deepseq` col `deepseq` ()
  rnf (ColumnRemoved tName colName) = tName `deepseq` colName `deepseq` ()
  rnf (ColumnTypeChanged tName colName c1 c2) = c1 `seq` c2 `seq` tName `deepseq` colName `deepseq` ()
  rnf (ColumnConstraintAdded tName cName cCon) = tName `deepseq` cName `deepseq` cCon `deepseq` ()
  rnf (ColumnConstraintRemoved tName colName cCon) = tName `deepseq` colName `deepseq` cCon `deepseq` ()
  rnf (EnumTypeAdded eName enum) = eName `deepseq` enum `deepseq` ()
  rnf (EnumTypeRemoved eName) = eName `deepseq` ()
  rnf (EnumTypeValueAdded eName inserted order insertionPoint) =
    eName `deepseq` inserted `deepseq` order `deepseq` insertionPoint `deepseq` ()
  rnf (SequenceAdded sName s) = sName `deepseq` s `deepseq` ()
  rnf (SequenceRemoved sName) = sName `deepseq` ()

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

noTableConstraints :: Set TableConstraint
noTableConstraints = mempty

noColumnConstraints :: Set ColumnConstraint
noColumnConstraints = mempty
