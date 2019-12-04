{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Database.Beam.Migrate.Types where

import           Control.Exception
import           Control.DeepSeq
import           GHC.Generics
import           Data.String
import           Data.Typeable
import           Data.Map                                 ( Map )
import           Data.Set                                 ( Set )
import           Data.Text                                ( Text )
import qualified Data.Text                               as T

import qualified Database.Beam.Backend.SQL.AST           as AST

--
-- Types (sketched)
--

data Schema = Schema { schemaTables        :: Tables
                     , schemaEnumerations  :: Enumerations
                     } deriving (Show, Eq, Generic)

instance NFData Schema

type Enumerations = Map EnumerationName Enumeration

newtype EnumerationName = EnumerationName { enumName :: Text } deriving (Show, Eq, Ord, Generic)
newtype Enumeration     = Enumeration { enumValues :: [Text] } deriving (Show, Eq, Ord, Generic)

instance NFData EnumerationName
instance NFData Enumeration

type Tables = Map TableName Table

newtype TableName = TableName { tableName :: Text } deriving (Show, Eq, Ord, NFData)

data Table = Table { tableConstraints :: Set TableConstraint
                   , tableColumns :: Columns
                   } deriving (Eq, Show, Generic)

instance NFData Table

type Columns = Map ColumnName Column

newtype ColumnName = ColumnName { columnName :: Text } deriving (Show, Eq, Ord, NFData)

instance IsString ColumnName where
    fromString = ColumnName . T.pack

data Column = Column {
    columnType        :: ColumnType
  , columnConstraints :: Set ColumnConstraint
  } deriving (Show, Eq)

-- Manual instance as 'AST.DataType' doesn't derive 'NFData'.
instance NFData Column where
    rnf c = rnf (columnConstraints c)

-- | Basic types for columns. We piggyback on 'beam-core' SQL types for now. Albeit they are a bit more
-- specialised (i.e, SQL specific), we are less subject from their and our representation to diverge.
data ColumnType = 
    SqlStdType AST.DataType
  -- ^ Standard SQL types.
  | PgSpecificType PgDataType
  -- ^ Postgres specific types.
  | DbEnumeration EnumerationName Enumeration
  -- ^ An enumeration implemented with text-based encoding.
  deriving (Show, Eq)

data PgDataType =
    PgJson
  | PgJsonB
  | PgRangeInt4
  | PgRangeInt8
  | PgRangeNum
  | PgRangeTs
  | PgRangeTsTz
  | PgRangeDate
  | PgEnumeration EnumerationName

deriving instance Show PgDataType
deriving instance Eq PgDataType

-- Newtype wrapper to be able to derive appropriate 'HasDefaultSqlDataType' for /Postgres/ enum types.
newtype PgEnum a = 
    PgEnum a deriving (Show, Eq, Typeable, Enum, Bounded)

-- Newtype wrapper to be able to derive appropriate 'HasDefaultSqlDataType' for /textual/ enum types.
newtype DbEnum a = 
    DbEnum a deriving (Show, Eq, Typeable, Enum, Bounded)

instance Semigroup Table where
  (Table c1 t1) <> (Table c2 t2) = Table (c1 <> c2) (t1 <> t2)

instance Monoid Table where
  mempty = Table mempty mempty

type ConstraintName = Text

data TableConstraint =
      PrimaryKey ConstraintName (Set ColumnName)
      -- ^ This set of 'Column's identifies the Table's 'PrimaryKey'.
    | ForeignKey ConstraintName TableName (Set (ColumnName, ColumnName)) ReferenceAction {- onDelete -} ReferenceAction {- onUpdate -}
      -- ^ This set of 'Column's identifies a Table's 'ForeignKey'. This is usually found in the 'tableConstraints'
      -- of the table where the foreign key is actually defined (in terms of 'REFERENCES').
      -- The set stores a (fk_column, pk_column) correspondence.
    | IsForeignKeyOf TableName (Set (ColumnName, ColumnName))
      -- ^ \"backward pointer\" to express the relation that the current table has some of its columns
      -- referenced in a 'ForeignKey' constraint. This is usually found in the 'tableConstraints' of the table
      -- where the foreign key \"points to\".
    | Unique ConstraintName (Set ColumnName)
    deriving (Show, Eq, Ord, Generic)

instance NFData TableConstraint

data ColumnConstraint =
      NotNull
    | Default Text {- the actual default -}
    deriving (Show, Eq, Ord, Generic)

instance NFData ColumnConstraint

data ReferenceAction =
      NoAction
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
data Edit =
    TableAdded TableName Table
  | TableRemoved TableName
  | TableConstraintAdded   TableName TableConstraint
  | TableConstraintRemoved TableName TableConstraint
  | ColumnAdded TableName ColumnName Column
  | ColumnRemoved TableName ColumnName
  | ColumnTypeChanged TableName ColumnName ColumnType {- old type -} ColumnType {- new type -}
  | ColumnConstraintAdded   TableName ColumnName ColumnConstraint
  | ColumnConstraintRemoved TableName ColumnName ColumnConstraint
  | EnumTypeAdded       EnumerationName Enumeration
  | EnumTypeRemoved     EnumerationName
  | EnumTypeValueAdded  EnumerationName Text {- added value -} InsertionOrder Text {- insertion point -}
  deriving (Show, Eq)

data InsertionOrder = 
    Before 
  | After deriving (Show, Eq, Generic)

instance NFData InsertionOrder

-- Manual instance as 'AST.DataType' doesn't derive 'NFData'.
instance NFData Edit where
  rnf (TableAdded tName tbl) = tName `deepseq` tbl `deepseq` ()
  rnf (TableRemoved tName) = rnf tName
  rnf (TableConstraintAdded   tName tCon) = tName `deepseq` tCon `deepseq` ()
  rnf (TableConstraintRemoved tName tCon) = tName `deepseq` tCon `deepseq` ()
  rnf (ColumnAdded tName cName col) = tName `deepseq` cName `deepseq` col `deepseq` ()
  rnf (ColumnRemoved tName colName) = tName `deepseq` colName `deepseq` ()
  rnf (ColumnTypeChanged tName colName c1 c2) = c1 `seq` c2 `seq` tName `deepseq` colName `deepseq` ()
  rnf (ColumnConstraintAdded   tName cName cCon) = tName `deepseq` cName `deepseq` cCon `deepseq` ()
  rnf (ColumnConstraintRemoved tName colName cCon) = tName `deepseq` colName `deepseq` cCon `deepseq` ()
  rnf (EnumTypeAdded       eName enum) = eName `deepseq` enum `deepseq` ()
  rnf (EnumTypeRemoved     eName) = eName `deepseq` ()
  rnf (EnumTypeValueAdded  eName inserted order insertionPoint) = 
      eName `deepseq` inserted `deepseq` order `deepseq` insertionPoint `deepseq` ()

-- | A possible enumerations of the reasons why a 'diff' operation might not work.
data DiffError =
    AutomaticDiffNotPossible
    -- ^ The diff couldn't be completed. TODO(adn) We need extra information
    -- we can later on reify into the raw SQL queries users can try to run
    -- themselves.
  | ValuesRemovedFromEnum EnumerationName [Text]
  -- ^ Postgres doesn't support removing values from an enum.
  deriving (Show, Generic, Eq)

instance Exception DiffError
instance NFData DiffError

--
-- Utility functions
--

noSchema :: Schema
noSchema = Schema mempty mempty

noTableConstraints :: Set TableConstraint
noTableConstraints = mempty

noColumnConstraints :: Set ColumnConstraint
noColumnConstraints = mempty
