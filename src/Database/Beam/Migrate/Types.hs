module Database.Beam.Migrate.Types where

import           Data.Map                       ( Map )
import           Data.Set                       ( Set )
import           Data.Text                      ( Text )

import qualified Database.Beam.Backend.SQL.AST as AST

--
-- Types (sketched)
--

newtype Schema = Schema { schemaTables :: Tables } deriving (Show, Eq)

type Tables = Map TableName Table

newtype TableName = TableName { tableName :: Text } deriving (Show, Eq, Ord)

data Table = Table { tableConstraints :: Set SchemaConstraint
                   , tableColumns :: Columns
                   } deriving (Eq, Show)

type Columns = Map ColumnName Column

newtype ColumnName = ColumnName { columnName :: Text } deriving (Show, Eq, Ord)

data Column = Column {
    columnType       :: ColumnType
  , columnConstrains :: Set SchemaConstraint
  } deriving (Show, Eq)

-- | Basic types for columns. We piggyback on 'beam-core' SQL types for now. Albeit they are a bit more
-- specialised (i.e, SQL specific), we are less subject from their and our representation to diverge.
type ColumnType = AST.DataType

instance Semigroup Table where
  (Table c1 t1) <> (Table c2 t2) = Table (c1 <> c2) (t1 <> t2)

instance Monoid Table where
  mempty = Table mempty mempty


noSchemaConstraints :: Set SchemaConstraint
noSchemaConstraints = mempty

data SchemaConstraint =
    PrimaryKey (Set ColumnName)
    -- ^ This set of 'Column's identifies the Table's 'PrimaryKey'.
    deriving (Show, Eq, Ord)

-- | A possible list of edits on a 'Schema'.
data Edit =
    ColumnAdded TableName ColumnName Column
  | ColumnRemoved TableName ColumnName
  | TableAdded TableName Table
  | TableRemoved TableName

-- | A possible enumerations of the reasons why a 'diff' operation might not work.
data DiffError =
    AutomaticDiffNotPossible
    -- ^ The diff couldn't be completed. TODO(adn) We need extra information
    -- we can later on reify into the raw SQL queries users can try to run
    -- themselves.
