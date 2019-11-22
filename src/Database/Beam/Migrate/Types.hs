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
    columnType        :: ColumnType
  , columnConstraints :: Set SchemaConstraint
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
    | ForeignKey (Set ColumnName) (Maybe ReferenceAction) {- onDelete -} (Maybe ReferenceAction) {- onUpdate -}
      -- ^ This set of 'Column's identifies a Table's 'ForeignKey'. This is found in the 'tableConstraints'
      -- of the table where the foreign key is actually defined (in terms of 'REFERENCES').
    | IsForeignKeyOf TableName (Set ColumnName)
      -- ^ \"backward pointer\" to express the relation that the current table has some of its columns
      -- referenced in a 'ForeignKey' constraint. This is found in the 'tableConstraints' of the table
      -- where the foreign key \"points to\".
    | NotNull
    | Unique
    deriving (Show, Eq, Ord)

data ReferenceAction =
      NoAction
    | Restrict
    | Cascade
    | SetNull
    | SetDefault
    deriving (Show, Eq, Ord)

-- | A possible list of edits on a 'Schema'.
data Edit =
    TableAdded TableName Table
  | TableRemoved TableName
  | TableConstraintsAdded   TableName (Set SchemaConstraint)
  | TableConstraintsRemoved TableName (Set SchemaConstraint)
  | ColumnAdded TableName ColumnName Column
  | ColumnRemoved TableName ColumnName
  | ColumnTypeChanged ColumnName ColumnType {- old type -} ColumnType {- new type -}
  | ColumnConstraintsAdded   ColumnName (Set SchemaConstraint)
  | ColumnConstraintsRemoved ColumnName (Set SchemaConstraint)
  deriving (Show, Eq)

-- | A possible enumerations of the reasons why a 'diff' operation might not work.
data DiffError =
    AutomaticDiffNotPossible
    -- ^ The diff couldn't be completed. TODO(adn) We need extra information
    -- we can later on reify into the raw SQL queries users can try to run
    -- themselves.
    deriving (Show, Eq)
