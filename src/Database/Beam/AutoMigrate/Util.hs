{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Database.Beam.AutoMigrate.Util where

import Control.Applicative.Lift
import Control.Monad.Except
import Data.Char
import Data.Functor.Constant
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Beam.AutoMigrate.Types (ColumnName(..), TableName(..))
import qualified Database.Beam.Schema as Beam
import Database.Beam.Schema.Tables
import Control.Lens ((^.))

--
-- Retrieving all the column names for a beam entity.
--

class HasColumnNames entity tbl where
  colNames :: tbl (Beam.TableField tbl) -> (tbl (Beam.TableField tbl) -> entity) -> [ColumnName]

instance
  Beam.Beamable (PrimaryKey tbl) =>
  HasColumnNames (PrimaryKey tbl (Beam.TableField c)) tbl
  where
  colNames field fn = map ColumnName (allBeamValues (\(Columnar' x) -> x ^. fieldName) (fn field))

instance
  Beam.Beamable (PrimaryKey tbl) =>
  HasColumnNames (PrimaryKey tbl (Beam.TableField c)) tbl'
  where
  colNames field fn = map ColumnName (allBeamValues (\(Columnar' x) -> x ^. fieldName) (fn field))

instance HasColumnNames (Beam.TableField tbl ty) tbl where
  colNames field fn = [ColumnName (fn field ^. Beam.fieldName)]

--
-- General utility functions
--

-- | Extracts the 'TableSettings' out of the input 'DatabaseEntity'.
tableSettings :: Beam.DatabaseEntity be db (TableEntity tbl) -> TableSettings tbl
tableSettings entity = dbTableSettings $ entity ^. dbEntityDescriptor

tableName :: Beam.Beamable tbl => Beam.DatabaseEntity be db (TableEntity tbl) -> TableName
tableName entity = TableName $ (entity ^. dbEntityDescriptor . dbEntityName)

-- | Extracts the primary key of a table as a list of 'ColumnName'.
pkFieldNames ::
  (Beamable (PrimaryKey tbl), Beam.Table tbl) =>
  Beam.DatabaseEntity be db (TableEntity tbl) ->
  [ColumnName]
pkFieldNames entity =
  map ColumnName (allBeamValues (\(Columnar' x) -> x ^. fieldName) (primaryKey . tableSettings $ entity))

-- | Similar to 'pkFieldNames', but it works on any entity that derives 'Beamable'.
fieldAsColumnNames :: Beamable tbl => tbl (Beam.TableField c) -> [ColumnName]
fieldAsColumnNames field = map ColumnName (allBeamValues (\(Columnar' x) -> x ^. fieldName) field)

-- | Returns /all/ the 'ColumnName's for a given 'DatabaseEntity'.
allColumnNames :: Beamable tbl => Beam.DatabaseEntity be db (TableEntity tbl) -> [ColumnName]
allColumnNames entity =
  let settings = dbTableSettings $ entity ^. dbEntityDescriptor
   in map ColumnName (allBeamValues (\(Columnar' x) -> x ^. fieldName) settings)

--
-- Reporting multiple errors at once
--
-- See https://teh.id.au/posts/2017/03/13/accumulating-errors/index.html

hoistErrors :: Either e a -> Errors e a
hoistErrors e =
  case e of
    Left es ->
      Other (Constant es)
    Right a ->
      Pure a

-- | Like 'sequence', but accumulating all errors in case of at least one 'Left'.
sequenceEither :: (Monoid e, Traversable f) => f (Either e a) -> Either e (f a)
sequenceEither =
  runErrors . traverse hoistErrors

-- | Evaluate each action in sequence, accumulating all errors in case of a failure.
-- Note that this means each action will be run independently, regardless of failure.
sequenceExceptT ::
  (Monad m, Monoid w, Traversable t) =>
  t (ExceptT w m a) ->
  ExceptT w m (t a)
sequenceExceptT es = do
  es' <- lift (traverse runExceptT es)
  ExceptT (return (sequenceEither es'))

-- NOTE(adn) Unfortunately these combinators are not re-exported by beam.

sqlOptPrec :: Maybe Word -> Text
sqlOptPrec Nothing = mempty
sqlOptPrec (Just x) = "(" <> fromString (show x) <> ")"

sqlOptCharSet :: Maybe Text -> Text
sqlOptCharSet Nothing = mempty
sqlOptCharSet (Just cs) = " CHARACTER SET " <> cs

-- | Escape a sql identifier according to the rules defined in the postgres manual
sqlEscaped :: Text -> Text
sqlEscaped t = if sqlValidUnescaped t
  then t
  else
    -- Double-quotes inside identifier names must be escaped by with an additional double-quote
    "\"" <> (T.intercalate "\"\"" $ T.splitOn "\"" t) <> "\""

-- | Check whether an identifier is valid without escaping (True) or must be escaped (False)
-- according to the postgres <https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS manual>
sqlValidUnescaped :: Text -> Bool
sqlValidUnescaped t = case T.uncons t of
  Nothing -> True
  Just (c, rest) -> validUnescapedHead c && validUnescapedTail rest && not (sqlIsReservedKeyword t)
  where
    validUnescapedHead c = c `elem` ("1234567890_"::String) || isAlpha c
    validUnescapedTail = all
      (\r -> (isAlpha r && isLower r) || r `elem` ("1234567890$_"::String)) . T.unpack

sqlIsReservedKeyword :: Text -> Bool
sqlIsReservedKeyword t = T.toCaseFold t `Set.member` postgresKeywordsReserved

-- | Reserved keywords according to
-- https://www.postgresql.org/docs/current/sql-keywords-appendix.html
postgresKeywordsReserved :: Set Text
postgresKeywordsReserved = Set.fromList $ map T.toCaseFold
  [ "ALL"
  , "ANALYSE"
  , "ANALYZE"
  , "AND"
  , "ANY"
  , "ARRAY"
  , "AS"
  , "ASC"
  , "ASYMMETRIC"
  , "BOTH"
  , "CASE"
  , "CAST"
  , "CHECK"
  , "COLLATE"
  , "COLUMN"
  , "CONSTRAINT"
  , "CREATE"
  , "CURRENT_CATALOG"
  , "CURRENT_DATE"
  , "CURRENT_ROLE"
  , "CURRENT_TIME"
  , "CURRENT_TIMESTAMP"
  , "CURRENT_USER"
  , "DEFAULT"
  , "DEFERRABLE"
  , "DESC"
  , "DISTINCT"
  , "DO"
  , "ELSE"
  , "END"
  , "EXCEPT"
  , "FALSE"
  , "FETCH"
  , "FOR"
  , "FOREIGN"
  , "FROM"
  , "GRANT"
  , "GROUP"
  , "HAVING"
  , "IN"
  , "INITIALLY"
  , "INTERSECT"
  , "INTO"
  , "LATERAL"
  , "LEADING"
  , "LIMIT"
  , "LOCALTIME"
  , "LOCALTIMESTAMP"
  , "NOT"
  , "NULL"
  , "OFFSET"
  , "ON"
  , "ONLY"
  , "OR"
  , "ORDER"
  , "PLACING"
  , "PRIMARY"
  , "REFERENCES"
  , "RETURNING"
  , "SELECT"
  , "SESSION_USER"
  , "SOME"
  , "SYMMETRIC"
  , "TABLE"
  , "THEN"
  , "TO"
  , "TRAILING"
  , "TRUE"
  , "UNION"
  , "UNIQUE"
  , "USER"
  , "USING"
  , "VARIADIC"
  , "WHEN"
  , "WHERE"
  , "WINDOW"
  , "WITH"
  ]

sqlSingleQuoted :: Text -> Text
sqlSingleQuoted t = "'" <> t <> "'"

sqlOptNumericPrec :: Maybe (Word, Maybe Word) -> Text
sqlOptNumericPrec Nothing = mempty
sqlOptNumericPrec (Just (prec, Nothing)) = sqlOptPrec (Just prec)
sqlOptNumericPrec (Just (prec, Just dec)) = "(" <> fromString (show prec) <> ", " <> fromString (show dec) <> ")"
