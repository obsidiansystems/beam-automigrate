{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.AutoMigrate.Util where

import Control.Applicative.Lift
import Control.Monad.Except
import Control.Monad.Trans.Class (lift)
import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.Functor.Constant
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)
import Database.Beam.AutoMigrate.Types (ColumnName (..), SequenceName (..), TableName (..))
import Numeric (showHex)
import qualified Database.Beam.Schema as Beam
import Database.Beam.Schema.Tables
import Lens.Micro ((^.))

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

-- | Limit identifiers to the maximum byte length that PostgreSQL permits.
maxIdentifierBytes :: Int
maxIdentifierBytes = 63

-- | Check that an identifier meets these requirements:
--
-- * The identifier is not empty.
-- * The identifier contains no NUL characters.
-- * The UTF-8 encoding contains at most 63 bytes.
--
-- SQL rendering uses double quotes, so reserved words and unusual characters are valid.
isValidIdentifier :: Text -> Bool
isValidIdentifier t =
  not (T.null t)
    && not (T.any (== '\NUL') t)
    && BS.length (TE.encodeUtf8 t) <= maxIdentifierBytes

-- | Quote an SQL identifier to preserve its name.
-- Always use double quotes for reserved words, names that start with digits, and names with mixed case.
sqlEscaped :: Text -> Text
sqlEscaped t = "\"" <> T.replace "\"" "\"\"" t <> "\""

-- | Escape a string literal independently of @standard_conforming_strings@.
-- Use the @E'...'@ form to escape apostrophes and backslashes.
sqlSingleQuoted :: Text -> Text
sqlSingleQuoted t = "E'" <> T.concatMap escapeChar t <> "'"
  where
    escapeChar c = case c of
      '\'' -> "''"
      '\\' -> "\\\\"
      _ -> T.singleton c

-- | Define the initial hash value and multiplier for FNV-1a-64.
fnvOffsetBasis, fnvPrime :: Word64
fnvOffsetBasis = 14695981039346656037
fnvPrime = 1099511628211

-- | Generate the same bounded identifier from the same inputs.
--
-- The first argument contains the complete readable name.
-- The second argument lists the semantic components, including the resource kind, for the FNV-1a-64 hash.
-- Keep names of at most 63 UTF-8 bytes unchanged.
--
-- For longer names, keep a complete UTF-8 prefix of at most 46 bytes.
-- Append @_@ followed by the hash as 16 lowercase hexadecimal digits.
-- The result contains at most 63 bytes.
generatedIdentifier :: Text -> [Text] -> Text
generatedIdentifier candidate components
  | BS.length cBytes <= maxIdentifierBytes = candidate
  | otherwise =
      let digest = T.justifyRight 16 '0' (T.pack (showHex (hashComponents components) ""))
       in utf8Prefix 46 cBytes <> "_" <> digest
  where
    cBytes = TE.encodeUtf8 candidate

    -- Keep the longest complete UTF-8 prefix that fits within @n@ bytes.
    utf8Prefix :: Int -> BS.ByteString -> Text
    utf8Prefix n bs =
      let start = BS.take n bs
       in case TE.decodeUtf8' start of
            Right t -> t
            Left _ -> case BS.unsnoc start of
              Nothing -> ""
              Just (shorter, _) -> utf8Prefix (BS.length shorter) bs

    -- Encode each component with its decimal UTF-8 byte length, a colon, and its bytes before hashing.
    hashComponents :: [Text] -> Word64
    hashComponents = foldl step fnvOffsetBasis
      where
        step acc comp =
          let bs = TE.encodeUtf8 comp
              header = TE.encodeUtf8 (T.pack (show (BS.length bs)) <> ":")
           in BS.foldl' fnvByte (BS.foldl' fnvByte acc header) bs
        fnvByte h b = (h `xor` fromIntegral b) * fnvPrime

-- | Order FK pairs by (local, remote) name. Pairing must happen before this
-- sort; the two sides are never sorted independently.
canonicalizeForeignKeyPairs :: NonEmpty (ColumnName, ColumnName) -> NonEmpty (ColumnName, ColumnName)
canonicalizeForeignKeyPairs =
  NE.fromList . sortOn (\(l, r) -> (columnName l, columnName r)) . NE.toList

-- | The deterministic constraint name for a foreign key, derived from every
-- semantic component: local table, all paired columns and the target table.
foreignKeyName :: TableName -> NonEmpty (ColumnName, ColumnName) -> TableName -> Text
foreignKeyName localTable pairs targetTable =
  generatedIdentifier candidate components
  where
    pairList = NE.toList pairs
    locals = map (unColumnName . fst) pairList
    remotes = map (unColumnName . snd) pairList
    candidate =
      T.intercalate
        "_"
        ([unTableName localTable] <> locals <> [unTableName targetTable] <> remotes <> ["fkey"])
    components =
      [ "foreign-key",
        unTableName localTable,
        unTableName targetTable,
        T.pack (show (length pairList))
      ]
        <> concatMap (\(l, r) -> [unColumnName l, unColumnName r]) pairList

    unTableName (TableName t) = t
    unColumnName (ColumnName c) = c

-- | Name the companion sequence for a serial column.
-- Keep @table___column___seq@ when it fits within 63 UTF-8 bytes to preserve existing sequence names.
-- Use 'generatedIdentifier' to shorten longer names.
mkSequenceName :: TableName -> ColumnName -> SequenceName
mkSequenceName tname cname =
  let candidate = unTable tname <> "___" <> unCol cname <> "___seq"
   in SequenceName (generatedIdentifier candidate ["sequence", unTable tname, unCol cname])
  where
    unTable (TableName t) = t
    unCol (ColumnName c) = c

sqlOptNumericPrec :: Maybe (Word, Maybe Word) -> Text
sqlOptNumericPrec Nothing = mempty
sqlOptNumericPrec (Just (prec, Nothing)) = sqlOptPrec (Just prec)
sqlOptNumericPrec (Just (prec, Just dec)) = "(" <> fromString (show prec) <> ", " <> fromString (show dec) <> ")"
