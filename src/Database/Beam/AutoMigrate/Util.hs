{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Beam.AutoMigrate.Util where

import Control.Applicative.Lift
import Control.Monad.Except
import Control.Monad.Trans.Class (lift)
import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.Functor.Constant
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

-- | PostgreSQL's hard limit on the length of an identifier, in bytes.
maxIdentifierBytes :: Int
maxIdentifierBytes = 63

-- | Validate a PostgreSQL identifier: non-empty, no NUL bytes, at most
-- 63 UTF-8 bytes long. Reserved words and unusual characters are fine, because
-- we always double-quote identifiers when rendering SQL.
isValidIdentifier :: Text -> Bool
isValidIdentifier t =
  not (T.null t)
    && not (T.any (== '\NUL') t)
    && BS.length (TE.encodeUtf8 t) <= maxIdentifierBytes

-- | Escape a sql identifier according to the rules defined in the postgres
-- manual. We always double-quote, so reserved words, digit-leading names and
-- mixed-case names are all handled uniformly.
sqlEscaped :: Text -> Text
sqlEscaped t = "\"" <> T.replace "\"" "\"\"" t <> "\""

-- | Emit an @E'...'@ string literal, escaping both apostrophes and backslashes,
-- independent of @standard_conforming_strings@.
sqlSingleQuoted :: Text -> Text
sqlSingleQuoted t = "E'" <> T.concatMap escapeChar t <> "'"
  where
    escapeChar c = case c of
      '\'' -> "''"
      '\\' -> "\\\\"
      _ -> T.singleton c

-- | FNV-1a-64 parameters.
fnvOffsetBasis, fnvPrime :: Word64
fnvOffsetBasis = 14695981039346656037
fnvPrime = 1099511628211

-- | Build a deterministic, collision-averse identifier.
--
-- The first argument is the /complete readable candidate/; the second is the
-- list of semantic components (including the resource kind) used to derive the
-- digest for overlong candidates. Candidates of at most 63 UTF-8 bytes are
-- preserved verbatim. Longer candidates get a @_@ plus a 16-digit lowercase
-- FNV-1a-64 digest of the components appended to a UTF-8-boundary-truncated
-- prefix of at most 46 bytes, so the result is always within the PostgreSQL
-- identifier limit.
generatedIdentifier :: Text -> [Text] -> Text
generatedIdentifier candidate components
  | BS.length cBytes <= maxIdentifierBytes = candidate
  | otherwise =
      let digest = T.justifyRight 16 '0' (T.pack (showHex (hashComponents components) ""))
       in utf8Prefix 46 cBytes <> "_" <> digest
  where
    cBytes = TE.encodeUtf8 candidate

    -- Longest complete UTF-8 prefix of at most @n@ bytes.
    utf8Prefix :: Int -> BS.ByteString -> Text
    utf8Prefix n bs =
      let start = BS.take n bs
       in case TE.decodeUtf8' start of
            Right t -> t
            Left _ -> case BS.unsnoc start of
              Nothing -> ""
              Just (shorter, _) -> utf8Prefix (BS.length shorter) bs

    -- Hash components as decimal UTF-8 byte length, colon, then the bytes.
    hashComponents :: [Text] -> Word64
    hashComponents = foldl step fnvOffsetBasis
      where
        step acc comp =
          let bs = TE.encodeUtf8 comp
              header = TE.encodeUtf8 (T.pack (show (BS.length bs)) <> ":")
           in BS.foldl' fnvByte (BS.foldl' fnvByte acc header) bs
        fnvByte h b = (h `xor` fromIntegral b) * fnvPrime

-- | The identifier used for the companion sequence of a serial column.
-- Valid-length candidates keep the historical @table___column___seq@ spelling,
-- so existing databases' sequences are not spuriously replaced; overlong
-- candidates fall back to 'generatedIdentifier'.
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
