-- | a fragment of a parser that can parse some of the sql that appears in postgres schema catalogs.
--
-- see "postgres/src/backend/parser/gram.y"
--     "postgres/src/backend/parser/scan.l"
-- for details.
--
module Database.Beam.AutoMigrate.Parser where


import Control.Applicative
import Data.Text (Text)
import Data.Char (toLower)
import qualified Data.Text as T
import Database.Beam.AutoMigrate.Types
import Data.Attoparsec.Text
import Text.Parser.Token (token, parens)
import qualified Text.Parser.Token as Parsers

parseDefaultExpr :: Text -> DefaultConstraint
parseDefaultExpr txt =
  case parseOnly defaultExprSyntax txt of
    Right ("nextval", (seqNm, "regclass")) ->
      Autoincrement (Just (SequenceName seqNm))
    _bad ->
      DefaultExpr txt

type DefaultExprSyntax = (SQL_IDENT, (SQL_STRING, SQL_IDENT))

defaultExprSyntax :: Parser DefaultExprSyntax
defaultExprSyntax = (,) <$> sqlIdent <*> parens ((,) <$> strQuotedIdent <* token "::" <*> sqlIdent)


type SQL_IDENT = Text
type SQL_STRING = Text

sqlIdent :: Parser SQL_IDENT
sqlIdent = token $
  (T.pack <$> (char '"' *> many ('"' <$ "\"\"" <|> notChar '"') <* char '"'))
  <|>
  (T.pack . fmap toLower <$> liftA2 (:) (satisfy (inClass identStart)) (many $ satisfy (inClass identCont)))
  where
    identStart = "_A-Za-z"
    identCont = identStart <> "0-9"

sqlStrLiteral :: Parser SQL_STRING
sqlStrLiteral = Parsers.stringLiteral'

strQuotedIdent :: Parser SQL_IDENT
strQuotedIdent = do
  quotedIdent <- sqlStrLiteral
  case parseOnly sqlIdent quotedIdent of
    Right ok -> pure ok
    Left bad -> fail $ "strQuotedIdent: " <> bad
