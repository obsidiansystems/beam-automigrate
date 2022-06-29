{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import qualified Data.List as L
import Data.Proxy
import qualified Data.Text.Lazy as TL
import Database.Beam.AutoMigrate
import Database.Beam.AutoMigrate.BenchUtil (cleanDatabase, tearDownDatabase)
import Database.Beam.AutoMigrate.Postgres (getSchema)
import Database.Beam.AutoMigrate.Schema.Gen
import Database.Beam.AutoMigrate.TestUtils
import Database.Beam.AutoMigrate.Validity
import Database.Beam.Postgres
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.Transaction as Pg
import System.Environment (getArgs)
import qualified Test.Database.Beam.AutoMigrate.Arbitrary as Pretty
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.Runners
import Test.Tasty.QuickCheck as QC

import qualified PostgresqlSyntax.Parsing as PgParsing
import qualified PostgresqlSyntax.Rendering as PgRendering
import PostgresqlSyntax.Ast
import Data.Generics.Schemes (everywhere)
import Data.Generics.Aliases (mkT)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Data
import Data.Int

import qualified Database.Beam.Backend.SQL.AST as AST

sch = Schema {schemaTables = Map.fromList [(TableName {tableName = "bTwcaUsZZr"},Table {tableConstraints = Set.fromList [], tableColumns = Map.fromList [(ColumnName {columnName = "adpJSmZjWq"},Column {columnType = SqlStdType AST.DataTypeBigInt, columnConstraints = Set.fromList [NotNull,Default "1"]})]})], schemaEnumerations = Map.fromList [], schemaSequences = Map.fromList []}

doit = do
  withConnection (ConnMethod_Gargoyle "dbx") $ \conn -> do
    let mig = migrate conn sch
    printMigrationIO mig
    runMigrationUnsafe conn mig
    printMigrationIO mig

main :: IO ()
main = do
  let opts = includingOptions [Option (Proxy :: Proxy ConnMethod)]
  optset <- parseOptions [opts] (testGroup "" [])
  withConnection (lookupOption optset) $ \conn ->
    defaultMainWithIngredients (opts : defaultIngredients) $ tests conn

tests :: Pg.Connection -> TestTree
tests c = testGroup "Tests" [properties c]

properties :: Pg.Connection -> TestTree
properties conn =
  testGroup
    "Integration tests"
    [ -- We test that if we generate and apply a migration from a 'hsSchema', when we read the
      -- 'Schema' back from the DB, we should end up with the original 'hsSchema'.
      QC.testProperty "Migration roundtrip (empty DB)" $
        \hsSchema -> hsSchema /= noSchema ==> do
          dbProperty conn $ \_ -> do
            let mig = migrate conn hsSchema
            runBeamPostgres conn (unsafeRunMigration mig)
            dbSchema <- getSchema conn
            let hsSchema' = normalizeDefaultExprs hsSchema
                dbSchema' = normalizeDefaultExprs dbSchema
            pure $ whenFail (printMigrationIO mig) $ hsSchema' Pretty.=== dbSchema'
      -- We test that after a successful migration, calling 'diff' should yield no edits.
    , QC.testProperty "Diffing after a migration yields no edits" $
        \hsSchema -> hsSchema /= noSchema ==> dbProperty conn $ \_ -> liftIO $ do
          let mig = migrate conn hsSchema
          runBeamPostgres conn (unsafeRunMigration mig)
          dbSchema <- getSchema conn
          pure $ diff (normalizeDefaultExprs hsSchema) (normalizeDefaultExprs dbSchema) === Right []
    ]

-- | Execute a monadic 'Property' while also cleaning up any database's data at the end.
dbProperty :: Testable prop => Pg.Connection -> (Pg.Connection -> IO prop) -> Property
dbProperty conn prop = withMaxSuccess 50 $ monadicIO $ liftIO $ Pg.withTransactionSerializable conn $ do
  cleanDatabase conn
  r <- prop conn
  pure r

class NormalizeDefaultExprs t where
  normalizeDefaultExprs :: t -> t

instance NormalizeDefaultExprs Schema where
  normalizeDefaultExprs s = s { schemaTables = Map.map normalizeDefaultExprs (schemaTables s) }

instance NormalizeDefaultExprs Table where
  normalizeDefaultExprs t = t { tableColumns = Map.map normalizeDefaultExprs (tableColumns t)}

instance NormalizeDefaultExprs Column where
  normalizeDefaultExprs c = c { columnConstraints = Set.map normalizeDefaultExprs (columnConstraints c) }

instance NormalizeDefaultExprs ColumnConstraint where
  normalizeDefaultExprs = \case
    NotNull -> NotNull
    Default t -> Default (normalizeDefaultExpr t)

normalizeDefaultExpr :: Text -> Text
normalizeDefaultExpr t = case PgParsing.run PgParsing.aExpr t of
  Left e -> t
  Right x -> PgRendering.toText . PgRendering.aExpr . everywhere (mkT simplifyAExpr) $ x

simplifyAExpr :: AExpr -> AExpr
simplifyAExpr e = case e of
  TypecastAExpr e' t -> e'
  CExprAExpr (InParensCExpr e' Nothing) -> e'
  e | Just n <- evaluableAExprInt64 e -> CExprAExpr (AexprConstCExpr (IAexprConst n))
  _ -> e

evaluableAExprInt64 :: AExpr -> Maybe Int64
evaluableAExprInt64 e = case e of
  MinusAExpr e' -> fmap negate (evaluableAExprInt64 e')
  CExprAExpr e' -> evaluableCExprInt64 e'
  _ -> Nothing

evaluableCExprInt64 :: CExpr -> Maybe Int64
evaluableCExprInt64 e = case e of
  AexprConstCExpr (IAexprConst n) -> Just n
  AexprConstCExpr (SAexprConst s) -> case reads (T.unpack s) of
    [(n,"")] -> Just n
    _ -> Nothing
  _ -> Nothing

deriving instance Data AExpr
deriving instance Data CExpr
deriving instance Data Typename
deriving instance Data Columnref
deriving instance Data AnyName
deriving instance Data AexprConst
deriving instance Data TypenameArrayDimensions
deriving instance Data IndirectionEl
deriving instance Data SymbolicExprBinOp
deriving instance Data CaseExpr
deriving instance Data SimpleTypename
deriving instance Data Ident
deriving instance Data FuncConstArgs
deriving instance Data QualOp
deriving instance Data FuncExpr
deriving instance Data FuncName
deriving instance Data MathOp
deriving instance Data WhenClause
deriving instance Data GenericType
deriving instance Data SortBy
deriving instance Data VerbalExprBinOp
deriving instance Data SelectWithParens
deriving instance Data ConstTypename
deriving instance Data Numeric
deriving instance Data FuncArgExpr
deriving instance Data AnyOperator
deriving instance Data OverClause
deriving instance Data NullsOrder
deriving instance Data AExprReversableOp
deriving instance Data ArrayExpr
deriving instance Data Interval
deriving instance Data Bit
deriving instance Data FuncApplication
deriving instance Data QualAllOp
deriving instance Data SelectNoParens
deriving instance Data AllOp
deriving instance Data WindowSpecification
deriving instance Data Row
deriving instance Data ImplicitRow
deriving instance Data Character
deriving instance Data FuncExprCommonSubexpr
deriving instance Data AscDesc
deriving instance Data ConstCharacter
deriving instance Data BExpr
deriving instance Data FuncApplicationParams
deriving instance Data ForLockingClause
deriving instance Data FrameClause
deriving instance Data SubType
deriving instance Data ConstDatetime
deriving instance Data InExpr
deriving instance Data SelectLimit
deriving instance Data ExtractList
deriving instance Data BExprIsOp
deriving instance Data ForLockingItem
deriving instance Data WindowExclusionClause
deriving instance Data SubqueryOp
deriving instance Data SimpleSelect
deriving instance Data OverlayList
deriving instance Data FrameExtent
deriving instance Data OffsetClause
deriving instance Data ExtractArg
deriving instance Data QualifiedName
deriving instance Data WithClause
deriving instance Data PositionList
deriving instance Data FrameClauseMode
deriving instance Data LimitClause
deriving instance Data ForLockingStrength
deriving instance Data WindowDefinition
deriving instance Data FrameBound
deriving instance Data SelectFetchFirstValue
deriving instance Data SubstrList
deriving instance Data GroupByItem
deriving instance Data CommonTableExpr
deriving instance Data SelectLimitValue
deriving instance Data TrimList
deriving instance Data TableRef
deriving instance Data SubstrListFromFor
deriving instance Data PreparableStmt
deriving instance Data TrimModifier
deriving instance Data OptTempTableName
deriving instance Data TablesampleClause
deriving instance Data InsertStmt
deriving instance Data Targeting
deriving instance Data AliasClause
deriving instance Data UpdateStmt
deriving instance Data TargetEl
deriving instance Data RelationExpr
deriving instance Data DeleteStmt
deriving instance Data OnConflict
deriving instance Data WhereOrCurrentClause
deriving instance Data SelectBinOp
deriving instance Data FuncAliasClause
deriving instance Data CallStmt
deriving instance Data InsertRest
deriving instance Data SetClause
deriving instance Data RelationExprOptAlias
deriving instance Data OnConflictDo
deriving instance Data FuncTable
deriving instance Data InsertTarget
deriving instance Data ConfExpr
deriving instance Data TableFuncElement
deriving instance Data OverrideKind
deriving instance Data SetTarget
deriving instance Data JoinedTable
deriving instance Data InsertColumnItem
deriving instance Data FuncExprWindowless
deriving instance Data IndexElem
deriving instance Data RowsfromItem
deriving instance Data JoinMeth
deriving instance Data IndexElemDef
deriving instance Data JoinQual
deriving instance Data JoinType
