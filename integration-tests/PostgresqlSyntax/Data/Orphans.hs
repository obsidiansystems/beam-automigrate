{-|
Description:
  Orphan Data instances for PostgresqlSyntax

These instances are being upstreamed <here https://github.com/nikita-volkov/postgresql-syntax/pull/6> and, once they are, this module will be removed.
-}
{-# Language CPP #-}
{-# Language DeriveDataTypeable #-}
{-# Language StandaloneDeriving #-}
{-# options_ghc -fno-warn-orphans #-}
module PostgresqlSyntax.Data.Orphans where

import Data.Data
import PostgresqlSyntax.Ast

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
#if MIN_VERSION_postgresql_syntax(0,4,1)
deriving instance Data CallStmt
#endif
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
