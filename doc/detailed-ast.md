# About AstType

For each following type, there exists a field `AstType` of this value.

```
{
  AstType = "BooleanExpr",
  Tokens = foo,
  Value = bar,
}
```

## BinopExpr

 * Lhs
 * Op
 * OperatorPrecedence
 * Rhs
 * Tokens

## BooleanExpr
 * Tokens
 * Value

## CallExpr
 * Base
 * Tokens

## ConstructorExpr
 * EntryList
 * Tokens

## DotsExpr
 * Tokens

## Function
 * Arguments
 * Body
 * IsLocal = true|false
 * Name
 * Scope
 * Tokens
 * VarArg = true|false

## IndexExpr
 * Base
 * Index
 * Tokens

## MemberExpr
 * Base
 * Ident
 * Indexer
 * Tokens

## NilExpr
 * Tokens

## NumberExpr
 * Tokens
 * Value

## Parentheses
 * Inner
 * Tokens

## StringCallExpr
 * Base
 * Tokens

## StringExpr
 * Tokens
 * Value

## TableCallExpr
 * Base
 * Tokens

## UnopExpr
 * Op
 * OperatorPrecedence
 * Rhs
 * Tokens

## VarExpr
 * Name
 * Tokens
 * Variable

## AssignmentStatement
 * Lhs
 * Rhs
 * Tokens

## BreakStatement
 * Tokens

## CallStatement
 * Expression
 * Tokens

## DoStatement
 * Body
 * Tokens

## Eof
 * Tokens

## GenericForStatement
 * Body
 * Generators
 * Scope
 * Tokens
 * VariableList

## GotoStatement
 * Label
 * Tokens

## IfStatement
 * Clauses
 * Tokens

## LabelStatement
 * Label
 * Tokens

## LocalStatement
 * InitList
 * LocalList
 * Tokens

## NumericForStatement
 * Body
 * End
 * Scope
 * Start
 * Tokens
 * Variable

## RepeatStatement
 * Body
 * End
 * Scope
 * Start
 * Tokens
 * Variable

## ReturnStatement
 * Tokens

## WhileStatement
 * Body
 * Condition
 * Tokens

## Statlist
 * Body =
 * * Arguments =
 * Scope =
 * Tokens =

