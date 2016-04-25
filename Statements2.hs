module Statements2 (parseStmt, Stmt, parseSequence, parseStmts) where
  import Parser
  import StringParser
  import Expressions

  data Stmt = String := Exp | Sequence [Stmt]
     deriving Show

  parseStmt :: Parser Stmt
  parseStmt = parseAssign +++ parseSequence

  parseAssign :: Parser Stmt
  parseAssign = do
    wMatch "let"
    name <- wIdentifier
    wToken '='
    ex <- parseExp
    return $ name := ex

  parseStmts :: Parser [Stmt]
  parseStmts = (do
    x <- parseStmt
    wToken ';'
    xs <- parseStmts
    return (x:xs))
    +++ return []

  parseSequence :: Parser Stmt
  parseSequence = do
    stmts <- parseStmts
    return $ Sequence stmts
