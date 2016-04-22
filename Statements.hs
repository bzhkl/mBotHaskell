module Statements (parseStmt) where
  import Parser
  import StringParser
  import SequenceParser
  import Expressions

  data Stmt = Nop | String := Exp | If Exp Stmt | While Exp Stmt | Block [Stmt] | Print Exp
     deriving Show

  parseStmt :: Parser Stmt
  parseStmt = parseAssign +++ parseIf +++ parseWhile +++ parseBlock +++ parsePrint

  parseAssign :: Parser Stmt
  parseAssign = do
    wMatch "let"
    name <- identifier
    wToken '='
    ex <- parseExp
    wToken ';'
    return $ name := ex

  parseIf :: Parser Stmt
  parseIf = do
    wMatch "if"
    ex <-parseExp
    stmt <- parseBlock
    return $ If ex stmt

  parseWhile:: Parser Stmt
  parseWhile = do
    wMatch "while"
    ex <-parseExp
    stmt <- parseBlock
    return $ While ex stmt

  parseBlock :: Parser Stmt
  parseBlock = do
    wToken '{'
    stmts <- star parseStmt
    wToken '}'
    return $ Block stmts

  parsePrint :: Parser Stmt
  parsePrint = do
    wMatch "console.log("
    ex <- parseExp
    wToken ')'
    return $ Print ex
