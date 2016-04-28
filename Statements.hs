module Statements (Stmt (..), parseSequence) where
  import Parser
  import StringParser
  import SequenceParser
  import Control.Monad
  import Expressions

  data Stmt = Noop | String := Exp | String ::= Exp | If Exp Stmt | While Exp Stmt | Sequence [Stmt] | Block Stmt | Print Exp
     deriving Show

  --Parse a single statement
  parseStmt :: Parser Stmt
  parseStmt = parseAssign `mplus` parseChange `mplus` parsePrint `mplus` parseBlock `mplus` parseIf `mplus` parseWhile `mplus` parseLineComment `mplus` parseBlockComment

  --Parse a sequence of statements, this is the only exported function of this module
  parseSequence :: Parser Stmt
  parseSequence = fmap Sequence (star parseStmt)

  --Parse a code block, a block is a nested sequence of statements
  parseBlock :: Parser Stmt
  parseBlock = fmap Block (bracket (wToken '{') parseSequence (wToken '}'))

  --Parse a variable assignment
  parseAssign :: Parser Stmt
  parseAssign = do
    wMatch "let"
    whitespace
    name <- identifier
    wToken '='
    ex <- parseExp
    delim
    return $ name := ex

  parseChange :: Parser Stmt
  parseChange = do
    name <- wIdentifier
    wToken '='
    ex <- parseExp
    delim
    return $ name ::= ex

  --Parse an if statement
  parseIf :: Parser Stmt
  parseIf = do
    wMatch "if"
    ex <- bracket (wToken '(') parseExp (wToken ')')
    stmt <- parseBlock
    return $ If ex stmt

  --Parse a while loop
  parseWhile:: Parser Stmt
  parseWhile = do
    wMatch "while"
    ex <- bracket (wToken '(') parseExp (wToken ')')
    stmt <- parseBlock
    return $ While ex stmt

  --Parse a print statement
  parsePrint :: Parser Stmt
  parsePrint = do
    wMatch "console.log"
    ex <- bracket (wToken '(') parseExp (wToken ')')
    delim
    return $ Print ex

  --Parse a single line comment
  parseLineComment :: Parser Stmt
  parseLineComment = do
    wMatch "//"
    parseLine
    return Noop

  --Parse a block comment
  parseBlockComment :: Parser Stmt
  parseBlockComment = do
    wMatch "/*"
    eatUntil "*/"
    return Noop

  --Parse a statement delimiter, this is a semicolon or a newline.
  delim :: Parser Char
  delim = wToken ';'
