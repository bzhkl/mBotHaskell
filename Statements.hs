module Statements (Stmt (..), parseProgram) where
  import Parser
  import StringParser
  import SequenceParser
  import Control.Monad
  import Expressions

  data Stmt = Noop | String := Exp | String ::= Exp | If Exp Stmt | While Exp Stmt |
              Sequence [Stmt] | Block Stmt | Print Exp | For Stmt Exp Stmt Stmt
     deriving Show

  --Parse an application
  parseProgram :: Parser Stmt
  parseProgram = whitespace >> parseSequence

  --Parse a single statement
  parseStmt :: Parser Stmt
  parseStmt = parseBlock       `mplus` parseIf           `mplus` parseWhile `mplus`
              parseLineComment `mplus` parseBlockComment `mplus` parseFor   `mplus`
              parseDelimited

  parseDelimited :: Parser Stmt
  parseDelimited = let stmt = parseAssign `mplus`
                              parseChange `mplus`
                              parsePrint
                   in stmt >>= \s -> delim >> return s

  --Parse a sequence of statements, this is the only exported function of this module
  parseSequence :: Parser Stmt
  parseSequence = fmap Sequence (star parseStmt)

  --Parse a code block, a block is a nested sequence of statements
  parseBlock :: Parser Stmt
  parseBlock = fmap Block (bracket (wToken '{') parseSequence (wToken '}'))

  --Parse a variable assignment
  parseAssign :: Parser Stmt
  parseAssign = wMatch "let" >> fmap (uncurry (:=)) assignHelper

  --Parse a variable change
  parseChange :: Parser Stmt
  parseChange = fmap (uncurry (::=)) assignHelper

  --Helper function used for variable assignment and variable change
  assignHelper :: Parser (String, Exp)
  assignHelper = do
    name <- wIdentifier
    _ <- wToken '='
    ex <- parseExp
    return (name, ex)

  --Parse an if statement
  parseIf :: Parser Stmt
  parseIf = wMatch "if" >> do
    ex <- parseExpBracket
    stmt <- parseBlock
    return $ If ex stmt

  --Parse a for loop
  parseFor :: Parser Stmt
  parseFor = wMatch "for" >> do
    (assign, predicate, change) <- roundBracket (do
        a <- parseAssign
        p <- bracket delim parseExp delim
        c <- parseChange
        return (a, p, c))
    stmt <- parseBlock
    return $ For assign predicate change stmt

  --Parse a while loop
  parseWhile :: Parser Stmt
  parseWhile = wMatch "while" >> do
    ex <- parseExpBracket
    stmt <- parseBlock
    return $ While ex stmt

  --Parse a print statement
  parsePrint :: Parser Stmt
  parsePrint = wMatch "console.log" >> fmap Print parseExpBracket

  --Parse a single line comment
  parseLineComment :: Parser Stmt
  parseLineComment = match "//" >> parseLine >> return Noop

  --Parse a block comment
  parseBlockComment :: Parser Stmt
  parseBlockComment = match "/*" >> eatUntil "*/" >> return Noop

  --Parse a semicolon followed by some whitespace
  delim :: Parser Char
  delim = parseWhitespace (token ';')
