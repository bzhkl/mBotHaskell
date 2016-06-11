module Statements (Stmt (..), Direction (..), Led(..), Sensor(..), parseProgram) where
  import Prelude hiding (Left, Right)
  import Parser
  import StringParser
  import SequenceParser
  import Control.Monad
  import Expressions

  data Stmt = Noop | Name := Exp | Name ::= Exp | If Exp Stmt | While Exp Stmt |
              Sequence [Stmt] | Block Stmt | Print Exp | For Stmt Exp Stmt Stmt |
              Move Direction Exp | SetLed Led Exp Exp Exp | ReadSensor Sensor Name |
              Wait Exp | IfElse Exp Stmt Stmt
     deriving Show

  data Sensor = Line | Distance
      deriving Show
  data Direction = Forward | Backward | Left | Right
     deriving Show
  data Led = Led1 | Led2
     deriving Show

  --Parse an application
  parseProgram :: Parser Stmt
  parseProgram = whitespace >> parseSequence

  --Parse a single statement
  parseStmt :: Parser Stmt
  parseStmt = parseBlock       `mplus` parseIf           `mplus` parseWhile    `mplus`
              parseLineComment `mplus` parseBlockComment `mplus` parseFor      `mplus`
              parseDelimited   `mplus` parseIfElse

  --Combine the semicolon ended parse functions
  parseDelimited :: Parser Stmt
  parseDelimited = let stmt = parseDeclare  `mplus`
                              parseChange   `mplus`
                              parsePrint    `mplus`
                              parseMoves    `mplus`
                              parseLeds     `mplus`
                              parseSensors  `mplus`
                              parseWait
                   in stmt >>= \s -> delim >> return s

  --Parse an MBot setLed statement
  parseLeds :: Parser Stmt
  parseLeds = parseLed "setLed1" Led1 `mplus`
              parseLed "setLed2" Led2
        where parseLed name led = do
                _ <- wMatch name
                [r, g, b] <- parseExpBracketN 3
                return $ SetLed led r g b

  --Parse an MBot move statement
  parseMoves :: Parser Stmt
  parseMoves = parseMove "moveLeft"     Left      `mplus`
               parseMove "moveRight"    Right     `mplus`
               parseMove "moveForward"  Forward   `mplus`
               parseMove "moveBackward" Backward
         where parseMove name direction = do
                 _ <- wMatch name
                 speed <- parseExpBracket
                 return $ Move direction speed

  --Parse a sequence of statements, this is the only exported function of this module
  parseSequence :: Parser Stmt
  parseSequence = fmap Sequence (star parseStmt)

  --Parse a code block, a block is a nested sequence of statements
  parseBlock :: Parser Stmt
  parseBlock = fmap Block (bracket (wToken '{') parseSequence (wToken '}'))

  --Parse a variable declaration
  parseDeclare :: Parser Stmt
  parseDeclare = wMatch "let" >> fmap (uncurry (:=)) assignHelper

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

  --Parse the MBot line and distance sensor
  parseSensors :: Parser Stmt
  parseSensors = parseSensor "MBOT_LINE" Line `mplus`
                 parseSensor "MBOT_DIST" Distance
    where parseSensor keyword sensor = do
            _ <- wMatch "let"
            name <- wIdentifier
            _ <- wToken '=' >> wMatch keyword
            return $ ReadSensor sensor name

  --Parse an if statement
  parseIf :: Parser Stmt
  parseIf = wMatch "if" >> do
    ex <- parseExpBracket
    stmt <- parseBlock
    return $ If ex stmt

  parseIfElse :: Parser Stmt
  parseIfElse = do
    (If ex ifStmt) <- parseIf
    _ <- wMatch "else"
    elseStmt <- parseBlock
    return $ IfElse ex ifStmt elseStmt

  --Parse a for loop
  parseFor :: Parser Stmt
  parseFor = wMatch "for" >> do
    (assign, predicate, change) <- roundBracket (do
        a <- parseDeclare
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

  --Parse a wait statement
  parseWait :: Parser Stmt
  parseWait = wMatch "wait" >> fmap Wait parseExpBracket

  --Parse a single line comment
  parseLineComment :: Parser Stmt
  parseLineComment = wMatch "//" >> parseLine >> return Noop

  --Parse a block comment
  parseBlockComment :: Parser Stmt
  parseBlockComment = wMatch "/*" >> eatUntil "*/" >> return Noop

  --Parse a semicolon followed by some whitespace
  delim :: Parser Char
  delim = addWhitespace (token ';')
