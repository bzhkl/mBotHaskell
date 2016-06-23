module Statements (Stmt (..), Direction (..), Led(..), Sensor(..), parseProgram) where
  import Prelude hiding (Left, Right)
  import Parser
  import StringParser
  import SequenceParser
  import Control.Monad
  import Expressions
  import Control.Applicative

  data Stmt = Noop | Name := Exp | Name ::= Exp | If Exp Stmt |
              While Exp Stmt | Sequence [Stmt] | Block Stmt |
              Print Exp | For Stmt Exp Stmt Stmt | Move Direction Exp |
              SetLed Led Exp Exp Exp | ReadSensor Sensor Name |
              Wait Exp | IfElse Exp Stmt Stmt
     deriving Show

  data Sensor = Line | Distance
      deriving Show
  data Direction = Forward | Backward | TurnLeft | TurnRight
     deriving Show
  data Led = Led1 | Led2
     deriving Show

  --Parse an application
  parseProgram :: Parser Stmt
  parseProgram = whitespace >> parseSequence

  --Parse a single statement
  parseStmt :: Parser Stmt
  parseStmt = parseBlock        `mplus` parseIf           `mplus`
              parseWhile        `mplus` parseLineComment  `mplus`
              parseBlockComment `mplus` parseFor          `mplus`
              parseDelimited    `mplus` parseIfElse

  --Combine the semicolon ended parse functions
  parseDelimited :: Parser Stmt
  parseDelimited = let stmt = parseDeclare  `mplus` parseChange   `mplus`
                              parsePrint    `mplus` parseMoves    `mplus`
                              parseLeds     `mplus` parseSensors  `mplus`
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
  parseMoves = parseMove "moveLeft"     TurnLeft  `mplus`
               parseMove "moveRight"    TurnRight `mplus`
               parseMove "moveForward"  Forward   `mplus`
               parseMove "moveBackward" Backward
         where parseMove name direction = Move direction <$>
                                          (wMatch name >> parseExpBracket)

  --Parse a sequence of statements,
  --this is the only exported function of this module
  parseSequence :: Parser Stmt
  parseSequence = Sequence <$> star parseStmt

  --Parse a code block, a block is a nested sequence of statements
  parseBlock :: Parser Stmt
  parseBlock = Block <$> bracket (wToken '{') parseSequence (wToken '}')

  --Parse a variable declaration
  parseDeclare :: Parser Stmt
  parseDeclare =  uncurry (:=) <$> (wMatch "let" >> assignHelper)

  --Parse a variable change
  parseChange :: Parser Stmt
  parseChange = uncurry (::=) <$> assignHelper

  --Helper function used for variable assignment and variable change
  assignHelper :: Parser (String, Exp)
  assignHelper = pair <$> wIdentifier <*> (wToken '=' >> parseExp)

  --Parse the MBot line and distance sensor
  parseSensors :: Parser Stmt
  parseSensors = parseSensor "MBOT_LINE" Line `mplus`
                 parseSensor "MBOT_DIST" Distance
    where parseSensor keyword sensor = do
            name <- wMatch "let" >> wIdentifier
            _ <- wToken '=' >> wMatch keyword
            return $ ReadSensor sensor name

  --Parse an if statement
  parseIf :: Parser Stmt
  parseIf = If <$> (wMatch "if" >> parseExpBracket) <*> parseBlock

  --Parse an if else statement
  parseIfElse :: Parser Stmt
  parseIfElse = do
    --Parse an if statement
    (If ex ifStmt) <- parseIf
    --Parse the else part
    _ <- wMatch "else"
    elseStmt <- parseBlock
    return $ IfElse ex ifStmt elseStmt

  --Parse a for loop
  parseFor :: Parser Stmt
  parseFor = wMatch "for" >> do
    --Parser for the three parts inside a for statement
    let partsParser = triple <$> parseDeclare <*>
                      bracket delim parseExp delim <*> parseChange
    --Surround with round brackets
    (assign, predicate, change) <- roundBracket partsParser
    stmt <- parseBlock
    return $ For assign predicate change stmt

  --Parse a while loop
  parseWhile :: Parser Stmt
  parseWhile = While <$> (wMatch "while" >> parseExpBracket) <*> parseBlock

  --Parse a print statement
  parsePrint :: Parser Stmt
  parsePrint = Print <$> (wMatch "console.log" >> parseExpBracket)

  --Parse a wait statement
  parseWait :: Parser Stmt
  parseWait = Wait <$> (wMatch "wait" >> parseExpBracket)

  --Parse a single line comment
  parseLineComment :: Parser Stmt
  parseLineComment = wMatch "//" >> parseLine >> return Noop

  --Parse a block comment
  parseBlockComment :: Parser Stmt
  parseBlockComment = wMatch "/*" >> eatUntil "*/" >> return Noop

  --Parse a semicolon followed by some whitespace
  delim :: Parser Char
  delim = addWhitespace (token ';')

  --Helper function to create a pair
  pair :: a -> b -> (a, b)
  pair a b = (a, b)

  --Helper function to create a triple
  triple :: a -> b -> c -> (a, b, c)
  triple a b c = (a, b, c)
