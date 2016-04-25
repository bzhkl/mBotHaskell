import Statements
import Parser

main :: IO ()
main = do
  file <- readFile "test.js"
  let stmt = parse parseSequence file
  putStr $ show stmt
