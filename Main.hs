import Statements
import Expressions

main :: IO ()
main = do
  file <- readFile "test.js"
  let stmt = parseStmt file
  putStr $ show stmt
