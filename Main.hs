import Statements
import Parser
import Executor
import Expressions
import Control.Monad.State
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  file <- readFile "test.js"
  let stmt = parse parseSequence file
  putStrLn $ show stmt
  let env = Map.empty :: Environment
  runStateT (execute stmt) env
  return ()
