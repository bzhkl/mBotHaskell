import System.Environment
import Statements
import Parser
import Executor
import Expressions
import Control.Monad.State
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  file <- readFile filename
  let stmt = parse parseProgram file
  print stmt
  let env = Map.empty :: Environment
  _ <- runStateT (execute stmt) env
  return ()
