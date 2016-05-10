module Executor(execute) where
  import Prelude hiding (lookup)
  import Expressions
  import Statements
  import Interpreter
  import qualified Data.Map.Strict as Map
  import Control.Monad.Trans.State
  import Control.Monad.Trans.Class

  execute :: Stmt -> StateT Environment IO ()

  --Execute the assignment of a new variable
  execute (name := ex) = do
    env <- get
    let value = evalExp ex env
    case value of
      Success a -> put $ Map.insert name a env
      Error s   -> raiseError s

  --Execute the change of an existing variable
  execute (name ::= ex) = do
    env <- get
    let value = lookup name env >> evalExp ex env
    case value of
      Success a -> put $ Map.insert name a env
      Error s   -> raiseError s

  --Execute an if-statement
  execute (If ex stmt) = do
    env <- get
    let predicate = evalExp ex env
    case predicate of
      Success (MyBool True)  -> execute stmt
      Success (MyBool False) -> next
      Error s                -> raiseError s
      _                      -> raiseError "Invalid type in if-statement"

  --Execute a while loop
  execute (While ex stmt) = do
    env <- get
    let predicate = evalExp ex env
    case predicate of
      Success (MyBool True)   -> execute stmt >> execute (While ex stmt)
      Success (MyBool False)  -> next
      Error s                 -> raiseError s
      Success a               -> raiseError ("Invalid type in while-statement: " ++ show a)

  --Execute a for loop
  execute (For assign predicate change stmt) = do
    let runFor = execute $ While predicate (Sequence [stmt, change])
    execute assign
    case change of
      _ ::= _ -> runFor
      Inc _   -> runFor
      Dec _   -> runFor
      _       -> raiseError "Invalid change statement in for loop"

  --Execute a block
  execute (Block stmt) = do
    initEnv <- get
    execute stmt
    endEnv <- get
    let changed = Map.intersection endEnv initEnv
    let updated = Map.union changed initEnv
    put updated

  --Execute a sequence
  execute (Sequence (st:sts)) = execute st >> execute (Sequence sts)

  --Execute a print statement
  execute (Print ex) = do
    env <- get
    let value = evalExp ex env
    lift $ putStrLn (case value of
      Success a -> show a
      Error s -> s)

  execute _ = next

  raiseError :: String -> StateT Environment IO ()
  raiseError s = lift $ putStrLn $ "RUNTIME ERROR: " ++ s

  --Do nothing
  next :: StateT Environment IO ()
  next = lift $ return ()
