module Executor(execute) where
  import Prelude hiding (lookup, getLine, Left, Right)
  import Expressions
  import Statements
  import Interpreter
  import qualified Data.Map.Strict as Map
  import Control.Monad.Trans.State
  import Control.Monad.Trans.Class
  import Control.Concurrent
  import MBotLibrary

  execute :: Stmt -> StateT Environment IO ()

  --Execute an MBot move
  execute (Move direction ex) = do
    env <- get
    mbot <- getMBot
    let value = evalExp env ex
    case value of
      Success (MyInt speed) -> lift $ moveMBot direction speed mbot
      Success _ -> raiseError "Invalid types in move-statement"
      Error s -> raiseError s
    where moveMBot :: Direction -> Int -> Device -> IO ()  --Mappings to the MBot API
          moveMBot Forward s dev  = forward s dev
          moveMBot Backward s dev = backward s dev
          moveMBot Left s dev     = turnLeft s dev
          moveMBot Right s dev    = turnRight s dev

  --Execute a led change command
  execute (SetLed led ex1 ex2 ex3) = do
    env <- get
    mbot <- getMBot
    let values = mapM (evalExp env) [ex1, ex2, ex3]
    case values of
      Success [MyInt r, MyInt g, MyInt b] -> lift $ setLed led r g b mbot
      Success _ -> raiseError "Invalid types in led-statement"
      Error s -> raiseError s
    where setLed :: Led -> Int -> Int -> Int -> Device -> IO ()     --Mappings to the MBot API
          setLed Led1 = leftLed
          setLed Led2 = rightLed

  --Execute a read line sensor statement
  execute (ReadSensor Line name) = do
    mbot <- getMBot
    env <- get
    line <- lift $ getLine mbot
    put $ Map.insert name (MyStr $ show line) env

  --Execute a read distance sensor statement
  execute (ReadSensor Distance name) = do
    mbot <- getMBot
    env <- get
    dist <- lift $ getDistance mbot
    put $ Map.insert name (MyDec $ realToFrac dist) env

  --Execute the assignment of a new variable
  execute (name := ex) = do
    env <- get
    let value = evalExp env ex
    case value of
      Success a -> put $ Map.insert name a env
      Error s   -> raiseError s

  --Execute the change of an existing variable
  execute (name ::= ex) = do
    env <- get
    let value = lookup name env >> evalExp env ex
    case value of
      Success a -> put $ Map.insert name a env
      Error s   -> raiseError s

  --Execute an if-statement
  execute (If ex stmt) = execute $ IfElse ex stmt Noop

  execute (IfElse ex ifStmt elseStmt) = do
    env <- get
    let predicate = evalExp env ex
    case predicate of
      Success (MyBool True)  -> execute ifStmt
      Success (MyBool False) -> execute elseStmt
      Error s                -> raiseError s
      _                      -> raiseError "Invalid type in if-statement"

  --Execute a while loop
  execute (While ex stmt) = do
    env <- get
    let predicate = evalExp env ex
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
      _       -> raiseError "Invalid change statement in for loop"

  --Execute a block
  execute (Block stmt) = do
    initEnv <- get
    execute stmt
    endEnv <- get
    let changed = Map.intersection endEnv initEnv
    let globals = Map.filterWithKey (\k _ -> isGlobal k) endEnv    --Global variables have to start with _
    let newEnv = changed `Map.union` initEnv `Map.union` globals
    put newEnv

  --Execute a sequence of statements
  execute (Sequence sts) = sequence_ $ fmap execute sts

  --Execute a print statement
  execute (Print ex) = do
    env <- get
    let value = evalExp env ex
    lift $ putStrLn (case value of
      Success a -> show a
      Error s -> s)

  execute (Wait ex) = do
    env <- get
    let time = evalExp env ex
    case time of
      Success (MyDec t) -> delay t
      Success (MyInt t) -> delay $ fromIntegral t
      Success _         -> raiseError "Invalid type in wait statement"
      Error   s         -> raiseError s
    where toMicros t = round $ t * 1000000
          delay = lift .threadDelay . toMicros

  --Do nothing
  execute Noop = next

  --Return the MBot device, this function opens the MBot if no _MBOT is
  --available in the Environment. Else the MBot from the Environment is
  --returned.
  getMBot :: StateT Environment IO Device
  getMBot = do
    env <- get
    case Map.lookup "_MBOT" env of                    --Check if env contains MBot
      Just (MyMBot m) -> return m                     --MBot exists
      _               -> do                           --No MBot found
        mbot <- lift openMBot                         --Open MBot
        put $ Map.insert "_MBOT" (MyMBot mbot) env    --Add MBot to environment
        return mbot

  --Show an error message
  raiseError :: String -> StateT Environment IO ()
  raiseError s = lift $ putStrLn $ "RUNTIME ERROR: " ++ s

  --Do nothing
  next :: StateT Environment IO ()
  next =  lift $ return ()

  --Test if a variable name represents a global variable
  isGlobal :: String -> Bool
  isGlobal s = head s == '_'
