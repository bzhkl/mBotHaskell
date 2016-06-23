module Executor(execute) where
  import Prelude hiding (lookup, getLine)
  import Expressions
  import Statements
  import qualified Data.Map.Strict as Map
  import Data.Set
  import Control.Monad.Trans.State
  import Control.Monad.Trans.Class
  import Control.Concurrent
  import MbotLibrary

  execute :: Stmt -> StateT Environment IO ()

  --Execute an MBot move
  execute (Move direction ex) = do
    env <- get
    mbot <- getMBot
    let value = evalExp env ex
    case value of
      Right (MyInt speed) -> lift $ moveMBot direction speed mbot
      Right _             -> raiseError "Invalid types in move-statement"
      Left  s             -> raiseError s

    where moveMBot :: Direction -> Int -> Device -> IO ()  --Mappings to the MBot API
          moveMBot Forward s dev    = forward s dev
          moveMBot Backward s dev   = backward s dev
          moveMBot TurnLeft s dev   = turnLeft s dev
          moveMBot TurnRight s dev  = turnRight s dev

  --Execute a led change command
  execute (SetLed led ex1 ex2 ex3) = do
    env <- get
    mbot <- getMBot
    let values = mapM (evalExp env) [ex1, ex2, ex3]
    case values of
      Right [MyInt r, MyInt g, MyInt b] -> lift $ setLed led r g b mbot
      Right _ -> raiseError "Invalid types in led-statement"
      Left  s -> raiseError s
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

  --Execute the declaration of a new variable
  execute (name := ex) = do
    env <- get
    let exists = name `member` Map.keysSet env
    if exists
      then raiseError "Variable name already exists"
      else let value = evalExp env ex in
           case value of
              Right a -> put $ Map.insert name a env
              Left  s -> raiseError s


  --Execute the change of an existing variable
  execute (name ::= ex) = do
    env <- get
    let value = lookup name env >> evalExp env ex
    case value of
      Right a -> put $ Map.insert name a env
      Left  s -> raiseError s

  --Execute an if-statement
  execute (If ex stmt) = execute $ IfElse ex stmt Noop

  execute (IfElse ex ifStmt elseStmt) = do
    env <- get
    let predicate = evalExp env ex
    case predicate of
      Right (MyBool True)  -> execute ifStmt
      Right (MyBool False) -> execute elseStmt
      Left  s              -> raiseError s
      _                    -> raiseError "Invalid type in if-statement"

  --Execute a while loop
  execute (While ex stmt) = do
    env <- get
    let predicate = evalExp env ex
    case predicate of
      Right (MyBool True)   -> execute stmt >> execute (While ex stmt)
      Right (MyBool False)  -> next
      Left  s               -> raiseError s
      Right a               -> raiseError ("Invalid type in while-statement: " ++ show a)

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
      Right a -> show a
      Left  s -> s)

  --Execute a wait statement
  execute (Wait ex) = do
    env <- get
    let time = evalExp env ex
    case time of
      Right (MyDec t) -> delay t
      Right (MyInt t) -> delay $ fromIntegral t  --Convert to double
      Right _         -> raiseError "Invalid type in wait statement"
      Left  s         -> raiseError s
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
  raiseError = lift . fail . ("RUNTIME ERROR: " ++)

  --Do nothing
  next :: StateT Environment IO ()
  next =  lift $ return ()

  --Test if a variable name represents a global variable
  isGlobal :: String -> Bool
  isGlobal s = head s == '_'
