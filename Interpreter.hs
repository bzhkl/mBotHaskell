module Interpreter (M, errorM) where
  import Control.Monad

  data M a = Success a | Error String

  instance Functor M where
    fmap = liftM

  instance Applicative M where
    pure  = return
    (<*>) = ap

  instance Monad M where
    return = Success
    (Success a) >>= f  = f a
    (Error s) >>= _  = Error s

  errorM :: String -> M a
  errorM = Error
