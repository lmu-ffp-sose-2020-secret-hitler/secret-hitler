module Random (
  Random,
  runRandomIO,
  withStdGen,
) where

import Control.Monad.State (State, evalState, get, put)
import System.Random (StdGen, newStdGen)

newtype Random a = Random (State StdGen a)
  deriving newtype (Functor, Applicative, Monad)

runRandomIO :: Random a -> IO a
runRandomIO monad =
  newStdGen >>= (return . evalRandom monad)

evalRandom :: Random a -> StdGen -> a
evalRandom (Random stateM) = evalState stateM

withStdGen :: (StdGen -> (a, StdGen)) -> Random a
withStdGen f = do
  rngOld <- Random get
  let (result, rngNew) = f rngOld
  Random $ put rngNew
  return result
