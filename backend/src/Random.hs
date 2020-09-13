module Random (
  Random,
  RandomT,
  runRandomIO,
  withStdGen,
) where

import Control.Monad.State.Strict (StateT, evalStateT, get, put)
import Data.Functor.Identity (Identity, runIdentity)
import System.Random (StdGen, newStdGen)

newtype RandomT m a = RandomT (StateT StdGen m a)
  deriving newtype (Functor, Applicative, Monad)

type Random = RandomT Identity

runRandomIO :: Random a -> IO a
runRandomIO = fmap runIdentity . runRandomIOT

runRandomIOT :: Monad m => RandomT m a -> IO (m a)
runRandomIOT monad =
  newStdGen >>= (return . evalRandomT monad)

evalRandomT :: Monad m => RandomT m a -> StdGen -> m a
evalRandomT (RandomT stateM) = evalStateT stateM

withStdGen :: Monad m => (StdGen -> (a, StdGen)) -> RandomT m a
withStdGen f = do
  rngOld <- RandomT get
  let (result, rngNew) = f rngOld
  RandomT $ put rngNew
  return result
