module Data.InfiniteList where

import Prelude hiding (cycle, head, tail)
import GHC.Generics (Generic)

infixr 5 :::

data InfiniteList element =
  (:::)
  {
    head :: element,
    tail :: InfiniteList element
  }
  deriving stock (Generic)

instance (Show element) => Show (InfiniteList element) where
  show (e1 ::: e2 ::: e3 ::: _) =
    "[" ++
    show e1 ++ ", " ++
    show e2 ++ ", " ++
    show e3 ++ ", " ++
    "..."
    

fold :: (element -> result -> result) -> InfiniteList element -> result
fold f (head ::: tail) = f head (fold f tail)

(+++) :: [element] -> InfiniteList element -> InfiniteList element
prefix +++ list = foldr (:::) list prefix

cycle :: [element] -> InfiniteList element
cycle finiteList =
  let
    infiniteList = finiteList +++ infiniteList
  in infiniteList
