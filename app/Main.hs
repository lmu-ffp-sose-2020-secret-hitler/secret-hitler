module Main where

import Game

main :: IO ()
main = do
  putStrLn "starting game"
  game <- generateRandomGame 5
  print game
  run $ game

run :: Game -> IO ()
run gameOld = do
  line <- getLine
  let event = read line :: ClientEvent
  --event <- readLn :: IO ClientEvent
  let (gameNew, gameEvent) = updateChecked event gameOld
  print gameNew
  print gameEvent
  run $ gameNew
