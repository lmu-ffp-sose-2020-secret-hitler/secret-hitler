module Main where

import           Game

main :: IO ()
main = do
  print "starting game"
  run $ newGame 5

run :: Game -> IO ()
run game = do
  print game
  case phase game of
    NominateChancellorPhase -> do
      putStrLn "Nominate a chancellor"
      putStr "playerIndex: "
      playerIndex <- readLn :: IO Int
      run $ nominateChancellor playerIndex game
    VotePhase{} -> do
      putStrLn "Vote"
      putStr "playerIndex: "
      playerIndex <- readLn :: IO Int
      putStr "vote: "
      vote <- readLn :: IO Bool
      run $ setVote playerIndex (Just vote) game
