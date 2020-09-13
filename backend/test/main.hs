-- import Game

main :: IO ()
main = do
  putStrLn "starting game"
  -- game <- generateRandomGame 5
  -- print game
  -- run $ game

-- run :: Game -> IO ()
-- run gameOld = do
--   event <- readLn :: IO ClientEvent
--   let (gameNew, gameEvent) = updateChecked event gameOld
--   print gameNew
--   print gameEvent
--   run $ gameNew
