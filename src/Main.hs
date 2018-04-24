module Main where

import Prelude hiding (iterate)
import Graphics.Gloss

import IFS

window :: Display
window = InWindow "Hacktals" (800, 600) (10, 10)

main :: IO ()
main = do
  putStrLn iterate
  display window white (Circle 80)
