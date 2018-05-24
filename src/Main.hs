module Main
  ( main
  )
where

import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Interface.Pure.Game hiding (Vector)

import Numeric.LinearAlgebra.Data

import System.Random

import IFS.Base

-- | Main function, when we launch, just start and display the window
main :: IO ()
main = do
  play window white fps initialState render handleEvents stepWorld

-- | Window for the program
window :: Display
window = InWindow "Hacktals" (800, 600) (10, 10)

-- | FPS
fps :: Int
fps = 300

-- | Data structure to hold the state of the fractal
data FractalState = FractalState
  { pointLoc :: Vector R        -- ^ Location of the tracing point
  , points :: [(Float, Float)]  -- ^ Trace of all previous points
  , generator :: StdGen         -- ^ Random generator instance for the "chaos game"
  , autoPlay :: Bool            -- ^ If True, automatically generate the new datapoint
  } deriving Show

-- | Initial state for the program setup
initialState :: FractalState
initialState = FractalState
  { pointLoc = vector [0, 0] -- ^ Start the point at the origin
  , points = [(0, 0)]        -- ^ Starting point
  , generator = mkStdGen 3   -- ^ Start the generator with seed 3
  , autoPlay = False         -- ^ Have autoplay turned off at the start
  }

-- | Rendering function
render :: FractalState -- ^ Fractal info to render
       -> Picture      -- ^ Rendered fractal

render fs = pictures $ map printTrace pts
  where
    pts = points fs
    printTrace (x, y) = translate x y $ color (dark red) $ circleSolid 1

-- | Reactive event handler function
handleEvents :: Event -> FractalState -> FractalState
handleEvents (EventKey (Char 'n') Down _ _) fs = let pl = pointLoc fs
                                                     gen = generator fs
                                                     (pl', (x, y), gen') = chaosStep gen pl
                                                 in fs { pointLoc = pl'
                                                       , generator = gen'
                                                       , points = (x, y):points fs
                                                       }


handleEvents (EventKey (Char 'p') Down _ _) fs = fs { autoPlay = not $ autoPlay fs }
handleEvents _ fs = fs

-- | IDK
stepWorld :: Float -> FractalState -> FractalState
stepWorld _ fs = if autoPlay fs
                 then let pl = pointLoc fs
                          gen = generator fs
                          (pl', (x, y), gen') = chaosStep gen pl
                      in fs { pointLoc = pl'
                            , generator = gen'
                            , points = (x, y):points fs
                            }
                 else fs


chaosStep :: StdGen -> Vector R -> (Vector R, (Float, Float), StdGen)
chaosStep gen pl = let (pl', gen') = iter squareLeafIFS gen pl
                       (x:y:[]) = toList pl'
                       x' = realToFrac (300 * x)
                       y' = realToFrac (300 * y)
                   in (pl', (x', y'), gen')

