module IFS.Base
  ( IFS(..)
  , Transformation(..)
  , iter
  , sierpinskiTIFS
  , crossFrac 
  , cantorSetIFS
  , squareLeafIFS
  , infTriangles
  , infTriangles2
  , infTriangles3
  , kochSnowFlakeIFS
  )
  where

import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.HMatrix
import System.Random

type Transformation a = Vector a -> Vector a

type IFS = [Transformation R]

iter :: IFS -> StdGen -> Vector R -> (Vector R, StdGen)
iter ifs g v = (randSim v, g')
  where
    (index, g') = randomR (0, length ifs - 1) g
    randSim = ifs !! index

rotate2D :: Double -> Matrix R
rotate2D theta = (2><2) [cos theta, -sin theta,
                         sin theta, cos theta]

sierpinskiTIFS :: IFS
sierpinskiTIFS =
  [ \v -> 1/2 * v
  , \v -> 1/2 * v + vector [1/2, 0]
  , \v -> 1/2 * v + vector [1/4, sqrt 3 / 4]
  ]

cantorSetIFS :: IFS
cantorSetIFS =
  [ \v -> 1/3 * v
  , \v -> 1/3 * v + vector [2/3, 0]
  ]

squareLeafIFS :: IFS
squareLeafIFS =
  [ \v -> 2/3 * v
  , \v -> 1/3 * v + vector [0, 2/3]
  , \v -> 1/3 * v + vector [2/3, 0]
  , \v -> 1/3 * v + vector [2/3, 2/3]
  ]

infTriangles :: IFS
infTriangles =
  [ \v -> 1/2 - 1/2 * v
  , \v -> 1/2 * v + vector [1/2, 0]
  , \v -> 1/2 * v + vector [0, 1/2]
  ]

infTriangles2 :: IFS
infTriangles2 =
  [ \v -> 1/2 + vector [-1/2, 1/2] * v
  , \v -> 1/2 * v + vector [1/2, 0]
  , \v -> 1/2 * v + vector [0, 1/2]
  ]

infTriangles3 :: IFS
infTriangles3 =
  [ \v -> vector [1/2, 0] + vector [-1/2, 1/2] * v
  , \v -> 1/2 * v + vector [1/2, 0]
  , \v -> 1/2 * v + vector [0, 1/2]
  ]

crossFrac :: IFS
crossFrac =
  [ \v -> 1/4 * v
  , \v -> 1/4 * v + vector [3/4, 0]
  , \v -> 1/4 * v + vector [0, 3/4]
  , \v -> 1/4 * v + vector [3/4, 3/4]
  , \v -> 1/2 * v + vector [1/4, 1/4]
  ]

kochSnowFlakeIFS :: IFS
kochSnowFlakeIFS =
  [ \v -> 1/3 * v
  , \v -> 1/3 * v + vector [2/3, 0]
  , \v -> rotate2D (-60) `app` (1/3 * v) + vector [1/3, 0]
  , \v -> rotate2D (60) `app` (1/3 * v) + vector [1/2, sqrt 3 / 6]
  ]
