{-# LANGUAGE LambdaCase #-}

module RewriteSystems.Base
  ( Rules
  , rewrite
  , getRWIterate
  , cantorSetRS
  )
  where

import Control.Applicative

type Rules = Char -> String

getRWIterate :: Rules -> String -> Int -> String
getRWIterate rules seed n = iterate (rewrite rules) seed !! n

rewrite :: Rules -> String -> String
rewrite rules s = foldMap rules s

cantorSetRS :: Rules
cantorSetRS = \case
  'a' -> "aba"
  'b' -> "bbb"
  _   -> ""
