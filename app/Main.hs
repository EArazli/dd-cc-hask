module Main (main) where

import Control.Category.Constrained.Prelude
import GAD
import Prelude ()

calc :: Inc Double Double
calc = alg (\x -> x * x + x)

main :: IO ()
main = do
  let (D f) = calc
  let (result, DelX next) = f 4
  let nextResult = next 4
  print result
  print nextResult
