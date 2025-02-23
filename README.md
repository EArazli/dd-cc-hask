# Incremental Functions in Haskell using constrained-categories

converts arbitrary haskell functions operating on `Double` values to incremental functions: `Double -> Double` `->` `Double -> (Double, (Double -> Double))`.

