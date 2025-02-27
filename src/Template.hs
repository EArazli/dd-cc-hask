{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Template (p) where

import Data.String (IsString (..))

data Segment env = Static String | Dynamic (env -> String)

instance IsString (Segment env) where
  fromString = Static

class ToSegment a env where
  toSegment :: a -> Segment env

instance ToSegment String env where
  toSegment = Static

instance ToSegment (env -> String) env where
  toSegment = Dynamic

instance ToSegment (Template env) env where
  toSegment t = Dynamic (renderT t)

mergeStatics :: [Segment env] -> [Segment env]
mergeStatics [] = []
mergeStatics (Static s1 : Static s2 : rest) = mergeStatics (Static (s1 ++ s2) : rest)
mergeStatics (x : xs) = x : mergeStatics xs

newtype Template env = Template {renderT :: env -> String}

compileT :: [Segment env] -> Template env
compileT segments = Template $ \env ->
  concatMap
    ( \case
        Static s -> s
        Dynamic f -> f env
    )
    (mergeStatics segments)

data Env = Env {name :: String, count :: Int}

greetingTemplate :: Template Env
greetingTemplate =
  compileT
    [ "Hello, ",
      toSegment name,
      "!"
    ]

mainTemplate :: Template Env
mainTemplate =
  compileT
    [ toSegment greetingTemplate,
      " You have ",
      toSegment (show . count),
      " new messages."
    ]

p :: IO ()
p = putStrLn $ renderT mainTemplate (Env "Alice" 5)