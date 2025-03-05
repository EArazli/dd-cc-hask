{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad.Trans.Reader (runReader)
import Blaze.ByteString.Builder (fromByteString)
import Control.Concurrent (threadDelay)
import Network.HTTP.Types (status200)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import           Data.Text                   (Text)
import           Text.Html.Nice              ((:$) (..), Attr (..),
                                           Render (..), render)
import Text.Html.Nice.Internal (FastMarkup(..))
import           Text.Html.Nice.Writer
import           Text.Html.Nice.Writer.Html5
import Text.Html.Nice.Shadow
import Data.Text.Internal.Builder
import Data.Void

-- application _ respond = respond $
--   responseStream status200 [("Content-Type", "text/html")] $
--     \send flush -> do
--       send $ fromByteString "<!DOCTYPE html><html><body><template shadowrootmode=\"open\"><h1>My First Heading</h1><slot name=\"content\">Loading...</slot><p>My first paragraph.</p></template></body></html>"
--       flush
--       threadDelay 1000000
--       send $ fromByteString "<p slot=\"content\">Loaded!</p>"

-- main = run 3000 application

data Env = Env {
  greeting :: Text
}

template :: (FastMarkup (Env -> Text))
template = compile $ do
  doctype_
  html_ $ do
    head_ $ title_ "Todo List"
    body_ $ do
      h1_ "Todo List!"
      b_ (dynamic greeting)


main = print "Hello"