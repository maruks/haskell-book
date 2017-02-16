{-# LANGUAGE OverloadedStrings #-}
module Scotty where

import Web.Scotty
import Data.Monoid (mconcat)
import Web.Scotty.Internal.Types (ActionT(..))
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy hiding (get)

main =
  scotty 3000 $
  get "/:word" $ do
    beam <- param "word"
    liftIO (putStrLn "hello")
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
