{-# LANGUAGE OverloadedStrings #-}
module Scotty where

import Web.Scotty
import Data.Monoid (mconcat)
import Control.Monad.Trans.Class

import Web.Scotty.Internal.Types (ActionT(..))

main = scotty 3000 $
  get "/:word" $ do
    beam <- param "word"
    (ActionT . lift . lift . lift)  $ putStrLn "hello"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
