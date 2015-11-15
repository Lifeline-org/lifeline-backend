{-# LANGUAGE
    OverloadedStrings
  , ExtendedDefaultRules
  , FlexibleContexts
  , ScopedTypeVariables
  #-}

module Pages.Home where

import Application.Types

import Data.Url
import Path.Extended as P
import Lucid
import qualified Data.Text as T

import Control.Monad.Trans


homePage :: ( MonadApp m
            ) => HtmlT m ()
homePage = do
  newClaimLoc <- lift $ toLocation AppNew
  newClaim    <- lift $ locUrl newClaimLoc

  form_ [action_ (T.pack newClaim), method_ "POST"] $ do
    div_ [] $ do
      label_ [for_ "name"] "Name: "
      input_ [name_ "name", type_ "text"]
    div_ [] $ do
      label_ [for_ "synopsis"] "Synopsis: "
      input_ [name_ "synopsis", type_ "text"]
    input_ [type_ "submit", value_ "Submit"]
