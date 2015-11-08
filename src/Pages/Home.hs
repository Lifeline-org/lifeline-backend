{-# LANGUAGE
    OverloadedStrings
  , ExtendedDefaultRules
  , FlexibleContexts
  #-}

module Pages.Home where

import Application.Types

import Data.Url
import Path.Extended
import Lucid
import qualified Data.Text as T

import Control.Monad.Trans


homePage :: ( MonadApp m
            ) => HtmlT m ()
homePage = do
  location <- fromPath <$> lift (parseAbsFile "/packages/new")
  link <- locUrl location
  form_ [action_ (T.pack link), method_ "POST"] $ do
    div_ [] $ do
      label_ [for_ "packageName"] "Name: "
      input_ [name_ "packageName", type_ "text"]
    div_ [] $ do
      label_ [for_ "packageSynopsis"] "Synopsis: "
      input_ [name_ "packageSynopsis", type_ "text"]
    input_ [type_ "submit", value_ "Submit"]
