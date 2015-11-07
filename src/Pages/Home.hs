{-# LANGUAGE
    OverloadedStrings
  , ExtendedDefaultRules
  #-}

module Pages.Home where

import Data.Url
import Lucid
import qualified Data.Text as T

import Control.Monad.Trans


homePage :: Monad m => HtmlT (AbsoluteUrlT T.Text m) ()
homePage = do
  newPackageLink <- lift $ plainUrl "packages/new"
  form_ [action_ newPackageLink, method_ "POST"] $ do
    div_ [] $ do
      label_ [for_ "packageName"] "Name: "
      input_ [name_ "packageName", type_ "text"]
    div_ [] $ do
      label_ [for_ "packageSynopsis"] "Synopsis: "
      input_ [name_ "packageSynopsis", type_ "text"]
    input_ [type_ "submit", value_ "Submit"]
