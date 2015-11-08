{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  #-}

module Pages.NotFound where

import Application.Types

import Path.Extended
import Data.Url
import Web.Page.Lucid
import qualified Data.Text as T
import Lucid
import Lucid.Base

import Data.Monoid
import Data.Default
import Control.Monad.Trans


notFoundContent :: ( MonadApp m
                   ) => HtmlT m ()
notFoundContent = do
  root <- T.pack <$> (pathUrl =<< lift (parseAbsDir "/"))

  let page = def { metaVars = meta_ [ makeAttribute "http-equiv" "refresh"
                                    , content_ $ "3;url=" <> root
                                    ]
                 }

  template page $
    div_ [] $ do
      h1_ [] "Not Found!"
      p_ [] $ do
        "Your request was not found. Redirecting you to "
        a_ [href_ root] "the homepage"
        "..."
