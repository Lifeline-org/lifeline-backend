{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  #-}

module Pages.NotFound where

import Application.Types
import Templates.Master

import Path.Extended
import Data.Url
import Web.Page.Lucid
import qualified Data.Text as T
import Lucid
import Lucid.Base

import Data.Monoid
import Data.Default
import Control.Monad.Trans
import Control.Monad.Reader (ask)


notFoundContent :: ( MonadApp m
                   ) => HtmlT m ()
notFoundContent = do
  h <- envAuthority <$> lift ask
  loc <- lift $ toLocation AppHome
  root <- T.pack <$> lift (locUrl loc)

  let page = masterPage { metaVars = do metaVars masterPage
                                        meta_ [ makeAttribute "http-equiv" "refresh"
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
