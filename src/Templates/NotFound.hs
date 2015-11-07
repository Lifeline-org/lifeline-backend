{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  #-}

module Templates.NotFound where

import Application.Types

import Data.Url
import Web.Page.Lucid
import qualified Data.Text as T
import Lucid
import Lucid.Base

import Data.Monoid
import Data.Default
import Control.Monad.Trans
import Control.Monad.Reader


notFoundContent :: ( Monad m
                   , Functor m
                   , MonadReader Env m
                   ) => HtmlT (AbsoluteUrlT T.Text m) ()
notFoundContent = do
  env <- lift $ lift ask

  let root :: T.Text
      root = T.pack $ envHostname env

      page :: Monad m => WebPage (HtmlT m ()) T.Text
      page = def { metaVars = meta_ [ makeAttribute "http-equiv" "refresh"
                                    , content_ $ "3;url=" <> root
                                    ]
                 }

  template page $
    div_ [] $ mconcat
      [ h1_ [] "Not Found!"
      , p_ [] $ mconcat
        [ "Your request was not found. Redirecting you to "
        , do
          home <- lift $ plainUrl ""
          a_ [href_ home] "the homepage"
        , "..."
        ]
      ]
