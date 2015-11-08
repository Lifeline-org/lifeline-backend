{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  #-}

module Templates.Master where

import Imports

import Data.Url
import qualified Data.Text as T
import Web.Page.Lucid
import Lucid

import Data.Monoid
import Data.Default
import Control.Monad.Trans
import Control.Monad.Reader


-- * Content-Type Functions

-- | Render without @mainTemplate@
htmlLight :: ( MonadApp m
             ) => Status
               -> HtmlT (AppTemplateT m) a
               -> FileExtListenerT (MiddlewareT m) m ()
htmlLight s content = do
  hostname <- envAuthority <$> lift ask
  bs <- lift $ runUrlReader (renderBST content) hostname
  bytestringStatus Html s [("Content-Type","text/html")] bs

-- | Shortcut for rendering with a template
html :: ( MonadApp m
        ) => GlobalState
          -> HtmlT (AppTemplateT m) ()
          -> FileExtListenerT (MiddlewareT m) m ()
html state content = htmlLight status200 $ mainTemplate state content


-- * Templates

masterPage :: Monad m => WebPage (HtmlT m ()) T.Text
masterPage = def

masterTemplate :: ( Monad m
                  ) => GlobalState
                    -> WebPage (HtmlT m ()) T.Text
                    -> HtmlT m ()
                    -> HtmlT m ()
masterTemplate _ = template

mainTemplate :: ( Monad m
                ) => GlobalState
                  -> HtmlT m ()
                  -> HtmlT m ()
mainTemplate state = masterTemplate state masterPage


-- * Utilities

appendTitle :: WebPage (HtmlT m ()) T.Text
            -> T.Text
            -> WebPage (HtmlT m ()) T.Text
appendTitle page x = page { pageTitle = x <> " - " <> pageTitle page }

