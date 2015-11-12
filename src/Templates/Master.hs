{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  #-}

module Templates.Master where

import Imports hiding (JavaScript, Css, Location (..))
import Templates.MainStyles

import Data.Url
import qualified Data.Text as T
import Web.Page.Lucid
import Lucid
import Path.Extended

import Data.Markup
import Data.Monoid
import Data.Default
import Data.Functor.Identity
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Catch
import Control.Monad.Morph


-- * Content-Type Functions

-- | Render without @mainTemplate@
htmlLight :: ( MonadApp m
             ) => Status
               -> HtmlT (AppTemplateT m) a
               -> FileExtListenerT (MiddlewareT m) m ()
htmlLight s content = do
  hostname <- envAuthority <$> lift ask
  liftIO (putStrLn "now here")
  bs <- lift $ runAppTemplate (renderBST content) hostname
  bytestringStatus Html s [("Content-Type","text/html")] bs

-- | Shortcut for rendering with a template
html :: ( MonadApp m
        ) => Maybe AppLink
          -> HtmlT (AppTemplateT m) ()
          -> FileExtListenerT (MiddlewareT m) m ()
html state content = htmlLight status200 $ mainTemplate state content


-- * Templates

masterPage :: ( MonadApp m
              ) => WebPage (HtmlT m ()) T.Text
masterPage =
  let page :: ( MonadApp m
              ) => WebPage (HtmlT m ()) T.Text
      page = def
  in page { beforeStylesScripts = beforeStylesScripts'
          , metaVars = metaVars'
          , styles = styles'
          , pageTitle = ("Lifeline" :: T.Text)
          }
  where
    beforeStylesScripts' = do
      h <- envAuthority <$> lift ask
      hoist (flip runGroundedUrlT h) localStylesScripts

    styles' = do
      h <- envAuthority <$> lift ask
      hoist (flip runGroundedUrlT h) jqueryMMenuCss
      inlineStyles


    localStylesScripts :: ( MonadApp m
                          ) => HtmlT (GroundedUrlT m) ()
    localStylesScripts = do
      loc <- lift $ toLocation JQuery
      deploy JavaScript Locally loc

    jqueryMMenuCss :: ( MonadApp m
                      ) => HtmlT (GroundedUrlT m) ()
    jqueryMMenuCss = do
      mmenu <- lift $ toLocation JQueryMMenuCss
      deploy Css Locally mmenu
      dragOpen <- lift $ toLocation JQueryMMenuDragOpenCss
      deploy Css Locally dragOpen

    inlineStyles :: ( MonadApp m
                    ) => (HtmlT m ())
    inlineStyles = deploy Css Inline mainStyles

    metaVars' = do
      meta_ [ name_ "viewport"
            , content_ "width=device-width initial-scale=1.0 maximum-scale=1.0 user-scalable=yes"
            ]
      meta_ [ charset_ "utf-8"
            ]


masterTemplate :: ( Monad m
                  ) => Maybe AppLink
                    -> WebPage (HtmlT m ()) T.Text
                    -> HtmlT m ()
                    -> HtmlT m ()
masterTemplate _ = template

mainTemplate :: ( MonadApp m
                ) => Maybe AppLink
                  -> HtmlT m ()
                  -> HtmlT m ()
mainTemplate state = masterTemplate state masterPage


-- * Utilities

appendTitle :: WebPage (HtmlT m ()) T.Text
            -> T.Text
            -> WebPage (HtmlT m ()) T.Text
appendTitle page x = page { pageTitle = x <> " - " <> pageTitle page }

