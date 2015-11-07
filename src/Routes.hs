{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  #-}

module Routes where

import Imports
import Pages.Home
import Templates.Master
import Templates.NotFound

import Data.Aeson
import Data.Monoid
import qualified Data.Text.Lazy as LT
import Control.Monad.Except


routes :: ( MonadApp m
          ) => RoutableActionT s e UploadData UploadError m ()
routes = do
  hereAction homeHandle
  handleAction ("new" </> o_) newHandle
  notFoundAction notFoundHandle
  where
    homeHandle = get $ do
      html (GlobalState HomeNav) homePage

    newHandle = post uploadParams handleUploaded
      where
        uploadParams req = do
          unparsed <- liftIO (strictRequestBody req)
          case decode unparsed of
            Nothing -> throwError (Just FailedJSONParse)
            Just d  -> return (UploadNew d)

        handleUploaded (Left Nothing) = do
          text "dun goofed"
        handleUploaded (Left (Just e)) = do
          text $ "dun goofed - " <> LT.pack (show e)
        handleUploaded (Right (UploadNew d)) = do
          text $ "new at: " <> LT.pack (show d)


    notFoundHandle = get $ do
      htmlLight status404 notFoundContent
      text "404 :("
