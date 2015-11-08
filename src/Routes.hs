{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  #-}

module Routes where

import Imports
import Pages.Home
import Pages.NotFound
import Templates.Master
import Schema

import Data.Aeson
import Data.Monoid
import qualified Data.Text.Lazy as LT
import Database.Persist hiding (get)
import Control.Monad.Except


routes :: ( MonadApp m
          ) => RoutableActionT s e UploadData UploadError m ()
routes = do
  hereAction homeHandle
  handleAction ("new" </> o_) newHandle
  handleAction ("get" </> o_) getHandle
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
          let loc = newLocation d
          lift . runDB . insert $
            Claim (newComplaint d)
                  (locLong loc)
                  (locLat loc)
                  (newReport d)
          text $ "new at: " <> LT.pack (show d)

    getHandle = post uploadParams handleUploaded
      where
        uploadParams req = do
          unparsed <- liftIO (strictRequestBody req)
          case decode unparsed of
            Nothing -> throwError (Just FailedJSONParse)
            Just d  -> return (UploadGet d)

        handleUploaded (Left Nothing) = do
          text "dun goofed"
        handleUploaded (Left (Just e)) = do
          text $ "dun goofed - " <> LT.pack (show e)
        handleUploaded (Right (UploadGet d)) = do
          let cs = getComplaints d
              ne = getNorthEast  d
              sw = getSouthWest  d
          xs <- lift . runDB $ selectList
                  [ ClaimComplaint <-. cs
                  , ClaimLong >=. locLong ne
                  , ClaimLong <=. locLong sw
                  , ClaimLat  >=. locLat  sw
                  , ClaimLat  <=. locLat  ne
                  ]
                  []
          text $ "get at: " <> LT.pack (show xs)

    notFoundHandle = get $ do
      htmlLight status404 notFoundContent
      text "404 :("
