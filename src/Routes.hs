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


routes :: ( MonadApp m
          ) => RoutableActionT s e u eu m ()
routes = do
  hereAction homeHandle
  notFoundAction notFoundHandle
  where
    homeHandle :: MonadApp m => ActionT eu u m ()
    homeHandle = get $ html (GlobalState HomeNav) homePage
    notFoundHandle :: MonadApp m => ActionT eu u m ()
    notFoundHandle = get $ do
      htmlLight status404 notFoundContent
      text "404 :("
