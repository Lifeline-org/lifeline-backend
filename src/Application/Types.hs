{-# LANGUAGE
    DeriveGeneric
  , TemplateHaskell
  , ConstraintKinds
  , FlexibleContexts
  , FlexibleInstances
  , OverloadedStrings
  , StandaloneDeriving
  , MultiParamTypeClasses
  #-}

module Application.Types
  ( module X
  , AppM
  , runApp
  , AppTemplateT
  , runAppTemplate
  , MonadApp
  , Env (..)
  , MainNav (..)
  , GlobalState (..)
  , appendActiveWhen
  ) where

import Application.Types.HTTP as X

import Path.Extended
import qualified Data.Text as T
import Data.Monoid
import Data.Url
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Control.Monad.Reader
import Control.Monad.Catch

import Lucid

-- FIXME: orphan

instance ( Monad m
         , MonadUrl b m
         ) => MonadUrl b (HtmlT m) where
  pathUrl   = lift . pathUrl
  locUrl    = lift . locUrl
  symbolUrl = lift . symbolUrl


-- * Monad Stack

type AppM a = LoggingT (ReaderT Env IO) a

runApp :: AppM a -> Env -> IO a
runApp hs env = runReaderT (runStderrLoggingT hs) env

type AppTemplateT m = AbsoluteUrlT m

runAppTemplate :: AppTemplateT m a -> UrlAuthority -> m a
runAppTemplate hs hostname = runAbsoluteUrlT hs hostname


type MonadApp m =
  ( MonadIO m
  , MonadThrow m
  , MonadLogger m
  , MonadUrl Abs m
  , MonadReader Env m
  , MonadBaseControl IO m
  )


-- * Available Data

-- The environment accessible from our application
data Env = Env
  { envAuthority :: UrlAuthority
  , envCwd       :: FilePath -- ^ for File Processing
  , envStatic    :: FilePath
  } deriving (Show, Eq)

-- | Data type representing top navigation bar
data MainNav = HomeNav
             | AboutNav
             | ContactNav
  deriving (Show, Eq)

data GlobalState = GlobalState
  { mainNav :: MainNav
  } deriving (Show, Eq)

appendActiveWhen :: GlobalState -> MainNav -> T.Text -> T.Text
appendActiveWhen (GlobalState HomeNav) HomeNav c = c <> " active"
appendActiveWhen (GlobalState AboutNav) AboutNav c = c <> " active"
appendActiveWhen (GlobalState ContactNav) ContactNav c = c <> " active"
appendActiveWhen _ _ c = c
