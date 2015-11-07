{-# LANGUAGE
    OverloadedStrings
  , ConstraintKinds
  , FlexibleContexts
  , DeriveGeneric
  #-}

module Application.Types where

import Data.Aeson
import qualified Data.Text as T
import Data.Monoid
import Data.Url
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Control.Monad.Reader

import GHC.Generics


-- * Monad Stack

type AppM a = LoggingT (ReaderT Env IO) a

runApp :: AppM a -> Env -> IO a
runApp hs env = runReaderT (runStderrLoggingT hs) env

type AppTemplateT m = AbsoluteUrlT T.Text m

runAppTemplate :: AppTemplateT m a -> T.Text -> m a
runAppTemplate hs hostname = runAbsoluteUrlT hs hostname


type MonadApp m =
  ( MonadReader Env m
  , MonadIO m
  , MonadBaseControl IO m
  , MonadLogger m
  )


-- * HTTP Data

data Complaint = DrugC
               | GangC
               | SexC
               | AbuseC
               | GeneralC String
  deriving (Show, Eq, Ord, Generic)

instance ToJSON   Complaint where
instance FromJSON Complaint where

data Location = Location
  { locLong :: Double
  , locLat  :: Double
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON   Location where
instance FromJSON Location where

data NewAPI = NewAPI
  { newComplaint :: Complaint
  , newLocation  :: Location
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON   NewAPI where
instance FromJSON NewAPI where

-- ** Upload Data

data UploadData =
    UploadNew NewAPI
  deriving (Show, Eq, Ord)

data UploadError =
    FailedJSONParse
  deriving (Show, Eq, Ord)


-- * Available Data

-- The environment accessible from our application
data Env = Env
  { envHostname :: String
  , envCwd      :: FilePath -- ^ for File Processing
  , envStatic   :: FilePath
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
