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
  , AppLink (..)
  , appendActiveWhen
  , decodeUrl
  ) where

import Application.Types.HTTP as X

import Path.Extended
import qualified Data.Text            as T
import qualified Data.ByteString      as BS
import qualified Data.ByteString.UTF8 as BS
import Data.Monoid
import Data.Url
import Data.List.Split
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Control.Monad.Reader
import Control.Monad.Catch
import Network.HTTP.Types (urlDecode)


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
  , MonadUrl Abs File m
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

-- | Data type representing intra-site links
data AppLink = AppHome
             | AppNew
             | AppGet
             | JQuery
             | JQueryMMenuCss
             | JQueryMMenuDragOpenCss
  deriving (Show, Eq)

instance ToLocation AppLink Abs File where
  toLocation AppHome = fromPath <$> parseAbsFile "/index"
  toLocation AppNew = fromPath <$> parseAbsFile "/new"
  toLocation AppGet = fromPath <$> parseAbsFile "/get"
  toLocation JQuery =  fromPath <$> parseAbsFile "/static/vendor/webcomponentsjs/webcomponents.js"
  toLocation JQueryMMenuCss = fromPath <$> parseAbsFile "/static/vendor/jQuery.mmenu/dist/core/css/jquery.mmenu.css"
  toLocation JQueryMMenuDragOpenCss = fromPath <$> parseAbsFile "/static/vendor/jQuery.mmenu/dist/addons/css/jquery.mmenu.dragopen.css"

appendActiveWhen :: AppLink -> AppLink -> T.Text -> T.Text
appendActiveWhen x y c | x == y    = c <> " active"
                       | otherwise = c


decodeUrl :: Bool -> BS.ByteString -> [(String, Maybe String)]
decodeUrl plus xs =
  let getVal s = case splitOn "=" s of
                   [k]   -> (k, Nothing)
                   [k,v] -> (k, Just v)
                   _     -> error "more than one value?"
  in map getVal
   . splitOn "&"
   . BS.toString
   $ urlDecode plus xs

