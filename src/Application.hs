{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  #-}

module Application where

import Imports hiding ((</>))
import Routes

import qualified Data.Text as T
import Control.Monad.Reader
import System.Directory
import Path



defApp :: Application
defApp _ respond =
  respond $ textOnlyStatus status404 "Not Found! :("

data AuthRole = NeedsLogin

data AuthErr = NeedsAuth

authorize :: ( Monad m
             ) => Request -> [AuthRole] -> m (Response -> Response, Maybe AuthErr)
-- authorize _ _ = return id -- uncomment to force constant authorization
authorize req ss | null ss   = return (id, Nothing)
                 | otherwise = return (id, Just NeedsAuth)

securityLayer :: MonadApp m => MiddlewareT m
securityLayer = extractAuth authorize (actionToMiddleware routes)

contentLayer :: MonadApp m => MiddlewareT m
contentLayer = routeAction routes

staticLayer :: MonadApp m => MiddlewareT m
staticLayer app req respond = do
    let fileRequested = T.unpack . T.intercalate "/" $ pathInfo req
    basePath <- envStatic <$> ask
    b <- liftIO $ parseAbsDir basePath
    f <- liftIO $ parseRelFile fileRequested
    let file :: FilePath
        file = toFilePath (b </> f)
    fileExists <- liftIO (doesFileExist file)
    if fileExists
        then liftIO $ respond $ responseFile status200 [] file Nothing
        else app req respond
