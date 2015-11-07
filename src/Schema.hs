{-# LANGUAGE
    TemplateHaskell
  , QuasiQuotes
  , FlexibleInstances
  , FlexibleContexts
  , MultiParamTypeClasses
  , TypeFamilies
  , GADTs
  , GeneralizedNewtypeDeriving
  , OverloadedStrings
  #-}

module Schema where

import Imports

import Data.Text (Text)
import Data.Time (UTCTime)

import Control.Monad.Trans.Resource

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Foo json
  foo Int
|]

runDB :: ( MonadApp m
         ) => SqlPersist (ResourceT m) a -> m a
runDB q = runResourceT . withSqliteConn "happ-store.sqlite3" . runSqlConn $ q
