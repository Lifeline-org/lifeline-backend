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

import Control.Monad.Trans.Resource

import Database.Persist.Sqlite
import Database.Persist.TH


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Claim json
  complaint Complaint
  long      Double
  lat       Double
  report    Report
  deriving Show Eq Ord
|]

runDB :: ( MonadApp m
         ) => SqlPersistT (ResourceT m) a -> m a
runDB q = runResourceT . withSqliteConn "lifeline.sqlite3" . runSqlConn $ q
