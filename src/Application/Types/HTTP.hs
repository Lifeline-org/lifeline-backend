{-# LANGUAGE
    TemplateHaskell
  , DeriveGeneric
  #-}

module Application.Types.HTTP where

import Data.Aeson
import Database.Persist.TH
import GHC.Generics

-- * HTTP Data

data Complaint =
    DrugC
  | GangC
  | SexC
  | AbuseC
  | GeneralC String
  deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON   Complaint where
instance FromJSON Complaint where

derivePersistField "Complaint"


data Location = Location
  { locLong :: Double
  , locLat  :: Double
  } deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON   Location where
instance FromJSON Location where

derivePersistField "Location"


data Report = Report
  { repHeadline :: String
  , repReport   :: String
  } deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON   Report where
instance FromJSON Report where

derivePersistField "Report"


data NewAPI = NewAPI
  { newComplaint :: Complaint
  , newLocation  :: Location
  , newReport    :: Report
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON   NewAPI where
instance FromJSON NewAPI where


data GetAPI = GetAPI
  { getComplaints :: [Complaint]
  , getNorthEast  :: Location
  , getSouthWest  :: Location
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON   GetAPI where
instance FromJSON GetAPI where


-- ** Upload Data

data UploadData =
    UploadNew NewAPI
  | UploadGet GetAPI
  deriving (Show, Eq, Ord)

data UploadError =
    FailedJSONParse
  deriving (Show, Eq, Ord)

