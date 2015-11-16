{-# LANGUAGE
    DeriveGeneric
  , TemplateHaskell
  , OverloadedStrings
  #-}

module Application.Types.HTTP where

import Data.Aeson
import Data.Monoid
import Database.Persist.TH
import GHC.Generics
import qualified Data.Text as T
import Control.Monad
import Control.Applicative


-- * HTTP Data

data Complaint =
    DrugC
  | GangC
  | SexC
  | ViolenceC
  | GeneralC String
  deriving (Show, Read, Eq, Ord)

instance ToJSON Complaint where
  toJSON DrugC     = toJSON ("drugs"    :: T.Text)
  toJSON GangC     = toJSON ("gangs"    :: T.Text)
  toJSON SexC      = toJSON ("sex"      :: T.Text)
  toJSON ViolenceC = toJSON ("violence" :: T.Text)

instance FromJSON Complaint where
  parseJSON (String s) | s == "drugs"    = pure DrugC
                       | s == "gangs"    = pure GangC
                       | s == "sex"      = pure SexC
                       | s == "violence" = pure ViolenceC
  parseJSON _ = empty

complaintFromString :: String -> Maybe Complaint
complaintFromString s | s == "drugs"    = pure DrugC
                      | s == "gangs"    = pure GangC
                      | s == "sex"      = pure SexC
                      | s == "violence" = pure ViolenceC
                      | otherwise       = Nothing


derivePersistField "Complaint"


data Location = Location
  { locLong :: Double
  , locLat  :: Double
  } deriving (Show, Read, Eq, Ord)

instance ToJSON Location where
  toJSON (Location long lat) =
    object ["long" .= long, "lat" .= lat]

instance FromJSON Location where
  parseJSON (Object v) =
    Location <$> v .: "long" <*> v .: "lat"
  parseJSON _ = empty

locationFromPairs :: [(String, Maybe String)] -> Maybe Location
locationFromPairs xs = do
  long' <- join (lookup "long" xs)
  lat'  <- join (lookup "lat"  xs)
  let long = reads long' :: [(Double, String)]
  let lat  = reads lat'  :: [(Double, String)]
  guard $ not (null long)
  guard $ not (null lat)
  return $ Location (fst $ head long) (fst $ head lat)

derivePersistField "Location"


data Report = Report
  { repHeadline :: String
  , repReport   :: String
  } deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON   Report where
instance FromJSON Report where

reportFromPairs :: [(String, Maybe String)] -> Maybe Report
reportFromPairs xs = do
  headline <- join (lookup "headline" xs)
  report <- join (lookup "report" xs)
  return $ Report headline report

derivePersistField "Report"


data NewAPI = NewAPI
  { newComplaint :: Complaint
  , newLocation  :: Location
  , newReport    :: Report
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON   NewAPI where
instance FromJSON NewAPI where

newAPIFromPairs :: [(String, Maybe String)] -> Maybe NewAPI
newAPIFromPairs xs = do
  complaint <- complaintFromString =<< join (lookup "complaint" xs)
  location  <- locationFromPairs xs
  report    <- reportFromPairs xs
  return $ NewAPI complaint location report


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
    FailedParse
  deriving (Show, Eq, Ord)

