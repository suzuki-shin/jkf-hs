{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module JKF.Type (
    JKF(..)
  , Header(..)
  , Initial(..)
  , StateFormat(..)
  , MoveMoveFormat(..)
  , MoveFormat(..)
  , Time(..)
  , TimeFormat(..)
  , Board
  , Cel(..)
  , PlaceFormat(..)
  , Color(..)
  , Piece(..)
  , kanjiPiece
  ) where

import           Data.Aeson     (Object, object, (.:), (.=))
import qualified Data.Aeson     as A
import           Data.Set       (Set)
import           Data.Text.Lazy (Text)
import           GHC.Generics   (Generic)

data JKF = JKF {
    jkfHeader  :: Header
  , jkfInitial :: Maybe Initial
  , jkfMoves   :: [MoveFormat]
  } deriving (Show, Eq, Generic)

instance A.ToJSON JKF where
instance A.FromJSON JKF where

data Header = Header {
    headerBlack          :: String
  , headerWhite          :: String
  , headerStartDatetime  :: String
  , headerFinishDatetime :: String
  , headerPresetString   :: String
  } deriving (Show, Eq)

instance A.ToJSON Header where
  toJSON (Header b w s f p) =
    object [ "先手" .= b
           , "後手" .= w
           , "開始日時" .= s
           , "終了日時" .= f
           , "手合割" .= p
           ]

instance A.FromJSON Header where
  parseJSON (A.Object v) =
    Header <$> v .: "先手"
           <*> v .: "後手"
           <*> v .: "開始日時"
           <*> v .: "終了日時"
           <*> v .: "手合割"

data Initial = Initial {
    initialPreset :: Text
  , initialData   :: StateFormat
  } deriving (Show, Eq, Generic)

instance A.ToJSON Initial where
instance A.FromJSON Initial where

data StateFormat = StateFormat {
    stateFormatColor :: Color
  , stateFormatBoard :: Board
  , stateFormatHands :: [Hand]
  } deriving (Show, Eq, Generic)

instance A.ToJSON StateFormat where
instance A.FromJSON StateFormat where

data MoveMoveFormat = MoveMoveFormat {
    moveMoveFormatColor    :: Color
  , moveMoveFormatFrom     :: Maybe PlaceFormat
  , moveMoveFormatTo       :: Maybe PlaceFormat
  , moveMoveFormatPiece    :: Piece
  , moveMoveFormatSame     :: Maybe Bool
  , moveMoveFormatPromote  :: Maybe Bool
  , moveMoveFormatCapture  :: Maybe Piece
  , moveMoveFormatRelative :: Maybe String
  } deriving (Show, Eq, Generic)

instance A.ToJSON MoveMoveFormat where
instance A.FromJSON MoveMoveFormat where

data MoveFormat = MoveFormat {
    moveFormatComments :: Maybe [Text]
  , moveFormatMove     :: Maybe MoveMoveFormat
  , moveFormatTime     :: Maybe Time
  , moveFormatSpecial  :: Maybe Text
  , moveFormatForks    :: Maybe [[MoveFormat]]
  } deriving (Show, Eq, Generic)

instance A.ToJSON MoveFormat where
instance A.FromJSON MoveFormat where

data Time = Time {
    timeNow   :: TimeFormat
  , timeTotal :: TimeFormat
  } deriving (Show, Eq, Generic)

instance A.ToJSON Time where
instance A.FromJSON Time where

data TimeFormat = TimeFormat {
    timeFormatH :: Maybe Int
  , timeFormatM :: Int
  , timeFormatS :: Int
  } deriving (Show, Eq, Generic)

instance A.ToJSON TimeFormat where
instance A.FromJSON TimeFormat where

data PlaceFormat = PlaceFormat {
    placeFormatX :: Int
  , placeFormatY :: Int
  } deriving (Show, Eq, Generic)

instance A.ToJSON PlaceFormat where
instance A.FromJSON PlaceFormat where

data Color = Black | White deriving (Show, Eq, Ord, Generic)

instance A.ToJSON Color where
instance A.FromJSON Color where

type Board = [[Maybe Cel]]

data Cel = Cel {
    celColor :: Color
  , celKind  :: Piece
} deriving (Show, Eq, Generic)

instance A.ToJSON Cel where
instance A.FromJSON Cel where

data Piece =
    FU | KY | KE | GI | KI | KA | HI | OU
  | TO | NY | NK | NG | UM | RY
  deriving (Show, Eq, Ord, Generic)

kanjiPiece FU = "歩"
kanjiPiece KY = "香"
kanjiPiece KE = "桂"
kanjiPiece GI = "銀"
kanjiPiece KI = "金"
kanjiPiece KA = "角"
kanjiPiece HI = "飛"
kanjiPiece OU = "玉"
kanjiPiece TO = "と"
kanjiPiece NY = "杏"
kanjiPiece NK = "圭"
kanjiPiece NG = "全"
kanjiPiece RY = "竜"


instance A.ToJSON Piece where
instance A.FromJSON Piece where

type Hand = Set (Piece, Int)
