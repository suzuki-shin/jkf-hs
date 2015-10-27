{-# LANGUAGE DeriveGeneric #-}
module JKF.Type (
    JSONKifuFormat(..)
  , Header
  , Initial(..)
  , StateFormat(..)
  , MoveMoveFormat(..)
  , MoveFormat(..)
  , Time(..)
  , TimeFormat(..)
  , Board
  , Cel(..)
  , PlaceFormat(..)
  ) where

import           Data.Set       (Set)
import           Data.Text.Lazy (Text)
import           GHC.Generics

data JSONKifuFormat = JSONKifuFormat {
    jsonKifuFormatHeader  :: [Header]
  , jsonKifuFormatInitial :: Maybe Initial
  , jsonKifuFormatMoves   :: [MoveFormat]
  } deriving (Show, Eq, Generic)

type Header = (Text, Text)

data Initial = Initial {
    initialPreset :: Text
  , initialData   :: StateFormat
  } deriving (Show, Eq, Generic)

data StateFormat = StateFormat {
    stateFormatColor :: Color
  , stateFormatBoard :: Board
  , stateFormatHands :: [Hand]
  } deriving (Show, Eq, Generic)

data MoveMoveFormat = MoveMoveFormat {
    moveMoveFormatColor    :: Color
  , moveMoveFormatFrom     :: Maybe PlaceFormat
  , moveMoveFormatTo       :: Maybe PlaceFormat
  , moveMoveFormatPiece    :: String
  , moveMoveFormatSame     :: Maybe Bool
  , moveMoveFormatPromote  :: Maybe Bool
  , moveMoveFormatCapture  :: Maybe String
  , moveMoveFormatRelative :: Maybe String
  } deriving (Show, Eq, Generic)

data MoveFormat = MoveFormat {
    moveFormatComments :: Maybe [Text]
  , moveFormatMove     :: Maybe MoveMoveFormat
  , moveFormatTime     :: Maybe Time
  , moveFormatSpecial  :: Maybe Text
  , moveFormatForks    :: Maybe [[MoveFormat]]
  } deriving (Show, Eq, Generic)

data Time = Time {
    timeNow   :: TimeFormat
  , timeTotal :: TimeFormat
  } deriving (Show, Eq, Generic)

data TimeFormat = TimeFormat {
    timeFormatH :: Maybe Int
  , timeFormatM :: Int
  , timeFormatS :: Int
  } deriving (Show, Eq, Generic)

data PlaceFormat = PlaceFormat {
    placeFormatX :: Int
  , placeFormatY :: Int
  } deriving (Show, Eq, Generic)

data Color = Black | White deriving (Show, Eq, Ord, Generic)

type Board = [[Maybe Cel]]

data Cel = Cel {
    celColor :: Color
  , celKind  :: Piece
} deriving (Show, Eq, Generic)

data Piece = Fu | Ky | Ke | Gi | Ki | Ka | Hi | Ou
           | To | NKy | NKe | NGi | Um | Ry
           deriving (Show, Eq, Ord, Generic)

type Hand = Set (Piece, Int)
