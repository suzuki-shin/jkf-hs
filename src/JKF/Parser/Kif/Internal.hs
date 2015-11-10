{-# LANGUAGE OverloadedStrings #-}
module JKF.Parser.Kif.Internal where

import           Control.Applicative                    ((<$>), (<*>))
import           Data.Char                              (digitToInt)
import           Data.Maybe                             (isNothing)
import           Data.Text                              (Text)
import           JKF.Type
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.String
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as P

kifu :: Parser JKF
kifu = do
  many skipLine
  comment_ <- many $ try comment
  headers1 <- many $ try header
  movemoves <- many $ try sashite
  let moves = map (\mm -> MoveFormat Nothing (Just mm) Nothing Nothing Nothing) movemoves
  return $ JKF (Header "sente" "gote" "1/1 00:00" "1/1 10:00" "hirate") Nothing moves

--   hs <- try headers
--   kls <- try kifuLines
--   return $ Kifu hs kls

comment :: Parser String
comment = do
  char '*'
  many space
  c <- many $ noneOf "\r\n"
  endOfLine
  return c

-- header :: Parser String
header = do
  k <- many $ noneOf "\r\n："
  string "："
  v <- many $ noneOf "\r\n："
  endOfLine
  return (k, v)

-- initialBoard = do
--   c <- many $ noneOf "\r\n"
--   endOfLine
--   return c

-- splitLine = return $ many $ string "手数----指手--" "-------消費時間--"

sashite :: Parser MoveMoveFormat
sashite = do
  spaces
  n <- many digit
  spaces
  toP <- toPos
  koma <- kanjiKoma
  nari <- (string "成" >> return (Just True))
      <|> (string "" >> return (Just False))
  spaces
  fromP <- optionMaybe fromPos
  endOfLine
  let
    color = if read n `mod` 2 == 1 then Black else White
    dou = if isNothing toP then Just True else Just False
  return $ MoveMoveFormat color fromP toP koma dou nari Nothing Nothing

skipLine :: Parser String
skipLine = do
  char '#'
  v <- many $ noneOf "\r\n"
  endOfLine
  return v

toPos :: Parser (Maybe PlaceFormat)
toPos =
  (do
    string "同"
    skipMany (string "　")
    return Nothing)
  <|> do
    x <- zenkakuNum
    y <- kanjiNum
    return $ Just $ PlaceFormat x y

fromPos :: Parser PlaceFormat
fromPos = do
  char '('
  x <- digitToInt <$> digit
  y <- digitToInt <$> digit
  char ')'
  return $ PlaceFormat x y

zenkakuNum =
      (string "１" >> return 1)
  <|> (string "２" >> return 2)
  <|> (string "３" >> return 3)
  <|> (string "４" >> return 4)
  <|> (string "５" >> return 5)
  <|> (string "６" >> return 6)
  <|> (string "７" >> return 7)
  <|> (string "８" >> return 8)
  <|> (string "９" >> return 9)

kanjiNum =
      (string "一" >> return 1)
  <|> (string "二" >> return 2)
  <|> (string "三" >> return 3)
  <|> (string "四" >> return 4)
  <|> (string "五" >> return 5)
  <|> (string "六" >> return 6)
  <|> (string "七" >> return 7)
  <|> (string "八" >> return 8)
  <|> (string "九" >> return 9)

kanjiKoma =
      (string "歩" >> return FU)
  <|> (string "香" >> return KY)
  <|> (string "桂" >> return KE)
  <|> (string "銀" >> return GI)
  <|> (string "金" >> return KI)
  <|> (string "角" >> return KA)
  <|> (string "飛" >> return HI)
  <|> (string "玉" >> return OU)
  <|> (string "王" >> return OU)
  <|> (string "と" >> return TO)
  <|> (string "杏" >> return NY)
  <|> (string "圭" >> return NK)
  <|> (string "全" >> return NG)
  <|> (string "竜" >> return RY)
  <|> (string "龍" >> return RY)
