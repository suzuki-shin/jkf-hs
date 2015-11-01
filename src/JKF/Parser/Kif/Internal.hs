{-# LANGUAGE OverloadedStrings #-}
module JKF.Parser.Kif.Internal where

import           Data.Text                              (Text)
import qualified JKF.Type                               as Type
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.String
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as P

-- kifu :: Parser Type.JKF
kifu = do
  many skipLine
  comment_ <- many $ try comment
  headers1 <- many $ try header
  moves <- many $ try sashite
  return (comment_, headers1, moves)

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

sashite = do
  spaces
  n <- digit
  spaces
  v <- many $ noneOf "\r\n"
  endOfLine
  return (n, v)

skipLine :: Parser String
skipLine = do
  char '#'
  v <- many $ noneOf "\r\n"
  endOfLine
  return v

fromPos :: Parser (Int, Int)
fromPos = do
  x <- zenkakuNum
  y <- kanjiNum
  return (x,y)

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