{-# LANGUAGE OverloadedStrings #-}
module JKF.Parser.Kif (
  parseKifu
  ) where

import           Data.Text                              (Text)
import qualified JKF.Type                               as Type
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.String
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as P

parseKifu s =
  case parse kifu "(kifu)" s of
    Left err -> error $ show err
    Right res -> res

kifu :: Parser Type.JSONKifuFormat
kifu = do
  comment_ <- many comment
  headers <- many header
  undefined
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
