{-# LANGUAGE OverloadedStrings #-}
module JKF.Parser.Kif (
  parseKifu
  ) where

-- import qualified JKF.Type                               as Type
import           JKF.Parser.Kif.Internal
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.String
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as P


parseKifu s =
  case parse kifu "(kifu)" s of
    Left err -> error $ show err
    Right res -> res
