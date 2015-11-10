{-# LANGUAGE ImplicitParams #-}
module Main where

import JKF.Parser.Kif
import Data.Encoding
import Data.Encoding.ISO2022JP
-- import Data.Encoding.CP932
import System.IO.Encoding (readFile)
import Prelude hiding (readFile)
import Control.Applicative ((<$>))

main :: IO ()
main = do
    let ?enc = ISO2022JP
--     s <- System.IO.Encoding.readFile "../json-kifu-format/test/files/kif/9fu.kif"
    s <- readTestKif "../json-kifu-format/test/files/kif/9fu.kif"
    print $ parseKifu s
--     let bs = Data.Encoding.encodeLazyByteString ISO2022JP s
--     let sbs = Codec.Text.IConv.convert "ISO-2022-JP" "CP932" bs
--     let ss = Data.Encoding.decodeLazyByteString CP932 sbs
--     let ?enc = CP932
--     System.IO.Encoding.putStrLn ss
    return ()

readTestKif filepath = do
  let ?enc = ISO2022JP
  System.IO.Encoding.readFile filepath
