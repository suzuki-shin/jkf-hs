{-# LANGUAGE OverloadedStrings #-}
module JKF.Parser.KifSpec where

import           Data.Aeson
import           Data.HashMap.Strict (fromList)
import           JKF.Parser.Kif
import           JKF.Parser.Kif.Internal
import           Test.Hspec

spec :: Spec
spec = do
  describe "JKF.Parser.Kif" $ do
    describe "sashite parser" $ do
      it "many sashite $ \"1 ７六歩(77)\n2 ３四歩(33)\n3 ２二角成(88)\n 4 同　銀(31)\n5 ４五角打\n\"" $
        parseTest (many sashite) "1 ７六歩(77)\n2 ３四歩(33)\n3 ２二角成(88)\n 4 同　銀(31)\n5 ４五角打\n"
        `shouldBe`
        [('1', "７六歩(77)"),('2', "３四歩(33)"),('3',"２二角成(88)"),('4',"同　銀(31)"),('5', "４五角打")]

--       it "toJSON (Header \"\" \"\" \"\" \"\" \"\")" $
--         toJSON h2
--         `shouldBe`
--         Object (fromList [
--                     ("後手",String "")
--                    ,("終了日時",String "")
--                    ,("手合割",String "")
--                    ,("先手",String "")
--                    ,("開始日時",String "")
--                    ])

--     describe "MoveMoveFormat toJSON" $ do
--       it "toJSON (MoveMoveFormat Black (Just (PlaceFormat 1 2)) (Just (PlaceFormat 1 3)) \"FU\" Nothing Nothing Nothing Nothing)" $
--         toJSON mmf1
--         `shouldBe`
--         Object (fromList [
--                     ("moveMoveFormatColor",String "Black")
--                    ,("moveMoveFormatFrom",Object (fromList [("placeFormatX",Number 1.0),("placeFormatY",Number 2.0)]))
--                    ,("moveMoveFormatTo",Object (fromList [("placeFormatX",Number 1.0),("placeFormatY",Number 3.0)]))
--                    ,("moveMoveFormatPiece",String "FU")
--                    ,("moveMoveFormatSame",Null)
--                    ,("moveMoveFormatRelative",Null)
--                    ,("moveMoveFormatCapture",Null)
--                    ,("moveMoveFormatPromote",Null)
--                    ])
