{-# LANGUAGE OverloadedStrings #-}
module JKF.TypeSpec where

import           Data.Aeson
import           Data.HashMap.Strict (fromList)
import           JKF.Type
import           Test.Hspec

spec :: Spec
spec = do
  let h1 = Header "habu" "moriuchi" "2015-10-28 07:43" "2015-10-28 20:40" "hirate"
      h2 = Header "" "" "" "" ""
      mmf1 = MoveMoveFormat Black (Just (PlaceFormat 1 2)) (Just (PlaceFormat 1 3)) "FU" Nothing Nothing Nothing Nothing
  describe "JKF.Type" $ do
    describe "Header toJSON" $ do
      it "toJSON (Header \"habu\" \"moriuchi\" \"2015-10-28 07:43\" \"2015-10-28 20:40\" \"hirate\")" $
        toJSON h1
        `shouldBe`
        Object (fromList [
                    ("後手",String "moriuchi")
                   ,("終了日時",String "2015-10-28 20:40")
                   ,("手合割",String "hirate")
                   ,("先手",String "habu")
                   ,("開始日時",String "2015-10-28 07:43")
                   ])
      it "toJSON (Header \"\" \"\" \"\" \"\" \"\")" $
        toJSON h2
        `shouldBe`
        Object (fromList [
                    ("後手",String "")
                   ,("終了日時",String "")
                   ,("手合割",String "")
                   ,("先手",String "")
                   ,("開始日時",String "")
                   ])

    describe "MoveMoveFormat toJSON" $ do
      it "toJSON (MoveMoveFormat Black (Just (PlaceFormat 1 2)) (Just (PlaceFormat 1 3)) \"FU\" Nothing Nothing Nothing Nothing)" $
        toJSON mmf1
        `shouldBe`
        Object (fromList [
                    ("moveMoveFormatColor",String "Black")
                   ,("moveMoveFormatFrom",Object (fromList [("placeFormatX",Number 1.0),("placeFormatY",Number 2.0)]))
                   ,("moveMoveFormatTo",Object (fromList [("placeFormatX",Number 1.0),("placeFormatY",Number 3.0)]))
                   ,("moveMoveFormatPiece",String "FU")
                   ,("moveMoveFormatSame",Null)
                   ,("moveMoveFormatRelative",Null)
                   ,("moveMoveFormatCapture",Null)
                   ,("moveMoveFormatPromote",Null)
                   ])
