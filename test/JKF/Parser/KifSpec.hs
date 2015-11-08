{-# LANGUAGE OverloadedStrings #-}
module JKF.Parser.KifSpec where

import           Data.Aeson
import           Data.HashMap.Strict     (fromList)
import           JKF.Parser.Kif
import           JKF.Parser.Kif.Internal
import           JKF.Type
import           Test.Hspec
import           Text.Parsec

spec :: Spec
spec = do
  describe "JKF.Parser.Kif" $ do
    describe "sashite parser" $ do
      it "sashite $ \"1 ７六歩(77)\\n\"" $
        parse sashite "sashite" "1 ７六歩(77)\n"
        `shouldBe`
        Right (MoveMoveFormat {
                  moveMoveFormatColor = Black
                , moveMoveFormatFrom = Just (PlaceFormat 7 7)
                , moveMoveFormatTo = Just (PlaceFormat 7 6)
                , moveMoveFormatPiece = FU
                , moveMoveFormatSame = Just False
                , moveMoveFormatPromote = Just False
                , moveMoveFormatCapture = Nothing
                , moveMoveFormatRelative = Nothing
                })

      it "sashite $ \" 4 同　銀(31)\\n\"" $
        parse sashite "sashite" " 4 同　銀(31)\n"
        `shouldBe`
        Right (MoveMoveFormat {
                  moveMoveFormatColor = White
                , moveMoveFormatFrom = Just (PlaceFormat 3 1)
                , moveMoveFormatTo = Nothing
                , moveMoveFormatPiece = GI
                , moveMoveFormatSame = Just True
                , moveMoveFormatPromote = Just False
                , moveMoveFormatCapture = Nothing
                , moveMoveFormatRelative = Nothing
                })


      it "sashite $ \"3 ２二角成(88)\\n\"" $
        parse sashite "sashite" "3 ２二角成(88)\n"
        `shouldBe`
        Right (MoveMoveFormat {
                  moveMoveFormatColor = Black
                , moveMoveFormatFrom = Just (PlaceFormat 8 8)
                , moveMoveFormatTo = Just (PlaceFormat 2 2)
                , moveMoveFormatPiece = KA
                , moveMoveFormatSame = Just False
                , moveMoveFormatPromote = Just True
                , moveMoveFormatCapture = Nothing
                , moveMoveFormatRelative = Nothing
                })

--       it "many sashite $ \"1 ７六歩(77)\n2 ３四歩(33)\n3 ２二角成(88)\n 4 同　銀(31)\n5 ４五角打\n\"" $
--         parse  (many sashite) "sashite" "1 ７六歩(77)\n2 ３四歩(33)\n3 ２二角成(88)\n 4 同　銀(31)\n5 ４五角打\n"
--         `shouldBe`
--         Right [("1", (7,6), "歩", (7,7)),("2", (3,4), "歩", (3,3)),("3", (2,2), "角成(88)"),("4","同　銀(31)"),("5", "４五角打")]

--       it "many sashite $ \"   1 ３四歩(33)        \n   2 ２六歩(27)        \n   3 ３三角(22)        \n   4 ２五歩(26)        \n   5 ４四歩(43)        \n   6 ４八銀(39)        \n   7 ３二銀(31)        \n   8 ５六歩(57)        \n   9 ４三銀(32)        \n  10 ７八銀(79)        \n  11 ２二飛(82)        \n  12 ７九角(88)        \n  13 ６二王(51)        \n  14 ５七角(79)        \n  15 ７二王(62)        \n  16 ６八王(59)        \n\"" $
--         parse  (many sashite) "sashite" "   1 ３四歩(33)        \n   2 ２六歩(27)        \n   3 ３三角(22)        \n   4 ２五歩(26)        \n   5 ４四歩(43)        \n   6 ４八銀(39)        \n   7 ３二銀(31)        \n   8 ５六歩(57)        \n   9 ４三銀(32)        \n  10 ７八銀(79)        \n  11 ２二飛(82)        \n  12 ７九角(88)        \n  13 ６二王(51)        \n  14 ５七角(79)        \n  15 ７二王(62)        \n  16 ６八王(59)        \n"
--         `shouldBe`
--         Right [("1", "３四歩(33)"),("2", "２六歩(27)"),("3", "３三角(22)"),("4", "２五歩(26)"),("5", "４四歩(43)"),("6", "４八銀(39)"),("7", "３二銀(31)"),("8", "５六歩(57)"),("9", "４三銀(32)"),("10", "７八銀(79)"),("11", "２二飛(82)"),("12", "７九角(88)"),("13", "６二王(51)"),("14", "５七角(79)"),("15", "７二王(62)"),("16", "６八王(59)")]

