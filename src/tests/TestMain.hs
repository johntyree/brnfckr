
module Main where

import Data.Word
import Data.Char
import Test.Hspec
import Test.QuickCheck

import Brnfckr.Parse
import Brnfckr.Eval

import Paths_brnfckr


helloWorld, loop, singleOp :: String
loop = "[>.<],[>.<-]"
singleOp = ",.-+><"
helloWorld = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+."


main :: IO ()
main = do
  bottlesBF <- getDataFileName "programs/bottles.bf" >>= readFile
  bottlesOut <- getDataFileName "programs/bottles.out" >>= readFile
  hspec $ do
    describe "Parsing" $ do
        it "unbalanced parens0" $ parseBrainFuck "[" `shouldBe` Left (UnbalancedBracket "[")
        it "unbalanced parens0" $ parseBrainFuck "]" `shouldBe` Left (UnbalancedBracket "]")
        it "unbalanced parens1" $ parseBrainFuck "+++++[>+++++++>++<<-]>.>.[" `shouldBe` Left (UnbalancedBracket "[")

    describe "Parsing errors bubble up while running" $ do
        it "unbalanced parens0" $ (fst . fst) (runBrainFuck "[" "") `shouldBe` Left (UnbalancedBracket "[")
        it "unbalanced parens0" $ (fst . fst) (runBrainFuck "]" "") `shouldBe` Left (UnbalancedBracket "]")
        it "unbalanced parens1" $ (fst . fst) (runBrainFuck "+++++[>+++++++>++<<-]>.>.[" "") `shouldBe` Left (UnbalancedBracket "[")

    describe "error handling" $
        it "insufficient input" $ executeString "," "" `shouldBe` Nothing

    describe "basic programs" $ do
        it "output until inclusive 0" $ let s = map (chr . fromIntegral) $ [1 :: Word8 .. 255] ++ [0]
                                        in executeString "+[,.]" s `shouldBe` Just s
        it "output n exclamation marks" $ property $ \w ->
            let plusThirtyThree = replicate (ord '!') '+'
            in executeString (">" ++ plusThirtyThree ++ "<,[>.<-]") [chr $ fromIntegral w] `shouldBe` Just (replicate (fromIntegral (w :: Word8)) '!')

        it "memory 0 initialized" $ executeString ".>." "" `shouldBe` Just (toChars [0, 0 :: Word8])
        it "memory operations" $ executeString ".>.+.<." "" `shouldBe` Just (toChars [0, 0, 1, 0 :: Word8])
        it "input output" $ property $ \w -> executeString ".,." [toChar w] `shouldBe` Just (toChars [0, w :: Word8])

        it "loops" $ property $ \w -> executeString "[>.<],[>.<-]" [toChar w] `shouldBe` Just (toChars $ replicate (fromIntegral w) (0 :: Word8))

    describe "complex programs" $ do
        it "Hello World!" $ executeString helloWorldBF "" `shouldBe` Just "Hello World!"
        it "Numbers" $ executeString numbersBF [chr 10] `shouldBe` Just "1, 1, 2, 3, 5, 8, 13, 21, 34, 55"
        it "Bottles of beer" $ executeString bottlesBF "" `shouldBe` Just bottlesOut

  where
    helloWorldBF = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+."
    numbersBF    = ",>+>>>>++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++++++++++++++++<<<<<<[>[>>>>>>+>+<<<<<<<-]>>>>>>>[<<<<<<<+>>>>>>>-]<[>++++++++++[-<-[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>[<<[>>>+<<<-]>>[-]]<<]>>>[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>[<<+>>[-]]<<<<<<<]>>>>>[++++++++++++++++++++++++++++++++++++++++++++++++.[-]]++++++++++<[->-<]>++++++++++++++++++++++++++++++++++++++++++++++++.[-]<<<<<<<<<<<<[>>>+>+<<<<-]>>>>[<<<<+>>>>-]<-[>>.>.<<<[-]]<<[>>+>+<<<-]>>>[<<<+>>>-]<<[<+>-]>[<+>-]<<<-]"
    toChar       = chr . fromIntegral
    toChars      = map toChar
