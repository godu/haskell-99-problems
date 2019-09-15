import NinetyNineProblems
  ( ListItem(Multiple, Single)
  , NestedList(Elem, List)
  , compress
  , decodeModified
  , diffSelect
  , dropEvery
  , dupli
  , elementAt
  , encode
  , encodeDirect
  , encodeModified
  , flatten
  , insertAt
  , isPalindrome
  , myButLast
  , myLast
  , myLength
  , myReverse
  , pack
  , range
  , removeAt
  , repli
  , rndPermu
  , rndSelect
  , rotate
  , slice
  , split
  )
import System.Random (mkStdGen)
import Test.Hspec (hspec, it, shouldBe, shouldReturn)

main :: IO ()
main =
  hspec $ do
    it "Problem 1" $ do
      myLast [1, 2, 3, 4] `shouldBe` 4
      myLast ['x', 'y', 'z'] `shouldBe` 'z'
    it "Problem 2" $ do
      myButLast [1, 2, 3, 4] `shouldBe` 3
      myButLast ['a' .. 'z'] `shouldBe` 'y'
    it "Problem 3" $ do
      elementAt [1, 2, 3] 2 `shouldBe` 2
      elementAt "haskell" 5 `shouldBe` 'e'
    it "Problem 4" $ do
      myLength [123, 456, 789] `shouldBe` 3
      myLength "Hello, world!" `shouldBe` 13
    it "Problem 5" $ do
      myReverse "A man, a plan, a canal, panama!" `shouldBe`
        "!amanap ,lanac a ,nalp a ,nam A"
      myReverse [1, 2, 3, 4] `shouldBe` [4, 3, 2, 1]
    it "Problem 6" $ do
      isPalindrome [1, 2, 3] `shouldBe` False
      isPalindrome "madamimadam" `shouldBe` True
      isPalindrome [1, 2, 4, 8, 16, 8, 4, 2, 1] `shouldBe` True
    it "Problem 7" $ do
      flatten (Elem 5) `shouldBe` [5]
      flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) `shouldBe`
        [1, 2, 3, 4, 5]
      (flatten (List []) :: [Int]) `shouldBe` []
    it "Problem 8" $ compress "aaaabccaadeeee" `shouldBe` "abcade"
    it "Problem 9" $
      pack
        ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] `shouldBe`
      ["aaaa", "b", "cc", "aa", "d", "eeee"]
    it "Problem 10" $
      encode "aaaabccaadeeee" `shouldBe`
      [(4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')]
    it "Problem 11" $
      encodeModified "aaaabccaadeeee" `shouldBe`
      [ Multiple 4 'a'
      , Single 'b'
      , Multiple 2 'c'
      , Multiple 2 'a'
      , Single 'd'
      , Multiple 4 'e'
      ]
    it "Problem 12" $
      decodeModified
        [ Multiple 4 'a'
        , Single 'b'
        , Multiple 2 'c'
        , Multiple 2 'a'
        , Single 'd'
        , Multiple 4 'e'
        ] `shouldBe`
      "aaaabccaadeeee"
    it "Problem 13" $
      encodeDirect "aaaabccaadeeee" `shouldBe`
      [ Multiple 4 'a'
      , Single 'b'
      , Multiple 2 'c'
      , Multiple 2 'a'
      , Single 'd'
      , Multiple 4 'e'
      ]
    it "Problem 14" $ dupli [1, 2, 3] `shouldBe` [1, 1, 2, 2, 3, 3]
    it "Problem 15" $ repli "abc" 3 `shouldBe` "aaabbbccc"
    it "Problem 16" $ dropEvery "abcdefghik" 3 `shouldBe` "abdeghk"
    it "Problem 17" $ split "abcdefghik" 3 `shouldBe` ("abc", "defghik")
    it "Problem 18" $
      slice ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'k'] 3 7 `shouldBe`
      "cdefg"
    it "Problem 19" $ do
      rotate ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'] 3 `shouldBe` "defghabc"
      rotate ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'] (-2) `shouldBe` "ghabcdef"
    it "Problem 20" $ removeAt 2 "abcd" `shouldBe` ('b', "acd")
    it "Problem 21" $ insertAt 'X' "abcd" 2 `shouldBe` "aXbcd"
    it "Problem 22" $ range 4 9 `shouldBe` [4, 5, 6, 7, 8, 9]
    it "Problem 23" $
      fst (rndSelect "abcdefgh" 3 (mkStdGen 1676486720)) `shouldBe` "cad"
    it "Problem 24" $
      fst (diffSelect 6 49 (mkStdGen 1676486720)) `shouldBe`
      [3, 36, 42, 46, 43, 23]
    it "Problem 25" $
      fst (rndPermu "abcdef" (mkStdGen 1676486720)) `shouldBe` "edbfac"
