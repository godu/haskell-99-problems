module Data.QueueSpec where

import Data.Queue (Queue, dequeue, empty, enqueue, fromList, isEmpty)
import Test.Hspec (Spec, it, shouldBe)

data Message = First | Second deriving (Show, Eq)

spec :: Spec
spec = do
  it "isEmpty" $ do
    isEmpty empty `shouldBe` True
    isEmpty (fromList []) `shouldBe` True
    isEmpty (enqueue First empty) `shouldBe` False
    isEmpty (snd $ dequeue $ enqueue First empty) `shouldBe` True

  it "enqueue" $ do
    (empty :: Queue Message) `shouldBe` fromList []
    enqueue First empty `shouldBe` fromList [First]
    enqueue Second (enqueue First empty) `shouldBe` fromList [First, Second]

  it "enqueue/dequeue" $ do
    dequeue (empty :: Queue ()) `shouldBe` (Nothing, empty)
    dequeue (fromList [First]) `shouldBe` (Just First, empty)
    dequeue (fromList [First, Second]) `shouldBe` (Just First, fromList [Second])
