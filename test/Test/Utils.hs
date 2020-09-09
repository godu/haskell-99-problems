module Test.Utils where

import Test.Hspec
  ( Spec,
    describe,
    it,
  )
import Test.QuickCheck (property)
import Test.QuickCheck.Classes
  ( Laws (..),
    lawsProperties,
    lawsTypeclass,
  )
import Prelude
  ( fst,
    mapM_,
    sequence_,
    snd,
    ($),
    (<$>),
  )

forAllLaws :: [Laws] -> Spec
forAllLaws = mapM_ toDescribe
  where
    toIt properties = it (fst properties) $ property (snd properties)
    toDescribe laws =
      describe (lawsTypeclass laws) (sequence_ $ toIt <$> lawsProperties laws)
