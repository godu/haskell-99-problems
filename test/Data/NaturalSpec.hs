module Data.NaturalSpec where

import Data.Natural
  ( Natural (..),
    fromInt,
  )
import Data.Proxy (Proxy (..))
import Test.Hspec (Spec)
import Test.QuickCheck.Arbitrary
  ( Arbitrary,
    arbitrary,
  )
import Test.QuickCheck.Classes
  ( eqLaws,
    ordLaws,
    semiringLaws,
  )
import Test.QuickCheck.Gen (choose)
import Test.Utils (forAllLaws)
import Prelude ((<$>))

instance Arbitrary Natural where
  arbitrary = fromInt <$> choose (0, 20)

spec :: Spec
spec =
  forAllLaws
    [ semiringLaws (Proxy :: Proxy Natural),
      eqLaws (Proxy :: Proxy Natural),
      ordLaws (Proxy :: Proxy Natural)
    ]
