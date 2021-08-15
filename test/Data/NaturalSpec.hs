module Data.NaturalSpec where

import Data.Natural
  ( Natural (..),
    fromInt,
  )
import Data.Proxy (Proxy (..))
import Data.Semiring (Semiring, fromNatural, one, plus, times, zero)
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
import Prelude (Eq, Ord, Show, (.), (<$>))

newtype Bar = Bar Natural deriving (Eq, Ord, Show)

instance Semiring Bar where
  zero = Bar zero
  one = Bar one
  fromNatural = Bar . fromNatural
  (Bar a) `plus` (Bar b) = Bar (a `plus` b)
  (Bar a) `times` (Bar b) = Bar (a `times` b)

instance Arbitrary Bar where
  arbitrary = Bar . fromInt <$> choose (0, 20)

spec :: Spec
spec =
  forAllLaws
    [ semiringLaws (Proxy :: Proxy Bar),
      eqLaws (Proxy :: Proxy Bar),
      ordLaws (Proxy :: Proxy Bar)
    ]
