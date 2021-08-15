module Data.IntegerSpec where

import Data.Integer
  ( Integer (..),
    fromInt,
  )
import Data.Proxy (Proxy (..))
import Data.Semiring (Ring, Semiring, fromNatural, negate, one, plus, times, zero)
import Test.Hspec (Spec)
import Test.QuickCheck.Arbitrary
  ( Arbitrary,
    arbitrary,
  )
import Test.QuickCheck.Classes
  ( eqLaws,
    ordLaws,
    ringLaws,
    semiringLaws,
  )
import Test.QuickCheck.Gen (choose)
import Test.Utils (forAllLaws)
import Prelude (Eq, Ord, Show, (.), (<$>))

newtype Bar = Bar Integer deriving (Eq, Ord, Show)

instance Semiring Bar where
  zero = Bar zero
  one = Bar one
  fromNatural = Bar . fromNatural
  (Bar a) `plus` (Bar b) = Bar (a `plus` b)
  (Bar a) `times` (Bar b) = Bar (a `times` b)

instance Ring Bar where
  negate (Bar a) = (Bar . negate) a

instance Arbitrary Bar where
  arbitrary = Bar . fromInt <$> choose (-10, 10)

spec :: Spec
spec =
  forAllLaws
    [ eqLaws (Proxy :: Proxy Bar),
      semiringLaws (Proxy :: Proxy Bar),
      ringLaws (Proxy :: Proxy Bar),
      ordLaws (Proxy :: Proxy Bar)
    ]
