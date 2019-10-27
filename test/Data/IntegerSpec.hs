module Data.IntegerSpec where

import Data.Integer (Integer(..), fromInt)
import Data.Proxy (Proxy(..))
import Prelude ((<$>))
import Test.Hspec (Spec)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Classes (eqLaws, ordLaws, ringLaws, semiringLaws)
import Test.QuickCheck.Gen (choose)
import Test.Utils (forAllLaws)

instance Arbitrary Integer where
  arbitrary = fromInt <$> choose (-10, 10)

spec :: Spec
spec =
  forAllLaws
    [ eqLaws (Proxy :: Proxy Integer)
    , semiringLaws (Proxy :: Proxy Integer)
    , ringLaws (Proxy :: Proxy Integer)
    , ordLaws (Proxy :: Proxy Integer)
    ]
