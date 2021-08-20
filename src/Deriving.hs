{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- https://kowainik.github.io/posts/deriving.html
-- Strategic Deriving

module Deriving where

import Control.Monad.Reader (MonadIO, MonadReader)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable)
import Data.Ix (Ix)
import Data.Text (Text)
import GHC.Generics (Generic, Generic1)
import Type.Reflection (typeOf)

-- Typesclasses and Instances

data Behavior = Naughty | Nice

class YearBehaviour a where
  yearBehaviour :: a -> Behavior

instance YearBehaviour Int where
  yearBehaviour :: Int -> Behavior
  yearBehaviour 0 = Naughty
  yearBehaviour _ = Nice

instance YearBehaviour Double where
  yearBehaviour :: Double -> Behavior
  yearBehaviour 0 = Naughty
  yearBehaviour n
    | isNaN n || isInfinite n = Naughty
    | otherwise = Nice

canHaveChristmasGift :: YearBehaviour a => a -> Text
canHaveChristmasGift x = case yearBehaviour x of
  Nice -> "Ho-ho-ho! Looks like somebody deserves a toy!"
  Naughty -> "You were naughty this year! Better luck next year ;)"

-- foo = canHaveChristmasGift ("Trust me, I am nice!" :: Text)
-- No instance for (YearBehaviour Text)

-- Deriving

data Gift = Gift
  { giftId :: Int,
    giftType :: GiftType
  }
  deriving (Eq, Ord)

data GiftType
  = Candies CandyCounter
  | Toy Name
  deriving stock (Eq, Ord)

newtype Name = Name
  { unName :: Text
  }
  deriving (Eq, Ord)

newtype CandyCounter = CandyCounter
  { unCandyCounter :: Int
  }
  deriving (Eq, Ord)

bar =
  compare
    (Gift 1 (Toy $ Name "Strawberry Bear"))
    (Gift 2 (Candies $ CandyCounter 1000))

-- Standard deriving

data Elf = Elf Text Int
  deriving (Show, Eq)

data ChristmasTreats = Candies' | Chocolate | MincedPie
  deriving stock (Show, Read, Eq, Ord, Enum, Bounded, Ix)

-- Auto derived

qux = typeOf Chocolate

-- Derive Whatever

data Gift' a
  = None
  | Some a
  deriving stock (Functor, Foldable, Traversable, Generic, Generic1)

-- Newtypes

newtype GiftCount = GiftCount Int
  deriving newtype (Num)

-- Any class derivations

class MyEq a where
  equal :: a -> a -> Bool
  default equal :: Show a => a -> a -> Bool
  equal x y = show x == show y

data Gift'' = Gift'' Text Int
  deriving stock (Show)
  deriving anyclass (MyEq)

data Elf' = Elf'
  { name :: Text,
    workGroupId :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

-- Via

class Beard faceOfTheWinterHoliday where
  beardDescription :: faceOfTheWinterHoliday -> String

newtype Santa = Santa String
  deriving newtype (Show)

instance Beard Santa where
  beardDescription (Santa name) = name <> " has long white beard"

newtype FatherFrost = FatherFrost String
  deriving newtype (Show)
  deriving (Beard) via Santa

-- Standalone deriving

data Gift''' a
  = None'''
  | Some''' a

deriving stock instance (Show a) => Show (Gift''' a)

-- deriving via (Last (Gift''' a)) instance Semigroup (Gift''' a)

-- Empty deriving

data EmptyGiftForNaugthy
  deriving stock (Show, Eq)

-- Training 1: Specify strategy

data GiftType' = Toy' | Bicycle' | Dress' | Shoe'
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Ix)

newtype Email = Email
  { unEmail :: Text
  }
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, Hashable)
  deriving anyclass (FromJSON, ToJSON)

data MyEnv

newtype App a = App
  { unApp :: ReaderT MyEnv IO a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader MyEnv)
