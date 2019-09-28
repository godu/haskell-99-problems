module Data.Peano where

data Peano
  = Zero
  | Succ Peano

instance Enum Peano where
  fromEnum Zero = 0
  fromEnum (Succ x) = 1 + fromEnum x
  toEnum 0 = Zero
  toEnum x
    | x > 0 = Succ (toEnum (x - 1))
    | otherwise = error ("toEnum " <> show x)

instance Eq Peano where
  (==) Zero Zero = True
  (==) Zero _ = False
  (==) _ Zero = False
  (==) (Succ a) (Succ b) = a == b

instance Show Peano where
  show = show . fromEnum

instance Ord Peano where
  (<=) Zero Zero = True
  (<=) Zero _ = True
  (<=) _ Zero = False
  (<=) (Succ a) (Succ b) = a <= b

instance Read Peano where
  readsPrec d = map (\(a, b) -> (toEnum a, b)) . readsPrec d
