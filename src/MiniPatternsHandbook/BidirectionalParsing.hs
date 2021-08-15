{-# LANGUAGE LambdaCase #-}

module MiniPatternsHandbook.BidirectionalParsing where

import Relude.Extra.Enum (inverseMap)

data Colour
  = Red
  | Green
  | Yellow
  | Blue
  deriving (Bounded, Enum, Show)

showColour :: Colour -> String
showColour = \case
  Red -> "red"
  Green -> "green"
  Yellow -> "yellow"
  Blue -> "blue"

parseColour :: String -> Maybe Colour
parseColour = inverseMap showColour

data FruitName
  = Apple
  | Orange
  | Lemon
  | Blueberry
  deriving (Show, Enum, Bounded)

showFruitName :: FruitName -> String
showFruitName = \case
  Apple -> "apple"
  Orange -> "orange"
  Lemon -> "lemon"
  Blueberry -> "blueberry"

parseFruitName :: String -> Maybe FruitName
parseFruitName = inverseMap showFruitName
