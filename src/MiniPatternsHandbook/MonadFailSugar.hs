module MiniPatternsHandbook.MonadFailSugar where

import Data.Maybe (Maybe (Nothing), fromMaybe)
import Text.Read (readMaybe)

sumThree :: String -> Maybe Int
sumThree s = do
  [a, b, c] <- traverse readMaybe $ words s
  return $ a + b + c

catMaybes :: [Maybe a] -> [a]
catMaybes xs = [x | Just x <- xs]

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f as = [b | a <- as, Just b <- [f a]]

threeNothing :: Maybe a -> Maybe b -> Maybe c -> Maybe ()
threeNothing ma mb mc = do
  Nothing <- Just ma
  Nothing <- Just mb
  Nothing <- Just mc
  return ()
