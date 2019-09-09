module Lib
  ( 
  )
where

import           Text.Read                      ( readMaybe )

parseItem :: String -> Maybe Int
parseItem = readMaybe . filter (/= '+')

parseList :: String -> Maybe [Int]
parseList = traverse parseItem . lines

-- day01a :: String -> IO ()
-- day01a s = print $ map sum $ parseList $ s

main = putStrLn "Hello World"
