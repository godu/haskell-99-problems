module MiniPatternsHandbook.SmartConstructor
  ( Tag,
    unTag,
    mkTag,
    TagsList,
    unTagsList,
    mkTagsList,
  )
where

import Data.List.NonEmpty (NonEmpty (..), nonEmpty)

newtype Tag = Tag String

unTag :: Tag -> String
unTag (Tag tag) = tag

mkTag :: String -> Maybe Tag
mkTag tag
  | null tag = Nothing
  | otherwise = return $ Tag tag

newtype TagsList = TagsList (NonEmpty Tag)

unTagsList :: TagsList -> NonEmpty Tag
unTagsList (TagsList tagsList) = tagsList

mkTagsList :: [String] -> Maybe TagsList
mkTagsList [] = Nothing
mkTagsList tags = TagsList <$> (traverse mkTag =<< nonEmpty tags)
