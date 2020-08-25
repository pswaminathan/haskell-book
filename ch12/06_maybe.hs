module MyMaybe where

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

isNothing :: Maybe a -> Bool
isNothing Nothing  = True
isNothing (Just _) = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee def _ Nothing = def
mayybee _ fn (Just x) = fn x

fromMaybe :: a -> Maybe a -> a
fromMaybe def = mayybee def id

listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes []             = []
catMaybes (Nothing : xs) = catMaybes xs
catMaybes (Just x : xs)  = x : catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe []            = Just []
flipMaybe (Nothing : _) = Nothing
flipMaybe (Just x : xs) = mayybee Nothing combine $ flipMaybe xs
    where combine ys = Just (x:ys)
