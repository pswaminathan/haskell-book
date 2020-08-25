module Ch10Db where

import           Data.Time

data DatabaseItem = DbString String
    | DbNumber Integer
    | DbDate UTCTime
    deriving (Eq, Ord, Show)


theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

isDbDate :: DatabaseItem -> Bool
isDbDate (DbDate t) = True
isDbDate _          = False

isDbNumber :: DatabaseItem -> Bool
isDbNumber (DbNumber _) = True
isDbNumber _            = False

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = (map utcTime) . (filter isDbDate)
  where utcTime (DbDate t) = t

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = (map num) . (filter isDbNumber)
  where num (DbNumber x) = x


mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = (foldr max nd) . (map utcTime) . (filter isDbDate)
  where nd = UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)
        utcTime (DbDate t) = t

mrEasy :: [DatabaseItem] -> UTCTime
mrEasy = maximum . (map utcTime) . (filter isDbDate)
  where utcTime (DbDate t) = t


sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (+) 0 . map num . filter isDbNumber
  where num (DbNumber x) = x

sumEasy :: [DatabaseItem] -> Integer
sumEasy = sum . map num . filter isDbNumber
  where num (DbNumber x) = x


avgDb :: [DatabaseItem] -> Double
avgDb [] = 0
avgDb db = (fromIntegral $ sum nums) / (fromIntegral $ length nums)
  where num (DbNumber x) = x
        nums = map num . filter isDbNumber $ db
