module Ex9Programmers where

data OperatingSystem = GnuPlusLinux
    | OpenBsdPlusNeverMindJustBsdStill
    | Mac
    | Windows
    deriving (Eq, Show, Enum)

data ProgLang = Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show, Enum)

data Programmer = Programmer
    { os   :: OperatingSystem
    , lang :: ProgLang
    }
    deriving (Eq, Show)


allProgrammers :: [Programmer]
allProgrammers = [Programmer { os = os
                             , lang = lang }
                             | os <- enumFrom $ toEnum 0
                             , lang <- enumFrom $ toEnum 0]
