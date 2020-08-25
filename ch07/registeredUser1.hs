module RegisteredUser where

newtype Username = Username String

newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser
    | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser
            (Username name)
            (AccountNumber acctNum)) =
  putStrLn $ name ++ " " ++ show acctNum

data WherePenguinsLive = Galapagos
    | Antarctica
    | Australia
    | SouthAfrica
    | SouthAmerica
    deriving (Eq, Show)

data Penguin = Peng WherePenguinsLive
    deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _           = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereItLives) = whereItLives

isGalapagosPenguin :: Penguin -> Bool
isGalapagosPenguin (Peng Galapagos) = True
isGalapagosPenguin _                = False

isAntarcticPenguin :: Penguin -> Bool
isAntarcticPenguin (Peng Antarctica) = True
isAntarcticPenguin _                 = False

isAntarcticOrGalapagos :: Penguin -> Bool
isAntarcticOrGalapagos p = (isAntarcticPenguin p) || (isGalapagosPenguin p)


f :: (a, b) -> (c, d) -> ((b, d), (a, c))
-- f x y = ((snd x, snd y) (fst x, fst y))
f (a, b) (c, d) = ((b, d), (a, c))
