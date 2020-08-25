module Ch7HOFs where

data Employee = Coder
    | Manager
    | Veep
    | CEO
    deriving (Eq, Ord, Show, Enum)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
  putStrLn $ show e ++
             " is the boss of " ++
             show e'

employeeRank :: Employee -> Employee -> IO ()
employeeRank e e' = case compare e e' of
                      GT -> reportBoss e e'
                      EQ -> putStrLn "Neither employee is the boss"
                      LT -> (flip reportBoss) e e'

codersRuleCeosDrool :: Employee -> Employee -> Ordering
codersRuleCeosDrool Coder Coder = EQ
codersRuleCeosDrool Coder _     = GT
codersRuleCeosDrool _ Coder     = LT
codersRuleCeosDrool e e'        = compare e e'

eRank :: (Employee -> Employee -> Ordering)
      -> Employee
      -> Employee
      -> IO ()
eRank cmp e e' =
  case cmp e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither is the boss"
    LT -> (flip reportBoss) e e'
