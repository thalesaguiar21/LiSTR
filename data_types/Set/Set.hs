

data Set a = Empty
    | Put a (Set a)
    deriving (Show, Read)


find :: Eq a => Set a -> a -> Bool
find Empty _ = False
find (Put x (residue)) y = if x == y then True
                         else find residue y
