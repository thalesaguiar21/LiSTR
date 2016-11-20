infixr 5 :>
data Set a = Empty | a :> (Set a) deriving (Read)

instance Show a => Show (Set a) where
  show Empty = "-"
  show (x :> Empty) = show x
  show (x :> rest) = show x ++ " " ++ show rest

isEmpty :: Eq a => Set a -> Bool
isEmpty Empty = True
isEmpty x     = False

in' :: Eq a => Set a -> a -> Bool
in' Empty _        = False
in' (x :> rest) y  = if x == y then True
                     else in' rest y

rm :: Eq a => Set a -> a -> Set a
rm  Empty _ = Empty
rm (x :> rest) y = if x == y then rest
                   else x :> rm rest y

size :: Eq a => Set a -> Int
size Empty = 0
size (x :> rest) = 1 + size rest
