module DataTypes.Set.Set
    ( Set (..)
    , isEmpty
    , in'
    , add
    , rm
    , card
    , subset
    , dif
    , (/\)
    , (\/)
    ) where

infixr 5 :>
data Set a = Empty | a :> (Set a) deriving (Read)

instance Show a => Show (Set a) where
  show s = "{" ++ wSet s

wSet :: Show a => Set a -> String
wSet Empty = "}"
wSet (x :> Empty) = show x ++ "}"
wSet (x :> rest) = show x ++ ", " ++ wSet rest

instance Eq a => Eq (Set a) where
  Empty == Empty = True
  Empty == (y :> t) = False
  (x :> s) == Empty = False
  (x :> s) == t = if not (card (x :> s) == card t) then False
                  else if not (in' x t) then False
                  else s == rm t x

-- Verifica se um Set é vazio
isEmpty :: Eq a => Set a -> Bool
isEmpty Empty = True
isEmpty x     = False

-- Verifica se um elemento pertence à um Set
in' :: Eq a => a -> Set a -> Bool
in' _ Empty       = False
in' y (x :> rest) = if x == y then True
                    else in' y rest

-- Adiciona um elemento à um Set
add :: Eq a => Set a -> a -> Set a
add Empty y = y :> Empty
add s y     = if in' y s then s
              else y :> s

-- Remove um elemento de um set
rm :: Eq a => Set a -> a -> Set a
rm  Empty _ = Empty
rm (x :> rest) y = if x == y then rest
                   else x :> rm rest y

-- Cria um Set a partir de uma lista
build :: Eq a => [a] -> Set a
build [] = Empty
build (x:s) = x :> build s

-- Calcula a cardinalidade de um Set
card :: Eq a => Set a -> Int
card Empty = 0
card (x :> rest) = 1 + card rest

-- Verifica se o primeiro Set é subconjunto do segundo
subset :: Eq a => Set a -> Set a -> Bool
subset Empty Empty = True
subset _ Empty = False
subset Empty _ = True
subset (x :> rest) s = if not (in' x s) then False
                       else subset rest s

-- Retorna a diferença entreo primeiro e segundo Set
dif :: Eq a => Set a -> Set a -> Set a
Empty `dif` Empty = Empty
Empty `dif` t     = Empty
s `dif` Empty     = Empty
(x :> s) `dif` t  = if not (in' x t) then x :> (s `dif` t)
                else s `dif` t

-- Retorna um Set contendo a interseção entre o primeiro e segundo Sets
(/\) :: Eq a => Set a -> Set a -> Set a
Empty /\ Empty = Empty
Empty /\ _ = Empty
_ /\ Empty = Empty
(x :> s) /\ t = if in' x t then x :> (s /\ t)
                else (s /\ t)

-- Retorna um set contendo a união entre o primeiro e sgundo Sets
(\/) :: Eq a => Set a -> Set a -> Set a
Empty \/ Empty = Empty
Empty \/ t = t
s \/ Empty = s
(x :> s) \/ t = if not (in' x t) then s \/ (x :> t)
                else s \/ t
