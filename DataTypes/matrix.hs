module DataTypes.Matrix
    (Matrix (..)
    {-
    , det
    , get
    , set
    , line
    , colum
    , transpose
    , inverse
    -}
    ) where

data Matrix a = CMatrix Int Int [[a]] --(Eq, Num)

instance Show a => Show (Matrix a) where
  show (CMatrix i j b) = printAllLines b

instance Eq a => Eq (Matrix a) where
  (CMatrix i j b) == (CMatrix k l c) = if (i/=k) || (j/=l) then False
                                        else b==c

{-instance Num a => Num (Matrix a) where
  (CMatrix i j b) + (CMatrix k l c) = if (i/=k) || (j/=l) then error "Matrizes de tamanhos diferentes não podem ser somadas"
                                      else let m = (CMatrix k l c)
-}

buildMatrix :: Num t => t -> Int -> Int -> Matrix t
buildMatrix t i j =  if ((i==0) || (j==0)) then error "O número de linhas e colunas deve ser maior que zero"
                else let line = take j (cycle [t])
                in
                ( CMatrix i j (take i (cycle [line])) )


-- PRINTS
printAllLines :: Show a => [[a]] -> String
printAllLines [] = "\n"
printAllLines (c:b) = startLine c ++ "\n" ++ printAllLines b

startLine :: Show t => [t] -> String
startLine [a] = " | " ++ show a ++ " | "
startLine (a:b) = " | " ++ show a ++ endLine b

endLine :: Show t => [t] -> String
endLine [a] = " " ++ show a ++ " | "
endLine (a:b) = " " ++ show a ++ endLine b
-- END PRINTS

get :: Matrix t -> Int -> Int -> t
get (CMatrix k l m) i j = if (k<=i) || (l<=j) || (i<0) || (j<0) then error "Indice fora de alcance"
                          else ((m !! i) !! j)

line :: Num t => Matrix t -> Int -> [t]
line (CMatrix i j m) l = (m !! l)


--colum :: Num t => Matrix t -> [t]
--colum (CMatrix i j m) l = [ x | x <- ]
