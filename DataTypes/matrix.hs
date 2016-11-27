module DataTypes.Matrix
    (Matrix (..)
    , get
    , set
    , line
    , colum
    , ident
    {-
    , transpose
    , inverse
    , det
    -}
    ) where

data Matrix a = CMatrix Int Int [[a]] --(Eq, Num)

instance Show a => Show (Matrix a) where
  show (CMatrix i j b) = printAllLines b

instance Eq a => Eq (Matrix a) where
  (CMatrix i j b) == (CMatrix k l c) = if (i/=k) || (j/=l) then False
                                        else b==c

instance Num a => Num (Matrix a) where
  (CMatrix i j m1) + (CMatrix k l m2) = if (i/=k) || (j/=l) then error "|+| >> As matrizes devem possuir o mesmo tamanho"
                                        else (CMatrix i j (sumMatrix m1 m2))

  (CMatrix i j m1) - (CMatrix k l m2) = if (i/=k) || (j/=l) then error "|-| >> As matrizes devem possuir o mesmo tamanho"
                                        else (CMatrix i j (subMatrix m1 m2))

  m1 * m2 = if (snd (size m1))/=(fst (size m2)) then error "O tamanho das matrizes não é compativel para multiplicar"
            else (CMatrix (fst (size m1)) (snd (size m2)) [  [dotProd (line m1 c1) (colum m2 c2) | c2<-[0..((snd (size m2))-1)] ] | c1<-[0..((fst (size m1))-1)]  ] )

  abs (CMatrix i j m1) = CMatrix i j [[ abs x | x<-(m1!!l) ] | l<-[0..(i-1)] ]

  signum (CMatrix i j m1) = CMatrix i j [[ signum x | x<-(m1!!l) ] | l<-[0..(i-1)] ]

  fromInteger i = (CMatrix 1 1 [[(fromInteger i)]])



buildMatrix :: Num t => t -> Int -> Int -> Matrix t
buildMatrix t i j =  if ((i==0) || (j==0)) then error "O número de linhas e colunas deve ser maior que zero"
                else let line = take j (cycle [t])
                in
                ( CMatrix i j (take i (cycle [line])) )


-- PRINTS \\\
printAllLines :: Show a => [[a]] -> String
printAllLines [] = "\n"
printAllLines (c:b) = startLine c ++ "\n" ++ printAllLines b

startLine :: Show t => [t] -> String
startLine [a] = " | " ++ show a ++ " | "
startLine (a:b) = " | " ++ show a ++ endLine b

endLine :: Show t => [t] -> String
endLine [a] = " " ++ show a ++ " | "
endLine (a:b) = " " ++ show a ++ endLine b
-- END PRINTS ///


-- SOMA \\\
sumLine :: Num t => [t] -> [t] -> [t]
sumLine l1 l2 = [(l1!!i)+(l2!!i)| i <- [0..((length l1)-1)] ]

sumMatrix :: Num t => [[t]] -> [[t]] -> [[t]]
sumMatrix m1 m2 = [sumLine (m1!!i) (m2!!i) | i <- [0..((length m1)-1)] ]
-- END SOMA ///


-- SUBTRAÇÃO \\\
subLine :: Num t => [t] -> [t] -> [t]
subLine l1 l2 = [(l1!!i)-(l2!!i)| i <- [0..((length l1)-1)] ]

subMatrix :: Num t => [[t]] -> [[t]] -> [[t]]
subMatrix m1 m2 = [subLine (m1!!i) (m2!!i) | i <- [0..((length m1)-1)] ]
-- END SUBTRAÇÃO ///

size :: Matrix t -> (Int, Int)
size (CMatrix i j m) = (i, j)

get :: Matrix t -> Int -> Int -> t
get (CMatrix k l m) i j = if (k<=i) || (l<=j) || (i<0) || (j<0) then error "Indice fora de alcance"
                          else ((m !! i) !! j)

set :: Matrix t -> Int -> Int -> t -> Matrix t
set (CMatrix i j m) linha coluna v = (CMatrix i j [ [if (l==linha) && (c==coluna) then v else ((m!!l)!!j) | c<-[0..(j-1)] ] | l<-[0..(i-1)] ])

line :: Num t => Matrix t -> Int -> [t]
line (CMatrix i j m) l = if l>=(length m) || l<0 then error "Esta linha não existe"
                          else (m !! l)

colum :: Num t => Matrix t -> Int -> [t]
colum (CMatrix i j m) c = if c>=(length (m!!0)) || c<0 then error "Esta linha não existe"
                          else [ (m!!x)!!c | x <- [0..((length m)-1)] ]

dotProd :: Num t => [t] -> [t] -> t
dotProd v1 v2 = if (length v1)/=(length v1) then error "Não é possivel multiplicar vetores de tamanhos diferentes"
                else sum [ (v1!!i)*(v2!!i) | i <- [0..((length v1)-1)]]

ident :: Num t => Int -> Matrix t
ident k = (CMatrix k k [[if i==d then 1 else 0 | i<-[0..k] ] | d<-[0..k] ] )

