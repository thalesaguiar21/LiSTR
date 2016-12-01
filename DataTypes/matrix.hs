module DataTypes.Matrix
    (Matrix (..)
    , get
    , set
    , line
    , colum
    , ident
    , det
    , transpose
    --, inverse
    ) where


import DataTypes.Racional
import DataTypes.Analytic.Complex

--data Matrix a = CMatrix Int Int [[a]] --(Eq, Num)

data Matrix = IMatrix Int Int [[Int]]
              | FMatrix Int Int [[Float]]
              | RMatrix Int Int [[Racional]]
              | CMatrix Int Int [[Complex]]
              | IIMatrix Int Int [[Integer]]

instance Show Matrix where
  show (IMatrix i j b) = printAllLines b
  show (FMatrix i j b) = printAllLines b
  show (RMatrix i j b) = printAllLines b
  show (CMatrix i j b) = printAllLines b
  show (IIMatrix i j b) = printAllLines b

instance Eq Matrix where
  (IMatrix i j b) == (IMatrix k l c) = if (i/=k) || (j/=l) then False
                                        else b==c
  (FMatrix i j b) == (FMatrix k l c) = if (i/=k) || (j/=l) then False
                                        else b==c
  (RMatrix i j b) == (RMatrix k l c) = if (i/=k) || (j/=l) then False
                                        else b==c
  (CMatrix i j b) == (CMatrix k l c) = if (i/=k) || (j/=l) then False
                                        else b==c
  (IIMatrix i j b) == (IIMatrix k l c) = if (i/=k) || (j/=l) then False
                                        else b==c

instance Num Matrix where
  (IMatrix i j m1) + (IMatrix k l m2) = if (i/=k) || (j/=l) then error "|+| >> As matrizes devem possuir o mesmo tamanho"
                                        else (IMatrix i j (sumMatrix m1 m2))
  (FMatrix i j m1) + (FMatrix k l m2) = if (i/=k) || (j/=l) then error "|+| >> As matrizes devem possuir o mesmo tamanho"
                                        else (FMatrix i j (sumMatrix m1 m2))
  (RMatrix i j m1) + (RMatrix k l m2) = if (i/=k) || (j/=l) then error "|+| >> As matrizes devem possuir o mesmo tamanho"
                                        else (RMatrix i j (sumMatrix m1 m2))
  (CMatrix i j m1) + (CMatrix k l m2) = if (i/=k) || (j/=l) then error "|+| >> As matrizes devem possuir o mesmo tamanho"
                                        else (CMatrix i j (sumMatrix m1 m2))
  (IIMatrix i j m1) + (IIMatrix k l m2) = if (i/=k) || (j/=l) then error "|+| >> As matrizes devem possuir o mesmo tamanho"
                                        else (IIMatrix i j (sumMatrix m1 m2))

  (CMatrix i j m1) - (CMatrix k l m2) = if (i/=k) || (j/=l) then error "|-| >> As matrizes devem possuir o mesmo tamanho"
                                        else (CMatrix i j (subMatrix m1 m2))

  m1 * m2 = if (snd (size m1))/=(fst (size m2)) then error "O tamanho das matrizes não é compativel para multiplicar"
            else (CMatrix (fst (size m1)) (snd (size m2)) [  [dotProd (line m1 c1) (colum m2 c2) | c2<-[0..((snd (size m2))-1)] ] | c1<-[0..((fst (size m1))-1)]  ] )

  abs (CMatrix i j m1) = CMatrix i j [[ abs x | x<-(m1!!l) ] | l<-[0..(i-1)] ]

  signum (CMatrix i j m1) = CMatrix i j [[ signum x | x<-(m1!!l) ] | l<-[0..(i-1)] ]

  fromInteger i = (CMatrix 1 1 [[(fromInteger i)]])



buildMatrix :: Num t => t -> Int -> Int -> Matrix t
buildMatrix t i j =  if ((i<=0) || (j<=0)) then error "O número de linhas e colunas deve ser maior que zero"
                else let line = take j (cycle [t])
                in
                ( CMatrix i j (take i (cycle [line])) )

readMatrix :: Num t => Int -> Int -> [[t]] -> Matrix t
readMatrix i j m = if (length m)/=i then error "A matriz inserida não possui o numero de linhas correspondente"
                   else let l = [length (m!!p) | p<-[0..(length m)-1] ]
                            columSize = [x | x<-l, x/=j ]
                   in
                   if (length columSize)/=0 then error "A matriz inserida não possui o numero de colunas correspondente em todas as linhas"
                   else (CMatrix i j m)

-- PRINTS \\\
printAllLines :: Show a => [[a]] -> String
printAllLines [] = "\n"
printAllLines (c:b) = startLine c ++ "\n" ++ printAllLines b

startLine :: Show t => [t] -> String
startLine [] = " |   | "
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
set (CMatrix i j m) linha coluna v = (CMatrix i j [ [if (l==linha) && (c==coluna) then v else ((m!!l)!!c) | c<-[0..(j-1)] ] | l<-[0..(i-1)] ])

line :: Num t => Matrix t -> Int -> [t]
line (CMatrix i j m) l = if l>=(length m) || l<0 then error "Esta linha não existe"
                          else (m !! l)

colum :: Num t => Matrix t -> Int -> [t]
colum (CMatrix i j m) c = if c>=(length (m!!0)) || c<0 then error "Esta linha não existe"
                          else [ (m!!x)!!c | x <- [0..((length m)-1)] ]

dotProd :: Num t => [t] -> [t] -> t
dotProd v1 v2 = if (length v1)/=(length v1) then error "Não é possivel multiplicar vetores de tamanhos diferentes"
                else sum [ (v1!!i)*(v2!!i) | i <- [0..((length v1)-1)]]

ident :: Num t => Int -> t -> t -> Matrix t
ident k zero um= (CMatrix k k [[if i==d then um else zero | i<-[0..k-1] ] | d<-[0..k-1] ] )


-- DETERMINANTE \\\
det :: Num t => Matrix t -> t
det (CMatrix i j m) = if i/=j then error "Apenas matrizes quadradas possuem determinante"
                      else calcDet m

calcDet :: Num t => [[t]] -> t
calcDet [[a]] = a
calcDet m = let l1 = (m!!0) 
                ls = length l1
            in
              sum [ ((-1)^j)*(l1!!j)*(calcDet (peaceOfMatrix m 0 j)) | j<-[0..ls-1] ]

peaceOfMatrix :: Num t => [[t]] -> Int -> Int -> [[t]]
peaceOfMatrix m linha coluna = let  nL = length m
                                    nC = length (m!!0)
                                in
                                [[ ((m!!l)!!c) | c<-([0..coluna-1]++[coluna+1..nC-1]) ] | l<-([0..linha-1]++[linha+1..nL-1]) ]
-- END DETERMINANTE ///

transpose :: Num t => Matrix t -> Matrix t
transpose (CMatrix i j m) = (CMatrix j i [ [(m!!c)!!l | c<-[0..i-1] ] | l<-[0..j-1] ])


-- MATRIZ INVERSA
inverse :: (Num t, Fractional t) => Matrix -> Matrix
inverse (CMatrix i j m) = snd (gausJordan ((CMatrix i j m), (ident i ((m!!0!!0)-(m!!0!!0)) (fromInteger 1)) ) 0)

gausJordan :: (Num t, Fractional t) => (Matrix, Matrix) -> Int -> (Matrix t, Matrix t)
gausJordan ((CMatrix i j mt1), (CMatrix a b mt2)) k = if k>=j then ((CMatrix i j mt1), (CMatrix a b mt2))
                                                      else let  (m1, m2) = makeDiagUni ((CMatrix i j mt1), (CMatrix a b mt2)) k
                                                                m = [[if l/=k then (get m1 l c)-((get m1 l k)*(get m1 k c)) else get m1 l c | c<-[0..j-1]] | l<-[0..i-1]]
                                                                id = [[if l/=k then (get m2 l c)-((get m1 l k)*(get m2 k c)) else get m2 l c | c<-[0..j-1]] | l<-[0..i-1]]
                                                      in
                                                      gausJordan ( (CMatrix i j m),(CMatrix a b id) ) (k+1)

makeDiagUni :: (Num t, Fractional t) => (Matrix t, Matrix t) -> Int -> (Matrix t, Matrix t)
makeDiagUni ((CMatrix i j m1), (CMatrix a b m2)) k = let id = (CMatrix i j [[ if l==k then (m2!!l!!c)/(m1!!l!!l) else (m2!!l!!c) | c<-[0..j-1] ]| l<-[0..i-1]])
                                                         m = (CMatrix i j [[ if l==k then (m1!!l!!c)/(m1!!l!!l) else (m1!!l!!c) | c<-[0..j-1] ]| l<-[0..i-1]])
                                                    in (m, id)
-- END MATRIZ INVERSA


-- Retorna um vetor com as matrizes da fatoração LU: [Gn, Gn-1, ..., G1]
gs :: (Num t, Fractional t,Eq t) => Matrix t -> Int -> [Matrix t]
gs (CMatrix i j m) k =  if k==i-1 then []
                        else
                        if (length [ m!!l!!k | l<-[k..i-1], m!!l!!k/=0 ])==0 then (gs (CMatrix i j m) (k+1))
                        else let zero = (m!!0!!0) - (m!!0!!0)
                                 m1 = [[if l>k && c==k then ((-1)*(m!!l!!c)/(m!!k!!k)) else (get (ident i zero (fromInteger 1)) l c) | c<- [0..j-1] ]| l<-[0..i-1] ]
                                 g = (CMatrix i j m1)
                        in
                        [g] ++ (gs (g*(CMatrix i j m)) (k+1))
