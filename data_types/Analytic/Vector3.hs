module Vector3
    ( Vector3 (..)
    , (|+|)
    , (|-|)
    , (|*|)
    , origin
    , start
    , end
    , norm
    , angle
    , scal
    , (><)
    , (<>)
    ) where

data Vector3 = Vector3 (Float, Float, Float, Float, Float, Float)
    deriving (Eq, Show)


-- Adição entre os vetores no espaço
(|+|) :: Vector3 -> Vector3 -> Vector3
Vector3 ( i, j, k, l, m, n ) |+| Vector3 ( o, p, q, r, s, t ) = Vector3 (i+o, j+p, k+q, l+r, m+s, n+t)

-- Subtração entre vetores no espaço
(|-|) :: Vector3 -> Vector3 -> Vector3
Vector3 ( i, j, k, l, m, n ) |-| Vector3 ( o, p, q, r, s, t ) = Vector3 (i-o, j-p, k-q, l-r, m-s, n-t)

-- Produto entre os vetores no espaço
(|*|) :: Vector3 -> Vector3 -> Vector3
Vector3 ( i, j, k, l, m, n ) |*| Vector3 ( o, p, q, r, s, t ) = Vector3 (i*o, j*p, k*q, l*r, m*s, n*t)

-- Vetor paraleo com início na origem
origin :: Vector3 -> (Float, Float, Float)
origin (Vector3 (i, j, k, l, m, n)) = (l-i, m-j, n-k)

-- Ponto inicial do vetor no espaço
start :: Vector3 -> (Float, Float, Float)
start (Vector3 (i, j, k, _, _, _)) = (i, j, k)

-- Ponto final do vetor no espaço
end :: Vector3 -> (Float, Float, Float)
end (Vector3 (_, _, _, l, m, n)) = (l, m, n)

-- Norma de um vetor no espaço
norm :: Vector3 -> Float
norm v = let (x, y, z) = origin v
         in sqrt (x^2 + y^2 + z^2)

-- Ângulo entre dois vetores no espaço
angle :: Vector3 -> Vector3 -> Float
angle v1 v2 = let dprod = v1 >< v2
              in acos dprod

-- Multiplicação por escalar
scal :: Float -> Vector3 -> Vector3
scal u (Vector3 (i, j, k, l, m, n)) = Vector3 (u*i, u*j, u*k, u*l, u*m, u*n)

-- Produto interno/escalar
(><) :: Vector3 -> Vector3 -> Float
v1 >< v2 = let (x1, y1, z1) = origin v1
               (x2, y2, z2) = origin v2
           in x1*x2 + y1*y2 + z1*z2

-- Produto vetorial
(<>) :: Vector3 -> Vector3 -> Vector3
v1 <> v2 = let (x1, y1, z1) = origin v1
               (x2, y2, z2) = origin v2
               i = y1*z2 - z1*y2
               j = x2*z1 - x1*z2
               k = x1*y2 - y1*x2
           in Vector3 (0, 0, 0, i, j, k)
