module Vector
    ( Vector (..)
    , (|+|)
    , (|-|)
    , (|*|)
    , origin
    , start
    , end
    , mag
    , angle
    , scal
    , (><)
    ) where


data Vector = Vector (Float, Float, Float, Float) deriving (Eq, Show)

toTuple :: Vector -> (Float, Float, Float, Float)
toTuple (Vector (i, j, k, l)) = (i, j, k, l)

-- Adição entre vetores no plano
(|+|) :: Vector -> Vector -> Vector
Vector ( i, j, k, l ) |+| Vector ( m, n, o, p ) = Vector (i+m, j+n, k+o, l+p)

-- Subtração entre vetores no plano
(|-|) :: Vector -> Vector -> Vector
Vector ( i, j, k, l ) |-| Vector ( m, n, o, p ) = Vector (i-m, j-n, k-o, l-p)

-- Produto entre vetores no plano
(|*|) :: Vector -> Vector -> Vector
Vector ( i, j, k, l ) |*| Vector ( m, n, o, p ) = Vector (i*m, j*n, k*o, l*p)

-- Vetor paralelo com início na origem
origin :: Vector -> Vector
origin (Vector (i, j, k, l)) = Vector (0, 0, k-i, l-j)

-- Ponto inicial do vetor no plano
start :: Vector -> (Float, Float)
start (Vector (i, j, _, _)) = (i, j)

-- Ponto final do vetor no plano
end :: Vector -> (Float, Float)
end (Vector (_, _, k, l)) = (k, l)

-- Multiplicação de escalares com vetores no plano
scal :: Float -> Vector -> Vector
scal n (Vector (i, j, k, l)) = Vector (n*i, n*j, n*k, n*l)

-- Produto interno/escalar de vetores no plano
(><) :: Vector -> Vector -> Float
v1 >< v2  = let Vector (_, _, x1, y1) = origin v1
                Vector (_, _, x2, y2) = origin v2
            in x1*x2 + y1*y2

-- Magnitude de um vetor no plano
mag :: Vector -> Float
mag v = let Vector (_, _, x, y) = origin v
        in sqrt (x^2 + y^2)

-- Ângulo entre vetores no plano
angle :: Vector -> Vector -> Float
angle v1 v2 = let Vector (_, _, x1, y1) = origin v1
                  Vector (_, _, x2, y2) = origin v2
              in (atan2 y2 x2) - (atan2 y1 x1)
