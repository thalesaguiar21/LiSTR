data Vector = Vector (Float, Float, Float, Float) deriving (Eq, Show)

(|+|) :: Vector -> Vector -> Vector
Vector ( i, j, k, l ) |+| Vector ( m, n, o, p ) = Vector (i+m, j+n, k+o, l+p)

(|-|) :: Vector -> Vector -> Vector
Vector ( i, j, k, l ) |-| Vector ( m, n, o, p ) = Vector (i-m, j-n, k-o, l-p)

(|*|) :: Vector -> Vector -> Vector
Vector ( i, j, k, l ) |*| Vector ( m, n, o, p ) = Vector (i*m, j*n, k*o, l*p)

origin :: Vector -> (Float, Float)
origin (Vector (i, j, k, l)) = (k-i, l-j)

start :: Vector -> (Float, Float)
start (Vector (i, j, _, _)) = (i, j)

end :: Vector -> (Float, Float)
end (Vector (_, _, k, l)) = (k, l)

dotProd :: Vector -> Vector -> Float
v1 `dotProd` v2  = let (x1, y1) = origin v1
                       (x2, y2) = origin v2
                   in x1*x2 + y1*y2

mag :: Vector -> Float
mag v = let (x, y) = origin v
        in sqrt (x^2 + y^2)

angle :: Vector -> Vector -> Float
angle v1 v2 = let (x1, y1) = origin v1
                  (x2, y2) = origin v2
              in (atan2 y2 x2) - (atan2 y1 x1)