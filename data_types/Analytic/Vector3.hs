data Vector3 = Vector3 (Float, Float, Float, Float, Float, Float)
	deriving (Eq, Show)



(|+|) :: Vector3 -> Vector3 -> Vector3
Vector3 ( i, j, k, l, m, n ) |+| Vector3 ( o, p, q, r, s, t ) = Vector3 (i+o, j+p, k+q, l+r, m+s, n+t)

(|-|) :: Vector3 -> Vector3 -> Vector3
Vector3 ( i, j, k, l, m, n ) |-| Vector3 ( o, p, q, r, s, t ) = Vector3 (i-o, j-p, k-q, l-r, m-s, n-t)

(|*|) :: Vector3 -> Vector3 -> Vector3
Vector3 ( i, j, k, l, m, n ) |*| Vector3 ( o, p, q, r, s, t ) = Vector3 (i*o, j*p, k*q, l*r, m*s, n*t)

origin :: Vector3 -> (Float, Float, Float)
origin (Vector3 (i, j, k, l, m, n)) = (l-i, m-j, n-k)

start :: Vector3 -> (Float, Float, Float)
start (Vector3 (i, j, k, _, _, _)) = (i, j, k)

end :: Vector3 -> (Float, Float, Float)
end (Vector3 (_, _, _, l, m, n)) = (l, m, n)

norm :: Vector3 -> Float
norm v = let (x, y, z) = origin v
         in sqrt (x^2 + y^2 + z^2)

dotProd :: Vector3 -> Vector3 -> Float
v1 `dotProd` v2 = let (x1, y1, z1) = origin v1
                      (x2, y2, z2) = origin v2
                  in x1*x2 + y1*y2 + z1*z2

angle :: Vector3 -> Vector3 -> Float
angle v1 v2 = let dprod = v1 `dotProd` v2
              in acos dprod