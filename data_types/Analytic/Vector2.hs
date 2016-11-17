data Vector = Vector (Float, Float, Float, Float) deriving (Eq, Show)



(|+|) :: Vector -> Vector -> Vector
Vector ( i, j, k, l ) |+| Vector ( m, n, o, p ) = Vector (i+m, j+n, k+o, l+p)

(|-|) :: Vector -> Vector -> Vector
Vector ( i, j, k, l ) |-| Vector ( m, n, o, p ) = Vector (i-m, j-n, k-o, l-p)

(|*|) :: Vector -> Vector -> Vector
Vector ( i, j, k, l ) |*| Vector ( m, n, o, p ) = Vector (i*m, j*n, k*o, l*p)

dotProd :: Vector -> Vector -> Float
Vector ( i, j, k, l ) `dotProd` Vector ( m, n, o, p )  = i*m+j*n+k*o+l*p

start :: Vector -> (Float, Float)
start (Vector (i, j, _, _)) = (i, j)

end :: Vector -> (Float, Float)
end (Vector (_, _, k, l)) = (k, l)







--innerProd :: Vector -> Vector -> Vector
--Vector ( i, j, k, l ) `innerProd` Vector ( m, n, o, p ) = 

--let a = Vector (1, 2, 3, 4)