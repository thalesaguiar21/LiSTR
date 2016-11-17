module Analytics.Point
( Point (..)
, (|+|)
, (|-|)
, (|*|)
, scal
, dist
, med
) where



data Point = Point (Float, Float) 
	| Point3 (Float, Float, Float) deriving ( Eq, Show )

(|+|):: Point -> Point -> Point
Point (a,b)    |+| Point (c,d)    = Point (a+c, b+d)
Point3 (a,b,c) |+| Point3 (d,e,f) = Point3 (a+d, b+e, c+f) 

(|-|):: Point -> Point -> Point
Point (a,b)    |-| Point (c,d)    = Point (a-c, b-d)
Point3 (a,b,c) |-| Point3 (d,e,f) = Point3 (a-d, b-e, c-f)

(|*|):: Point -> Point -> Point
Point (a,b)    |*| Point (c,d)    = Point (a*c, b*d)
Point3 (a,b,c) |*| Point3 (d,e,f) = Point3 (a*d, b*e, c*f)

scal :: Float -> Point -> Point
scal k (Point (a,b))    = Point (k*a, k*b)
scal k (Point3 (a,b,c)) = Point3 (k*a, k*b, k*c)

dist :: Point -> Point -> Float
dist (Point (a,b)) (Point (c,d))       = sqrt ((a-c)^2 + (b-d)^2)
dist (Point3 (a,b,c)) (Point3 (d,e,f)) = sqrt ((a-d)^2 + (b-e)^2 + (c-f)^2)

med :: Point -> Point -> Point
med (Point (a,b)) (Point (c,d))       = Point ((a+c)/2, (b+d)/2)
med (Point3 (a,b,c)) (Point3 (d,e,f)) = Point3 ((a+d)/2, (b+e)/2, (c+f)/2)