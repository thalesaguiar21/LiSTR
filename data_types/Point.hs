module Analytics.Point
( Point (..)
, (|+|)
, (|-|)
, (|*|)
) where



data Point = Point ( Float, Float ) deriving ( Eq, Show )

(|+|):: Point -> Point -> Point
Point (a,b) |+| Point (c,d) = Point (a+c, b+d)

(|-|):: Point -> Point -> Point
Point (a,b) |-| Point (c,d) = Point (a-c, b-d)

(|*|):: Point -> Point -> Point
Point (a,b) |*| Point (c,d) = Point (a*c, b*d)

abc' :: Point -> Float
abc' Point (a,b) = a


