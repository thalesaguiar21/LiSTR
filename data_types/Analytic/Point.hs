module Point
( Point2 (..)
, Point3 (..)
) where


data Point2 a  = Point2 (a, a) deriving (Eq, Show)

instance (Num x) => Num (Point2 x) where
	Point2 (x, y) + Point2 (w, z) = Point2 (x+w, y+z)
	Point2 (x, y) - Point2 (w, z) = Point2 (x-w, y-z)
	Point2 (x, y) * Point2 (w, z) = Point2 (x*w, y*z)
	abs    (Point2 (x,y)) = Point2 (abs x,    abs y) 
	signum (Point2 (x,y)) = Point2 (signum x, signum y)
	fromInteger i = Point2 (fromInteger i, fromInteger i)

data Point3 a = Point3 (a, a, a) deriving (Eq, Show)

instance (Num x) => Num (Point3 x) where
	Point3 (x, y, z) + Point3 (i, j, k) = Point3 (x+i, y+j, z+k)
	Point3 (x, y, z) - Point3 (i, j, k) = Point3 (x-i, y-j, z-k)
	Point3 (x, y, z) * Point3 (i, j, k) = Point3 (x*i, y*i, z*k)
	abs    (Point3 (x, y, z)) = Point3 (abs x, abs y, abs z) 
	signum (Point3 (x, y, z)) = Point3 (signum x, signum y, signum z) 
	fromInteger i = Point3 (fromInteger i, fromInteger i, fromInteger i)