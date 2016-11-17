module Vector2
( Vector2 (..)
) where

import Point

data Vector2 a = Vector2 (Point2 (a, a)) (Point2 (a, a))