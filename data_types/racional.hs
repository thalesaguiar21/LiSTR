module Racional
    ( Racional (..)
    , simplify
    , numerator
    , denominator
    , toFloat
    ) where

data Racional = PRacional Int Int

nu :: Racional -> Int
nu (PRacional r _) = r

de :: Racional -> Int
de (PRacional _ l) = l

mdc :: Int -> Int -> Int
mdc a b = let g = if a>b then a else b
              l = if g==a then b else a
            in
          if (g`mod`l)==0 then l
          else mdc l (g`mod`l)

mmc :: Int -> Int -> Int
mmc a b = let n = a*b
              d = mdc a b
            in
          quot n d

instance Show Racional where
  show (PRacional r l) = if r /= 0 then show r ++ " / " ++ show l
                       else "0"

instance Eq Racional where
  r1 == r2 = (nu r1) == (nu r2) && (de r1) == (de r2)

instance Num Racional where
    r1 + r2 = PRacional ((nu r1 * de r2) + (nu r2 * de r1)) (mmc (de r1) (de r2))

    r1 - r2 = PRacional ((nu r1 * de r2) - (nu r2 * de r1)) (mmc (de r1) (de r2))

    r1 * r2 = let r = nu r1 * nu r2
                  l = de r1 * de r2
            in PRacional r l

{-    r1 / r2 = let r = nu r1 * de r2
                  l = de r1 * nu r2
              in PRacional r l
-}

    signum r1 = if de r1 == 0 then error "O denominador não pode ser zero."
                else PRacional (signum ((nu r1) * (de r1))) 1

    abs r1 = let  n = abs (nu r1)
                  d = abs (de r1)
                in PRacional n d

    fromInteger i = PRacional (fromInteger i) 1

numerator :: Racional -> Int
numerator r1 = nu r1

denominator :: Racional -> Int
denominator r1 = de r1

toFloat :: Racional -> Float
toFloat (PRacional n d) = (fromIntegral n) / (fromIntegral d)

simplify :: Racional -> Racional
simplify (PRacional n d) = PRacional (quot n (mdc n d)) (quot d (mdc n d))