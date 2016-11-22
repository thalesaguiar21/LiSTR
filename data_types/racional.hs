module Racional
    ( Racional (..)
    , min
    , equal
    , numerator
    , denominator
    , toFloat
    ) where

data Racional = PRacional Int Int

nu :: Racional -> Int
nu (PComplex r _) = r


de :: Racional -> Int
de (PComplex _ l) = l

instance Show Racional where
  show (PRacional r l) = if r /= 0 then show r ++ " / " ++ show l
                       else "0"

instance Eq Racional where
  r1 == r2 = (nu r1) == (nu r2) && (de r1) == (de r2)

instance Num Racional where
    r1 + r2 = let r = nu r1 + nu r2
                  l = de r1 + de r2
            in PRacional r l

    r1 - r2 = let r = nu r1 - nu r2
                  l = de r1 - de r2
            in PRacional r l

    r1 * r2 = let r = nu r1 * nu r2
                  l = de r1 * de r2
            in PRacional r l

    r1 / r2 = let r = nu r1 * de r2
                  l = de r1 * nu r2
              in PRacional r l

    signum r1 = let signum nu r1 * signum de r1
  			  in PRacional r l

    abs r1 = let nu r1 / de r1
  		     in PRacional r l

