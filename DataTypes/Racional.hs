module DataTypes.Racional
    ( Racional (..)
    , simplify
    , numerator
    , denominator
    , toFloat
    ) where

import Data.Char

data Racional = PRacional Int Int

nu :: Racional -> Int
nu (PRacional r _) = r

de :: Racional -> Int
de (PRacional _ l) = l

mdc :: Int -> Int -> Int
mdc a b = let g = if (abs a)>(abs b) then a else b
              l = if (abs g)==(abs a) then b else a
            in
          if (g`mod`l)==0 then l
          else mdc l (g`mod`l)

mmc :: Int -> Int -> Int
mmc a b = let n = a*b
              d = mdc a b
            in
          quot n d

whites :: String -> String
whites str = dropWhile isSpace str

nextNumber :: String -> (String, String)
nextNumber str = span isDigit (whites str)

instance Read Racional where
    readsPrec _ input = let (v1, r0) = nextNumber input
                            n = read v1 :: Int
                            (r1) = whites r0 in
                            if (null r1) then [(PRacional n 1, r1)]
                            else if (head r1 /= '/') then [(PRacional n 1, r1)]
                                 else let d:r2 = r1
                                          (v2, s) = nextNumber (r2)
                                          m = read v2 :: Int in
                                          if(d == '/') then [(PRacional n m, s)]
                                          else []

instance Show Racional where
  show (PRacional r l) = if r /= 0 then show r ++ " / " ++ show l
                       else "0"

instance Eq Racional where
  r1 == r2 = (nu r1) == (nu r2) && (de r1) == (de r2)

instance Ord Racional where
  r1 <= r2 = (signum (r1 - r2)) <= 0

instance Num Racional where
    r1 + r2 = PRacional ((nu r1 * de r2) + (nu r2 * de r1)) ((de r1)*(de r2))

    r1 - r2 = PRacional ((nu r1 * de r2) - (nu r2 * de r1)) ((de r1)*(de r2))

    r1 * r2 = let r = nu r1 * nu r2
                  l = de r1 * de r2
            in PRacional r l

    signum r1 = if de r1 == 0 then error "O denominador não pode ser zero."
                else PRacional (signum ((nu r1) * (de r1))) 1

    abs r1 = let  n = abs (nu r1)
                  d = abs (de r1)
                in PRacional n d

    fromInteger i = PRacional (fromInteger i) 1

instance Fractional Racional where
    r1 / r2 = let r = nu r1 * de r2
                  l = de r1 * nu r2
            in PRacional r l

    fromRational r = findEquivRational r 0 1

    recip (PRacional n d) = if n==0 then error "Não existe divisão por zero"
                            else (PRacional d n)

findEquivRational :: Rational -> Integer -> Integer -> Racional
findEquivRational f n d = if ((fromInteger n)/fromInteger(d)-f)==f then (PRacional (fromInteger n) (fromInteger d))
                          else if (fromInteger n)/fromInteger(d)>f then (findEquivRational f n (d+1))
                          else (findEquivRational f (n+1) d)

numerator :: Racional -> Int
numerator r1 = nu r1

denominator :: Racional -> Int
denominator r1 = de r1

toFloat :: Racional -> Float
toFloat (PRacional n d) = (fromIntegral n) / (fromIntegral d)

simplify :: Racional -> Racional
simplify (PRacional n d) = PRacional (quot n (mdc n d)) (quot d (mdc n d))

-- Coloca o racional no formato em que o sinal negativo está sempre no numerador
upSignal :: Racional -> Racional
upSignal (PRacional n d) =  if (n<0) == (d<0) then PRacional (abs n) (abs d)
                            else PRacional (-(abs n)) (abs d)