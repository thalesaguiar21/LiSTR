module DataTypes.Analytic.Complex
    ( Complex (..)
    , re
    , im
    , divi
    , conj
    , arg
    , toPolar
    ) where

data Complex = PComplex Float Float | RComplex Float

-- Parte real de um número complexo
re :: Complex -> Float
re (PComplex r _) = r
re (RComplex r) = r

-- Parte imaginária de um número complexo
im :: Complex -> Float
im (PComplex _ i) = i
im (RComplex r) = 0.0

instance Show Complex where
  show (PComplex r i) = if i /= 0 then show r ++ " + " ++ show i ++ "i"
                       else show r
  show (RComplex r)  = show r

instance Eq Complex where
  c1 == c2 = (re c1) == (re c2) && (im c1) == (im c2)

instance Num Complex where
  -- Adição para numeros complexos
  c1 + c2 = let r = re c1 + re c2
                i = im c1 + im c2
            in PComplex r i
  -- Subtração para números complexos
  c1 - c2 = let r = re c1 - re c2
                i = im c1 - im c2
            in PComplex r i
  -- Multiplicação para números complexos
  c1 * c2 = let r = re c1 * re c2 - im c1 * im c2
                i = im c1 * re c2 - re c1 * im c2
            in PComplex r i
  -- Valor absoluto para um número complexo
  abs c1 = let r = sqrt ((re c1)^2 + (im c1)^2)
           in RComplex r
  -- Signum para complexos
  signum c1 = RComplex 1.0
  -- Conversão para inteiro
  fromInteger number = RComplex (fromInteger number)

-- Divisão entre complexos
divi :: Complex -> Complex -> Complex
c1 `divi` c2 = let nume = c1 * (conj c2)
                   deno = c2 * (conj c2)
                   r = re nume / re deno
                   i = im nume / im deno
               in PComplex r i

-- Conjugado de um número complexo
conj :: Complex -> Complex
conj c = let r = re c
             i = im c
         in if i == 0.0 then RComplex r
            else PComplex r (-i)

-- Calcula o argumento 'phi' para conversão de um número
-- complexo em sua forma polar
arg :: Complex -> Float
arg c = atan2 (im c) (re c)

-- Converte um número complexo para sua forma polar
toPolar :: Complex -> Complex
toPolar c = PComplex (re r * cos phi) (re r * sin phi)
  where r = abs c
        phi = arg c
