import Data.List
import Data.Char

-- 1. 
data Frac = F Integer Integer
-- (a) 
mdc :: Integer -> Integer -> Integer
mdc x y | x == y = x
        | x > y = mdc (x-y) y
        | x < y = mdc x (y-x)

normaliza :: Frac -> Frac
normaliza (F 0 x) = F 0 1
normaliza (F x y) = let md = mdc (abs x) (abs y)
                        sinal = if x * y > 0 then 1 else (-1)
                    in  F (sinal * (div (abs x) md)) (div (abs y) md)

-- (b)
instance Eq Frac where
    (==) (F n1 n2) (F n3 n4) = n1*n2 == n3*n4 

-- (c) 
instance Ord Frac where 
    compare (F n1 n2) (F n3 n4) | n1*n2 == n3*n4 = EQ
                                | n1*n2 < n3*n4 = LT
                                | n1*n2 > n3*n4 = GT

-- (d) 
instance Show Frac where 
    show (F x 1) = show x
    show (F x y) = show x ++ "/" ++ show y

-- (e)
instance Num Frac where 
    (+) (F n1 n2) (F n3 n4) = normaliza (F ((n1*n4) + (n2*n3)) (n2*n4))
    (*) (F n1 n2) (F n3 n4) = normaliza (F (n1*n3) (n2*n4))
    (-) (F n1 n2) (F n3 n4) = normaliza (F ((n1*n4) - (n2*n3)) (n2*n4))
    negate (F n1 n2) = normaliza (F (-n1) n2)
    abs (F n1 n2) = F (abs n1) (abs n2)
    signum (F n1 n2) | n1 == 0 = F 0 1
                     | n1*n2 > 0 = F 1 1
                     | n1*n2 < 0 = F (-1) 1
    fromInteger n1 = F n1 1 

-- (f)
maisQdobros :: Frac -> [Frac] -> [Frac]
maisQdobros f l = filter (\j -> j > 2*f) l

-- 2.  
data Exp a = Const a
           | Simetrico (Exp a)
           | Mais (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)


calcula :: Num a => Exp a -> a
calcula (Const x) = x
calcula (Simetrico x) = - calcula x
calcula (Mais x y) = calcula x + calcula y
calcula (Menos x y) = calcula x - calcula y
calcula (Mult x y) = calcula x * calcula y

infixa :: Show a => Exp a -> String
infixa (Const x) = show x
infixa (Simetrico x) = '-' : '(' :  infixa x ++ ")"
infixa (Mais x y) = '(' : infixa x ++ " + " ++ infixa y ++ ")"
infixa (Menos x y) = '(' : infixa x ++ " - " ++ infixa y ++ ")"
infixa (Mult x y) = '(' : infixa x ++ " * " ++ infixa y ++ ")"           

-- (a) 
instance Show a => Show (Exp a) where 
        show e = infixa e 

-- (b)
instance (Eq a,Num a) => Eq (Exp a) where
    (==) e1 e2 = calcula e1 == calcula e2

-- (c)
instance Num a => Num (Exp a) where 
    (+) e1 e2 = Const (calcula e1 + calcula e2)
    (-) e1 e2 = Const (calcula e1 - calcula e2)
    (*) e1 e2 = Const (calcula e1 * calcula e2)
    abs e = Const (abs (calcula e))
    negate e = Const (negate (calcula e))
    signum e = Const (signum (calcula e))
    fromInteger n = Const (fromInteger n)

-- 3.
data Movimento = Credito Float | Debito Float
data Data = D Int Int Int
data Extracto = Ext Float [(Data, String, Movimento)]

-- (a) 
instance Eq Data where
    (==) e1 e2 = e1 == e2

instance Ord Data where
    compare (D d1 m1 a1) (D d2 m2 a2) | (a1,m1,d1) == (a2,m2,d2) = EQ
                                      | (a1,m1,d1) > (a2,m2,d2) = GT
                                      | (a1,m1,d1) < (a2,m2,d2) = LT

-- (b)
instance Show Data where 
    show (D d m a) = show a ++ "/" ++ show m ++ "/" ++ show d

-- (c)
ordena::Extracto->Extracto
ordena (Ext vi l) = let lf = sortOn (\(d,_,_)->d) l
                    in (Ext vi lf)

-- (d) 
saldo :: Extracto -> Float
saldo (Ext valor lm) = foldl(\ini (_,_,mov) -> case mov of Credito x -> x + ini
                                                           Debito x -> ini - x ) valor lm

instance Show Extracto where
    show (Ext n l) = "Saldo anterior: " ++ show n ++
                     "\n---------------------------------------" ++
                     "\nData       Descricao   Credito   Debito" ++
                     "\n---------------------------------------\n" ++ concatMap (\(dat,str,mov) -> show dat ++ replicate (11 - (length (show dat))) ' ' ++ map (toUpper) str ++ "    \n") l ++
                     "---------------------------------------" ++
                     "\nSaldo actual: " ++ show (saldo (Ext n l))