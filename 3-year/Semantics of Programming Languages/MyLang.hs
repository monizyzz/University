module MyLang where
import Language.Haskell.TH (Exp)

--------------------------------------------------
----- Part 1 -------------------------------------
--------------------------------------------------

-- Define a type for variables in our language.
data Vars = X1 | X2 deriving (Show, Eq)

-- A State is represented as a function mapping variables to integer values.
type State = Vars -> Int

-- Define the syntax for arithmetic expressions (AExp).
data AExp 
    = VAE Vars          -- Variable in an arithmetic expression
    | I Int             -- Integer constant (should be represented differently)
    | Sum AExp AExp     -- Addition of two arithmetic expressions
    | Mult AExp AExp    -- Multiplication of two arithmetic expressions
    deriving Show

-- Write down a few examples of simple arithmetic expressions
-- e.g. 
x1PlusOne = Sum (VAE X1) (I 1)
x2PlusTwo = Sum (VAE X2) (I 2)
more = Mult x1PlusOne x2PlusTwo


-- ....

tenPlus2 = Sum (I 10) (I 2)

x1PlusFive = Sum (VAE X1) (I 5)
sumx1Plus5Plus2 = Sum x1PlusFive (I 2)

{- 
    > s = \_ -> 0
-}


neg :: AExp -> AExp
neg e = Mult e (I (-1))

sub :: AExp -> AExp -> AExp
sub e1 e2 = Sum e1 (neg e2)

-- Implement the semantics of arithmetic expressions
-- you developed at home
-- Given an arithmetic expression and a state, it evaluates to an integer.
semAE :: AExp -> State -> Int
semAE (VAE x) s = s x 
semAE (I n) s = n
semAE (Sum e1 e2) s = semAE e1 s + semAE e2 s
semAE (Mult e1 e2) s = semAE e1 s * semAE e2 s

--------------------------------------------------
--------------------------------------------------
--------------------------------------------------

--------------------------------------------------
----- Part 2 -------------------------------------
--------------------------------------------------

-- Define the syntax for boolean expressions (BExp).
data BExp 
    = LE AExp AExp      -- Less than or equal comparison between two arithmetic expressions
    | Tt                -- Boolean constant: True
    | Ff                -- Boolean constant: False 
    | Neg BExp          -- Boolean negation
    | And BExp BExp     -- Boolean conjunction (AND operation)
    deriving Show

-- Define the syntax for programs (Prog).
data Prog 
    = Asg Vars AExp        -- Assignments
    | Seq Prog Prog        -- Sequential composition 
    | Cond BExp Prog Prog  -- Conditionals
    | While BExp Prog      -- While loops
    deriving Show

-- Write down a few examples of simple programs 
-- e.g. 
x1PlusOne_Seq_x2PlusTwo = Seq (Asg X1 x1PlusOne) (Asg X2 x2PlusTwo) -- x1 = x1 + 1; x2 = x2 + 2
div = While Tt (Asg X1 x1PlusOne) -- while true do x1 = x1 + 1

-- ....

mul = While Tt (Asg X2 (Mult (VAE X2) (I 2))) -- while true do x2 = x2 * 2


-- Define the semantics of boolean expressions.
-- Given a boolean expression and a state, it evaluates to a boolean value.
semBE :: BExp -> State -> Bool
semBE (LE e1 e2) s = semAE e1 s <= semAE e2 s
semBE Tt s = True
semBE Ff s = False
semBE (Neg b) s = not (semBE b s)
semBE (And b1 b2) s = semBE b1 s && semBE b2 s


chMen :: Vars -> Int -> State -> State
chMen x v s = \y -> if x == y then v else s y


-- Define the semantics of programs.
-- Given a program and an initial state, it produces a new state after execution.
semProg :: Prog -> State -> State
semProg (Asg v e) s = chMen v (semAE e s) s
semProg (Seq p1 p2) s = let s' = semProg p2 s in semProg p2 s'
semProg (Cond b p1 p2) s = if semBE b s then semProg p1 s else semProg p2 s
semProg (While b p) s = if semBE b s then semProg (While b p) (semProg p s) else s 

--------------------------------------------------
--------------------------------------------------
--------------------------------------------------

--------------------------------------------------
----- Part 3 -------------------------------------
--------------------------------------------------

-- Define the syntax for programs (Prog) that can produce errors.
data EProg 
    = EAsg Vars AExp        -- Assignments
    | ESeq EProg EProg        -- Sequential composition 
    | ECond BExp EProg EProg  -- Conditionals
    | EWhile BExp EProg      -- While loops
    | Err
    deriving Show

-- Define the semantics of programs.  Given a program and an initial state, it
-- **possibly** produces a new state after execution.
semEProg :: Prog -> State -> Maybe State
semEProg = undefined


