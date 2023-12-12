import Data.Char
import Data.Functor (void)
import Control.Applicative
import qualified Data.Map as Map

--------------------- Арифметические выражения -----------------------------------

newtype Parser a = Parser
  { runParser :: String -> [(a, String)] }

instance Functor Parser where
  fmap f q = Parser h where
    h s = [(f y, s') | (y, s') <- runParser q s]

instance Applicative Parser where
  pure x = Parser (\s -> [(x, s)])
  Parser p <*> Parser q = Parser h where
    h s = [(f y, s'') | (f, s') <- p s, (y, s'') <- q s']

instance Alternative Parser where
  empty = Parser (const [])
  Parser p <|> Parser q = Parser h where
    h s = p s ++ q s

acceptIf :: (Char -> Bool) -> Parser Char
acceptIf p = Parser f where
  f (c:cs) | p c = [(c, cs)]
  f _ = []

acceptChar c = acceptIf (==c)

letter :: Parser Char
letter = acceptIf isAlpha

digit :: Parser Char
digit = acceptIf isDigit

blank = acceptIf isSpace

ignoreIf :: (Char -> Bool) -> Parser ()
ignoreIf p = void (acceptIf p)

ignoreChar c = ignoreIf (==c)

times :: Int -> Parser a -> Parser [a]
times 0 _ = pure []
times n p = liftA2 (:) p (times (n-1) p)

upTo :: Int -> Parser a -> Parser [a]
upTo 0 _ = pure []
upTo n p = (liftA2 (:) p (upTo (n-1) p)) <|> upTo 0 p

fromTo :: Int -> Int -> Parser a -> Parser [a]
fromTo m n p = liftA2 (++) (times m p) (upTo (n-m) p)

-- <Expr>       ::= <Sum>
-- <Sum>        ::= <Prod> { <AddOp> <Prod> }
-- <Prod>       ::= <Term> { <MulOp> <Term> }
-- <Term>       ::= <Numeral> | '(' <Sum> ')'
-- <AddOp>      ::= '+' | '-'
-- <MulOp>      ::= '*' | '/'
-- <Numeral>    ::= <Digit> { <Digit> }

data AddOp = Add | Sub deriving (Show)
data MulOp = Mul | Div deriving (Show)
data Sum = Sum Prod [(AddOp, Prod)] deriving (Show)
data Prod = Prod Term [(MulOp, Term)] deriving (Show)
data Term = Numeral Int | Subexpr Sum | Variable String deriving (Show)

addOpE :: Parser AddOp
addOpE = (const Add <$> acceptChar '+') <|> (const Sub <$> acceptChar '-')

mulOpE :: Parser MulOp
mulOpE = (const Mul <$> acceptChar '*') <|> (const Div <$> acceptChar '/')

sumE :: Parser Sum
sumE = liftA2 Sum prodE (many (liftA2 (,) addOpE prodE) )

prodE :: Parser Prod
prodE = liftA2 Prod termE (many (liftA2 (,) mulOpE termE))

termE :: Parser Term
termE = (Numeral . read <$> some digit) <|> (liftA3 (\_ x _ -> Subexpr x) (ignoreChar '(') sumE (ignoreChar ')')) <|> (Variable <$> some letter)

parseSum = requireComplete . head . runParser sumE where
    requireComplete (x, "") = x

evalSum :: Sum -> Float
evalSum (Sum a bs) = (evalProd a) + (sum $ map f bs) where
    f (op, b) = g op $ evalProd b
    g Add = id
    g Sub = negate

evalProd :: Prod -> Float
evalProd (Prod a bs) = evalTerm a * (product $ map f bs) where
    f (op, b) = g op $ evalTerm b
    g Mul = id
    g Div = recip

evalTerm :: Term -> Float
evalTerm (Numeral x) = fromIntegral x
evalTerm (Subexpr x) = evalSum x



--------------------- Стековая машина -----------------------------------

data StackOp a
  = UnOp (a -> a)
  | BinOp (a -> a -> a)
  | SpecOp Int ([a] -> [a])

applyStackOp :: StackOp a -> [a] -> [a]
applyStackOp (UnOp f) (x:xs) = (f x) : xs
applyStackOp (BinOp f) (x1:(x2:xs)) = (f x1 x2) : xs
applyStackOp (SpecOp n f) xs = (f $ take n xs) ++ (drop n xs)
applyStackOp _ stack = stack


data Command a 
  = DataCommand (StackOp a)
  | ControlCommand (a -> Int -> Int)

type State a = ([a], Int)


performCommand :: Command a -> State a -> State a
performCommand (DataCommand c) (stack, index) = (applyStackOp c stack, index + 1)
performCommand (ControlCommand c) (top:stack, index) = (stack, c top index)


run :: [StackOp a] -> [a] -> [a]
run program stack = foldl (flip applyStackOp) stack program

stAdd = BinOp (+)
stSub = BinOp (-)
stMul = BinOp (*)
stDiv = BinOp div
stMod = BinOp mod

stNeg = UnOp negate

stDpl = SpecOp 1 (\[x] -> [x, x])
stIgn = SpecOp 1 (\[_] -> [])
stPush x = SpecOp 0 (\[] -> [x])
stPushVar name = SpecOp 0 (\[] -> [getVariableValue variableMap name])
stRol n = SpecOp n (\(t:ts) -> ts ++ [t])

variableMap = Map.fromList [("x", 10), ("y", 20), ("z", 30)]

getVariableValue varMap name = case Map.lookup name varMap of
  Just value -> value
  Nothing -> error ("Variable " ++ name ++ " is undefined")



----------------------- Объединение -----------------------------------

compileSum :: Sum -> [StackOp Int]
compileSum (Sum prod ops) = compileProd prod ++ concatMap compileOp ops
  where
    compileOp (addOp, prod') = compileProd prod' ++ [getOp addOp]
    getOp Add = stAdd
    getOp Sub = stSub

compileProd :: Prod -> [StackOp Int]
compileProd (Prod term ops) = compileTerm term ++ concatMap compileOp ops
  where
    compileOp (mulOp, term') = compileTerm term' ++ [getOp mulOp]
    getOp Mul = stMul
    getOp Div = stDiv

compileTerm :: Term -> [StackOp Int]
compileTerm (Numeral x) = [stPush x]
compileTerm (Subexpr sumExpr) = compileSum sumExpr
compileTerm (Variable name) = [stPushVar name]

compileExpression :: String -> [StackOp Int]
compileExpression input = compileSum (parseSum input) 

executeExpression :: String -> [Int] -> [Int]
executeExpression input stack = run (compileExpression input) stack


-- executeExpression "2*(3+4)" []
-- executeExpression "2*5+3*4" []
-- executeExpression "2*(3+x)" []


