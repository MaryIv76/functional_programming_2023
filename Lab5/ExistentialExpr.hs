data IntBinOp
  = Add
  | Sub
  | Mul
  | Div

data BoolBinOp
  = And
  | Or

data CompareBinOp
  = Eq
  | NotEq
  | Greater
  | Less


data IntExpr
  = IntConstNode Int
  | IntBinOpNode IntBinOp IntExpr IntExpr
  | IntTernNode BoolExpr IntExpr IntExpr

data BoolExpr
  = BoolConstNode Bool
  | BoolBinOpNode BoolBinOp BoolExpr BoolExpr
  | BoolTernNode BoolExpr BoolExpr BoolExpr
  | CompareBinOpNode CompareBinOp IntExpr IntExpr


getIntValue (IntConstNode x) = x
getBoolValue (BoolConstNode x) = x


class Expr a where
  eval :: a -> a
  toString :: a -> String

instance Expr IntExpr where
  eval (IntConstNode x) = IntConstNode x
  eval (IntBinOpNode op x y) = IntConstNode (decodeIntBinOp op (getIntValue $ eval x) (getIntValue $ eval y))
  eval (IntTernNode cond x y) = if (getBoolValue $ eval cond) then eval x else eval y
  toString (IntConstNode x) = show x
  toString (IntBinOpNode op x y) = "(" ++ (toString x) ++ (show op) ++ (toString y) ++ ")"
  toString (IntTernNode cond x y) = (toString cond) ++ " ? " ++ (toString x) ++ " : " ++ (toString y)

instance Expr BoolExpr where
  eval (BoolConstNode x) = BoolConstNode x
  eval (BoolBinOpNode op x y) = BoolConstNode (decodeBoolBinOp op (getBoolValue $ eval x) (getBoolValue $ eval y))
  eval (BoolTernNode cond x y) = if (getBoolValue $ eval cond) then eval x else eval y
  eval (CompareBinOpNode op x y) = BoolConstNode (decodeCompareBinOp op (getIntValue $ eval x) (getIntValue $ eval y))
  toString (BoolConstNode x) = show x
  toString (BoolBinOpNode op x y) = "(" ++ (toString x) ++ (show op) ++ (toString y) ++ ")"
  toString (BoolTernNode cond x y) = (toString cond) ++ " ? " ++ (toString x) ++ " : " ++ (toString y)
  toString (CompareBinOpNode op x y) = "(" ++ (toString x) ++ (show op) ++ (toString y) ++ ")"


decodeIntBinOp Add = (+)
decodeIntBinOp Sub = (-)
decodeIntBinOp Mul = (*)
decodeIntBinOp Div = div

decodeBoolBinOp And = (&&)
decodeBoolBinOp Or = (||)

decodeCompareBinOp Eq = (==)
decodeCompareBinOp NotEq = (/=)
decodeCompareBinOp Greater = (>)
decodeCompareBinOp Less = (<)


instance Show IntBinOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

instance Show BoolBinOp where
  show And = "&"
  show Or = "|"

instance Show CompareBinOp where
  show Eq = "="
  show NotEq = "#"
  show Greater = ">"
  show Less = "<"



data SomeExpr = forall a . Expr a => SomeExpr a

expr0 = IntConstNode 1
expr1 = IntBinOpNode Add (IntConstNode 1) (IntConstNode 2)
expr2 = IntBinOpNode Mul (IntConstNode 5) (IntConstNode 2)
expr3 = IntBinOpNode Sub (IntConstNode 5) (IntConstNode 7)
expr4 = IntBinOpNode Div (IntConstNode 5) (IntConstNode 5)
expr5 = BoolBinOpNode And (CompareBinOpNode Less expr1 expr2) (CompareBinOpNode NotEq expr3 expr4)
expr6 = BoolBinOpNode Or (BoolConstNode False) (BoolConstNode True)
expr7 = BoolTernNode (BoolConstNode True) expr5 expr6
expr8 = IntTernNode (BoolConstNode True) expr1 expr2

expressions = [SomeExpr expr0, SomeExpr expr1, SomeExpr expr2, SomeExpr expr3, SomeExpr expr4, SomeExpr expr5, SomeExpr expr6, SomeExpr expr7, SomeExpr expr8]

--  map (\(SomeExpr expr) -> toString expr) expressions
--  map (\(SomeExpr expr) -> toString (eval expr)) expressions

