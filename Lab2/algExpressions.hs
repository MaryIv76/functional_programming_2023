-- Algebraic Expressions
import qualified Data.Map as Map

data Expr 
 = Const Double
 | Variable String 
 | Unary String Expr
 | Binary String Expr Expr


-- toString
toString (Const x) = show x
toString (Variable name) = name
toString (Unary f expr) = 
  case expr of 
    Binary _ expr1 expr2 -> f ++ toString expr
    _ -> f ++ "(" ++ toString expr ++ ")"
toString (Binary f expr1 expr2) = "(" ++ toString expr1 ++ " " ++ f ++ " " ++ toString expr2 ++ ")"


-- calculateExpr
calculateExpr (Const x) _ = x
calculateExpr (Variable name) vars = case Map.lookup name vars of
  Just value -> value
  Nothing -> error ("Variable " ++ name ++ " is undefined")
calculateExpr (Unary f expr) vars = applyUnary f (calculateExpr expr vars)
calculateExpr (Binary f expr1 expr2) vars = applyBinary f (calculateExpr expr1 vars) (calculateExpr expr2 vars)

applyUnary "sin" x = sin x
applyUnary "cos" x = cos x
applyUnary "sqrt" x = sqrt x
applyUnary _ x = error "Unary operation is not supported"

applyBinary "+" x y = x + y
applyBinary "-" x y = x - y
applyBinary "*" x y = x * y
applyBinary "/" x y = x / y
applyBinary _ x y = error "Binary operation is not supported"


-- simplifyExpr
simplifyExpr (Const x) = Const x
simplifyExpr (Variable name) = Variable name
simplifyExpr (Unary f expr) =
  let simplifiedExpr = simplifyExpr expr
  in case simplifiedExpr of
    Const x -> Const (applyUnary f x)
    _ -> Unary f simplifiedExpr
simplifyExpr (Binary f expr1 expr2) =
  let simplifiedExpr1 = simplifyExpr expr1
      simplifiedExpr2 = simplifyExpr expr2
  in case (simplifiedExpr1, simplifiedExpr2) of
    (Const x, Const y) -> Const (applyBinary f x y)
    (Const 0, _) -> case f of
      "+" -> simplifiedExpr2
      "-" -> Unary "-" simplifiedExpr2
      "*" -> Const 0
      "/" -> Const 0
      _ -> Binary f simplifiedExpr1 simplifiedExpr2
    (_, Const 0) -> case f of
      "+" -> simplifiedExpr1
      "-" -> simplifiedExpr1
      "*" -> Const 0
      "/" -> error "Error! Division by Zero"
      _ -> Binary f simplifiedExpr1 simplifiedExpr2
    (Const 1, _) -> case f of
      "*" -> simplifiedExpr2
      _ -> Binary f simplifiedExpr1 simplifiedExpr2
    (_, Const 1) -> case f of
      "*" -> simplifiedExpr1
      "/" -> simplifiedExpr1
      _ -> Binary f simplifiedExpr1 simplifiedExpr2
    (Variable name1, Variable name2) -> case f of
      "-" ->
        if name1 == name2
          then Const 0
          else Binary f simplifiedExpr1 simplifiedExpr2
      "/" ->
        if name1 == name2
          then Const 1
          else Binary f simplifiedExpr1 simplifiedExpr2
      _ -> Binary f simplifiedExpr1 simplifiedExpr2
    _ -> Binary f simplifiedExpr1 simplifiedExpr2


-- differentiateExpr
differentiateExpr (Const _) _ = Const 0
differentiateExpr (Variable name) varName =
  if name == varName
    then Const 1
    else Const 0
differentiateExpr (Unary f expr) varName = 
  case expr of
    Const x -> Const 0
    Variable name -> 
      if name == varName
        then differentiateUnary f (Variable name)
        else Const 0
    _ -> Binary "*" (differentiateUnary f expr) (differentiateExpr expr varName)
differentiateExpr (Binary f expr1 expr2) varName =
  let diffExpr1 = differentiateExpr expr1 varName
      diffExpr2 = differentiateExpr expr2 varName
  in case f of
    "+" -> Binary "+" diffExpr1 diffExpr2
    "-" -> Binary "-" diffExpr1 diffExpr2
    "*" -> Binary "+" (Binary "*" diffExpr1 expr2) (Binary "*" expr1 diffExpr2)
    "/" -> Binary "/" (Binary "-" (Binary "*" diffExpr1 expr2) (Binary "*" expr1 diffExpr2)) (Binary "*" expr2 expr2)
    _ -> error "Binary operation is not supported"


differentiateUnary "sin" x = Unary "cos" x
differentiateUnary "cos" x = Binary "*" (Const (-1.0)) (Unary "sin" x)
differentiateUnary _ x = error "Unary operation is not supported"


-- Examples

-- expr1 = 2 * (x + sqrt(9)) + y
expr1 = Binary "+" (Binary "*" (Const 2) (Binary "+" (Variable "x") (Unary "sqrt" (Const 9)))) (Variable "y")
variables = Map.fromList [("x", 5.0), ("y", 4.0)]
-- toString expr1
-- calculateExpr expr1 variables
-- Output: 20.0


-- expr2 = 4x + 2xsin(2x) + cos(y) - (sinx / 2x) + x*x + y*y
expr2 = Binary "+" (Binary "*" (Const 4) (Variable "x")) (Binary "+" (Binary "*" (Const 2) (Binary "*" (Variable "x") (Unary "sin" (Binary "*" (Const 2) (Variable "x"))))) (Binary "+" (Unary "cos" (Variable "y")) (Binary "+" (Binary "*" (Variable "x") (Variable "x")) (Binary "-" (Binary "*" (Variable "y") (Variable "y")) (Binary "/" (Unary "sin" (Variable "x")) (Binary "*" (Const 2) (Variable "x")))))))
-- toString expr2
-- toString $ differentiateExpr expr2 "x"
-- toString $ simplifyExpr $ differentiateExpr expr2 "x"
-- toString $ simplifyExpr $ differentiateExpr expr2 "y"
-- expr2' = 4 + 2 * (sin2x + 2xcos2x) + 2x - ((2xcosx - 2sinx) / (4*x*x)) по "x"
-- expr2' = 2y - sin(y) по "y"


-- expr3 = (t-t) + (t+t)*0 + 0 + 0*t + 5*1 + (6-6) + 0/7 + 5/1 + t/t 
expr3 = Binary "+" (Binary "-" (Variable "t") (Variable "t")) (Binary "+" (Binary "*" (Binary "+" (Variable "t") (Variable "t")) (Const 0)) (Binary "+" (Const 0) (Binary "+" (Binary "*" (Const 0) (Variable "t")) (Binary "+" (Binary "*" (Const 5) (Const 1)) (Binary "+" (Binary "-" (Const 6) (Const 6)) (Binary "+" (Binary "/" (Const 0) (Const 7)) (Binary "+" (Binary "/" (Const 5) (Const 1)) (Binary "/" (Variable "t") (Variable "t")))))))))
-- toString expr3
-- toString $ simplifyExpr expr3
-- Output: 11.0

