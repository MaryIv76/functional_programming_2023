import qualified Data.Map as Map
import Data.List (foldl)
import Data.Text (pack, unpack)
import Data.Text (splitOn)
import Text.Read (readMaybe)
import Expr

-- Stack Machine

data Command a
  = Push a
  | PushVar String
  | Pop
  | Add
  | Subtract
  | Multiply
  | Divide
  | Negate
  | Sinus
  | Cosinus
  | If a Int
  | Jump Int
  deriving (Show)


data MachineState a = MachineState [a]
  deriving (Show)

getStack (MachineState stack) = stack


type DoubleCommand = Command Double
type DoubleMachineState = MachineState Double


-- runProgram
runProgram commands inputStack vars decodeFunc = case finalState of (state, _, _) -> state
  where
    finalState = foldl executeCmd (initialState, vars, decodeFunc) commands
    initialState = MachineState inputStack

runProgramDouble commands inputStack vars = runProgram commands inputStack vars decodeOp


-- executeCmd
executeCmd (machineState, vars, decodeFunc) op = (decodeFunc machineState op vars, vars, decodeFunc)


-- decodeOp
decodeOp machineState op vars = case op of
  (Push x) -> MachineState (x:(getStack machineState))
  (PushVar name) -> MachineState ((vars Map.! name):(getStack machineState))
  Pop -> if (getStack machineState /= []) then MachineState (tail(getStack machineState)) else MachineState []
  Add -> decodeOpHelper machineState (+)
  Subtract -> decodeOpHelper machineState (-)
  Multiply -> decodeOpHelper machineState (*)
  Divide -> if ((head $ getStack machineState) == 0) then error "Zero division!" else decodeOpHelper machineState (/)
  Negate -> MachineState ((negate $ head $ getStack machineState):(tail $ getStack machineState))
  Sinus -> MachineState ((sin $ head $ getStack machineState):(tail $ getStack machineState))
  Cosinus -> MachineState ((cos $ head $ getStack machineState):(tail $ getStack machineState))
  (If x num) -> if head (getStack machineState) /= x then MachineState (drop num (getStack machineState)) else machineState
  (Jump num) -> MachineState (drop num (getStack machineState))

decodeOpHelper machineState op =
  let stackValues = getStack machineState
    in case stackValues of
      a : b : rest -> MachineState (op b a : rest)
      _            -> machineState


-- parseCmds
parseCmdsHelper [] cmds = cmds
parseCmdsHelper (input:inputs) cmds = parseCmdsHelper inputs (cmds ++ [parseCmd input])
  where
    parseCmd str = case words str of
      ["Push", val] -> Push $ read val
      ["PushVar", name] -> PushVar name
      ["Pop"]          -> Pop
      ["Add"]          -> Add
      ["Subtract"]     -> Subtract
      ["Multiply"]     -> Multiply
      ["Divide"]       -> Divide
      _                -> error "Operation is not supported"

parseCmds input = parseCmdsHelper (map unpack (splitOn (pack ", ") (pack input))) []


-- fromExpr
fromExprHelper (BinOpNode op expr1 expr2) res = (fromExprHelper expr2 (fromExprHelper expr1 res)) ++ [fromExprBinOp op]
fromExprHelper (UnOpNode op expr) res = (fromExprHelper expr res) ++ [fromExprUnOp op]
fromExprHelper (Const x) res = res ++ [Push x]
fromExprHelper (Var name) res = res ++ [PushVar name]

fromExpr expr = fromExprHelper expr []


fromExprBinOp exprOp = case exprOp of
  AddOp -> Add
  Sub -> Subtract
  Mul -> Multiply
  Div -> Divide

fromExprUnOp exprOp = case exprOp of
  Neg -> Negate
  Sin -> Sinus
  Cos -> Cosinus


-- examples
cmds = [Push 2, PushVar "y", Add, Push 4, Multiply, PushVar "x", Subtract, Add, Push 1, Pop, Push 2, Push 4, If 100 2, Push 225, Jump 1, PushVar "y", Divide]
inputStack = []
variables = Map.fromList [("x", 5.0), ("y", 3.0)]
result = runProgramDouble cmds inputStack variables


cmdsString = "Push 2, PushVar y, Add, Push 4, Multiply, PushVar x, Subtract, Add, Push 1, Pop, PushVar y, Divide"
cmds2 = parseCmds cmdsString
result2 = runProgramDouble cmds2 inputStack variables


expr = BinOpNode AddOp (BinOpNode Mul (Const 2) (BinOpNode AddOp (Var "x") (UnOpNode Neg (Const 9)))) (Var "y")
variables3 = Map.fromList [("x", 5.0), ("y", 6.0)]
cmds3 = fromExpr expr
result3 = runProgramDouble cmds3 inputStack variables3
