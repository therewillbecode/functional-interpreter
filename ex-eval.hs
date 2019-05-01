-- Step 1:

data Exp
    = Number Integer
    | Variable String
    | Add Exp Exp
    | Mul Exp Exp

eval :: Map String Integer -> Exp -> Integer


-- test.hs
-- import TomsCode
-- let res = eval (Map.ofList [("x", 42), ("y", 23)]) (Add (Variable "x") (Variable "y")) -- 65
-- assert_eq res 65

-- Step 2:

data Exp
    = Number Integer
    | Variable String
    | Add Exp Exp
    | Mul Exp Exp
    | Let String Exp Exp -- let name = exp in exp


eval {} (Let "x" 42 (Add (Variable x) (Number 23))) -- 65


-- Step 3:

data Exp
    = Number Integer
    | Variable String
    | Add Exp Exp
    | Mul Exp Exp
    | Let String Exp Exp -- let name = exp in exp
    | Funcall Exp [Exp]
    | Lambda String Exp

data Value = NumVal Integer | FunVal (String -> Value)

eval :: Map String Value -> Value

eval {} (Let "f" (Lambda "x" (Add (Variable x) (Variable x))) (FunCall (Variable "f") [Number 21])) -- 42

-- Not part of the exercise
data Type
    = IntegerType
    | FunctionType Type Type
    | TypeVariable String

typeOf :: Expr -> Maybe Type

typeOf (Add (Number 1) (Number 2)) -- Some IntegerType
typeOf (Add (Number 1) (Lambda "x" (Variable "x"))) -- None because type error
typeOf (Lambda "x" (Add (Integer 1) (Variable "x"))) -- FunctionType IntegerType IntegerType

typeOf (Lambda "x" (Variable "x")) -- FunctionType (TypeVariable "a") (TypeVariable "a")