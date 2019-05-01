module D where
{-

data langError = TypeError || ....

instance Error

eval :: VarMap -> Expr -> Either LangError Int 
eval mem exp@(Add _ _) = validate *> add exp

validate :: Expr -> Either LangError ()

-- Not part of the exercise
data Type
  = IntegerType
  | FunctionType Type
                 Type
  | TypeVariable String

typeOf :: Expr -> Maybe Type
typeOf (Add (Number 1) (Number 2)) -- Some IntegerType

typeOf (Add (Number 1) (Lambda "x" (Variable "x"))) -- None because type error

typeOf (Lambda "x" (Add (Integer 1) (Variable "x"))) -- FunctionType IntegerType IntegerType

typeOf (Lambda "x" (Variable "x")) -- FunctionType (TypeVariable "a") (TypeVariable "a")

-}
