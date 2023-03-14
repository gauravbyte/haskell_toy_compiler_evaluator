
module Typing where 
import Grammar



type TypeEnvironment = [ (String,VarType) ]

getBinding :: String -> TypeEnvironment -> VarType
getBinding x [] = error "Variable binding not found"
getBinding x ((y,t):tenv) | x == y  = t
                        | otherwise = getBinding x tenv

addBinding :: String -> VarType -> TypeEnvironment -> TypeEnvironment
addBinding x t tenv = (x,t):tenv

typeOf :: TypeEnvironment -> Expr -> VarType
typeOf tenv (Int _)  = TypeInt

typeOf tenv (VarTrue) = TypeBool

typeOf tenv (VarFalse) = TypeBool

typeOf tenv (LessThan e1 e2) = TypeBool
  where (TypeInt,TypeInt) = (typeOf tenv e1, typeOf tenv e2)

typeOf tenv (GreaterThan e1 e2) = TypeBool
  where (TypeInt,TypeInt) = (typeOf tenv e1, typeOf tenv e2)

typeOf tenv (IsEqual e1 e2) = TypeBool
  where (TypeInt,TypeInt) = (typeOf tenv e1, typeOf tenv e2)
  
typeOf tenv (Add e1 e2) = TypeInt 
  where (TypeInt,TypeInt) = (typeOf tenv e1, typeOf tenv e2)

typeOf tenv (Times e1 e2) = TypeInt 
  where (TypeInt,TypeInt) = (typeOf tenv e1, typeOf tenv e2)
  
typeOf tenv (Var x) = getBinding x tenv

typeOf tenv (If e1 e2 e3) | t2 == t3 = t2
  where (TypeBool,t2,t3) = (typeOf tenv e1, typeOf tenv e2, typeOf tenv e3)

typeOf tenv (Lambda x t e) = TypeFun t u 
  where u = typeOf (addBinding x t tenv) e

typeOf tenv (App e1 e2) | t1 == t3 = t2
  where ((TypeFun t1 t2),t3) = (typeOf tenv e1, typeOf tenv e2)

typeOf tenv (Let x t e1 e2)  = typeOf (addBinding x t tenv) e2
  where t1 = typeOf tenv e1

typeOf tenv _ = error "Type Error"

-- Function for printing the results of the TypeCheck
unparseType :: VarType -> String
unparseType TypeBool = "Bool"
unparseType TypeInt = "Int"
unparseType (TypeFun t1 t2) = (unparseType t1) ++ " -> " ++ (unparseType t2)
