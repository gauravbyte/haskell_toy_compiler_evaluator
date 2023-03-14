-- Author: Julian Rathke, 2018
-- Provides a CEK implementation of the \ language from the lecture notes
module Evaluator where
import Grammar

--Data structures as defined in Grammar:
--data Type = TypeInt | TypeBool | TypeFun Type Type
--type Environment = [(String,Expr)]
--data Expr = Int Int | True | False | LessThan Expr Expr 
--           | Add Expr Expr | Var String 
--           | If Expr Expr Expr | Let String Type Expr Expr
--           | Lambda String Type Expr | App Expr Expr
--           | Cl (String Type Expr Environment)

data Ast = HLessThan Expr Environment | LessThanH Expr
           |HGreaterThan Expr Environment | GreaterThanH Expr
           | HIsEqual Expr Environment | IsEqualH Expr
           | HAdd Expr Environment | AddH Expr
           | HTimes Expr Environment | TimesH Expr
           | HIf Expr Expr Environment | HLet String VarType Expr Environment
           | HApp Expr Environment | AppH Expr
type Kontinuation = [Ast]
type State = (Expr,Environment,Kontinuation)

-- Function to unpack a closure to extract the underlying lambda term and environment
unpack :: Expr -> (Expr,Environment)
unpack (Cl x t e env1) = ((Lambda x t e) , env1)
unpack e = (e,[])

-- Look up a value in an environment and unpack it
getValueBinding :: String -> Environment -> (Expr,Environment)
getValueBinding x [] = error "Variable binding not found"
getValueBinding x ((y,e):env) | x == y  = unpack e
                              | otherwise = getValueBinding x env

update :: Environment -> String -> Expr -> Environment
update env x e = (x,e) : env

-- Checks for terminated expressions
isValue :: Expr -> Bool
isValue (Int _) = True
isValue VarTrue = True
isValue VarFalse = True
isValue (Cl _ _ _ _) = True
isValue _ = False

--Small step evaluation function
eval1 :: State -> State
eval1 ((Var x),env,k) = (e',env',k) 
                    where (e',env') = getValueBinding x env

-- Rule for terminated evaluations
eval1 (v,env,[]) | isValue v = (v,env,[])
                  
-- Evaluation rules for less than operator
eval1 ((LessThan e1 e2),env,k) = (e1,env,(HLessThan e2 env):k)
eval1 ((Int n),env1,(HLessThan e env2):k) = (e,env2,(LessThanH (Int n)) : k)
eval1 ((Int m),env,(LessThanH (Int n)):k) | n < m = (VarTrue,[],k)
                                              | otherwise = (VarFalse,[],k)
-- Evaluation rules for greaterthan operator
eval1 ((GreaterThan e1 e2),env,k) = (e1,env,(HGreaterThan e2 env):k)
eval1 ((Int n),env1,(HGreaterThan e env2):k) = (e,env2,(GreaterThanH (Int n)) : k)
eval1 ((Int m),env,(GreaterThanH (Int n)):k) | n > m = (VarTrue,[],k)
                                              | otherwise = (VarFalse,[],k)


eval1 ((IsEqual e1 e2),env,k) = (e1,env,(HIsEqual e2 env):k)
eval1 ((Int n),env1,(HIsEqual e env2):k) = (e,env2,(IsEqualH (Int n)) : k)
eval1 ((Int m),env,(IsEqualH (Int n)):k) | n == m = (VarTrue,[],k)
                                              | otherwise = (VarFalse,[],k)
-- Evaluation rules for plus operator
eval1 ((Add e1 e2),env,k) = (e1,env,(HAdd e2 env):k)
eval1 ((Int n),env1,(HAdd e env2):k) = (e,env2,(AddH (Int n)) : k)
eval1 ((Int m),env,(AddH (Int n)):k) = (Int (n + m),[],k)

-- Evaluate Multiplication
eval1 ((Times e1 e2),env,k) = (e1,env,(HTimes e2 env):k)
eval1 ((Int n),env1,(HTimes e env2):k) = (e,env2,(TimesH (Int n)) : k)
eval1 ((Int m),env,(TimesH (Int n)):k) = (Int (n * m),[],k)


-- Evaluation rules for if-then-else
eval1 ((If e1 e2 e3),env,k) = (e1,env,(HIf e2 e3 env):k)
eval1 (VarTrue,env1,(HIf e2 e3 env2):k) = (e2,env2,k)
eval1 (VarFalse,env1,(HIf e2 e3 env2):k) = (e3,env2,k)

-- Evaluation rules for Let blocks
eval1 ((Let x typ e1 e2),env,k) = (e1,env,(HLet x typ e2 env):k)
eval1 (v,env1,(HLet x typ e env2):k) | isValue v = (e, update env2 x v , k)

--  Rule to make closures from lambda abstractions.
eval1 ((Lambda x typ e),env,k) = ((Cl x typ e env), [], k)

-- Evaluation rules for application
eval1 ((App e1 e2),env,k) = (e1,env, (HApp e2 env) : k)
eval1 (v,env1,(HApp e env2):k ) | isValue v = (e, env2, (AppH v) : k)
eval1 (v,env1,(AppH (Cl x typ e env2) ) : k )  = (e, update env2 x v, k)

-- Rule for runtime errors
eval1 (e,env,k) = error "Evaluation Error"

-- Function to iterate the small step reduction to termination
evalLoop :: Expr -> Expr 
evalLoop e = evalLoop' (e,[],[])
  where evalLoop' (e,env,k) = if (e' == e) && (isValue e') && (null k) then e' else evalLoop' (e',env',k')
                       where (e',env',k') = eval1 (e,env,k) 

boolify::Expr->Bool
boolify (VarTrue) = True 
boolify (VarFalse) = False


-- Function to unparse underlying values from the AST term
unparse :: Expr -> String 
unparse (Int n) = show n
unparse (VarTrue) = "true"
unparse (VarFalse) = "false"
unparse (Cl _ _ _ _) = "Function Value"
unparse _ = "Unknown"
