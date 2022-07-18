

module Ast where


import TokenLexer




{-
 this are the recursive descent grammar rules are defined we perform pattern matching on using these rules in function and parser program
 using recursive descent parser
 at the time of submission i can not get boolean to work that part is commented out in a program but goal is to get complete parser 
-}

data ParseTree = ParseTree Code
data Code   =   Rule0 Stmt
data Stmt   =   Rule1 If Expr Then Stmt Else Stmt Endif
            |   Rule2 Let Stmt In Stmt 
            |   Rule4 Identifier Assign Expr
            |   Rule6 Begin ListofStmt
data ListofStmt = Rule7 Stmt SSemicol ListofStmt
            |   Rule8 End
data SSemicol = Rule9 Semicolon
data Bexpr   = Rule25 Bterm Mbexpr
data Bterm  = Rule47 Bfactor MBterm
data Mbexpr = Rule40 Implies MBterm
            | Rule43
    
data MBterm  =  Rule27 Or NMBexpr
            |   Rule28 And NMBexpr
            |   Rule29 Xor NMBexpr
            |   Rule30 Equalto NMBexpr
            |   Rule31 

data ImpBexpr = Rule39 
data NMBexpr =  Rule32 Not Factor
            |   Rule33 
data CmpExpr = Rule36  Greaterthan MoreExpr
            |  Rule35  Lessthan MoreExpr
            |  Rule41 Expr
data Expr    =  Rule10 Term MoreExpr
        --  |   Rule34 Startbool Bexpr Endbool
data MoreExpr = Rule11 Add Expr
            |   Rule12 Sub Expr
            |   Rule13
data Term =     Rule14 Factor MoreTerm
data MoreTerm = Rule15 Mul Term
            |   Rule16 Div Term
            |   Rule17
data Factor =   Rule18 LPar Expr SRPar
            |   Rule19 Identifier
            |   Rule20 Ast.Num
            |   Rule23 Constant
            |   Rule21 Invert Ast.Num
            |   Rule24
data Bfactor = Rule44 LPar Bexpr SRPar
            | Rule37 Constant
            | Rule38 Identifier
            | Rule45 Not Bfactor 

data SRPar  =   Rule22 RPar
data If = If
data Then = Then
data Else = Else
data Let = Let
data In = In
data Input = Input
data Identifier = Identifier String
data Num = Num Int
data Assign = Assign
data Write = Write
data Begin = Begin
data End = End
data Semicolon = Semicolon
data Add = Add
data Sub = Sub
data Mul = Mul
data Div = Div
data LPar = LPar
data RPar = RPar
data Invert = Invert
data Constant = Constant String
data Or = Or
data And = And
data Xor = Xor
data Implies = Implies
data Equalto = Equalto
data Not = Not
data Endif = Endif
data Negatenum = Negatenum
data Startbool = Starbool
data Endbool = Endbool
data Greaterthan = Greaterthan
data Lessthan = Lessthan
data EndLet = EndLet




{-  These are the pattern matching rules to generate parse tree from tokens these patterns uses patterns created from grammar data rules to get proper matching-}
parse :: [(Pos,TokenClass)] -> (ParseTree, [(Pos,TokenClass)])
parse [] = error "Parsing Error - Empty token list"
parse ts = ((ParseTree s), ts1) where
    (s,ts1) = prog ts

prog :: [(Pos,TokenClass)] -> (Code, [(Pos,TokenClass)])
prog ts = ((Rule0 s), ts1) where
    (s,ts1) = stmt ts

stmt :: [(Pos,TokenClass)] -> (Stmt, [(Pos,TokenClass)])
stmt ((_,IF):ts) =
    ((Rule1 If e Then s1 Else s2 Endif), ts3) where
    (e,ts1) = expr ts
    (s1,ts2) = stmt $ _stmt1 ts1
    (s2,ts3) = stmt $ _stmt2 ts2
    _stmt1 ((_,THEN):ts) = ts
    _stmt1 ts = ts
    _stmt2 ((_,ELSE):ts) = ts
    _stmt2 ts = ts
stmt ((_,LET):ts) = 
    ((Rule2 Let s1 In s2 ), ts2) where 
    (s1,ts1) = stmt ts 
    (s2,ts2) = stmt $ _stmt1 ts1
    _stmt1 ((_,IN):ts) = ts
    _stmt1 ts = ts

stmt ((_,ID str):(_,ASSIGNMENT):ts) = 
    ((Rule4 (Identifier str) Assign e), ts1) where
    (e,ts1) = expr ts

stmt ((_,BEGIN):ts) = 
    ((Rule6 Begin l), ts1) where
    (l,ts1) = stmtlist ts 
stmt ((p,t):ts) =
    error $ parseError p t stmtExpected



stmtlist :: [(Pos,TokenClass)] -> (ListofStmt, [(Pos,TokenClass)])
stmtlist ((_,END):ts) = 
    ((Rule8 End), ts)
-- Look ahead by one to see if we're entering a statement
stmtlist ((p,IF):ts)              = doStmt ((p,IF):ts)
stmtlist ((p,LET):ts)           = doStmt ((p,LET):ts)
stmtlist ((p,ID str):ts)  = doStmt ((p,ID str):ts)
stmtlist ((p,BEGIN):ts)           = doStmt ((p,BEGIN):ts)
stmtlist ((p,t):ts) = 
    error $ parseError p t stmtListExpected 
doStmt ts = 
    ((Rule7 s sem l), ts3) where
    (s,ts1) = stmt ts
    (sem,ts2) = semicolon ts1
    (l,ts3) = stmtlist ts2 

semicolon :: [(Pos,TokenClass)] -> (SSemicol, [(Pos,TokenClass)])
semicolon ((_, SEMICOLON):ts) = 
    ((Rule9 Semicolon), ts)
semicolon ((p,t):ts) =
    error $ parseError p t semicolonExpected

expr :: [(Pos,TokenClass)] -> (Expr, [(Pos,TokenClass)])
expr ts = 
    ((Rule10 t me), ts2) where
    (t,ts1) = term ts
    (me,ts2) = moreExpr ts1

-- expr ((_,STARTBOOL):ts) = 
--     (Rule34 Startbool b Endbool)
--     where
--         (b,ts1) = bexpr ts 


-- cmpexpr:: [(Pos,TokenClass)] -> (CmpExpr, [(Pos,TokenClass)])
-- cmpexpr((_,GREATERTHAN):ts) =
--     ((Rule36 Greaterthan e)) where
--         (e:ts1)=moreExpr ts
-- cmpexpr((_,LESSTHAN):ts) =
--     ((Rule35 Lessthan e):ts) where
--         (e:ts1)=moreExpr ts
-- cmpexpr ts = 
--     moreExpr ts

moreExpr :: [(Pos,TokenClass)] -> (MoreExpr, [(Pos,TokenClass)])
moreExpr ((_,PLUS):ts) = 
    ((Rule11 Add e), ts1) where
    (e,ts1) = expr ts
moreExpr ((_,MINUS):ts) = 
    ((Rule12 Sub e), ts1) where
    (e,ts1) = expr ts
moreExpr ts = 
    ((Rule13), ts)



term :: [(Pos,TokenClass)] -> (Term, [(Pos,TokenClass)])
term ts = 
    ((Rule14 f mt), ts2) where
    (f,ts1) = factor ts
    (mt,ts2) = moreTerm ts1

moreTerm :: [(Pos,TokenClass)] -> (MoreTerm, [(Pos,TokenClass)])
moreTerm ((_,TIMES):ts) = 
    ((Rule15 Mul t), ts1) where
    (t,ts1) = term ts
moreTerm ((_,DEVIDE):ts) = 
    ((Rule16 Div t), ts1) where
    (t,ts1) = term ts
moreTerm ts = 
    ((Rule17), ts)

factor :: [(Pos,TokenClass)] -> (Factor, [(Pos,TokenClass)])
factor ((_,LPAREN):ts) = 
    ((Rule18 LPar e srp), ts2) where
    (e,ts1) = expr ts
    (srp,ts2) = rPar ts1
factor ((_,ID str):ts) = 
    ((Rule19 (Identifier str)), ts)
factor ((_,NUM n):ts) = 
    ((Rule20 (Num n)), ts)
factor ((_,NEGATE):((_,NUM n):ts)) = 
    ((Rule21 Invert (Num n)), ts)
factor ((_,CONST str):ts) = 
    ((Rule23 (Constant str)), ts)
factor ((p,t):ts) =
   error $ parseError p t factorExpected
{- 
bexpr :: [(Pos,TokenClass)] -> (Bexpr, [(Pos,TokenClass)])
bexpr ts = 
    ((Rule25 t me), ts2) where
    (t,ts1) = bterm ts
    (me,ts2) = bmoreExpr ts1
bexpr(_,STARTBO)
bexpr ts
bmoreExpr :: [(Pos,TokenClass)] -> (Mbexpr, [(Pos,TokenClass)])
bmoreExpr ((_,PLUS):ts) = 
    ((Rule11 Add e), ts1) where
    (e,ts1) = expr ts
bmoreExpr ((_,MINUS):ts) =  
    ((Rule12 Sub e), ts1) where
    (e,ts1) = expr ts
bmoreExpr ts = 
    ((Rule13), ts)            

bterm :: [(Pos,TokenClass)] -> (BTerm, [(Pos,TokenClass)])
bterm ts = 
    ((Rule14 f mt), ts2) where
    (f,ts1) = factor ts
    (mt,ts2) = moreTerm ts1

bmoreTerm :: [(Pos,TokenClass)] -> (MBterm, [(Pos,TokenClass)])
bmoreTerm ((_,TIMES):ts) = 
    ((Rule15 Mul t), ts1) where
    (t,ts1) = term ts
bmoreTerm ((_,DEVIDE):ts) = 
    ((Rule16 Div t), ts1) where
    (t,ts1) = term ts
bmoreTerm ts =
    ((Rule17), ts)

bfactor :: [(Pos,TokenClass)] -> (Factor, [(Pos,TokenClass)])
bfactor ((_,LPAREN):ts) = 
    ((Rule18 LPar e srp), ts2) where
    (e,ts1) = expr ts
    (srp,ts2) = rPar ts1
bfactor ((_,ID str):ts) = 
    ((Rule19 (Identifier str)), ts)
bfactor ((_,NUM n):ts) = 
    ((Rule20 (Num n)), ts)
bfactor ((_,NEGATE):((_,NUM n):ts)) = 
    ((Rule21 Invert (Num n)), ts)
bfactor ((_,CONST str):ts) = 
    ((Rule23 (Constant str)), ts)
bfactor ((p,t):ts) =
    error $ parseError p t factorExpected

 -}

rPar :: [(Pos,TokenClass)] -> (SRPar, [(Pos,TokenClass)])
rPar ((_, RPAREN):ts) = 
    ((Rule22 RPar), ts)
rPar ((p,t):ts) =
    error $ parseError p t rParExpected






{-    All the possible errors are collected here -}
parsingError p msg =
    error $ "SyntaxError" ++ msg ++ " at " ++ show p

stmtExpected        = "if, let, in, [identifier], or {"
progExpected        = stmtExpected
parseExpected       = progExpected
factorExpected      = "'(', [identifier], or [number] or [Bool] "
termExpected        = factorExpected
exprExpected        = termExpected
stmtListExpected    = "end, " ++ stmtExpected
semicolonExpected   = "';'"
rParExpected        = "')'"
moreExprExpected    = "'+', '-', " ++ termExpected
moreTermExpected    = "'*', '/', " ++ factorExpected

parseError p s expected = parsingError p msg where
    msg = "unexpected token '" ++ show s ++ "' (expected " ++
        expected ++ ")"




data Ast    = Ast AST_Code
data AST_Code = AST_Code AST_Stmt
data AST_Stmt = AST_IfThenElse AST_Expr AST_Stmt AST_Stmt
            | AST_Let AST_Stmt AST_Stmt
            | AST_Input AST_Identifier
            | AST_Assign AST_Identifier AST_Expr
            | AST_Block [AST_Stmt]
data AST_Expr = AST_Add AST_Expr AST_Expr 
            | AST_Sub AST_Expr AST_Expr
            | AST_Mul AST_Expr AST_Expr
            | AST_Div AST_Expr AST_Expr
            | AST_Var AST_Identifier
            |AST_Int AST_Num 
            |AST_Const AST_Bool
-- data AST_Bexpr = AST_Implies AST_Bexpr AST_BExpr
--             |AST_Or AST_Bexpr AST_BExpr
--             |AST_Xor AST_Bexpr AST_BExpr
--             |AST_And AST_Bexpr AST_BExpr
--             |AST_Equalto Bexpr AST_Bexpr
--             -- |AST_Const AST_Bool
--             |AST_Id AST_Bool
data AST_Identifier = AST_Identifier String
data AST_Num  = AST_Num Int 
data AST_Bool = AST_Bool String





{-functions these are used to convert parsing tree into AST tree representation -}
outParseTree :: ParseTree -> Ast
outParseTree (ParseTree p) = Ast (outCode p)

outCode (Rule0 s) = AST_Code (outStmt s)

outStmt (Rule1 If e Then s1 Else s2 Endif) = 
    AST_IfThenElse (outExpr e) (outStmt s1) (outStmt s2)
outStmt (Rule2 Let s1 In s2) = 
    AST_Let (outStmt s1) (outStmt s2)

outStmt (Rule4 (Identifier s) Assign e) = 
    AST_Assign (AST_Identifier s) (outExpr e)
outStmt (Rule6 Begin l) =
    AST_Block (outListofStmt l)

outListofStmt (Rule7 s (Rule9 Semicolon) l) =
    (outStmt s):(outListofStmt l)
outListofStmt (Rule8 End) =
    []

outExpr (Rule10 t (Rule11 Add e)) =
    AST_Add (outTerm t) (outExpr e)
outExpr (Rule10 t (Rule12 Sub e)) = 
    AST_Sub (outTerm t) (outExpr e)
outExpr (Rule10 t (Rule13)) =
    outTerm t

-- outExpr (Rule34 Startbool()  Endbool) = 
    
-- -- outExpr (Rule10 t (Rule13)) =
-- --     outTerm t

outTerm (Rule14 f (Rule15 Mul t)) =
    AST_Mul (outFactor f) (outTerm t)
outTerm (Rule14 f (Rule16 Div t)) =
    AST_Div (outFactor f) (outTerm t)
outTerm (Rule14 f (Rule17)) =
    outFactor f

outFactor (Rule18 LPar e (Rule22 RPar)) =
    outExpr e
outFactor (Rule19 (Identifier s)) =
    AST_Var (AST_Identifier s)
outFactor (Rule20 (Ast.Num n)) =
   AST_Int (AST_Num n)
outFactor (Rule21 Invert (Ast.Num n)) =
   AST_Int (AST_Num (-1 * n))
outFactor (Rule23 (Constant s)) =
    AST_Const (AST_Bool s)






{- THese functions are ussed to customize the output of of AST treee-}

instance Show Ast where
    show (Ast a) = "STARTPROGRAM (\n" ++ show a ++ ") EOF"
instance Show AST_Code where
    show (AST_Code a) = show a
instance Show AST_Stmt where
    show (AST_IfThenElse e s1 s2) = "IF " ++ show e ++ 
        " THEN (\n" ++ show s1 ++ ") " ++
        "ELSE (\n" ++ show s2 ++ ") ENDIF;\n"
    show (AST_Let e s) = "LET " ++ show e ++ " In (\n" ++ show s 
        ++ ") ENDLET\n"
    show (AST_Input i) = "INPUT (" ++ show i ++ ");\n"
    show (AST_Assign i e) = "(" ++
        show i ++ " := " ++ show e ++ ");\n"
    show (AST_Block l) = "LiST OF STATEMTNT[\n" ++ foldr1 (++) (map show l) ++ 
        "] ENDLIST;\n"
instance Show AST_Expr where
    show (AST_Add e t) = "Binaryexp(ADDExp("++ show e ++ "+"  ++ show t++")"
    show (AST_Sub e t) = "Binaryexp(MinusExp("++show e ++ " - " ++ show t++")"
    show (AST_Mul e t) = "Binaryexp(MULExp("++show e ++ " * " ++ show t ++")"
    show (AST_Div e t) = "BinarExp(DIVExp("++show e ++ " / " ++ show t++")"
    show (AST_Var i) = "( " ++ show i ++ ")"
    show (AST_Int n) = "(" ++ show n ++ ")"
    show (AST_Const b) = "("++ show b ++")"
instance Show AST_Identifier where
    show (AST_Identifier s) = "( ID "++ show s++")"
instance Show AST_Num where
    show (AST_Num n) = if n >= 0 then id show n
                     else ("UnaryExp(Negate"++(id show n)++")")
instance Show AST_Bool where 
    show (AST_Bool s) = "(Bool "++show s++")"

