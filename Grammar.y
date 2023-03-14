{ 
module Grammar where 
import Lexer 
}

%name parseCalc 
%tokentype { VarToken } 
%error { parseError }
%token 
    Bool   { TYPEBOOL _ } 
    Int    { TYPEINT _ } 
    arrow  { TokenArrow _ } 
    int    { TokenInt _ $$ } 
    true   { TokenVarTrue _ }
    false  { TokenVarFalse _ }
    '<'    { LESSTHAN _ }
    '+'    { PLUS _ }
    var    { ID _ $$ }
    if     { IF _ }
    then   { THEN _ }
    else   { ELSE _ }
    lam    { TokenFn _ }
    let    { LET _ }
    ':'    { TokenCons _ }
    ':='    { ASSIGN _ }
    in     { IN _ }
    '('    { LPAREN _ } 
    ')'    { RPAREN _ } 
    '*'    {TIMES _ }
    '>'    {GREATERTHAN _ }
    '='    { EQUALS  _ }
%left arrow lam '<' '>' '+' APP
%left '*'
%right let in
%nonassoc if then else int true false var '(' ')'

%% 
Exp : int                                       { Int $1 } 
    | var                                       { Var $1 }
    | true                                      { VarTrue }
    | false                                     { VarFalse } 
    | Exp '<' Exp                               { LessThan $1 $3 } 
    | Exp '>' Exp                               { GreaterThan $1 $3 }
    | Exp '+' Exp                               { Add $1 $3 }
    | Exp '=' Exp                               { IsEqual $1 $3}
    | Exp '*' Exp                               { Times $1 $3 }
    | if Exp then Exp else Exp                  { If $2 $4 $6 } 
    | lam '(' var ':' Type ')' Exp              { Lambda $3 $5 $7 }
    | let '(' var ':' Type ')' '=' Exp in Exp   { Let $3 $5 $8 $10 }
    | Exp Exp %prec APP                         { App $1 $2 } 
    | '(' Exp ')'                               { $2 }

Type : Bool            { TypeBool } 
     | Int             { TypeInt } 
     | Type arrow Type { TypeFun $1 $3 } 


{ 
parseError :: [VarToken] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data VarType = TypeInt | TypeBool | TypeFun VarType VarType
   deriving (Show,Eq)

type Environment = [(String,Expr)]

data Expr = Int Int | VarTrue | VarFalse | LessThan Expr Expr 
            | VarTrue Bool
            | VarFalse Bool
            | GreaterThan Expr Expr
            | IsEqual Expr Expr
            | Add Expr Expr
            | Times Expr Expr 
            | Var String 
            | If Expr Expr Expr | Let String VarType Expr Expr
            | Lambda String VarType Expr | App Expr Expr 
            | Cl String VarType Expr Environment
    deriving (Show,Eq)
} 