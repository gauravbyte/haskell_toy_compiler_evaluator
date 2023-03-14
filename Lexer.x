{ 
module Lexer where 
}

%wrapper "posn" 
$digit = 0-9     
-- digits 
$alpha = [a-zA-Z]    
-- alphabetic characters

tokens :-
$white+       ; 
  ";"            ;
  "("             {tok (\p s -> LPAREN p) }
  ")"             {tok (\p s -> RPAREN p) }
  -- int ops
  "+"            {tok    (\p s -> PLUS p) }
  "-"             {tok   (\p s -> MINUS p) }
  "*"             { tok   (\p s ->TIMES p) }
  "~"            {tok    (\p s -> NEGATE p )}
  "="            {tok    (\p s ->  EQUALS  p )}
  "<"            {tok    (\p s -> LESSTHAN p) }
  ">"             {tok    (\p s -> GREATERTHAN p) }
  
  -- bool ops 
  "!"              {tok    (\p s -> NOT        p) }
  "&&"            {tok    (\p s -> AND   p )}
  "||"             {tok    (\p s -> OR    p )}
  "^"             {tok    (\p s ->  XOR       p) }
  
  -- ite
  "if"             {tok    (\p s -> IF p) }
  "then"           {tok    (\p s -> THEN p) }
  "else"           {tok    (\p s -> ELSE p) }
  "fi"             {tok    (\p s ->  FI     p )}



-- let 
  let            {tok    (\p s -> LET p )}
  ":="            {tok    (\p s -> ASSIGN p )}
  in             {tok    (\p s -> IN p )}
  end            {tok (\p s -> END p)}
  "->"           {tok    (\p s -> TokenArrow     p) }

--atoms
  $digit+        {tok    (\p s -> TokenInt p (read s)) }
  true           {tok    (\p s -> TokenVarTrue p) }
  false          {tok    (\p s -> TokenVarFalse p) }



-- function
 "fun"            {tok    (\p s -> FUNCTION  p) }
  "=>"           {tok (\p s  -> FUNDECL p)}
  "fn"           {tok    (\p s -> TokenFn p) }
  "::"            {tok    (\p s -> TokenCons p) }
  

 -- data types 
  bool           {tok    (\p s -> TYPEBOOL p)} 
  int            {tok    (\p s -> TYPEINT   p) }
  $alpha [$alpha $digit \_ \’]*     { tok (\p s -> ID p s) } 

{ 
-- Each action has type :: AlexPosn -> String -> MDLToken 

-- Helper function
tok f p s = f p s

-- The token type: 
data VarToken = 
  LPAREN             AlexPosn           |
  RPAREN             AlexPosn           |
  PLUS               AlexPosn           |
  MINUS              AlexPosn           |  
  TIMES              AlexPosn           |  
  NEGATE             AlexPosn           |
  EQUALS             AlexPosn           |
  LESSTHAN           AlexPosn           |
  GREATERTHAN         AlexPosn          |
  NOT                AlexPosn           |
  AND                AlexPosn           |
  OR                 AlexPosn           |
  XOR                AlexPosn           |
  IF                 AlexPosn           |
  THEN               AlexPosn           |
  ELSE               AlexPosn           |
  FI                 AlexPosn           |
  LET                AlexPosn           |
  ASSIGN             AlexPosn           |
  IN                 AlexPosn           |
  END                 AlexPosn          |
  TokenArrow         AlexPosn           |
  TokenInt           AlexPosn    Int    | 
  TokenVarTrue       AlexPosn           |
  TokenVarFalse      AlexPosn           |
  FUNCTION           AlexPosn           |
  FUNDECL            AlexPosn           |
  TokenFn            AlexPosn           | 
  TokenCons          AlexPosn           |
  TYPEBOOL           AlexPosn           | 
  TYPEINT            AlexPosn           | 
  ID                 AlexPosn    String
  deriving (Eq,Show) 

tokenPosn :: VarToken -> String
tokenPosn (      TokenInt        (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (      ID              (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn  (     LPAREN          (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn  (     RPAREN          (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn  (     PLUS            (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn  (     MINUS           (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn  (     TIMES           (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn  (     NEGATE          (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn  (     EQUALS          (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn  (     LESSTHAN        (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn  (     GREATERTHAN     (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn  (     NOT             (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn  (     AND             (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn  (     OR              (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn  (     XOR             (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn  (     IF              (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn  (     THEN            (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn  (     ELSE            (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn  (     FI              (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn  (     LET             (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn  (     ASSIGN          (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn  (     IN              (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn  (     END             (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn  (     TokenArrow      (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn  (     TokenVarTrue    (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn  (     TokenVarFalse   (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn  (     FUNCTION        (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn  (     FUNDECL         (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn  (     TokenFn         (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn  (     TokenCons       (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn  (     TYPEBOOL        (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 
tokenPosn  (     TYPEINT         (AlexPn a l c)) = show(l) ++ ":" ++ show(c) 

}





