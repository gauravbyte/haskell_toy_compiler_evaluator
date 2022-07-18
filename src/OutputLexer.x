{

module OutputLexer (
  Token(..),

  alexScanTokens
) where
}
%wrapper "basic" 
$digit = 0-9     
-- digits 
$alpha = [a-zA-Z]    
-- alphabetic characters
$eol   = [\n]

tokens :-
$white+       ; 
let       {\s ->    LET         s}   
if        {\s ->    IF          s} 
fi        {\s ->    ENDIF       s}
then      {\s  ->    THEN       s} 
end       {\s ->    ENDLET      s} 
else       {\s ->   ELSE      s} 
in        {\s ->    IN          s} 
True      {\s ->    CONST       s} 
False     {\s ->    CONST       s} 
\!        {\s ->    NOT         s} 
\|        {\s ->    OR          s} 
\^        {\s ->    XOR         s}  
\&        {\s ->    AND         s}  
\%        {\s ->    IMPLIES     s} 
\>        {\s ->    GREATERTHAN s} 
\<        {\s ->    LESSTHAN    s} 
[$digit]+ {\s ->    INT         s} 
"neg"     {\s ->    NEGATE      s}
\=        {\s ->    EQUALS      s} 
\+        {\s ->    PLUS        s} 
\-        {\s ->    MINUS       s} 
\*        {\s ->    TIMES       s} 
\~        {\s ->    NEGATE      s} 
\(        {\s ->    LPAREN      s}
"["       { \s-> STARTBOOL     s}
"]"       {\s-> ENDBOOL        s} 
\)        {\s ->    RPAREN      s} 
":="      {\s -> ASSIGNMENT s}
"}"       {\s -> BEGIN s }
"{"       {\s -> END s}
";"       {\s -> SEMICOLON s}

  $alpha [$alpha $digit \_ \’]*   {\s -> VAR s } 

{ 
-- Each action has type :: String -> Token 
-- The token type: 
data Token = 
  LET         String  | 
  IN          String  |
  IF          String  |
  ELSE        String  |
  BEGIN       String  |
  END         String  |
  ENDIF       String  |
  ENDLET      String  |
  LESSTHAN    String  |
  NOT         String  |
  GREATERTHAN String  |
  XOR         String  |
  IMPLIES     String  |
  OR          String  |
  AND         String  |
  CONST       String  |
  INT         String  |
  VAR         String  | 
  EQUALS      String  |
  PLUS        String  |
  NEGATE      String  |
  MINUS       String  |
  TIMES       String  |    
  LPAREN      String  |
  THEN        String  |
  BOOl        String |
  ENDBOOL     String |
  STARTBOOL String |
  ASSIGNMENT String|
  STMTBEG     String|
  SEMICOLON String |
  RPAREN      String      
    deriving (Eq,Show) 
--
--getLineNum :: AlexPosn -> Int
--getLineNum (AlexPn _ lineNum _) = lineNum
--
--getColumnNum :: AlexPosn -> Int
--getColumnNum (AlexPn _ _ colNum) = colNum
--
--tokenUnwrap :: [Token] -> [(Pos, TokenClass)]
--tokenUnwrap = map (\(Token p t) -> (p,t))
--
--scanTokens :: String -> Except String [Token]
--scanTokens str = go (alexStartPos,'\n',[],str)
--  where
--    go inp@(pos,_,_,str) =
--      case alexScan inp 0 of
--        AlexEOF -> return []
--        AlexError ((AlexPn _ line column),_,_,_) -> throwError $ "lexical error at " ++ (show line) ++ " line, " ++ (show column) ++ " column"
--        AlexSkip  inp' _       -> go inp'
--        AlexToken inp' len act -> do
--          res <- go inp'
--          let rest = act pos (take len str)
--          return (rest : res)
--
}