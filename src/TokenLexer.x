{

{-


    Usage
    =====
    cat $filename | runhaskell $lexername.hs

    Tokenize the infile according to the Minisculus syntax, writing the
    token list to stdout. If the lexing fails, an error message is
    displayed.

       
-}

module TokenLexer where

import Data.List
import Data.Bool
}

%wrapper "monad"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
    $white+                     ;
    if                      {  wrapTok IF }
    then                    {  wrapTok THEN }
    fi                      {  wrapTok ENDIF}
    let                     {  wrapTok LET }
    in                      {  wrapTok IN }
    end                     { wrapTok ENDLET}
    else                    {  wrapTok ELSE }
    "{"                  {  wrapTok BEGIN }
    "}"                     {  wrapTok END }
    True                    {constantify }
    False                   {constantify}
   ":="                    {  wrapTok ASSIGNMENT }
    xor                     {wrapTok XOR}
    or                      {  wrapTok OR} 
    and                     { wrapTok AND}
    "!"                     { wrapTok NOT}
    "=="                    { wrapTok EQUALS}
    "->"                    { wrapTok IMPLIES}
    "+"                     {  wrapTok PLUS }   
    "-"                     {  wrapTok MINUS }
    "*"                     {  wrapTok TIMES }
    "neg"                   {  wrapTok NEGATE}
    "/"                     {  wrapTok DEVIDE }
    "("                     {  wrapTok LPAREN }
    ")"                     {  wrapTok RPAREN }
    ";"                     {  wrapTok SEMICOLON }
    "["                     { wrapTok STARTBOOL}
    "]"                     { wrapTok ENDBOOL}
    ">"                     {wrapTok GREATERTHAN}
    "<"                     {wrapTok LESSTHAN}
    end                     {  wrapTok END }
    $alpha [$alpha $digit]* {  identifier } 
    $digit+                 {  number }
    .                       {  lexError }
    
{

{- Lexer Token definitions and main routines -}

-- Token Data Type.
data Token = Token Pos TokenClass deriving (Eq, Show)

data TokenClass  = ID String
            | NUM Int
            | IF
            | ENDIF
            | THEN
            | LET
            | IN
            | ELSE
            | BEGIN
            | AND
            | END
            | PLUS
            | CONST String
            | NEGATE
            | ASSIGNMENT
            | MINUS
            | TIMES
            | DEVIDE
            | LPAREN
            | RPAREN
            | SEMICOLON
            | IMPLIES
            | EQUALS
            | NOT
            | XOR
            | OR
            |STARTBOOL
            |ENDBOOL
            | T_EOF
            |GREATERTHAN
            |LESSTHAN
            |ENDLET

            deriving (Eq,Show)
--
--instance Show TokenClass where


alexEOF :: Alex Token
alexEOF = do
    (p,_,_,_) <- alexGetInput
    return $ Token (toPos p) T_EOF

toPos :: AlexPosn -> Pos
toPos (AlexPn _ l c) = Pos (Line l) (Column c)

-- Extracts the token type, returning as an Alex Token datum.
wrapTok :: TokenClass -> AlexInput -> Int -> Alex Token 
wrapTok c (p, _, _, _) _ = return $
    Token (toPos p) c

-- Constructs a ID token from the currently parsed string.
identifier :: AlexInput -> Int -> Alex Token
identifier (p, _, _, inp) len = return $ 
    Token (toPos p) (ID (take len inp))

-- Constructs a NUM token from the currently parsed number.
number :: AlexInput -> Int -> Alex Token
number (p, _, _, inp) len = return $
    Token (toPos p) (NUM (read $ take len inp))

constantify:: AlexInput -> Int -> Alex Token
constantify (p, _, _, inp) len = return $ 
    Token (toPos p) (CONST (take len inp))


lexError :: AlexInput -> Int -> Alex Token 
lexError (p, _, _, inp) len = alexError $ 
    "unexpected token '" ++ (take len inp) ++ "' at " ++ showPos p




showPos (AlexPn _ l c) = show l ++ ":" ++ show c


gettokens str = runAlex str $ do
    let loop tokPairs = do 
        tok <- alexMonadScan; 
        case tok of
            Token _ T_EOF   -> return (reverse tokPairs)
            _               -> do loop (tok:tokPairs)
    loop []



--wrapping token for typechecking

tokenUnwrap :: [Token] -> [(Pos, TokenClass)]
tokenUnwrap = map (\(Token p t) -> (p,t))

tokenize s = do
    let l = gettokens s
    case l of
        Left s  -> error s
        Right t -> putStrLn $ foldr1 (\s1 s2 -> s1 ++ "  " ++ s2) $
                        map show t

data Pos    = Pos Line Column   deriving (Eq)
data Line   = Line Int          deriving (Eq)
data Column = Column Int        deriving (Eq)

instance Show Pos where
    show (Pos l c)  = show l ++ ":" ++ show c
instance Show Line where
    show (Line n)   = show n
instance Show Column where
    show (Column n) = show n

lexingError msg =
    error $ "Lexical Error - " ++ msg


scanT str = go (alexStartPos,'\n',[],str)
                      where
                        go inp@(pos,_,_,str) =
                          case alexScan inp 0 of
                            AlexEOF -> return []
                            AlexError ((AlexPn _ line column),_,_,s) -> error ( "Unknown token:" ++ (show line) ++ ":" ++ (show column) ++ " column")
                            AlexSkip  inp' _       -> go inp'
                            AlexToken inp' len act -> do
                            res <- go inp'
                            let rest = (take len str)
                            return (rest : res)

}

