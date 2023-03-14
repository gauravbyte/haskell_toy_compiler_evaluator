# Haskell Toy Language Tokenizer , Parser and Compiler
 this is the tokenizer and parser completely designed haskell which is functional programming language
 Built in Haskell Programming Language
 
Parse tree structure
ParseTree = ParseTree Code
 Code   =     Stmt
 Stmt   =     If Expr Then Stmt Else Stmt Endif
            |     Let Stmt In Stmt 
            |     Identifier Assign Expr
            |     Begin ListofStmt  <br />
 ListofStmt =   Stmt SSemicol ListofStmt
            |     End  <br />
 SSemicol =   Semicolon
 Bexpr   =   Bterm Mbexpr
 Bterm  =   Bfactor MBterm
 Mbexpr =   Implies MBterm
            | E <br />
    
 MBterm  =    Or NMBexpr
            |     And NMBexpr
            |     Xor NMBexpr
            |     Equalto NMBexpr
            | E     <br />

 NMBexpr =    Not Factor
            |  E    <br />
 CmpExpr =    Greaterthan MoreExpr
            |     Lessthan MoreExpr
            |    Expr  <br />
 Expr    =    Term MoreExpr
        --  |     Startbool Bexpr Endbool  <br />
 MoreExpr =   Add Expr
            |     Sub Expr
            |   E  <br />
 Term =       Factor MoreTerm  <br />
 MoreTerm =   Mul Term
            |     Div Term
            |    E  <br />
 Factor =     LPar Expr SRPar
            |     Identifier
            |     Ast.Num
            |     Constant
            |     Invert Ast.Num
            |   E  <br />
 Bfactor =   LPar Bexpr SRPar
            |   Constant
            |   Identifier
            |   Not Bfactor   <br />

 SRPar  =     RPar
 If = If                                                <br />
 Then = Then             <br />
 Else = Else             <br />
 Let = Let             <br />
 In = In             <br />
 Input = Input             <br />
 Identifier = Identifier String             <br />
 Num = Num Int             <br />
 Assign = Assign             <br />
 Write = Write             <br />
 Begin = Begin             <br />
 End = End             <br />
 Semicolon = Semicolon             <br />
 Add = Add             <br />
 Sub = Sub             <br />
 Mul = Mul             <br />
 Div = Div             <br />
 LPar = LPar             <br />
 RPar = RPar             <br />
 Invert = Invert             <br />
 Constant = Constant String             <br />
 Or = Or             <br />
 And = And             <br />
 Xor = Xor             <br />
 Implies = Implies             <br />
 Equalto = Equalto             <br />
 Not = Not             <br />
 Endif = Endif             <br />
 Negatenum = Negatenum             <br />
 Startbool = Starbool             <br />
 Endbool = Endbool             <br />
 Greaterthan = Greaterthan             <br />
 Lessthan = Lessthan             <br />
 EndLet = EndLet             <br />




usage :
use make command in terminal it will generate parse tree for for given program

program files in source folder to not accidently deleting file while cleaning


type make outside src folder
or you can do
./a2 Filenam


I implemented some additional implementation such as

list of statement must be inside { } block for statement out side block more than one statement will give syntax errors
statements inside {} are separated by semicolon

Boolean Expression are to be implemented in grammar





sampleinput output 

if i:= 0 then {j:=0;n :=23+65+65*43*43;}else {m := 2+3+4; Let j:=1 in {if 4+4*2then j:=234+544 else fi:=2; J:=23}
output

[IF "if",VAR "i",ASSIGNMENT ":=",INT "0",THEN "then",END "{",VAR "j",ASSIGNMENT ":=",INT "0",SEMICOLON ";",VAR "n",ASSIGNMENT 
":=",INT "23",PLUS "+",INT "65",PLUS "+",INT "65",TIMES "*",INT "43",TIMES "*",
INT "43",SEMICOLON ";",BEGIN "}",ELSE "else",END "{",VAR "m",ASSIGNMENT ":=",INT "2",PLUS "+",INT "3",PLUS "+",INT "4",SEMICOLON ";",VAR "Let",
VAR "j",ASSIGNMENT ":=",INT "1",IN "in",END "{",IF "if",INT "4",PLUS "+",INT "4",TIMES "*",INT "2",THEN "then",VAR "j",ASSIGNMENT ":=",
INT "234",PLUS "+",INT "544",ELSE "else",ENDIF "fi",ASSIGNMENT ":=",INT "2",SEMICOLON ";",VAR "J",ASSIGNMENT ":=",INT "23",BEGIN "}"]

STARTPROGRAM (
IF ( ( ID "i")) THEN (
a2: SyntaxErrorunexpected token 'ASSIGNMENT' (expected if, let, in, [identifier], or {) at 1:5
CallStack (from HasCallStack):
  error, called at ./Ast.hs:293:5 in main:Ast
make: *** [Makefile:7: src] Error 1










./a2 input.txt
output
[IF "if",INT "2",PLUS "+",INT "4",PLUS "+",INT "50",THEN "then",END "{",VAR "j",ASSIGNMENT ":=",INT "0",SEMICOLON ";",VAR "n",ASSIGNMENT ":=",
INT "23",PLUS "+",INT "65",PLUS "+",INT "65",TIMES "*",INT "43",TIMES "*",INT "43",SEMICOLON ";",BEGIN "}",ELSE "else",END "{",VAR "j",ASSIGNMENT ":=",
INT "343",TIMES "*",INT "532",TIMES "*",INT "43",PLUS "+",INT "23",PLUS "+",INT "54",SEMICOLON ";",BEGIN "}"]

STARTPROGRAM (
IF Binaryexp(ADDExp((2)+Binaryexp(ADDExp((4)+(50))) THEN (
LiST OF STATEMTNT[
(( ID "j") := (0));
(( ID "n") := Binaryexp(ADDExp((23)+Binaryexp(ADDExp((65)+Binaryexp(MULExp((65) * Binaryexp(MULExp((43) * (43))))));
] ENDLIST;
) ELSE (
LiST OF STATEMTNT[
(( ID "j") := Binaryexp(ADDExp(Binaryexp(MULExp((343) * Binaryexp(MULExp((532) * (43)))+Binaryexp(ADDExp((23)+(54))));
] ENDLIST;
) ENDIF;
) EOF
