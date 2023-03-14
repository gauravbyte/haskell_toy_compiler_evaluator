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
