CC=ghc
LX=alex

default : clean mcc 

mcc: TokenLexer.hs  Ast.hs Main.hs OutputLexer.hs
	$(CC) Main.hs -o a2 
	-rm *.o *.hi 

TokenLexer.hs: TokenLexer.x
	$(LX) TokenLexer.x

OutputLexer.hs: OutputLexer.x
	${LX}  OutputLexer.x
Ast.hs:

clean: 
	-rm a2 TokenLexer.hs *.o *.hi
