# 
# Rules for compiling and linking the typechecker/evaluator
#
# Type
#   make         to rebuild the executable files
#   make clean   to remove all intermediate and temporary files
#   

# Files that need to be generated from other files
DEPEND += Lexer.hs Grammar.hs Typing.hs Evaluator.hs

# When "make" is invoked with no arguments, we build an executable 
#  after building everything that it depends on
all: $(DEPEND) a3term a3

# Build an executable for interpreter
a3 : $(DEPEND)
	ghc a3.hs

# Build an executable for interactive mode
a3term: $(DEPEND) a3term.hs
	ghc a3term.hs

# Generate ML files from a parser definition file
Grammar.hs : Grammar.y
	@rm -f Grammar.hs
	happy Grammar.y
	@chmod -w Grammar.hs

# Generate ML files from a lexer definition file
Lexer.hs : Lexer.x
	@rm -f Lexer.hs
	alex Lexer.x
	@chmod -w Lexer.hs

# Clean up the directory
clean::
	rm -rf Lexer.hs Grammar.hs *.hi *.o *.info


