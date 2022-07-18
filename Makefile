default: src 

.PHONY: src
src:
	$(MAKE) -C src/
	mv src/a2 .
	./a2 input.txt
clean:
	$(MAKE) -C src/ clean
	-rm a2

