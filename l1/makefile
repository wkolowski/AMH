all:
	ghc -O2 Main.hs -hidir bin -odir bin -o bin/tabuSearch > compilation.txt 2>&1

clean:
	rm bin/*


run: all
	bin/tabuSearch
