all:
	ghc -O2 Main.hs -hidir bin -odir bin -o bin/tabuSearch > /dev/null 2>&1

clean:
	rm bin/*


run: all
	bin/tabuSearch
