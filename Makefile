muri: src/*.hs
	ghc -isrc -outputdir build -O $(GHCFLAGS) -o $@ src/Main.hs

.PHONY: clean
clean:
	rm -rf build muri
