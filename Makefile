GHC =	ghc

.PHONY: all clean

all: bfc

bfc: bfc.hs
	${GHC} bfc.hs

clean:
	rm -f bfc *.hi *.o
