SOURCES=charset.hs numeric-regex.hs 

all:
	ghc -o regen -O2 $(SOURCES)

prof:
	ghc -o regen -prof -fprof-auto -rtsopts $(SOURCES)

