all:
	ghc -O2 numeric-regex.hs 

prof:
	ghc -prof -fprof-auto -rtsopts numeric-regex.hs 

