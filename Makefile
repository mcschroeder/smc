## At the moment this only works on OS X

MINISAT_DIR = MiniSat-p_v1.14
CXX = g++
CFLAGS = -ggdb -D DEBUG
PROF_OPTS = -rtsopts -prof -auto-all

Main: minisat
	ghc --make -O $(PROF_OPTS) Main.hs libminisat.dylib

minisat: clean
	cd $(MINISAT_DIR); make
	$(CXX) $(CFLAGS) -c minisat.c
	$(CXX) $(CFLAGS) -dynamiclib -o libminisat.dylib minisat.o \
	  $(MINISAT_DIR)/{Solver,Proof,File}.o

clean:
	cd $(MINISAT_DIR); make clean
	rm -f minisat.o libminisat.dylib
	rm -f Main Main.hi Minisat.hi Main.o