## At the moment this only works on OS X

MINISAT_DIR = MiniSat-p_v1.14
CXX = g++
#CFLAGS = -ggdb -D DEBUG
#PROF_OPTS = -rtsopts -prof -auto-all
APP = smc

smc: clean minisat
	ghc --make -O $(PROF_OPTS) -o $(APP) Main.hs libminisat.dylib

minisat: clean-minisat
	cd $(MINISAT_DIR); make
	$(CXX) $(CFLAGS) -c minisat.c
	$(CXX) $(CFLAGS) -dynamiclib -o libminisat.dylib minisat.o \
	  $(MINISAT_DIR)/{Solver,Proof,File}.o

clean-minisat:
	cd $(MINISAT_DIR); make clean

clean:
	rm -f *.hi *.o *.dylib
	rm -f MiniSat_stub.h
	rm -f $(APP)
