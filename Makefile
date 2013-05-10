## 

MINISAT_DIR = MiniSat-p_v1.14
CXX = g++

minisat:
	cd $(MINISAT_DIR); make
	$(CXX) -c minisat.c
	$(CXX) -dynamiclib -o libminisat.dylib minisat.o \
	  $(MINISAT_DIR)/{Solver,Proof,File}.o

clean:
	cd $(MINISAT_DIR); make clean
	rm -f minisat.o libminisat.dylib