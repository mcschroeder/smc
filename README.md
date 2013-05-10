## How to use the MiniSat FFI with ghci

1) compile the original MiniSat libraries:

	cd MiniSat-p_v1.14
	make

2) compile the C wrapper

	g++ -c minisat.c

3) make a dylib for ghci use

	g++ -dynamiclib -o libminisat.dylib minisat.o MiniSat-p_v1.14/{Solver,Proof,File}.o

4) invoke ghci

	ghci -lminisat