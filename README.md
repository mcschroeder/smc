**Programming assignment for the course 184.747 Software Model Checking at the Vienna University of Technology.**

This is an interpolation-based sequential model checker for finite-state models encoded in the [AIGER] format. You have a choice of three interpolation systems: McMillan, Inverse McMillan and Symmetric (aka HKP).

For SAT-solving we use [MiniSat] 1.14 with proof-logging.

[AIGER]: http://fmv.jku.at/aiger/

[MiniSat]: http://minisat.se/MiniSat.html

## Install

*Note: At the moment, the Makefile works only on OS X.*

I couldn't get the MiniSat C++ FFI stuff to reliably link with cabal, so you'll need to manually ensure you have all the dependencies before you run `make`. You will need GHC 7.6 or higher.

	cabal update
	cabal install containers parsec transformers vector
	make

## Usage

	smc <model.aag> [ McMillan | Symmetric | InverseMcMillan ]

## How to use the MiniSat FFI with ghci

If you want to play around with this in ghci, you have to first build the MiniSat FFI library and then load it when starting ghci:

	make minisat
	ghci -lminisat
