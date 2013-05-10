#include "MiniSat-p_v1.14/Solver.h"

extern "C" Solver* minisat_newSolver() 
{
	return new Solver();
}

extern "C" void minisat_deleteSolver(Solver *solver)
{
	delete solver;
}

extern "C" Var minisat_newVar(Solver *solver)
{
	return solver->newVar();
}

extern "C" int minisat_nVars(Solver *solver)
{
	return solver->nVars();
}