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

extern "C" void minisat_addClause(Solver *solver, vec<Lit>* lits)
{
	solver->addClause(*lits);
}

extern "C" void minisat_solve(Solver *solver)
{
	solver->solve();
}

extern "C" int minisat_okay(Solver* solver)
{
    return solver->okay();
}

//////////////////////////////////////////////////////////////////////////////

extern "C" vec<Lit>* minisat_newVecLit(void)
{
    return new vec<Lit>();
}

extern "C" void minisat_deleteVecLit(vec<Lit>* lits)
{
    delete lits;
}

extern "C" void minisat_vecLit_pushVar(vec<Lit>* lits, int var, int sign)
{
	Lit p = toLit(var);
	lits->push(sign ? ~p : p);
}
