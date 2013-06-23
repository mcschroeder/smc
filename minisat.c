#include "MiniSat-p_v1.14/Solver.h"

extern "C" Solver * minisat_newSolver() 
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

extern "C" void minisat_addClause(Solver *solver, Lit *ls, int n)
{
	vec<Lit> v(ls, n);
	solver->addClause(v);
	v.release();
}

extern "C" void minisat_addUnit(Var v1, int s1, Solver *solver)
{
	solver->addUnit(s1 ? ~Lit(v1) : Lit(v1));
}

extern "C" void minisat_addBinary(Var v1, int s1, 
								  Var v2, int s2, 
								  Solver *solver)
{
	solver->addBinary(s1 ? ~Lit(v1) : Lit(v1),
					  s2 ? ~Lit(v2) : Lit(v2));
}

extern "C" void minisat_addTernary(Var v1, int s1, 
								   Var v2, int s2, 
								   Var v3, int s3, 
								   Solver *solver)
{
	solver->addTernary(s1 ? ~Lit(v1) : Lit(v1),
					   s2 ? ~Lit(v2) : Lit(v2),
					   s3 ? ~Lit(v3) : Lit(v3));
}

extern "C" void minisat_solve(Solver *solver)
{
	solver->solve();
}

extern "C" int minisat_okay(Solver *solver)
{
    return solver->okay();
}

//////////////////////////////////////////////////////////////////////////////

typedef void RootCallback(const Lit *c, int c_size);
typedef void ChainCallback(const ClauseId *cs, int cs_size, 
						   const Var      *xs, int xs_size);
typedef void DeletedCallback(ClauseId c);

struct Traverser : public ProofTraverser {	
	RootCallback *rootCallback;
	ChainCallback *chainCallback;
	DeletedCallback *deletedCallback;

	void root (const vec<Lit>& c) {
		rootCallback((const Lit *)&*c, c.size());
	}
	void chain (const vec<ClauseId>& cs, const vec<Var>& xs) {
		chainCallback((const ClauseId *)&*cs, cs.size(), 
					  (const Var      *)&*xs, xs.size());
	}
	void deleted(ClauseId c) {
		deletedCallback(c);
	}
};

extern "C" Traverser * minisat_newProof(RootCallback *root,
								        ChainCallback *chain,
									    DeletedCallback *deleted,
									    Solver *solver)
{
	Traverser *t = new Traverser();
	t->rootCallback = root;
	t->chainCallback = chain;
	t->deletedCallback = deleted;
	solver->proof = new Proof(*t);
	return t;
}

extern "C" void minisat_deleteProof(Traverser *traverser, Solver *solver)
{
	delete traverser;
	delete solver->proof;
	solver->proof = NULL;
}
