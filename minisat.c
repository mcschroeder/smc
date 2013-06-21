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

extern "C" void minisat_addClause(Solver *solver, vec<Lit> *lits)
{
	solver->addClause(*lits);
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

typedef void RootCallback(const vec<Lit>& c);
typedef void ChainCallback(const ClauseId *cs, int cs_size, 
						   const Var      *xs, int xs_size);
typedef void DeletedCallback(ClauseId c);

struct Traverser : public ProofTraverser {	
	RootCallback *rootCallback;
	ChainCallback *chainCallback;
	DeletedCallback *deletedCallback;

	void root (const vec<Lit>& c) {
		rootCallback(c);
	}
	void chain (const vec<ClauseId>& cs, const vec<Var>& xs) {
		chainCallback((const ClauseId *)&*cs, cs.size(), 
					  (const Var      *)&*xs, xs.size());
	}
	void deleted(ClauseId c) {
		deletedCallback(c);
	}
};

extern "C" void minisat_setProofTraverser(Solver *solver, 
									      RootCallback *root,
									      ChainCallback *chain,
									      DeletedCallback *deleted)
{
	Traverser *t = new Traverser();
	t->rootCallback = root;
	t->chainCallback = chain;
	t->deletedCallback = deleted;
	solver->proof = new Proof(*t);
	// TODO: free traverser
}

//////////////////////////////////////////////////////////////////////////////

extern "C" vec<Lit> * minisat_newVecLit(void)
{
    return new vec<Lit>();
}

extern "C" void minisat_deleteVecLit(vec<Lit> *lits)
{
    delete lits;
}

extern "C" void minisat_vecLit_pushVar(vec<Lit> *lits, Var var, int sign)
{
	lits->push(sign ? ~Lit(var) : Lit(var));
}

extern "C" int minisat_vecLit_size(vec<Lit> *lits)
{
	return lits->size();
}

extern "C" const Lit * minisat_vecLit_data(vec<Lit> *lits)
{
	return *lits;
}

