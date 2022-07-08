//******************************************************************************
//*  FILE:    stochkit.cpp
//*
//*  AUTHOR: HongLi
//*
//*  CREATED: Mar 06, 2005
//*
//*  LAST MODIFIED: Feb 03, 2008
//*             BY: Stephen Gilmore
//*             TO: import BioPEPA parameters
//*  SUMMARY:
//*
//*
//*  NOTES:
//*
//*
//*
//*  TO DO:
//*
//*
//****************************************************************************|
//        1         2         3         4         5         6         7         8
//2345678901234567890123456789012345678901234567890123456789012345678901234567890

#include "ProblemDefinition.h"
#include "SolverOptions.h"
#include "CollectStats.h"
#include "Vector.h"
#include "Matrix.h"
#include "Random.h"
#include "Solver.h"
#include "biopepa.h"
#include <stdlib.h>
#include <iostream>
#include <sstream>
#include <fstream>
#include <string>

using namespace CSE::Math;
using namespace CSE::StochRxn;
using namespace std;

Vector Initialize();
Matrix Stoichiometry();
Vector Propensity(const Vector& x);


 
inline string stringify(int x)
{
  ostringstream o;
  if (!(o << x))
    cerr << "stringify(int)";
  return o.str();
}


int main(int argc, const char* argv[])
{
  try {
    
    // Parse the command line:
    const char *outFile;

    if (argc != 2) {
	    cerr << "Usage:  stochkit <output file>";
	    exit(EXIT_FAILURE);
    }
    else {
         outFile = argv[1];
    }

      time_t curTime = time(0);
      CSE::Math::Seeder(static_cast<unsigned int>(curTime), curTime);

    // Set up the problem:
    Vector X0 = Initialize();
    Matrix nu = Stoichiometry();
    ReactionSet rxns(nu, Propensity);

    // Configure solver
    SolverOptions opt = ConfigStochRxn(0);

    opt.stepsize_selector_func =  SSADirect_Stepsize;
    opt.single_step_func = SSA_SingleStep;
    opt.progress_interval = ProgressInterval; /* from biopepa.h */
    opt.initial_stepsize = 0.2;
     
    // Get final time from biopepa.h
    // double TimeFinal = 300.0;

    // Make all the runs:
    int i;
    string outputName(string(outFile) + "_stochkit_results_");

    //    ofstream endTimes(string(outFile) + "_stochkit_endtimes.dat");

    for (i = 1 ; i <= Iterations ; i++) {
	SolutionHistory sln = StochRxn(X0, 0, TimeFinal, rxns, opt);
	WriteHistoryFile(sln, outputName + stringify(i) + ".dat");

	cerr << "Run " << i << " ended at " << (sln.at(sln.size()-1).Time()) << "\n";

	if (i == 1) {
	  if (i % Report ==0) 
	    cout << "Completed " << i << " iteration of " << outFile << ".\n";
	} else {
	  if (i % Report ==0) 
	    cout << "Completed " << i << " iterations of " << outFile << ".\n";
	}
    }
    cout << "Completed all iterations of " << outFile << ".\n";
    //    endTimes.close();
  }
  catch (const exception& ex){ 
    cerr << "\nCaught " << ex.what() << '\n';
  }

  return 0;
}