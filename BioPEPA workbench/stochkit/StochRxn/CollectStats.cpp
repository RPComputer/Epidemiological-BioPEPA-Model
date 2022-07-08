//*****************************************************************************|
//*  FILE:    CollectStats.cpp
//*
//*  AUTHOR:  Andrew Hall
//*
//*  CREATED: February 19, 2003
//*
//*  LAST MODIFIED:  
//*             BY:  
//*             TO:  
//*
//*  SUMMARY:
//*
//*
//*  NOTES:
//*
//*
//*  TO DO:
//*
//*
//*****************************************************************************|
//       1         2         3         4         5         6         7         8
//345678901234567890123456789012345678901234567890123456789012345678901234567890
#include "CollectStats.h"
#include "StoreState.h"
#include <stdio.h>
#include <iostream>


namespace CSE {
  namespace StochRxn {


    EndPtStats CollectStats(unsigned int runs,
                            const Vector& x0,
                            double t0,
                            double tf,
                            const ReactionSet& reactions,
                            const SolverOptions& options)
    {
      // We'll use basically the options the user passed in,
      // but we want to make sure we only store the end state, not the
      // whole solution history.
      SolverOptions solvOpts(options);
      solvOpts.store_state_func = NoHistory_StoreState;

      EndPtStats stats(runs, x0); // Pre-allocate enough space
                                  // for all the runs we want to make

      SolutionHistory runResult;
      for (unsigned int i = 0; i < runs; ++i) {
        runResult = StochRxn(x0, t0, tf, reactions, solvOpts);
        stats[i] = runResult.back().State();
      }

      return stats;
    }


    void WriteStatFile(const EndPtStats& stats, const std::string& fileName)
    {
      FILE* outFile = fopen(fileName.c_str(), "w");
      
      const unsigned int vecLength = stats.front().Size();

      for (unsigned int i = 0; i < stats.size(); ++i) {
        const Vector& state = stats[i];
        // fprintf(outFile, "%d", (int)state(0));
        fprintf(outFile, "%f", state(0));
        for (unsigned int j = 1; j < vecLength; ++j) {
        //    fprintf(outFile, "\t%d", (int)state(j));
 	   fprintf(outFile, "\t%f", state(j));
        }
        fprintf(outFile, "\n");
      }
      fclose(outFile); 
    }


  } // Close CSE::StochRxn namespace
} // Close CSE namespace
