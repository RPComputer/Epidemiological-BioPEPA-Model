// StochKit input generated by the Bio-PEPA Workbench 0.3 "Jimmy Page"

#include "ProblemDefinition.h"


/* Constants for reaction identifiers */
#define ___r1 0
#define ___rm1 1
#define ___r2 2
#define ___REACTIONS 3

/* Constants for reaction definitions */
/*      r1 = [k1 * E * S] */
#define r1   (k1 * E * S)
/*      rm1 = [km1 * E:S] */
#define rm1   (km1 * E_colon_S)
/*      r2 = [k2 * E:S] */
#define r2   (k2 * E_colon_S)

/* Constants for species identifiers */
#define ___E   0
#define    E   ___discreteSpeciesCount(0)
#define ___S   1
#define    S   ___discreteSpeciesCount(1)
#define ___E_colon_S   2
#define    E_colon_S   ___discreteSpeciesCount(2)
#define ___P   3
#define    P   ___discreteSpeciesCount(3)
#define ___SPECIES 4

/* Reaction constants */
double
    k1 = 1,
    km1 = 0.1,
    k2 = 0.01,
    TimeFinal = 300.0;


Matrix Stoichiometry ()
{
  Matrix ___stoichiometry(___SPECIES, ___REACTIONS, 0.0);

  /* E = (r1, 1) << E + (rm1, 1) >> E + (r2, 1) >> E */
  ___stoichiometry(___E, ___r1) = -1;
  ___stoichiometry(___E, ___rm1) = +1;
  ___stoichiometry(___E, ___r2) = +1;

  /* S = (r1, 1) << S + (rm1, 1) >> S */
  ___stoichiometry(___S, ___r1) = -1;
  ___stoichiometry(___S, ___rm1) = +1;

  /* E:S = (r1, 1) >> E:S + (rm1, 1) << E:S + (r2, 1) << E:S */
  ___stoichiometry(___E_colon_S, ___r1) = +1;
  ___stoichiometry(___E_colon_S, ___rm1) = -1;
  ___stoichiometry(___E_colon_S, ___r2) = -1;

  /* P = (r2, 1) >> P */
  ___stoichiometry(___P, ___r2) = +1;

  return ___stoichiometry;
}

Vector Initialize ()
{
  Vector ___initialSpeciesCount(___SPECIES, 0.0);
  ___initialSpeciesCount(___E) = 100;
  ___initialSpeciesCount(___S) = 100;
  ___initialSpeciesCount(___E_colon_S) = 0;
  ___initialSpeciesCount(___P) = 0;
  return ___initialSpeciesCount;
}

Vector Propensity (const Vector& ___discreteSpeciesCount)
{
  Vector ___propensity(___REACTIONS);
  ___propensity(___r1) = r1;
  ___propensity(___rm1) = rm1;
  ___propensity(___r2) = r2;
  return ___propensity;
}
