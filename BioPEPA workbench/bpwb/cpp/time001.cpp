// StochKit input generated by the Bio-PEPA Workbench 1.0 "Charlie Mingus"

#include "ProblemDefinition.h"
#include <iostream>
#include <stdlib.h>
#include <math.h>


/* Constants for reaction identifiers */
#define ___contact1 0
#define ___REACTIONS 1


/* Constants for species identifiers */
#define ___S   0
#define    S   ___discreteSpeciesCount(0)
#define ___SPECIES 1

/* Reaction constants */
double
    k1 = 0.035;


Matrix Stoichiometry ()
{
  Matrix ___stoichiometry(___SPECIES, ___REACTIONS, 0.0);

  /* S = (contact1, 1) << S */
  ___stoichiometry(___S, ___contact1) = -1;

  return ___stoichiometry;
}

Vector Initialize ()
{

  /* Reaction constant initialisation */
  k1 = 0.035;

  Vector ___initialSpeciesCount(___SPECIES, 0.0);
  ___initialSpeciesCount(___S) = 10000;
  return ___initialSpeciesCount;
}

#include "KineticFunctions.cpp"

Vector Propensity (const Vector& ___discreteSpeciesCount, double t)
{
  Vector ___propensity(___REACTIONS);

  /*      contact1 = [k1*(delay())*S] */
  ___propensity(___contact1) = (k1*(delay())*S);
  return ___propensity;
}
