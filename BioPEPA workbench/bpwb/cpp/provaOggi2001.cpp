// StochKit input generated by the Bio-PEPA Workbench 1.0 "Charlie Mingus"

#include "ProblemDefinition.h"
#include <iostream>
#include <stdlib.h>
#include <math.h>


/* Constants for reaction identifiers */
#define ___infect 0
#define ___recover 1
#define ___death 2
#define ___REACTIONS 3


/* Constants for species identifiers */
#define ___S   0
#define    S   ___discreteSpeciesCount(0)
#define ___I   1
#define    I   ___discreteSpeciesCount(1)
#define ___R   2
#define    R   ___discreteSpeciesCount(2)
#define ___D   3
#define    D   ___discreteSpeciesCount(3)
#define ___SPECIES 4

/* Reaction constants */
double
    k1 = 2,
    k2 = 0.4,
    k3 = 0.1;


Matrix Stoichiometry ()
{
  Matrix ___stoichiometry(___SPECIES, ___REACTIONS, 0.0);

  /* S = (infect, 1) << S */
  ___stoichiometry(___S, ___infect) = -1;

  /* I = (infect, 1) >> I + (recover, 1) << I + (death, 1) << I */
  ___stoichiometry(___I, ___infect) = +1;
  ___stoichiometry(___I, ___recover) = -1;
  ___stoichiometry(___I, ___death) = -1;

  /* R = (recover, 1) >> R */
  ___stoichiometry(___R, ___recover) = +1;

  /* D = (death, 1) >> D */
  ___stoichiometry(___D, ___death) = +1;

  return ___stoichiometry;
}

Vector Initialize ()
{

  /* Reaction constant initialisation */
  k1 = 2;
  k2 = 0.4;
  k3 = 0.1;

  Vector ___initialSpeciesCount(___SPECIES, 0.0);
  ___initialSpeciesCount(___S) = 10000;
  ___initialSpeciesCount(___I) = 5;
  ___initialSpeciesCount(___R) = 0;
  ___initialSpeciesCount(___D) = 0;
  return ___initialSpeciesCount;
}

#include "KineticFunctions.cpp"

Vector Propensity (const Vector& ___discreteSpeciesCount, double t)
{
  Vector ___propensity(___REACTIONS);

  /*      infect = [(k1/(S+I+R))*S*I] */
  ___propensity(___infect) = ((k1/(S+I+R))*S*I);

  /*      recover = [k2*I] */
  ___propensity(___recover) = (k2*I);

  /*      death = [k3*I] */
  ___propensity(___death) = (k3*I);
  return ___propensity;
}
