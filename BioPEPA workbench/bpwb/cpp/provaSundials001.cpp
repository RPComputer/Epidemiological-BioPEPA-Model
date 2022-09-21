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
    placeholder = 1,
    gamma1 = 0.196,
    alpha = 0.004;


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
  placeholder = 1;
  gamma1 = 0.196;
  alpha = 0.004;

  Vector ___initialSpeciesCount(___SPECIES, 0.0);
  ___initialSpeciesCount(___S) = 60000000;
  ___initialSpeciesCount(___I) = 5;
  ___initialSpeciesCount(___R) = 0;
  ___initialSpeciesCount(___D) = 0;
  return ___initialSpeciesCount;
}

#include "KineticFunctions.cpp"

Vector Propensity (const Vector& ___discreteSpeciesCount, double t)
{
  Vector ___propensity(___REACTIONS);

  /*      infect = [(I*(S+R)*(alpha+gamma1)*placeholder)/(S+I+R)] */
  ___propensity(___infect) = ((I*(S+R)*(alpha+gamma1)*placeholder)/(S+I+R));

  /*      recover = [gamma1*I] */
  ___propensity(___recover) = (gamma1*I);

  /*      death = [alpha*I] */
  ___propensity(___death) = (alpha*I);
  return ___propensity;
}
