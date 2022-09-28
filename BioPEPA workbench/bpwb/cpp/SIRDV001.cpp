// StochKit input generated by the Bio-PEPA Workbench 1.0 "Charlie Mingus"

#include "ProblemDefinition.h"
#include <iostream>
#include <stdlib.h>
#include <math.h>


/* Constants for reaction identifiers */
#define ___infect 0
#define ___infectcount 1
#define ___recover 2
#define ___death 3
#define ___vaccinateS 4
#define ___vaccinateR 5
#define ___REACTIONS 6


/* Constants for species identifiers */
#define ___S   0
#define    S   ___discreteSpeciesCount(0)
#define ___I   1
#define    I   ___discreteSpeciesCount(1)
#define ___R   2
#define    R   ___discreteSpeciesCount(2)
#define ___D   3
#define    D   ___discreteSpeciesCount(3)
#define ___V   4
#define    V   ___discreteSpeciesCount(4)
#define ___CUMI   5
#define    CUMI   ___discreteSpeciesCount(5)
#define ___SPECIES 6

/* Reaction constants */
double
    placeholder = 1,
    vaccinerate = 1,
    gamma1 = 0.196,
    alpha = 0.004;


Matrix Stoichiometry ()
{
  Matrix ___stoichiometry(___SPECIES, ___REACTIONS, 0.0);

  /* S = (infect, 1) << S + (vaccinateS, 1) << S */
  ___stoichiometry(___S, ___infect) = -1;
  ___stoichiometry(___S, ___vaccinateS) = -1;

  /* I = (infect, 1) >> I + (recover, 1) << I + (death, 1) << I */
  ___stoichiometry(___I, ___infect) = +1;
  ___stoichiometry(___I, ___recover) = -1;
  ___stoichiometry(___I, ___death) = -1;

  /* R = (recover, 1) >> R + (vaccinateR, 1) << R */
  ___stoichiometry(___R, ___recover) = +1;
  ___stoichiometry(___R, ___vaccinateR) = -1;

  /* D = (death, 1) >> D */
  ___stoichiometry(___D, ___death) = +1;

  /* V = (vaccinateS, 1) >> V + (vaccinateR, 1) >> V */
  ___stoichiometry(___V, ___vaccinateS) = +1;
  ___stoichiometry(___V, ___vaccinateR) = +1;

  /* CUMI = (infectcount, 1) >> CUMI */
  ___stoichiometry(___CUMI, ___infectcount) = +1;

  return ___stoichiometry;
}

Vector Initialize ()
{

  /* Reaction constant initialisation */
  placeholder = 1;
  vaccinerate = 1;
  gamma1 = 0.196;
  alpha = 0.004;

  Vector ___initialSpeciesCount(___SPECIES, 0.0);
  ___initialSpeciesCount(___S) = 59641488;
  ___initialSpeciesCount(___I) = 2;
  ___initialSpeciesCount(___R) = 0;
  ___initialSpeciesCount(___D) = 0;
  ___initialSpeciesCount(___V) = 0;
  ___initialSpeciesCount(___CUMI) = 2;
  return ___initialSpeciesCount;
}

#include "KineticFunctions.cpp"

Vector Propensity (const Vector& ___discreteSpeciesCount, double t)
{
  Vector ___propensity(___REACTIONS);

  /*      infect = [(I*(S+R+V)*(alpha+gamma1)*placeholder)/(S+I+R+V)] */
  ___propensity(___infect) = ((I*(S+R+V)*(alpha+gamma1)*placeholder)/(S+I+R+V));

  /*      infectcount = [(I*(S+R+V)*(alpha+gamma1)*placeholder)/(S+I+R+V)] */
  ___propensity(___infectcount) = ((I*(S+R+V)*(alpha+gamma1)*placeholder)/(S+I+R+V));

  /*      recover = [gamma1*I] */
  ___propensity(___recover) = (gamma1*I);

  /*      death = [alpha*I] */
  ___propensity(___death) = (alpha*I);

  /*      vaccinateS = [S*vaccinerate/(S+R)] */
  ___propensity(___vaccinateS) = (S*vaccinerate/(S+R));

  /*      vaccinateR = [R*vaccinerate/(S+R)] */
  ___propensity(___vaccinateR) = (R*vaccinerate/(S+R));
  return ___propensity;
}
