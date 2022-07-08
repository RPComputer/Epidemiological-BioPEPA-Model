/* A kinetic functions file used by the Bio-PEPA Workbench */

double fMA(double rate, double Species1, double Species2) {
  return rate * Species1 * Species2;
}

double fMA_TODO_FIXME(double rate) {
  return rate;
}

double fMM(double v_M, double K_M, double Enzyme, double Substrate) {
  return (v_M * Enzyme * Substrate) / (K_M + Substrate);
}

double fH(double v, double K, double n, double Species) {
  return (v * pow(Species, n)) / (K + pow(Species, n));
}

double min(double x, double y) {
  if (x < y) 
    return x;
  else
    return y;
}

double max(double x, double y) {
  if (x > y)
    return x;
  else
    return y;
}

/**
 * Returns 0 if the argument is negative, and 1 if the
 * argument is nonnegative.
 */
double theta(double pArg)
{
  double retVal = 0.0;
  if(pArg > 0.0)
    {
      retVal = 1.0;
    }
  return(retVal);
}


/* Add your own functions here.  Have fun. */
