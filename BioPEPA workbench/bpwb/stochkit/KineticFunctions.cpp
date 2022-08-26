/* A kinetic functions file used by the Bio-PEPA Workbench */


double fMA(double rate, double Species1, double Species2)
{
  return rate * Species1 * Species2;
}

double fMA_TODO_FIXME(double rate)
{
  return rate;
}

double fMM(double v_M, double K_M, double Enzyme, double Substrate)
{
  return (v_M * Enzyme * Substrate) / (K_M + Substrate);
}

double fH(double v, double K, double n, double Species)
{
  return (v * pow(Species, n)) / (K + pow(Species, n));
}

double min(double x, double y)
{
  if (x < y)
    return x;
  else
    return y;
}

double max(double x, double y)
{
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
  if (pArg > 0.0)
  {
    retVal = 1.0;
  }
  return (retVal);
}


/* Add your own functions here.  Have fun. */
#include <string>
#include <vector>
#include <cmath>
#include <sstream>
#include <iostream>
#include <fstream>
#include "ProblemDefinition.h"

double testTime(double t)
{
  if (t < 10)
    return 10;
  return 0.1;
}

std::vector<double> RtValues;

double readRt(double t, const char *filename, double x)
{
  /*
    1 first execution: read the whole file in a vector of Rt values. The index of the vector is equal to the daytime of the value
    2 access vector at index t and get the element
  */
  if (RtValues.empty())
  {
    std::string myText;
    // Read from the text file
    std::ifstream MyReadFile(filename);
    std::getline(MyReadFile, myText); //ignore header line
    double readvalue;
    // Use a while loop together with the getline() function to read the file line by line
    while (std::getline(MyReadFile, myText))
    {
      std::vector<std::string> line;
      std::stringstream sstream(myText); //create string stream from the string
      while(sstream.good()) {
          std::string substr;
          std::getline(sstream, substr, ','); //get first string delimited by comma
          line.push_back(substr);
      }
      readvalue = atof(line.at(2).c_str());
      RtValues.push_back(readvalue);
      line.clear();
    }

    // Close the file
    MyReadFile.close();
  }
  double result;
  result = RtValues.at(round(t));
  return result;
}