#include <stdio.h>
#include <stdlib.h>
#include <math.h>

double *rtValues;
int first = 1;
int lines = 0;

int countlines(char *filename)
{
  // count the number of lines in the file called filename
  FILE *fp = fopen(filename, "r");
  int ch = 0;
  int lines = 0;

  if (fp == NULL)
    return 0;

  lines++;
  while (!feof(fp))
  {
    ch = fgetc(fp);
    if (ch == '\n')
    {
      lines++;
    }
  }
  fclose(fp);
  return lines;
}

double readRt(double t, const char *filename, double x)
{
  /*
    1 first execution: read the whole file in a vector of Rt values. The index of the vector is equal to the daytime of the value
    2 access vector at index t and get the element
  */
  if (first)
  {
    lines = countlines(filename);
    int n = 0;
    char str1[10], str2[15];
    double temp;
    rtValues = (double *)malloc(lines * sizeof(double));
    FILE *fp = fopen(filename, "r");

    if (fp == NULL)
      return 0;
    while (!feof(fp))
    {
      fscanf(fp, "%s,%s,%lf", str1, str2, temp);
      rtValues[n] = temp;
      n++;
    }
    fclose(fp);

    first = 0;
  }
  int p = round(t);
  if (p > lines)
    return 1.0;
  return rtValues[p];
}