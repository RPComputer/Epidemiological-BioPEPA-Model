#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>

double *rtValues;
int first = 1;
int lines = 0;

int countlines(const char *filename)
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
  char buffer[100];
  if (first)
  {
    lines = countlines(filename);
    int n = 0;
    char str1[15], str2[20];
    double temp;
    int check;
    rtValues = (double *)malloc(lines * sizeof(double));
    FILE *fp = fopen(filename, "r");

    if (fp == NULL)
      return 0;
    fgets(buffer, 100, fp); //skip header line
    while (!feof(fp))
    {
      check = fscanf(fp, "%[^,],%[^,],%lf", str1, str2, &temp);
      assert(check == 3 || check == 1);
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

/*-------------------------------------*/

double *datatableValues;
char **datatableHeader;
int datatableFirst = 1;
int datatableLines = 0;

double readDatatable(double t, const char *filename, const char *calssName)
{
  /*
    1 first execution: read the whole file in a vector of Rt values. The index of the vector is equal to the daytime of the value
    2 access vector at index t and get the element
  */
  char buffer[500];
  if (datatableFirst)
  {
    datatableLines = countlines(filename);
    int n = 0;
    char str1[15], str2[20];
    double temp;
    int check;
    rtValues = (double *)malloc(lines * sizeof(double));
    FILE *fp = fopen(filename, "r");

    if (fp == NULL)
      return 0;
    fgets(buffer, 500, fp); //skip header line
    while (!feof(fp))
    {
      check = fscanf(fp, "%[^,],%[^,],%lf", str1, str2, &temp);
      assert(check == 3 || check == 1);
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