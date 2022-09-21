#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <string.h>

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

double readRt(double t, const char *filename)
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
    fgets(buffer, 100, fp); // skip header line
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
#define maxClassNameLenght 30
double **datatableValues;
char **datatableHeader;
int datatableFirst = 1;
int datatableLines = 0;
int classNumber = 0;

void removeChar(char *str, char charToRemmove)
{
  int i, j;
  int len = strlen(str);
  for (i = 0; i < len; i++)
  {
    if (str[i] == charToRemmove)
    {
      for (j = i; j < len; j++)
      {
        str[j] = str[j + 1];
      }
      len--;
      i--;
    }
  }
}

int countNumber(const char *filename)
{
  // count the number of columns in the file called filename
  FILE *fp = fopen(filename, "r");
  int number = 0;
  char str[1000];
  if (fp == NULL)
    return 0;
  fgets(str, 1000, fp);
  char *token = strtok(str, ",");
  while (token)
  {
    token = strtok(NULL, ",");
    number++;
  }
  fclose(fp);
  return number;
}

int findColumn(const char *calssName)
{
  for (int i = 0; i < classNumber; i++)
  {
    if (strcmp(calssName, datatableHeader[i]) == 0)
      return i;
  }
  return 0;
}

double readDatatable(double t, const char *filename, const char *calssName)
{
  /*
    1 first execution: read the whole file in a vector of Rt values. The index of the vector is equal to the daytime of the value
    2 access vector at index t and get the element
  */
  if (datatableFirst)
  {
    // initialization of metadata variables
    char buffer[1000];
    datatableLines = countlines(filename);
    classNumber = countNumber(filename);
    datatableHeader = (char **)malloc(classNumber * sizeof(char *));
    for (int i = 0; i < classNumber; i++)
    {
      datatableHeader[i] = (char *)malloc(maxClassNameLenght * sizeof(char));
    }
    datatableValues = (double **)malloc(datatableLines * sizeof(double *));
    // read the file
    FILE *fp = fopen(filename, "r");
    int j;
    double value;
    if (fp == NULL)
      return 0;
    fgets(buffer, 500, fp); // save header line
    char headerbuffer[maxClassNameLenght];
    char *token = strtok(buffer, ",");
    int h = 0;
    while (token)
    {
      strcpy(headerbuffer, token);
      removeChar(headerbuffer, '"');
      removeChar(headerbuffer, '\n');
      strcpy(datatableHeader[h], headerbuffer);
      token = strtok(NULL, ",");
      h++;
    }
    for (int i = 0; i < datatableLines; i++) // for each line parse the values and store in allocated array
    {
      datatableValues[i] = (double *)malloc(classNumber * sizeof(double));
      fgets(buffer, 1000, fp);
      char *token = strtok(buffer, ",");
      j = 0;
      while (token && j < classNumber)
      {
        value = atof(token);
        datatableValues[i][j] = value;
        token = strtok(NULL, ",");
        j++;
      }
    }
    fclose(fp);
    datatableFirst = 0;
  }
  int p = round(t);
  if (p > datatableLines)
    return 0;
  int c = findColumn(calssName);
  return datatableValues[p][c];
}