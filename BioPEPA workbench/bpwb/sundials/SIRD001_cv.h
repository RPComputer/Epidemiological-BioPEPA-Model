/*
 *  SIRD001_cv.h
 *
 *  CVODE C prototype file for the functions defined in SIRD001_cv.c
 *
 *  This file was generated by the program VFGEN, version: 2.6.0.dev3
 *  Generated on 20-Sep-2022 at 19:09
 */

int SIRD001_vf(realtype, N_Vector, N_Vector, void *);
int SIRD001_jac(long int, DenseMat, realtype,
                N_Vector, N_Vector, void *,
                N_Vector, N_Vector, N_Vector);
