/*
 *  example2_sird001_cv.h
 *
 *  CVODE C prototype file for the functions defined in example2_sird001_cv.c
 *
 *  This file was generated by the program VFGEN, version: 2.6.0.dev3
 *  Generated on 28-Sep-2022 at 18:47
 */

int example2_sird001_vf(realtype, N_Vector, N_Vector, void *);
int example2_sird001_jac(long int, DenseMat, realtype,
                N_Vector, N_Vector, void *,
                N_Vector, N_Vector, N_Vector);