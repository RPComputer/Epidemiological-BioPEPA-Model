/*
 *  SIRDV001_cv.h
 *
 *  CVODE C prototype file for the functions defined in SIRDV001_cv.c
 *
 *  This file was generated by the program VFGEN, version: 2.6.0.dev3
 *  Generated on 21-Sep-2022 at 01:40
 */

int SIRDV001_vf(realtype, N_Vector, N_Vector, void *);
int SIRDV001_jac(long int, DenseMat, realtype,
                N_Vector, N_Vector, void *,
                N_Vector, N_Vector, N_Vector);
