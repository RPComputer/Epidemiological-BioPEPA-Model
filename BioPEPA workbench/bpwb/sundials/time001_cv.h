/*
 *  time001_cv.h
 *
 *  CVODE C prototype file for the functions defined in time001_cv.c
 *
 *  This file was generated by the program VFGEN, version: 2.6.0.dev3
 *  Generated on 15-Jul-2022 at 13:12
 */

int time001_vf(realtype, N_Vector, N_Vector, void *);
int time001_jac(long int, DenseMat, realtype,
                N_Vector, N_Vector, void *,
                N_Vector, N_Vector, N_Vector);
