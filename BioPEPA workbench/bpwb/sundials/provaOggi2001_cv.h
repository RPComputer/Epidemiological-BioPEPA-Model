/*
 *  provaOggi2001_cv.h
 *
 *  CVODE C prototype file for the functions defined in provaOggi2001_cv.c
 *
 *  This file was generated by the program VFGEN, version: 2.6.0.dev3
 *  Generated on 13-Jul-2022 at 17:59
 */

int provaOggi2001_vf(realtype, N_Vector, N_Vector, void *);
int provaOggi2001_jac(long int, DenseMat, realtype,
                N_Vector, N_Vector, void *,
                N_Vector, N_Vector, N_Vector);
