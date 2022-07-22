/*
 *  bratu_cv.c
 *
 *  CVODE C file for the vector field named: bratu
 *
 *  This file was generated by the program VFGEN, version: 2.6.0.dev3
 *  Generated on 20-Jul-2022 at 17:44
 */

#include <math.h>

/* Include headers for CVODE 2.5.0 */
#include <sundials/sundials_types.h>
#include <sundials/sundials_dense.h>
#include <sundials/sundials_nvector.h>
#include <nvector/nvector_serial.h>
#include <cvode/cvode.h>
#include <cvode/cvode_dense.h>

/*
 *  The vector field.
 */

int bratu_vf(realtype t, N_Vector y_, N_Vector f_, void *params)
{
    realtype x, y;
    realtype a;
    realtype *p_;

    p_ = (realtype *) params;

    x          = NV_Ith_S(y_,0);
    y          = NV_Ith_S(y_,1);

    a          = p_[0];

    NV_Ith_S(f_,0) =  a*exp(x)+y+-2.0*x;
    NV_Ith_S(f_,1) =  -2.0*y+exp(y)*a+x;
    return 0;
}

/*
 *  The Jacobian.
 */

int bratu_jac(long int N_, DenseMat jac_, realtype t,
                N_Vector y_, N_Vector fy_, void *params,
                N_Vector tmp1, N_Vector tmp2, N_Vector tmp3)
{
    realtype x, y;
    realtype a;
    realtype *p_;

    p_ = (realtype *) params;

    x          = NV_Ith_S(y_,0);
    y          = NV_Ith_S(y_,1);

    a          = p_[0];

    DENSE_ELEM(jac_, 0, 0) =  a*exp(x)-2.0;
    DENSE_ELEM(jac_, 0, 1) = 1.0;
    DENSE_ELEM(jac_, 1, 0) = 1.0;
    DENSE_ELEM(jac_, 1, 1) =  exp(y)*a-2.0;
    return 0;
}

/*
 *  User-defined functions. 
 */

int bratu_func(realtype t, N_Vector y_, realtype *func_, void *params)
{
    realtype x, y;
    realtype a;
    realtype *p_;

    p_ = (realtype *) params;

    x          = NV_Ith_S(y_,0);
    y          = NV_Ith_S(y_,1);

    a          = p_[0];

    /* userf1:  */
    func_[0] =  a-2.0000000000000001e-01;
    return 0;
}