/*
 *  time001_cv.c
 *
 *  CVODE C file for the vector field named: time001
 *
 *  This file was generated by the program VFGEN, version: 2.6.0.dev3
 *  Generated on 15-Jul-2022 at 13:12
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

int time001_vf(realtype t, N_Vector y_, N_Vector f_, void *params)
{
    realtype S_;
    realtype k1_;
    realtype contact1_;
    realtype *p_;

    p_ = (realtype *) params;

    S_         = NV_Ith_S(y_,0);

    k1_        = p_[0];

    contact1_ = ( t+1.0)*k1_*S_;

    NV_Ith_S(f_,0) = -contact1_;
    return 0;
}

/*
 *  The Jacobian.
 */

int time001_jac(long int N_, DenseMat jac_, realtype t,
                N_Vector y_, N_Vector fy_, void *params,
                N_Vector tmp1, N_Vector tmp2, N_Vector tmp3)
{
    realtype S_;
    realtype k1_;
    realtype *p_;

    p_ = (realtype *) params;

    S_         = NV_Ith_S(y_,0);

    k1_        = p_[0];

    DENSE_ELEM(jac_, 0, 0) = -( t+1.0)*k1_;
    return 0;
}
