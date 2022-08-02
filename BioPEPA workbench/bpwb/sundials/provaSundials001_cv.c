/*
 *  provaSundials001_cv.c
 *
 *  CVODE C file for the vector field named: provaSundials001
 *
 *  This file was generated by the program VFGEN, version: 2.6.0.dev3
 *  Generated on  2-Aug-2022 at 17:16
 */

#include "CustomFunctions.c"
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

int provaSundials001_vf(realtype t, N_Vector y_, N_Vector f_, void *params)
{
    realtype S_, I_, R_, D_;
    realtype placeholder_, delta_, gamma1_;
    realtype infect_, recover_, death_;
    realtype *p_;

    p_ = (realtype *) params;

    S_         = NV_Ith_S(y_,0);
    I_         = NV_Ith_S(y_,1);
    R_         = NV_Ith_S(y_,2);
    D_         = NV_Ith_S(y_,3);

    placeholder_ = p_[0];
    delta_     = p_[1];
    gamma1_    = p_[2];

    infect_ = readRt(t, "Rt.csv", I_)/( R_+S_+I_)*S_*delta_*I_;
    recover_ = delta_*I_;
    death_ = gamma1_*I_;

    NV_Ith_S(f_,0) = -infect_;
    NV_Ith_S(f_,1) = -recover_-death_+infect_;
    NV_Ith_S(f_,2) = recover_;
    NV_Ith_S(f_,3) = death_;
    return 0;
}

/*
 *  The Jacobian.
 */

int provaSundials001_jac(long int N_, DenseMat jac_, realtype t,
                N_Vector y_, N_Vector fy_, void *params,
                N_Vector tmp1, N_Vector tmp2, N_Vector tmp3)
{
    realtype S_, I_, R_, D_;
    realtype placeholder_, delta_, gamma1_;
    realtype *p_;

    p_ = (realtype *) params;

    S_         = NV_Ith_S(y_,0);
    I_         = NV_Ith_S(y_,1);
    R_         = NV_Ith_S(y_,2);
    D_         = NV_Ith_S(y_,3);

    placeholder_ = p_[0];
    delta_     = p_[1];
    gamma1_    = p_[2];

    DENSE_ELEM(jac_, 0, 0) = -readRt(t, "Rt.csv", I_)/( R_+S_+I_)*delta_*I_+readRt(t, "Rt.csv", I_)/pow( R_+S_+I_,2.0)*S_*delta_*I_;
    DENSE_ELEM(jac_, 0, 1) = -readRt(t, "Rt.csv", I_)/( R_+S_+I_)*S_*delta_+readRt(t, "Rt.csv", I_)/pow( R_+S_+I_,2.0)*S_*delta_*I_;
    DENSE_ELEM(jac_, 0, 2) = readRt(t, "Rt.csv", I_)/pow( R_+S_+I_,2.0)*S_*delta_*I_;
    DENSE_ELEM(jac_, 1, 0) =  readRt(t, "Rt.csv", I_)/( R_+S_+I_)*delta_*I_-readRt(t, "Rt.csv", I_)/pow( R_+S_+I_,2.0)*S_*delta_*I_;
    DENSE_ELEM(jac_, 1, 1) = -gamma1_+readRt(t, "Rt.csv", I_)/( R_+S_+I_)*S_*delta_-delta_-readRt(t, "Rt.csv", I_)/pow( R_+S_+I_,2.0)*S_*delta_*I_;
    DENSE_ELEM(jac_, 1, 2) = -readRt(t, "Rt.csv", I_)/pow( R_+S_+I_,2.0)*S_*delta_*I_;
    DENSE_ELEM(jac_, 2, 1) = delta_;
    DENSE_ELEM(jac_, 3, 1) = gamma1_;
    return 0;
}