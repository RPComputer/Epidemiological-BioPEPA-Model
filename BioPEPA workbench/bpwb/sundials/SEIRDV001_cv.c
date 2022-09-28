/*
 *  SEIRDV001_cv.c
 *
 *  CVODE C file for the vector field named: SEIRDV001
 *
 *  This file was generated by the program VFGEN, version: 2.6.0.dev3
 *  Generated on 28-Sep-2022 at 15:34
 */

#include "CustomFunctions.c"
#include "CustomFunctions.c"
#include "CustomFunctions.c"
#include "CustomFunctions.c"
#include "CustomFunctions.c"
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

int SEIRDV001_vf(realtype t, N_Vector y_, N_Vector f_, void *params)
{
    realtype S_, E_, I_, R_, D_, V_, CUMI_;
    realtype placeholder_, vaccinerate_, delta_, gamma1_, alpha_;
    realtype infect_, infectcount_, incubate_, recover_, death_, vaccinateS_, vaccinateR_;
    realtype *p_;

    p_ = (realtype *) params;

    S_         = NV_Ith_S(y_,0);
    E_         = NV_Ith_S(y_,1);
    I_         = NV_Ith_S(y_,2);
    R_         = NV_Ith_S(y_,3);
    D_         = NV_Ith_S(y_,4);
    V_         = NV_Ith_S(y_,5);
    CUMI_      = NV_Ith_S(y_,6);

    placeholder_ = p_[0];
    vaccinerate_ = p_[1];
    delta_     = p_[2];
    gamma1_    = p_[3];
    alpha_     = p_[4];

    infect_ = 1.0/( R_+S_+E_+V_+I_)*( R_+S_+V_)*readRt(t, "Rt_SEIRDV.csv")*( gamma1_+alpha_)*I_;
    infectcount_ = 1.0/( R_+S_+E_+V_+I_)*( R_+S_+V_)*readRt(t, "Rt_SEIRDV.csv")*( gamma1_+alpha_)*I_;
    incubate_ = delta_*E_;
    recover_ = gamma1_*I_;
    death_ = alpha_*I_;
    vaccinateS_ = S_*readDatatable(t, "vacciniSEIRDV.csv", "n")/( R_+S_);
    vaccinateR_ = R_*readDatatable(t, "vacciniSEIRDV.csv", "n")/( R_+S_);

    NV_Ith_S(f_,0) = -vaccinateS_-infect_;
    NV_Ith_S(f_,1) = -incubate_+infect_;
    NV_Ith_S(f_,2) =  incubate_-recover_-death_;
    NV_Ith_S(f_,3) = -vaccinateR_+recover_;
    NV_Ith_S(f_,4) = death_;
    NV_Ith_S(f_,5) =  vaccinateS_+vaccinateR_;
    NV_Ith_S(f_,6) = infectcount_;
    return 0;
}

/*
 *  The Jacobian.
 */

int SEIRDV001_jac(long int N_, DenseMat jac_, realtype t,
                N_Vector y_, N_Vector fy_, void *params,
                N_Vector tmp1, N_Vector tmp2, N_Vector tmp3)
{
    realtype S_, E_, I_, R_, D_, V_, CUMI_;
    realtype placeholder_, vaccinerate_, delta_, gamma1_, alpha_;
    realtype *p_;

    p_ = (realtype *) params;

    S_         = NV_Ith_S(y_,0);
    E_         = NV_Ith_S(y_,1);
    I_         = NV_Ith_S(y_,2);
    R_         = NV_Ith_S(y_,3);
    D_         = NV_Ith_S(y_,4);
    V_         = NV_Ith_S(y_,5);
    CUMI_      = NV_Ith_S(y_,6);

    placeholder_ = p_[0];
    vaccinerate_ = p_[1];
    delta_     = p_[2];
    gamma1_    = p_[3];
    alpha_     = p_[4];

    DENSE_ELEM(jac_, 0, 0) = -readDatatable(t, "vacciniSEIRDV.csv", "n")/( R_+S_)+S_*readDatatable(t, "vacciniSEIRDV.csv", "n")/pow( R_+S_,2.0)-1.0/( R_+S_+E_+V_+I_)*readRt(t, "Rt_SEIRDV.csv")*( gamma1_+alpha_)*I_+1.0/pow( R_+S_+E_+V_+I_,2.0)*( R_+S_+V_)*readRt(t, "Rt_SEIRDV.csv")*( gamma1_+alpha_)*I_;
    DENSE_ELEM(jac_, 0, 1) = 1.0/pow( R_+S_+E_+V_+I_,2.0)*( R_+S_+V_)*readRt(t, "Rt_SEIRDV.csv")*( gamma1_+alpha_)*I_;
    DENSE_ELEM(jac_, 0, 2) = -1.0/( R_+S_+E_+V_+I_)*( R_+S_+V_)*readRt(t, "Rt_SEIRDV.csv")*( gamma1_+alpha_)+1.0/pow( R_+S_+E_+V_+I_,2.0)*( R_+S_+V_)*readRt(t, "Rt_SEIRDV.csv")*( gamma1_+alpha_)*I_;
    DENSE_ELEM(jac_, 0, 3) =  S_*readDatatable(t, "vacciniSEIRDV.csv", "n")/pow( R_+S_,2.0)-1.0/( R_+S_+E_+V_+I_)*readRt(t, "Rt_SEIRDV.csv")*( gamma1_+alpha_)*I_+1.0/pow( R_+S_+E_+V_+I_,2.0)*( R_+S_+V_)*readRt(t, "Rt_SEIRDV.csv")*( gamma1_+alpha_)*I_;
    DENSE_ELEM(jac_, 0, 5) = -1.0/( R_+S_+E_+V_+I_)*readRt(t, "Rt_SEIRDV.csv")*( gamma1_+alpha_)*I_+1.0/pow( R_+S_+E_+V_+I_,2.0)*( R_+S_+V_)*readRt(t, "Rt_SEIRDV.csv")*( gamma1_+alpha_)*I_;
    DENSE_ELEM(jac_, 1, 0) =  1.0/( R_+S_+E_+V_+I_)*readRt(t, "Rt_SEIRDV.csv")*( gamma1_+alpha_)*I_-1.0/pow( R_+S_+E_+V_+I_,2.0)*( R_+S_+V_)*readRt(t, "Rt_SEIRDV.csv")*( gamma1_+alpha_)*I_;
    DENSE_ELEM(jac_, 1, 1) = -delta_-1.0/pow( R_+S_+E_+V_+I_,2.0)*( R_+S_+V_)*readRt(t, "Rt_SEIRDV.csv")*( gamma1_+alpha_)*I_;
    DENSE_ELEM(jac_, 1, 2) =  1.0/( R_+S_+E_+V_+I_)*( R_+S_+V_)*readRt(t, "Rt_SEIRDV.csv")*( gamma1_+alpha_)-1.0/pow( R_+S_+E_+V_+I_,2.0)*( R_+S_+V_)*readRt(t, "Rt_SEIRDV.csv")*( gamma1_+alpha_)*I_;
    DENSE_ELEM(jac_, 1, 3) =  1.0/( R_+S_+E_+V_+I_)*readRt(t, "Rt_SEIRDV.csv")*( gamma1_+alpha_)*I_-1.0/pow( R_+S_+E_+V_+I_,2.0)*( R_+S_+V_)*readRt(t, "Rt_SEIRDV.csv")*( gamma1_+alpha_)*I_;
    DENSE_ELEM(jac_, 1, 5) =  1.0/( R_+S_+E_+V_+I_)*readRt(t, "Rt_SEIRDV.csv")*( gamma1_+alpha_)*I_-1.0/pow( R_+S_+E_+V_+I_,2.0)*( R_+S_+V_)*readRt(t, "Rt_SEIRDV.csv")*( gamma1_+alpha_)*I_;
    DENSE_ELEM(jac_, 2, 1) = delta_;
    DENSE_ELEM(jac_, 2, 2) = -gamma1_-alpha_;
    DENSE_ELEM(jac_, 3, 0) = R_*readDatatable(t, "vacciniSEIRDV.csv", "n")/pow( R_+S_,2.0);
    DENSE_ELEM(jac_, 3, 2) = gamma1_;
    DENSE_ELEM(jac_, 3, 3) = -readDatatable(t, "vacciniSEIRDV.csv", "n")/( R_+S_)+R_*readDatatable(t, "vacciniSEIRDV.csv", "n")/pow( R_+S_,2.0);
    DENSE_ELEM(jac_, 4, 2) = alpha_;
    DENSE_ELEM(jac_, 5, 0) =  readDatatable(t, "vacciniSEIRDV.csv", "n")/( R_+S_)-R_*readDatatable(t, "vacciniSEIRDV.csv", "n")/pow( R_+S_,2.0)-S_*readDatatable(t, "vacciniSEIRDV.csv", "n")/pow( R_+S_,2.0);
    DENSE_ELEM(jac_, 5, 3) =  readDatatable(t, "vacciniSEIRDV.csv", "n")/( R_+S_)-R_*readDatatable(t, "vacciniSEIRDV.csv", "n")/pow( R_+S_,2.0)-S_*readDatatable(t, "vacciniSEIRDV.csv", "n")/pow( R_+S_,2.0);
    DENSE_ELEM(jac_, 6, 0) =  1.0/( R_+S_+E_+V_+I_)*readRt(t, "Rt_SEIRDV.csv")*( gamma1_+alpha_)*I_-1.0/pow( R_+S_+E_+V_+I_,2.0)*( R_+S_+V_)*readRt(t, "Rt_SEIRDV.csv")*( gamma1_+alpha_)*I_;
    DENSE_ELEM(jac_, 6, 1) = -1.0/pow( R_+S_+E_+V_+I_,2.0)*( R_+S_+V_)*readRt(t, "Rt_SEIRDV.csv")*( gamma1_+alpha_)*I_;
    DENSE_ELEM(jac_, 6, 2) =  1.0/( R_+S_+E_+V_+I_)*( R_+S_+V_)*readRt(t, "Rt_SEIRDV.csv")*( gamma1_+alpha_)-1.0/pow( R_+S_+E_+V_+I_,2.0)*( R_+S_+V_)*readRt(t, "Rt_SEIRDV.csv")*( gamma1_+alpha_)*I_;
    DENSE_ELEM(jac_, 6, 3) =  1.0/( R_+S_+E_+V_+I_)*readRt(t, "Rt_SEIRDV.csv")*( gamma1_+alpha_)*I_-1.0/pow( R_+S_+E_+V_+I_,2.0)*( R_+S_+V_)*readRt(t, "Rt_SEIRDV.csv")*( gamma1_+alpha_)*I_;
    DENSE_ELEM(jac_, 6, 5) =  1.0/( R_+S_+E_+V_+I_)*readRt(t, "Rt_SEIRDV.csv")*( gamma1_+alpha_)*I_-1.0/pow( R_+S_+E_+V_+I_,2.0)*( R_+S_+V_)*readRt(t, "Rt_SEIRDV.csv")*( gamma1_+alpha_)*I_;
    return 0;
}
