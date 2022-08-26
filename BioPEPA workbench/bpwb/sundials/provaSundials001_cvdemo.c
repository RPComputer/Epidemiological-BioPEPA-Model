/*
 *  provaSundials001_cvdemo.c
 *
 *
 *  CVODE ODE solver for the vector field named: provaSundials001
 *
 *  This file was generated by the program VFGEN, version: 2.6.0.dev3
 *  Generated on 26-Aug-2022 at 16:58
 *
 */

#include <stdlib.h>
#include <string.h>
#include <math.h>

/* Include headers for CVODE 2.5.0 */
#include <sundials/sundials_types.h>
#include <sundials/sundials_dense.h>
#include <sundials/sundials_nvector.h>
#include <nvector/nvector_serial.h>
#include <cvode/cvode.h>
#include <cvode/cvode_dense.h>

#include "provaSundials001_cv.h"


int use(int argc, char *argv[], int nv, char *vname[], double y_[], int np, char *pname[], const double p_[])
{
    int i;
    printf("use: %s [options]\n", argv[0]);
    printf("options:\n");
    printf("    -h    Print this help message.\n");
    for (i = 0; i < nv; ++i) {
        printf("    %s=<initial_condition>   Default value is %e\n", vname[i], y_[i]);
    }
    for (i = 0; i < np; ++i) {
        printf("    %s=<parameter_value>   Default value is %e\n", pname[i], p_[i]);
    }
    printf("    abserr=<absolute_error_tolerance>\n");
    printf("    relerr=<relative_error_tolerance>\n");
    printf("    stoptime=<stop_time>\n");
    return 0;
}


int assign(char *str[], int ns, double v[], char *a)
{
    int i;
    char name[256];
    char *e;

    e = strchr(a, '=');
    if (e == NULL) {
        return -1;
    }
    *e = '\0';
    strcpy(name, a);
    *e = '=';
    ++e;
    for (i = 0; i < ns; ++i) {
        if (strcmp(str[i], name) == 0) {
            break;
        }
    }
    if (i == ns) {
        return -1;
    }
    v[i] = atof(e);
    return i;
}


int main (int argc, char *argv[])
{
    int i, j;
    int flag;
    const int N_ = 4;
    const int P_ = 3;
    const realtype def_p_[3] = {
        RCONST(1.0),
        RCONST(7.0000000000000007e-02),
        RCONST(1.0000000000000000e-02)
    };
    const realtype placeholder_ = def_p_[0];
    const realtype delta_     = def_p_[1];
    const realtype gamma1_    = def_p_[2];
    realtype def_y_[4] = {RCONST(60000000.0), RCONST(120.0), RCONST(0.0), RCONST(0.0)};
    realtype y_[4];
    realtype p_[3];
    realtype solver_param_[3] = {RCONST(1.0e-6), RCONST(0.0), RCONST(10.0)};
    char *varnames_[4] = {"S_", "I_", "R_", "D_"};
    char *parnames_[3] = {"placeholder_", "delta_", "gamma1_"};
    char *solver_param_names_[3] = {"abserr", "relerr", "stoptime"};

    for (i = 0; i < N_; ++i) {
        y_[i] = def_y_[i];
    }
    for (i = 0; i < P_; ++i) {
        p_[i] = def_p_[i];
    }
    for (i = 1; i < argc; ++i) {
        int j;
        if (strcmp(argv[i], "-h") == 0) {
            use(argc, argv, N_, varnames_, def_y_, P_, parnames_, def_p_);
            exit(0);
        }
        j = assign(varnames_, N_, y_, argv[i]);
        if (j == -1) {
            j = assign(parnames_, P_, p_, argv[i]);
            if (j == -1) {
                j = assign(solver_param_names_, 3, solver_param_, argv[i]);
                if (j == -1) {
                    fprintf(stderr, "unknown argument: %s\n", argv[i]);
                    use(argc, argv, N_, varnames_, def_y_, P_, parnames_, def_p_); 
                    exit(-1);
                }
            }
        }
    }

    N_Vector y0_;
    y0_ = N_VNew_Serial(N_);
    for (i = 0; i < N_; ++i) {
        NV_Ith_S(y0_, i) = y_[i];
    }

    /* For non-stiff problems:   */
    void *cvode_mem = CVodeCreate(CV_ADAMS, CV_FUNCTIONAL);
    /* For stiff problems:       */
    /* void *cvode_mem = CVodeCreate(CV_BDF, CV_NEWTON); */

    realtype t = RCONST(0.0);
    flag = CVodeMalloc(cvode_mem, provaSundials001_vf, t, y0_, CV_SS, solver_param_[1], &(solver_param_[0]));
    flag = CVodeSetFdata(cvode_mem, &(p_[0]));
    flag = CVDense(cvode_mem, N_);
    flag = CVDenseSetJacFn(cvode_mem, provaSundials001_jac, &(p_[0]));

    realtype t1 = solver_param_[2];
    /* Print the initial condition  */
    printf("%.8e", t);
    for (j = 0; j < N_; ++j) {
        printf(" %.8e", NV_Ith_S(y0_, j));
    }
    printf("\n");
    flag = CVodeSetStopTime(cvode_mem, t1);
    while (t < t1) {
        /* Advance the solution */
        flag = CVode(cvode_mem, t1, y0_, &t, CV_ONE_STEP_TSTOP);
        if (flag != CV_SUCCESS && flag != CV_TSTOP_RETURN) {
            fprintf(stderr, "flag=%d\n", flag);
            break;
        }
        /* Print the solution at the current time */
        printf("%.8e", t);
        for (j = 0; j < N_; ++j) {
            printf(" %.8e", NV_Ith_S(y0_,j));
        }
        printf("\n");
    }

    N_VDestroy_Serial(y0_);
    CVodeFree(&cvode_mem);
}
