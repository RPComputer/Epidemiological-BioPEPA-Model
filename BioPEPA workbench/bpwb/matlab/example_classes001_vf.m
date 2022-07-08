%
% example_classes001_vf.m
%
% MATLAB vector field function for: example_classes001
%
% This file was generated by the program VFGEN, version: 2.6.0.dev3
% Generated on 12-May-2022 at 18:07
%
%
function vf_ = example_classes001_vf(t,x_,p_)
    Iy_        = x_(1);
    Im_        = x_(2);
    Io_        = x_(3);
    Sy_        = x_(4);
    Sm_        = x_(5);
    So_        = x_(6);
    Ry_        = x_(7);
    Rm_        = x_(8);
    Ro_        = x_(9);
    D_         = x_(10);
    k1_        = p_(1);
    k2_        = p_(2);
    k3_        = p_(3);
    k4_        = p_(4);
    k5_        = p_(5);
    k6_        = p_(6);
    k7_        = p_(7);
    r1_        = p_(8);
    r2_        = p_(9);
    r3_        = p_(10);
    r4_        = p_(11);
    r5_        = p_(12);
    r6_        = p_(13);
    contactyy_ = 1/2*k1_*Sy_*Iy_*(Ry_+Sy_+Iy_)^(-1);
    contactym_ = (Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-1)*Sy_*k2_*Im_;
    contactyo_ = k1_*(Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-1)*Sy_*Io_;
    contactmy_ = (Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-1)*Iy_*Sm_*k3_;
    contactmm_ = 1/2*Sm_*Im_*(Rm_+Sm_+Im_)^(-1)*k4_;
    contactmo_ = k5_*Sm_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-1)*Io_;
    contactoy_ = (Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-1)*Iy_*k6_*So_;
    contactom_ = k2_*Im_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-1)*So_;
    contactoo_ = 1/2*k7_*So_*Io_*(Ro_+So_+Io_)^(-1);
    recoveryy_ = r1_*Iy_;
    recoverym_ = r2_*Im_;
    recoveryo_ = r3_*Io_;
    deathy_ = r4_*Iy_;
    deathm_ = r5_*Im_;
    deatho_ = r6_*Io_;
    vf_ = zeros(10,1);
    vf_(1) = contactyo_-deathy_+contactyy_-recoveryy_+contactym_;
    vf_(2) = contactmo_+contactmy_+contactmm_-deathm_-recoverym_;
    vf_(3) = -recoveryo_+contactoo_+contactoy_+contactom_-deatho_;
    vf_(4) = -contactyo_-contactyy_-contactym_;
    vf_(5) = -contactmo_-contactmy_-contactmm_;
    vf_(6) = -contactoo_-contactoy_-contactom_;
    vf_(7) = recoveryy_;
    vf_(8) = recoverym_;
    vf_(9) = recoveryo_;
    vf_(10) = deathy_+deathm_+deatho_;
