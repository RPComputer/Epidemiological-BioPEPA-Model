%
% example_classes001_jac.m
%
% This MATLAB function computes the Jacobian of the vector field
% defined in example_classes001_vf.m.
%
% This file was generated by the program VFGEN, version: 2.6.0.dev3
% Generated on 12-May-2022 at 18:07
%
%
function jac_ = example_classes001_jac(t,x_,p_)
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
    jac_ = zeros(10,10);
    jac_(1,1) = -r4_-r1_+1/2*k1_*Sy_*(Ry_+Sy_+Iy_)^(-1)-k1_*(Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-2)*Sy_*Io_-1/2*k1_*Sy_*Iy_*(Ry_+Sy_+Iy_)^(-2)-(Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-2)*Sy_*k2_*Im_;
    jac_(1,2) = (Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-1)*Sy_*k2_-(Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-2)*Sy_*k2_*Im_;
    jac_(1,3) = -k1_*(Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-2)*Sy_*Io_+k1_*(Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-1)*Sy_;
    jac_(1,4) = 1/2*k1_*Iy_*(Ry_+Sy_+Iy_)^(-1)+(Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-1)*k2_*Im_-k1_*(Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-2)*Sy_*Io_+k1_*(Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-1)*Io_-1/2*k1_*Sy_*Iy_*(Ry_+Sy_+Iy_)^(-2)-(Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-2)*Sy_*k2_*Im_;
    jac_(1,5) = -(Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-2)*Sy_*k2_*Im_;
    jac_(1,6) = -k1_*(Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-2)*Sy_*Io_;
    jac_(1,7) = -k1_*(Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-2)*Sy_*Io_-1/2*k1_*Sy_*Iy_*(Ry_+Sy_+Iy_)^(-2)-(Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-2)*Sy_*k2_*Im_;
    jac_(1,8) = -(Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-2)*Sy_*k2_*Im_;
    jac_(1,9) = -k1_*(Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-2)*Sy_*Io_;
    jac_(2,1) = -(Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-2)*Iy_*Sm_*k3_+(Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-1)*Sm_*k3_;
    jac_(2,2) = -1/2*Sm_*Im_*(Rm_+Sm_+Im_)^(-2)*k4_-(Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-2)*Iy_*Sm_*k3_+1/2*Sm_*(Rm_+Sm_+Im_)^(-1)*k4_-r5_-r2_-k5_*Sm_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-2)*Io_;
    jac_(2,3) = -k5_*Sm_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-2)*Io_+k5_*Sm_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-1);
    jac_(2,4) = -(Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-2)*Iy_*Sm_*k3_;
    jac_(2,5) = -1/2*Sm_*Im_*(Rm_+Sm_+Im_)^(-2)*k4_-(Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-2)*Iy_*Sm_*k3_+(Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-1)*Iy_*k3_-k5_*Sm_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-2)*Io_+k5_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-1)*Io_+1/2*Im_*(Rm_+Sm_+Im_)^(-1)*k4_;
    jac_(2,6) = -k5_*Sm_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-2)*Io_;
    jac_(2,7) = -(Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-2)*Iy_*Sm_*k3_;
    jac_(2,8) = -1/2*Sm_*Im_*(Rm_+Sm_+Im_)^(-2)*k4_-(Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-2)*Iy_*Sm_*k3_-k5_*Sm_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-2)*Io_;
    jac_(2,9) = -k5_*Sm_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-2)*Io_;
    jac_(3,1) = (Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-1)*k6_*So_-(Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-2)*Iy_*k6_*So_;
    jac_(3,2) = k2_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-1)*So_-k2_*Im_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-2)*So_;
    jac_(3,3) = -1/2*k7_*So_*Io_*(Ro_+So_+Io_)^(-2)-k2_*Im_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-2)*So_+1/2*k7_*So_*(Ro_+So_+Io_)^(-1)-r6_-r3_-(Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-2)*Iy_*k6_*So_;
    jac_(3,4) = -(Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-2)*Iy_*k6_*So_;
    jac_(3,5) = -k2_*Im_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-2)*So_;
    jac_(3,6) = 1/2*k7_*Io_*(Ro_+So_+Io_)^(-1)-1/2*k7_*So_*Io_*(Ro_+So_+Io_)^(-2)-k2_*Im_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-2)*So_+k2_*Im_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-1)-(Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-2)*Iy_*k6_*So_+(Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-1)*Iy_*k6_;
    jac_(3,7) = -(Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-2)*Iy_*k6_*So_;
    jac_(3,8) = -k2_*Im_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-2)*So_;
    jac_(3,9) = -1/2*k7_*So_*Io_*(Ro_+So_+Io_)^(-2)-k2_*Im_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-2)*So_-(Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-2)*Iy_*k6_*So_;
    jac_(4,1) = -1/2*k1_*Sy_*(Ry_+Sy_+Iy_)^(-1)+k1_*(Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-2)*Sy_*Io_+1/2*k1_*Sy_*Iy_*(Ry_+Sy_+Iy_)^(-2)+(Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-2)*Sy_*k2_*Im_;
    jac_(4,2) = -(Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-1)*Sy_*k2_+(Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-2)*Sy_*k2_*Im_;
    jac_(4,3) = k1_*(Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-2)*Sy_*Io_-k1_*(Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-1)*Sy_;
    jac_(4,4) = -1/2*k1_*Iy_*(Ry_+Sy_+Iy_)^(-1)-(Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-1)*k2_*Im_+k1_*(Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-2)*Sy_*Io_-k1_*(Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-1)*Io_+1/2*k1_*Sy_*Iy_*(Ry_+Sy_+Iy_)^(-2)+(Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-2)*Sy_*k2_*Im_;
    jac_(4,5) = (Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-2)*Sy_*k2_*Im_;
    jac_(4,6) = k1_*(Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-2)*Sy_*Io_;
    jac_(4,7) = k1_*(Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-2)*Sy_*Io_+1/2*k1_*Sy_*Iy_*(Ry_+Sy_+Iy_)^(-2)+(Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-2)*Sy_*k2_*Im_;
    jac_(4,8) = (Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-2)*Sy_*k2_*Im_;
    jac_(4,9) = k1_*(Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-2)*Sy_*Io_;
    jac_(5,1) = (Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-2)*Iy_*Sm_*k3_-(Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-1)*Sm_*k3_;
    jac_(5,2) = 1/2*Sm_*Im_*(Rm_+Sm_+Im_)^(-2)*k4_+(Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-2)*Iy_*Sm_*k3_-1/2*Sm_*(Rm_+Sm_+Im_)^(-1)*k4_+k5_*Sm_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-2)*Io_;
    jac_(5,3) = k5_*Sm_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-2)*Io_-k5_*Sm_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-1);
    jac_(5,4) = (Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-2)*Iy_*Sm_*k3_;
    jac_(5,5) = 1/2*Sm_*Im_*(Rm_+Sm_+Im_)^(-2)*k4_+(Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-2)*Iy_*Sm_*k3_-(Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-1)*Iy_*k3_+k5_*Sm_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-2)*Io_-k5_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-1)*Io_-1/2*Im_*(Rm_+Sm_+Im_)^(-1)*k4_;
    jac_(5,6) = k5_*Sm_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-2)*Io_;
    jac_(5,7) = (Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-2)*Iy_*Sm_*k3_;
    jac_(5,8) = 1/2*Sm_*Im_*(Rm_+Sm_+Im_)^(-2)*k4_+(Ry_+Sy_+Iy_+Rm_+Sm_+Im_)^(-2)*Iy_*Sm_*k3_+k5_*Sm_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-2)*Io_;
    jac_(5,9) = k5_*Sm_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-2)*Io_;
    jac_(6,1) = -(Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-1)*k6_*So_+(Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-2)*Iy_*k6_*So_;
    jac_(6,2) = -k2_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-1)*So_+k2_*Im_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-2)*So_;
    jac_(6,3) = 1/2*k7_*So_*Io_*(Ro_+So_+Io_)^(-2)+k2_*Im_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-2)*So_-1/2*k7_*So_*(Ro_+So_+Io_)^(-1)+(Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-2)*Iy_*k6_*So_;
    jac_(6,4) = (Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-2)*Iy_*k6_*So_;
    jac_(6,5) = k2_*Im_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-2)*So_;
    jac_(6,6) = -1/2*k7_*Io_*(Ro_+So_+Io_)^(-1)+1/2*k7_*So_*Io_*(Ro_+So_+Io_)^(-2)+k2_*Im_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-2)*So_-k2_*Im_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-1)+(Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-2)*Iy_*k6_*So_-(Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-1)*Iy_*k6_;
    jac_(6,7) = (Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-2)*Iy_*k6_*So_;
    jac_(6,8) = k2_*Im_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-2)*So_;
    jac_(6,9) = 1/2*k7_*So_*Io_*(Ro_+So_+Io_)^(-2)+k2_*Im_*(Rm_+Sm_+Im_+Ro_+So_+Io_)^(-2)*So_+(Ry_+Sy_+Iy_+Ro_+So_+Io_)^(-2)*Iy_*k6_*So_;
    jac_(7,1) = r1_;
    jac_(8,2) = r2_;
    jac_(9,3) = r3_;
    jac_(10,1) = r4_;
    jac_(10,2) = r5_;
    jac_(10,3) = r6_;
