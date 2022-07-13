%
% provaOggi2001_jacp.m
%
% This MATLAB function computes the Jacobian with respect to the parameters
% of the vector field defined in provaOggi2001_vf.m.
%
% This file was generated by the program VFGEN, version: 2.6.0.dev3
% Generated on 13-Jul-2022 at 17:59
%
%
function jacp_ = provaOggi2001_jacp(t,x_,p_)
    S_         = x_(1);
    I_         = x_(2);
    R_         = x_(3);
    D_         = x_(4);
    k1_        = p_(1);
    k2_        = p_(2);
    k3_        = p_(3);
    jacp_ = zeros(4,3);
    jacp_(1,1) = -(S_+I_+R_)^(-1)*S_*I_;
    jacp_(2,1) = (S_+I_+R_)^(-1)*S_*I_;
    jacp_(2,2) = -I_;
    jacp_(2,3) = -I_;
    jacp_(3,2) = I_;
    jacp_(4,3) = I_;

