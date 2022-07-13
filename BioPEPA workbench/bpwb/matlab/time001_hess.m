%
% time001_hess.m
%
% This MATLAB function computes the Hessians of the vector field
% defined in time001_vf.m.
%
% hess_(n,i,j) is the second partial derivative of the n-th component
% of the vector field, taken with respect to the i-th and j-th variables.
%
% This file was generated by the program VFGEN, version: 2.6.0.dev3
% Generated on 13-Jul-2022 at 18:36
%
%
function hess_ = time001_hess(t,x_,p_)
    S_         = x_(1);
    k1_        = p_(1);

    hess_ = zeros(1,1,1);

