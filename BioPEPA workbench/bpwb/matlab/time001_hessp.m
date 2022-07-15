%
% time001_hessp.m
%
% This MATLAB function computes the derivatives of the vector field
% defined in time001_vf.m.
%
% hessp_(n,i,j) is the second partial derivative of the n-th component
% of the vector field, taken with respect to the i-th variable
% and the j-th parameter.
%
% This file was generated by the program VFGEN, version: 2.6.0.dev3
% Generated on 15-Jul-2022 at 13:12
%
%
function hessp_ = time001_hessp(t,x_,p_)
    S_         = x_(1);
    k1_        = p_(1);

    hessp_ = zeros(1,1,1);

    hessp_(1,1,1) = -1-t;

