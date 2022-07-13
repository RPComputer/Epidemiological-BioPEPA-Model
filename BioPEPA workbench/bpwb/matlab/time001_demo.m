%
% time001_demo.m
%
% MATLAB demo function for the vector field: time001
%
% This file was generated by the program VFGEN, version: 2.6.0.dev3
% Generated on 13-Jul-2022 at 18:36
%
%

function time001_demo
    figure;
    clf
    set(gcf,'Position',[0 0 250 120]);
    v = [];
    uicontrol('Style','text','Position',[10 100 100 18],'String','S_(0)');
    ui = uicontrol('Style','edit','Position',[115 100 100 18],'String','450');
    v = [v; ui];
    uicontrol('Style','text','Position',[10 80 100 18],'String','k1_');
    ui = uicontrol('Style','edit','Position',[115 80 100 18],'String','0.035');
    v = [v; ui];
    uicontrol('Style','text','Position',[10 60 100 18],'String','Stop Time');
    ui = uicontrol('Style','edit','Position',[115 60 100 18],'String','10');
    v = [v; ui];
    ui = uicontrol('Style','checkbox','Position',[10 40 200 18],'String','Separate Axes');
    v = [v; ui];
    uicontrol('Style','pushbutton','Position',[10 20 40 18],'String','Go','Callback',@go_cb,'UserData',v);
end

function go_cb(arg1,arg2)
    v = get(arg1,'UserData');
    ic = zeros(size(v,1)-2,1);
    for k = 1:size(v,1)-2,
        ic(k) = eval(get(v(k),'String'));
        if (isnan(ic(k)))
            ic(k) = 0.0;
        end;
    end;
    stoptime = str2double(get(v(end-1),'String'));
    if (isnan(stoptime))
        stoptime = 0.0;
    end;
    abstol = 1e-9;
    reltol = 1e-7;
    opts = odeset('AbsTol',abstol,'RelTol',reltol,'Jacobian',@time001_jac);
    % Change ode45 to ode15s for stiff differential equations.
    [t,z_] = ode45(@time001_vf,[0 stoptime],ic(1:1),opts,ic(2:end));
    figure;
    clf;
    a = get(v(end),'Value');
    if (a == 0),
        plot(t,z_);
        xlabel('t');
        legend('S_');
    grid on
    else
        subplot(1,1,1);
        plot(t,z_(:,1))
        xlabel('t');
        ylabel('S_')
        grid on
    end
end