function [ fitResults, fitErrors, fitMessages ] = fitOneSoliton(u,expectHeight)

particleNum=[1:length(u)]';
u = u - u(1);

if (size(u,1)<size(u,2))
    u=u';
end

fitDefinition = fittype( @(width,position,height,x) height/(pi/2)*atan( exp( (x-position)/width ) ),'indep','x');

fitSettings = fitoptions(fitDefinition);
fitSettings.Lower=     [1 10 expectHeight*0.9];
fitSettings.Upper=     [particleNum(end)/2 particleNum(end)-10 expectHeight*1.1];
%fitSettings.StartPoint=[10   s1e              0.10   s2e              -SolSign*2.456    -SolSign*2.456   0];
%fitSettings.Algorithm='Trust-Region';
%fitSettings.Robust='LAR';

[ fitResults, fitErrors, fitMessages ] = fit(particleNum,u,fitDefinition,fitSettings);
