function [SG0, SG1] = savitzyGolaySmoothing(N,W,y,dx)
% Savitzy-Golay smoothing of y with width W and order N polynomial
% dx is needed if SG1 (derivative) is needed.
% N = 2;                 % Order of polynomial fit
% W = 15;                % Window length
% the first (W-1)/2 returned will be zero
% the returned array does not include estimates for the last (W+1)/2 points, instead using the last calculated to fill the array
% if fitting a triplicate, you want datapoints (length(y)+1):(2*length(y))
[b,g] = sgolay(N,W);   % Calculate S-G coefficients
HalfWin  = ((W+1)/2) -1;
for n = (W+1)/2:length(y)-(W+1)/2,
  % Zero-th derivative (smoothing only)
  SG0(n) =   dot(g(:,1), y(n - HalfWin: n + HalfWin));
  
  % 1st differential
  SG1(n) =   dot(g(:,2), y(n - HalfWin: n + HalfWin));
end
SG0(end+1:length(y))=SG0(end);
SG0(1:(W+1)/2-1)=SG0((W+1)/2);
SG1(end+1:length(y))=SG1(end);
SG1(1:(W+1)/2-1)=SG1((W+1)/2);
% dx=1; % x axis data sep
SG1=SG1/dx;
