% Solving F&vdM with SL code.
% EndEnergy
close all;
% Notation as in Frank and van der Merwe (1949).
invP0 = 0.05;           % 1/P0
k=60;
h=1.53e-21;
lambda=1.228e-10;
L0 = sqrt(k*lambda^2/4/h);                % l_0
kbar=(lambda/2/pi)^2*k/h;
N = 100;                % Number of particles
xi0s = 1 * linspace(-0e-1, -0e-1, 1);
for j = 1:length(xi0s)
    xi(1,j) = xi0s(j);
%    xi(2,j) = -xi(1,j);                         % position of second particle also set
    xi(2,j) = xi(1,j) + pi/(2*L0^2)* sin(2*pi*xi(1,j)) + invP0;
    for i = 3:N
        xi(i,j) = 2*xi(i-1,j) - xi(i-2,j) + pi/(2*L0^2)*sin(2*pi*xi(i-1,j));
    end
end
% And, for the final mass:
% xi(N)  + pi/(2*L0^2) * sin(2*pi*xi(N)) = xi(N-1) + invP0
%
colorstring = 'kbgry';
for j = 1:length(xi0s)
plot(1:N, xi(:,j), '-o');
hold on;
end
xlabel('Mass number', 'FontSize', 14);
ylabel('Position', 'FontSize', 14);
title(sprintf('SL:  gbar=%.2f, 1/P0=%.2f, L0=%.2f',kbar,invP0,L0),'interpreter','tex');
legend(cellfun(@num2str,num2cell(xi0s),'UniformOutput',false));
set(gca, 'FontSize', 14);
