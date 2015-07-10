i=1:100;
N=length(i);
WL=2.456e-10;
WLperN=2;
lambda=WL/WLperN;
aPercent=1.03;
k=1000;
h=1.53e-21;
l0=sqrt(k*lambda*lambda/4.0/h);
x=(i-1)*WL/WLperN;
oneOverP0=aPercent-1;
positioningMove=(0.7)*lambda;
u0s=linspace(-1,-1,1)
%figure;
%hold on;

for j=1:length(u0s)
ui=uFvdM(u0s(j),[],N,l0,oneOverP0);
%plot(i,ui/lambda,'o-');
end
x=x+ui;
%title(sprintf('TBS:  g=%.2f, 1/P0=%.2f, L0=%.2f',k,oneOverP0,l0),'interpreter','tex');
%xlabel('particle number');
%ylabel('u_i');
%legend(cellfun(@num2str,num2cell(u0s),'UniformOutput',false));
