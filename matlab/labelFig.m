function labelFig(t,x,y,l)
t=strrep(t,'_','\_');
title(t);
xlabel(x);
ylabel(y);
if length(l)>0
    legend(l);
end
