function col=colorLinePlot(x,y,explicitColor,linewidth,varargin)
% Plot a line or scatter plot with a nice color.
% X-axis if empty will default to plotting y versus integers 1:length(y).
% ExplicitColor is an integer from 1 to 32, or zero if not specifying directly.
% If explicitColor is zero (technically ~>0), color will come from calling getaNiceColor(n)
    
c = getaNiceColor();
if (explicitColor>0)
    c=getaNiceColor(explicitColor);
end

if length(varargin)==0
    style='-';
elseif length(varargin)==1
    style=varargin{1};
else 
    disp(['WARNING: TOO MANY ARGS to ' mfilename]);
end

if (isempty(x))
    plot(y,style,'Color',c,'LineWidth',linewidth);
else
    plot(x,y,style,'Color',c,'LineWidth',linewidth);
end
col=c;
