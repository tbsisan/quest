function col=colorLinePlot(x,y,explicitColor,linewidth,varargin)
    
if (length(varargin)==0)
    % used to have two get childrens, but the count was messing up  for the second plot
    % childs=(get(get(gca,'Children'),'Children'));
    childs=((get(gca,'Children')));
    c=size(childs,1)+1;
    style='-';
elseif (length(varargin)==1)
    %c=varargin(1);
    childs=((get(gca,'Children')));
    c=size(childs,1)+1;
    style=varargin{1};
else 
    warn('too many args');
end
if (explicitColor>0)
    c=explicitColor;
end
%if (~ishold)
%   c=1; 
%end
if (c>32) c=c-32; end
if (isempty(x))
    plot(y,style,'Color',getaNiceColor(c),'LineWidth',linewidth);
    %plot(y,'Color',mycolors(c,:),'LineWidth',2);
else
    plot(x,y,style,'Color',getaNiceColor(c),'LineWidth',linewidth);
    %plot(x,y,'Color',mycolors(c,:),'LineWidth',2);
end
col=getaNiceColor(c);
