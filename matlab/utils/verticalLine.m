function verticalLine(x,spec,explicitColor,varargin)

ax=axis;
yLimits=[ax(3) ax(4)];
hold on

if explicitColor==0
    explicitColor=getaNiceColor();
end

lineWidth=1; 

for linei=1:length(x)
    xi=x(linei);
    colorLinePlot([xi xi], yLimits, explicitColor, lineWidth, spec)
    explicitColor=getaNiceolor();
end

if ~isempty(varargin)
    y=varargin{1}*ax(4);
    txt=varargin{2};
    if (strcmp(varargin{3},'r'))
        text(x,y,{['\leftarrow',txt],num2str(x,3)},'BackgroundColor',1-(1-col)*0.5,'HorizontalAlignment','left','Margin',1e-6);
    else
        text(x,y,{[txt,'\rightarrow'],num2str(x,3)},'BackgroundColor',1-(1-col)*0.5,'HorizontalAlignment','right','Margin',1e-6);
    end
    
end
