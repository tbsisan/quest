function horizontalLine(y,spec,explicitColor,varargin)
% Plot a horizontal line, of type specified in lineSpec, spec.

% Get limits of x-axis so we know x-axis extent of a horizontal line.
ax=axis;
xLimits=[ax(1) ax(2)];
hold on;

if explicitColor==0
    explicitColor=getaNiceColor();
end

lineWidth=1; 

for linei=1:length(y)
    yi=y(linei);
    colorLinePlot(xLimits, [yi yi], explicitColor, lineWidth, spec)
end

%Old stuff for annotations.
if ~isempty(varargin)
    x=varargin{1}*ax(1);
    txt=varargin{2};
    if (strcmp(varargin{3},'r'))
        text(x,y,{['\leftarrow',txt],num2str(x)},'BackgroundColor',1-(1-col)*0.5,'HorizontalAlignment','left','Margin',1e-6);
    else
        text(x,y,{[txt,'\rightarrow'],num2str(x)},'BackgroundColor',1-(1-col)*0.5,'HorizontalAlignment','right','Margin',1e-6);
    end
    
end
