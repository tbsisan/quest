function [ plotHandle ] = cFKplot( x, y, color, w, lineSpec, subTitle, subx, suby, subLegend, label );

[ plotHandle ] = colorLinePlot(x,y,color,w,lineSpec);

if (label)
    labelFig(subTitle,subx,suby,subLegend);
%    title(subTitle);
%    xlabel(subx, 'FontSize', 14);
%    ylabel(suby, 'FontSize', 14);
%    if (length(subLegend)>0)
%        legend(subLegend,'FontSize',6);
%    end
end
