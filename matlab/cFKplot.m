function [ plotHandle ] = cFKplot( x, y, color, w, lineSpec, subTitle, subx, suby, subLegend, label );

if ( all(size(squeeze(x)) ~= size(squeeze(y))) && all(size(squeeze(x')) ~= size(squeeze(y))) )
    disp('Vectors to cFKplot not same size');
    disp(['size x: ' num2str(size(x))]);
    disp(['size y: ' num2str(size(y))]);
else
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
end

end
