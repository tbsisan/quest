function labelFig(t,x,y,l)
    t=strrep(t,'_','\_');
    title(t);
    xlabel(x);
    ylabel(y);
    if length(l)>0
        legend(l);
        copied_legend = findobj(gcf(),'type','axes','Tag','legend');
        set(copied_legend, 'FontSize', 8);
    end
end
