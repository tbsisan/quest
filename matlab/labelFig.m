function labelFig(t,x,y,l)
global isOctave
    t=strrep(t,'_','\_');
    title(t);
    xlabel(x);
    ylabel(y);
    if length(l)>0
        legend(l);
        if (~isOctave)
            copied_legend = findobj(gcf(),'type','axes','Tag','legend');
            set(copied_legend, 'FontSize', 6);
        end
    end
end
