function savefigs(dir,basename,makefigfile,varargin)
%function savefigs(dir,basename,makefigfile,varargin)

hfigs = get(0, 'children');                          %Get list of figures

if (numel(varargin)>0)
    images=cell2mat(varargin);
else
    images=hfigs(1:length(hfigs));
end

for m = 1:length(images)
    display('saving image of figure handle ');
    %images(m)
    figure(images(m))                                %Bring Figure to foreground
    %filename = input('Filename? (0 to skip)\n', 's')%Prompt user
    %if strcmp(filename, '0')                        %Skip figure when user types 0
    %    continue
    %else
    set(images(m),'PaperPositionMode','auto');
    dateLabel = datestr(now,'MM.SS.FFF');

    pngfnm=[dir basename dateLabel '.png']
    print(images(m),'-dpng','-r300',pngfnm);

    if (makefigfile)
        saveas(images(m), [dir basename dateLabel '.fig']);
    end
    %print(images(m),'-dpng','-zbuffer','-r125',pngfnm);
    %saveas(images(m), pngfnm) %Matlab .FIG file
    %saveas(hfigs(m), [filename '.emf']) %Windows Enhanced Meta-File (best for powerpoints)
    %saveas(hfigs(m), [filename '.png']) %Standard PNG graphics file (best for web)
    %eval(['print -depsc2 ' pngfnm])   %Enhanced Postscript (Level 2 color) (Best for LaTeX documents)
    %end
end

end
