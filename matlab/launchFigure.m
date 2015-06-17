function [ fh ] = launchFigure( flags )
    fh=figure();
    if amember(flags,'hideFigs')
        set(fh,'Visible','off');
    end
end
