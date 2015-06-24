function [ fh ] = launchFigure( flags, varargin )
    fh=figure();
    if length(varargin)>0
        set(fh,'Position',[10 10 varargin{1} varargin{2}]);
    end
    if amember(flags,'hideFigs')
        set(fh,'Visible','off');
    end
end
