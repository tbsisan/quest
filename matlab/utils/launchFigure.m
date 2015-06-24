function [ fh ] = launchFigure( cFKflags )

fh = figure;
if amember(cFKflags,'hideFigs')
    set( fh, 'Visible', 'off' );
end
