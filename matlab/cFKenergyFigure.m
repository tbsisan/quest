function [ fh, cFKaxes ] = cFKenergyFigure( w, h, cFKflags )
    rowsOfAxes=5; %endfile-startfile
    colsOfAxes=1; %endfile-startfile
    %xAxes = figuregrid(rowsOfAxes,1);
    %tempAxes = figuregrid(rowsOfAxes,1);
    [ fh ] = launchFigure( cFKflags );
    set(fh,'Position',[10 10 w h]);
    % set(fh,'Visible','off');
    cFKaxes.kinetic = subplot(rowsOfAxes,colsOfAxes,1);
    cFKaxes.spring = subplot(rowsOfAxes,colsOfAxes,2)
    cFKaxes.substrate = subplot(rowsOfAxes,colsOfAxes,3);
    cFKaxes.total = subplot(rowsOfAxes,colsOfAxes,4);
    cFKaxes.solitons = subplot(rowsOfAxes,colsOfAxes,5);
end
