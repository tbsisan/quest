function [ fh, cFKaxes ] = cFKfigure( w, h )
    rowsOfAxes=4; %endfile-startfile
    colsOfAxes=2; %endfile-startfile
    %xAxes = figuregrid(rowsOfAxes,1);
    %tempAxes = figuregrid(rowsOfAxes,1);
    fh=figure();
    set(fh,'Visible','off');
    set(fh,'Position',[0 0 w h]);
    % set(fh,'Visible','off');
    cFKaxes.temp = subplot(rowsOfAxes,colsOfAxes,1);
    cFKaxes.energy = subplot(rowsOfAxes,colsOfAxes,3)
    cFKaxes.x1 = subplot(rowsOfAxes,colsOfAxes,5);
    cFKaxes.chainwidth = subplot(rowsOfAxes,colsOfAxes,7);
    cFKaxes.sol = subplot(rowsOfAxes,colsOfAxes,[2,rowsOfAxes*colsOfAxes]);
end
