function [ fh, cFKaxes ] = cFKenergyFigure( w, h, cFKflags )
    rowsOfAxes=5; %endfile-startfile
    colsOfAxes=1; %endfile-startfile
    %xAxes = figuregrid(rowsOfAxes,1);
    %tempAxes = figuregrid(rowsOfAxes,1);
    [ fh ] = launchFigure( cFKflags );
    set(fh,'Position',[10 10 w h]);
    % set(fh,'Visible','off');
    cFKaxes.kinetic = subplot(rowsOfAxes,colsOfAxes,1);
    bumpDimension(cFKaxes.kinetic,3,0.8)
    cFKaxes.spring = subplot(rowsOfAxes,colsOfAxes,2)
    bumpDimension(cFKaxes.spring,3,0.8)
    cFKaxes.substrate = subplot(rowsOfAxes,colsOfAxes,3);
    bumpDimension(cFKaxes.substrate,3,0.8)
    cFKaxes.total = subplot(rowsOfAxes,colsOfAxes,4);
    bumpDimension(cFKaxes.total,3,0.8)
    cFKaxes.solitons = subplot(rowsOfAxes,colsOfAxes,5);
    bumpDimension(cFKaxes.solitons,3,0.8)
    % cFKaxes.labels = subplot(rowsOfAxes,colsOfAxes,[2,10]);
end

function bumpDimension(ax,dim,amnt)
    rect = get(ax,'Position')
    rect(dim)=rect(dim)*amnt;
    set(ax,'Position',rect);
end
