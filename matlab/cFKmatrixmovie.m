dataM=((saveSolM));
expectedSol=(aPM-1)*200
X=aPM';
Y=gMsort';
figTitle = 'Final number solitons';
xTitle = 'a/lambda';
yTitle = 'g';

[ fh ] = launchFigure( cFKflags );
hold on;
% [ textHandle ] = text( 0.05, 0.95, sprintf('Temp: %.2f (K)',str2num(T(1,:))),'Units','normalized');
cFKplot( [], [], 1, 1, 'o', figTitle, xTitle, yTitle, {}, 1 );

ax=gca;
ax.Units = 'pixels';
pos = ax.Position;
ti = ax.TightInset;
rect = [-ti(1)-10, -ti(2)-10, pos(3)+ti(1)+ti(3)+20, pos(4)+ti(2)+ti(4)+20];

cFKmovie(size(saveSolM,3)) = struct('cdata',[],'colormap',[]);

writerObj = VideoWriter([paths.movieStor '/phaseplane' movieName '.avi']);
writerObj.FrameRate = 1;
open(writerObj);

[textHandle ] = text( 0.05,0.95, {   sprintf(    'frame %i / %i', 0, length(T) ); ...
                         sprintf(   'Temp: %.0f (K)', 0 ) ...
                         },'Units','normalized' );
for datai=1:size(saveSolM,3)

    plotData=squeeze(dataM(:,:,datai))./expectedSol;

    if datai==1
        contourLevels=[0 1 2 5:5:40];
        %s=contour(X,Y,dataM(:,:,datai)',contourLevels); %logspace(0,log10(40),10)])
        s=surf(X,Y,plotData')
        caxis([0 2]);
        colormap(parula(41));
        colorbar;
        shading interp;
        s.LineStyle='None';
        ax=gca;
        ax.NextPlot = 'replaceChildren';
    else
        axch=get(gca,'Children');
        set( axch(1), 'Zdata',plotData');
        % s=surf(X,Y,plotData')
        caxis([0 2]);
        %s=contour(ax,X,Y,dataM(:,:,datai)',contourLevels);%logspace(0,log10(40),10)])
        %colorbar;
    end

    set(ax,'YScale','log');
    set( textHandle, 'String',  {   sprintf(    'frame %i / %i', datai, length(T) ); ...
                                    sprintf(   'Temp: %.0f (K)', str2num(T(datai,:)) ) ...
                                    } );
%    text( 0.05,0.95, {   sprintf(    'frame %i / %i', datai, length(T) ); ...
%                         sprintf(   'Temp: %.0f (K)', str2num(T(datai,:)) ) ...
%                         },'Units','normalized' );
    drawnow;
    pause(.1);
    cFKmovie(datai) = getframe(gca,rect);
    writeVideo(writerObj,cFKmovie(datai));
    hold on;

end
writeVideo(writerObj,cFKmovie(datai));
writeVideo(writerObj,cFKmovie(datai));
close(writerObj);
