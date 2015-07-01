runinstance=runinstance+1;
T=      [...
        '1.00E+01';...
        '1.78E+01';...
        '3.16E+01';...
        '5.62E+01';...
        '7.70E+01';...
        '1.00E+02';...
        '1.20E+02';...
        '1.40E+02';...
       ]; 
cFKparameters;
[ fh ] = launchFigure( cFKflags );
figTitle = 'Time to zero solitons';
[ textHandle ] = text( 0.05, 0.95, sprintf('Temp: %.2f (K)',str2num(T(1,:))),'Units','normalized');
cFKplot( [], [], 1, 1, 'o', figTitle, 'a/lambda', 'g', {}, 1 );
%set( gca , 'NextPlot' , 'replacechildren' ) ;
hold on;
ax=gca;
ax.Units = 'pixels';
pos = ax.Position;
ti = ax.TightInset;
rect = [-ti(1)-10, -ti(2)-10, pos(3)+ti(1)+ti(3)+20, pos(4)+ti(2)+ti(4)+20];
cFKmovie(length(T)) = struct('cdata',[],'colormap',[]);
writerObj = VideoWriter([paths.movieStor '/controllability' num2str(runinstance) '.avi']);
writerObj.FrameRate = 1;
open(writerObj);
clear saveSolM;

for Ti=1:size(T,1)
    cFKpattern=['x.oneSoliton*a*k*h1.53*T' T(Ti,:) '*n5.00E+09*F0*dat']
    cFKprocessor;
    % cFKplot( [], [], 1, 1, 'o', figTitle, 'a/lambda', 'g', {}, 1 );
    %[ textHandle ] = text( 0.05, 0.9, sprintf('Temp: %.2f (K)',str2num(T(Ti,:))),'Units','normalized');
    set( textHandle, 'String',  {   sprintf(    'frame %i / %i', Ti, length(T) ); ...
                                    sprintf(   'Temp: %.0f (K)', str2num(T(Ti,:)) ) ...
                                    } );
    aP = [saveParams(:).a]./[saveParams(:).lambda];
    g = [saveParams(:).g];
    aPM = reshape(aP,length(unique(g)),length(unique(aP)));
    gM = reshape(g,length(unique(g)),length(unique(aP)));
    solRmse = reshape(solitonFitRmse,length(unique(g)),length(unique(aP)));
    [gMsort, sorti] = sort(gM);
    solRmseSort=solRmse(sorti(:,1),:);
    if Ti==1
        surf(aPM',gMsort',solRmseSort')
        colorbar;
    else
        axch=get(gca,'Children');
        set( axch(1), 'Zdata',solRmseSort');
    end
    saveSolM(1:size(solRmseSort,1),1:size(solRmseSort,2),Ti)=solRmseSort;
    hold off;
    %cFKplot( aP(kinkGroundState), g(kinkGroundState), Ti, 1, 'o', '', 'a/lambda', 'g', {}, 0 );
    %cFKplot( aP(~kinkGroundState), g(~kinkGroundState), Ti, 1, '.', '', 'a/lambda', 'g', {}, 0 );
    set(ax,'YScale','log');
    drawnow;
    pause(0.1);
    cFKmovie(Ti) = getframe(gca,rect);
    writeVideo(writerObj,cFKmovie(Ti));
end
pause(1);
close(writerObj);
%    set( textHandle, 'String',  {   sprintf(    'frame %i / %i', Ti, length(T) ); ...
%                                    sprintf(   'Temp: %.0f (K)', str2num(T(Ti,:)) ) ...
%                                    } );
