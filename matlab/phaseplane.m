runinstance=runinstance+1;
T=      ['1.00E+00';...
        '1.78E+00';...
        '3.16E+00';...
        '5.62E+00';...
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
figTitle = 'Final number solitons';
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
writerObj = VideoWriter([paths.movieStor '/phaseplane' num2str(runinstance) '.avi']);
writerObj.FrameRate = 1;
open(writerObj);
clear saveSolM;

for Ti=1:size(T,1)
    cFKpattern=['x.*Ti000_L250*a1*k*h1.53*T' T(Ti,:) '*n1.00E+10*F0*dat']
    cFKprocessor;
    groundStateSolitons = abs(moduleData.solitons.aP-1) .* moduleData.solitons.N
    kinkGroundState = numSolitons>(groundStateSolitons*0.7);
    % cFKplot( [], [], 1, 1, 'o', figTitle, 'a/lambda', 'g', {}, 1 );
    %[ textHandle ] = text( 0.05, 0.9, sprintf('Temp: %.2f (K)',str2num(T(Ti,:))),'Units','normalized');
    set( textHandle, 'String',  {   sprintf(    'frame %i / %i', Ti, length(T) ); ...
                                    sprintf(   'Temp: %.0f (K)', str2num(T(Ti,:)) ) ...
                                    } );
    aP = moduleData.solitons.aP;
    g = moduleData.solitons.g;
    aPM = reshape(aP,length(unique(g)),length(unique(aP)));
    gM = reshape(g,length(unique(g)),length(unique(aP)));
    solM = reshape(numSolitons,length(unique(g)),length(unique(aP)));
    [gMsort, sorti] = sort(gM);
    solMsort=solM(sorti(:,1),:);
    if Ti==1
        surf(aPM',gMsort',solMsort')
        colorbar;
    else
        axch=get(gca,'Children');
        set( axch(1), 'Zdata',solMsort');
    end
    saveSolM(1:size(solMsort,1),1:size(solMsort,2),Ti)=solMsort;
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
