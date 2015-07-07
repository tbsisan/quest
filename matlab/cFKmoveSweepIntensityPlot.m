% Make phase space (g,a) movie For cFK free end chains to check if solitons form.
% At every temperature listed, sweep through all g (spring constant) values and a (spring resting length) values.
% The solitonCount module in cFKparameters.m should be turned on to calculate when solitons form.

runinstance=runinstance+1;
T=      [...
        '5.00E+00';...
        '1.50E+01';...
        '4.50E+01';...
%        '3.16E+01';...
%        '5.62E+01';...
%        '7.70E+01';...
%        '1.00E+02';...
%        '1.20E+02';...
%        '1.40E+02';...
       ]; 
cFKparameters;
[ fh ] = launchFigure( cFKflags );
figTitle = 'Time when soliton generated';
[ textHandle ] = text( 0.05, 0.95, sprintf('Temp: %.2f (K)',str2num(T(1,:))),'Units','normalized');
cFKplot( [], [], 1, 1, 'o', figTitle, 'a/lambda', 'g', {}, 1 );
%set( gca , 'NextPlot' , 'replacechildren' ) ;
hold on;
ax=gca;
set(ax,'Units','pixels');
%ax.Units = 'pixels';
pos = get(ax,'Position');
ti = get(ax,'TightInset');
rect = [-ti(1)-10, -ti(2)-10, pos(3)+ti(1)+ti(3)+20, pos(4)+ti(2)+ti(4)+20];
cFKmovie(length(T)) = struct('cdata',[],'colormap',[]);
if (~isOctave)
    writerObj = VideoWriter([paths.movieStor '/movePartial' num2str(runinstance) '.avi']);
    writerObj.FrameRate = 1;
    open(writerObj);
end
clear saveM;
    text( 0.05, 0.9,  {   sprintf(    'frame %i / %i', 0, length(T) ); ...
                                    sprintf(   'Temp: %.0f (K)', str2num(T(1,:)) ) ...
                                    } );

for Ti=1:size(T,1)
    cFKpattern=['x.movePartial*_M6*a*k*h1.53*T' T(Ti,:) '*n5.00E+09*F0*dat']
    cFKprocessor;
    % cFKplot( [], [], 1, 1, 'o', figTitle, 'a/lambda', 'g', {}, 1 );
    %[ textHandle ] = text( 0.05, 0.9, sprintf('Temp: %.2f (K)',str2num(T(Ti,:))),'Units','normalized');
%    set( textHandle, 'String',  {   sprintf(    'frame %i / %i', Ti, length(T) ); ...
%                                    sprintf(   'Temp: %.0f (K)', str2num(T(Ti,:)) ) ...
%                                    } );
    aP = [saveParams(:).a]./[saveParams(:).lambda];
    g = [saveParams(:).kbar];
    aPM = reshape(aP,length(unique(g)),length(unique(aP)));
    gM = reshape(g,length(unique(g)),length(unique(aP)));
    dataM = reshape(solitonPresent,length(unique(g)),length(unique(aP)));
    %dataM2 = reshape(solitonGenesis,length(unique(g)),length(unique(aP)));
    %dataM3 = reshape(solitonFitRmse,length(unique(g)),length(unique(aP)));
    %dataM4 = reshape(solitonFitEgress,length(unique(g)),length(unique(aP)));
    [gMsort, sorti] = sort(gM);
    dataMsort=dataM(sorti(:,1),:);
    %dataM2sort=dataM2(sorti(:,1),:);
    %dataM3sort=dataM3(sorti(:,1),:);
    %dataM4sort=dataM4(sorti(:,1),:);
    [aPMsort,sorti]=sort(aPM,2);
    dataMsort=dataMsort(:,sorti(1,:));
    %dataM2sort=dataM2sort(:,sorti(1,:));
    %dataM3sort=dataM3sort(:,sorti(1,:));
    %dataM4sort=dataM4sort(:,sorti(1,:));
    if Ti==1
        surf(aPMsort',gMsort',dataMsort')
        colorbar;
        %shading interp;
    else
        axch=get(gca,'Children');
        set( axch(1), 'Zdata',dataMsort');
    end
    saveM(1:size(dataMsort,1),1:size(dataMsort,2),Ti)=dataMsort;
    %saveM2(1:size(dataM2sort,1),1:size(dataM2sort,2),Ti)=dataM2sort;
    %saveM3(1:size(dataM3sort,1),1:size(dataM3sort,2),Ti)=dataM3sort;
    %saveM4(1:size(dataM4sort,1),1:size(dataM4sort,2),Ti)=dataM4sort;
    hold off;
    %cFKplot( aP(kinkGroundState), g(kinkGroundState), Ti, 1, 'o', '', 'a/lambda', 'g', {}, 0 );
    %cFKplot( aP(~kinkGroundState), g(~kinkGroundState), Ti, 1, '.', '', 'a/lambda', 'g', {}, 0 );
    %mov%set(ax,'YScale','log');
    %mov%drawnow;
    pause(0.1);
        if isOctave
            pause(0.5);
        else
           %mov% cFKmovie(Ti) = getframe(gca,rect);
           %mov% writeVideo(writerObj,cFKmovie(Ti));
        end
end
pause(1);
%    set( textHandle, 'String',  {   sprintf(    'frame %i / %i', Ti, length(T) ); ...
%                                    sprintf(   'Temp: %.0f (K)', str2num(T(Ti,:)) ) ...
%                                    } );
solitonsCreated=saveM;
%solitonsCreated=saveM2;
%solitonSeemsToStay=saveM3;
%solitonEgressGuess=saveM4;
%save('pulling.mat','timeToEgress','solitonsCreated','solitonSeemsToStay','solitonEgressGuess');
if (~isOctave) close(writerObj); end
