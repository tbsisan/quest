function [ plotHandle, cFKmovie ] = cFKanimate( ts, xs, sols, temps, cFKsimParams, cFKflags, paths )
% function [ plotHandle, cFKmovie ] = cFKanimate( ts, sols, temps, cFKsimParams, cFKflags, paths )
    global isOctave angstrom;
    blue = 1; red=2; green=3; thin = 1;
    an.title = sprintf('u_i during a portion of %.1f ns cFK polymer sim',cFKsimParams.simSeconds*1e9);
    an.xlabel = 'Particle number';
    an.ylabel = ['u_i (' angstrom ')'];
    an.legend = '';
    an.labelflag = 1;
    sols = sols *1e10;
    particles = 1:size(sols,2);
    [ fh ] = launchFigure( cFKflags, 800, 600 );

    aa.ui = subplot(4,1,[1,3]);
    aa.port = subplot(4,1,4);

    axes(aa.ui);
    axis([1 particles(end) min(sols(:)) max(sols(:))]);
    axis manual; hold all;

    axes(aa.port);
    startPort=20;
    endPort=40;
    axis([particles(end)-10 particles(end)+2 -2 2]);
    axis([ startPort endPort -2 2]);
    axis manual; hold all;

    uiBar = sols/(cFKsimParams.lambda*1e10);
    uiInt = round(uiBar);
    axes(aa.ui)
    [ plotHandle ] = cFKplot( particles, sols(1,:), blue, thin, '.', an.title, '', an.ylabel, an.legend, an.labelflag );
    [ plotHandleInt ] = cFKplot( particles, uiInt(1,:), red, thin, '-o', an.title, '', an.ylabel, an.legend, an.labelflag );
    [ textHandle ] = text( 0.05, 0.9, sprintf('time: %.2f (ns)',ts(1)),'Units','normalized');
    [ infoHandle ] = text( 0.60, 0.7, sprintf(cFKsimParams.allStr),'Units','normalized');
    axes(aa.port)
    [ portHandle ] = cFKplot( xs(1,startPort-10:endPort+10)/cFKsimParams.lambda, cos(2*pi*xs(1,startPort-10:endPort+10)/cFKsimParams.lambda), red, thin, 'o', '', an.xlabel, '', an.legend, an.labelflag );
    [ particleHandle ] = cFKplot( xs(1,round((startPort+endPort)/2))/cFKsimParams.lambda, cos(2*pi*xs(1,round((startPort+endPort)/2))/cFKsimParams.lambda), green, thin, 'o', '', an.xlabel, '', an.legend, an.labelflag );
    thetas=[particles(end)-11:0.01:particles(end)+2];
    plot(thetas,cos(thetas*2*pi),'b-');
    %verticalLine(59+0.5+0.7,'-',0);
    %set(aa.ui,'Units','pixels');
    %set(aa.port,'Units','pixels');
    %ax.Units = 'pixels';

    cFKmovie(length(ts)) = struct('cdata',[],'colormap',[]);
    % writerObj = VideoWriter('test.avi','Indexed AVI');
    if ~isOctave
        videoName = [paths.movieStor '/' cFKsimParams.fn '.avi'];
        writerObj = VideoWriter(videoName);
        writerObj.FrameRate = 2;
        open(writerObj);
    end

    for t=1:length(ts)
        axes(aa.ui)
        set( plotHandle, 'Ydata', sols(t,:) );
        set( plotHandleInt, 'Ydata', uiInt(t,:) );
        set( textHandle, 'String',  {   sprintf(    'frame %i / %i', t, length(ts) ); ...
                                        sprintf(   'time: %.2f (ns)', ts(t)*1e9 ); ...
                                        sprintf(   'T: %.0f (K)', temps(t) ) ...

                                    } );
        drawnow;
        axes(aa.port);
        set( portHandle, 'Xdata', xs(t,startPort-10:endPort+10)/cFKsimParams.lambda );
        set( portHandle, 'Ydata', cos(2*pi*xs(t,startPort-10:endPort+10)/cFKsimParams.lambda) );
        set( particleHandle, 'Xdata', xs(t,round((startPort+endPort)/2))/cFKsimParams.lambda );
        set( particleHandle, 'Ydata', cos(2*pi*xs(t,round((startPort+endPort)/2))/cFKsimParams.lambda) );
        drawnow;
        %pause(0.1);
        if isOctave
            pause(0.5);
        else
            cFKmovie(t) = getframe( fh );
            writeVideo(writerObj,cFKmovie(t));
        end
    end

    if (~isOctave) close(writerObj); end
    close(fh);
    disp(videoName);

end
