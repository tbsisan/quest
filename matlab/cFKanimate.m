function [ plotHandle, cFKmovie ] = cFKanimate( ts, sols, temps, cFKsimParams, cFKflags, paths )
    global isOctave angstrom;
    blue = 1; thin = 1;
    an.title = sprintf('u_i during a portion of %.1f ns cFK polymer sim',cFKsimParams.simSeconds*1e9);
    an.xlabel = 'Particle number';
    an.ylabel = ['u_i (' angstrom ')'];
    an.legend = '';
    an.labelflag = 1;
    sols = sols *1e10;
    particles = 1:size(sols,2);
    [ fh ] = launchFigure( cFKflags );
    set(fh,'Position',[10 10 800 600]);
    axis([1 particles(end) min(sols(:)) max(sols(:,end))]);
    axis manual; hold all;
    [ plotHandle ] = cFKplot( particles, sols(1,:), blue, thin, '.', an.title, an.xlabel, an.ylabel, an.legend, an.labelflag );
    [ textHandle ] = text( 0.05, 0.9, sprintf('time: %.2f (ns)',ts(1)),'Units','normalized');
    [ infoHandle ] = text( 0.60, 0.7, sprintf(cFKsimParams.allStr),'Units','normalized');
    ax=gca;
    set(ax,'Units','pixels');
    %ax.Units = 'pixels';
    pos = get(ax,'Position');
    ti = get(ax,'TightInset');
    rect = [-ti(1), -ti(2), pos(3)+ti(1)+ti(3), pos(4)+ti(2)+ti(4)];

    cFKmovie(length(ts)) = struct('cdata',[],'colormap',[]);
    % writerObj = VideoWriter('test.avi','Indexed AVI');
    if ~isOctave
        writerObj = VideoWriter([paths.movieStor '/' cFKsimParams.fn '.avi']);
        writerObj.FrameRate = 10;
        open(writerObj);
    end
    for t=1:length(ts)
        set( plotHandle, 'Ydata', sols(t,:) );
        set( textHandle, 'String',  {   sprintf(    'frame %i / %i', t, length(ts) ); ...
                                        sprintf(   'time: %.2f (ns)', ts(t)*1e9 ); ...
                                        sprintf(   'T: %.0f (K)', temps(t) ) ...

                                    } );
        drawnow;
        %pause(0.1);
        ~isOctave && (cFKmovie(t) = getframe(gca,rect));
        if isOctave
            pause(0.5);
        else
            writeVideo(writerObj,cFKmovie(t));
        end
    end
    ~isOctave && close(writerObj);
    close(fh);

end
