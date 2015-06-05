%
% The main program to get the simulation data from cFK simulations
%

%
% Run the file that sets the parameters:
% dcdPath, dcds, dcdSettings, dcdFlags, dataToSave, optionalFunctions, dcdPruneStrings
%
cFKparameters; 

[ cFKpruned ]   = pruneFileList( cFKs, pruneStrings ); % Prune the list according to patterns
clear savedData;
savedData(length(cFKpruned)) = struct(); % Initialize empty structure array to hold saved data


startfile=1;
endfile=length(cFKpruned);
ensList=cell(1,endfile-startfile);
if 1
    rowsOfAxes=3; %endfile-startfile
    colsOfAxes=2; %endfile-startfile
    %xAxes = figuregrid(rowsOfAxes,1);
    %tempAxes = figuregrid(rowsOfAxes,1);
    figure('Position',[100 100 1000 800]);
    tempAxes = subplot(rowsOfAxes,colsOfAxes,1);
    xAxes = subplot(rowsOfAxes,colsOfAxes,3);
    widthAxes = subplot(rowsOfAxes,colsOfAxes,5);
    solAxes = subplot(rowsOfAxes,colsOfAxes,[2,6]);
end

for cFKi=startfile:endfile
     
    % if dcdFlags == 'useLastDcd' && dcdi>1; display(sprintf('skipping dcd %i',dcdi)); continue; end
    % if dcdFlags ~= 'useLastDcd' || ~exist( 'xyzs', 'var' );

    cFKfullFile = [paths.projectStor '/' cFKpruned{cFKi}];
    cFKfiles    = getcFKfiles( cFKfullFile );
    
    [ cFKsimParams ]                                                 = getcFKsimParams( cFKfiles.log );
    ensList{cFKi} = cFKsimParams.ensStr;

    % 
    % Read in the data in the dcd file
    %
    [ Uxs, ts, sol, solo, fn ]  = readFortran( cFKfiles.Ux, 1 );
    [ xs, ts, sol, solo, fn ]   = readFortran( cFKfiles.x, 1.228e-10 );
    
    %
    % Process the xyz data
    %
    [ reducedTimes, timeIndexes ] = reduceTimes( ts, cFKsettings.shortTimeSteps );
    [ reducedXs ] = xs( timeIndexes, : );

    % TODO: unwrap the z coordinates using the PBCs

    % Sort trajectories on z-coordinate if requested
    % if {dcdSettings.sortTrajectory} == 'sortz'
    %     hostAtomsXyzs=sort(hostAtomsXyzs,2); %TODO: this is a temporary space holder, it's pseudocode
    % end
        
    if 1
        % axes(tempAxes(min(cFKi,length(tempAxes))));
        axes(tempAxes);
        plot(ts,Uxs(:,1)/1.38e-23/90*2,':'); hold on;
        title('Temperature'); ylabel('T (K)');
        if (cFKi==endfile) legend(ensList); end

        %axes(xAxes(min(cFKi,length(xAxes))));
        axes(xAxes);
        plot(ts,xs(:,1)); hold on;
        title('Position of particle 1'); ylabel('x (m)');

        axes(widthAxes);
        plot(ts,xs(:,end)-xs(:,1),':'); hold on;
        if (cFKi==endfile) horplot( (cFKsimParams.N-1)*cFKsimParams.a, '--'); end
        title('Chain Length'); xlabel('time (s)'); ylabel('length (m)');

        %axes(solAxes(min(cFKi,length(solAxes))));
        axes(solAxes);
        plot(sol(end,:)); hold on;
        title('Soliton at end of cooling'); xlabel('particle i'); ylabel('u_i');
        cFKparamStr = evalc('cFKsimParams');
        if (cFKi==endfile) text(0.05,0.8,cFKparamStr,'Units','normalized'); end
    end
    
    %
    % Modules for extra data processing.
    % External module scripts import data into the moduleData structure
    %
    if amember(moduleList,'fft')
        % [ hostAtomsFullXyzs, fullTimes ] = getHostAtomsTrajsAndReducet( xyzs, timeParams, atomsPerFluidMol, 0 ); % 3d (spatial dim, atom, timestep)
        fftSettings = struct( 'smoothingWindow', 15 );
        fftFlags = { 'plotOn' };
        % oneAtomX=squeeze(hostAtomsFullXyzs(1,1,:));
        % oneAtomZ=squeeze(hostAtomsFullXyzs(3,1,:));
        [ moduleData(cFKi).ffts ] = computeFFT( fftSettings, fftFlags, ts, xs ); 
        % moduleData(1).ffts.hostAtomsZ.smoothPower is the smoothed fft power of hostAtomsZ
        % clear hostAtomsFullXyzs fullTimes hostAtomsX hostAtomsZ;
    end

    if amember(moduleList,'makeMovie')

    end

    %
    % Save some data for easier data exploring, after this script is finished
    %
    savedData(cFKi).fn = cFKpruned{cFKi}; % for convenience, always save the dcd file name
    for saveIndex=1:length(dataToSave)
        savedData(cFKi).(dataToSave{saveIndex}) = eval( dataToSave{saveIndex} );
    end
    
end
