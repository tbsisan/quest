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
for cFKi=startfile:endfile
     
    % if dcdFlags == 'useLastDcd' && dcdi>1; display(sprintf('skipping dcd %i',dcdi)); continue; end
    % if dcdFlags ~= 'useLastDcd' || ~exist( 'xyzs', 'var' );

    cFKfullFile = [paths.dataPath '/' cFKpruned{cFKi}];
    cFKfiles    = getcFKfiles( cFKfullFile );
    
    [ cFKdata ]                                                 = getcFKdata( cFKfiles.log );

    % 
    % Read in the data in the dcd file
    %
    [ xs, ts, sol, solo, fn ]   = readFortran( cFKfiles.x, 1.228e-10 );
    [ Uxs, ts, sol, solo, fn ]  = readFortran( cFKfiles.Ux, 1 );
    
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
        
    if 0
        cFKaxes = figuregrid(4,1);
    end
    
    %
    % Modules for extra data processing.
    % External module scripts import data into the moduleData structure
    %
    if moduleList == 'fft'
        % [ hostAtomsFullXyzs, fullTimes ] = getHostAtomsTrajsAndReducet( xyzs, timeParams, atomsPerFluidMol, 0 ); % 3d (spatial dim, atom, timestep)
        fftSettings = struct( 'smoothingWindow', 15 );
        fftFlags = { 'plotOn' };
        % oneAtomX=squeeze(hostAtomsFullXyzs(1,1,:));
        % oneAtomZ=squeeze(hostAtomsFullXyzs(3,1,:));
        [ moduleData(cFKi).ffts ] = computeFFT( fftSettings, fftFlags, ts, xs ); 
        % moduleData(1).ffts.hostAtomsZ.smoothPower is the smoothed fft power of hostAtomsZ
        % clear hostAtomsFullXyzs fullTimes hostAtomsX hostAtomsZ;
    end

    if moduleList == 'makeMovie'

    end

    %
    % Save some data for easier data exploring, after this script is finished
    %
    savedData(cFKi).fn = cFKpruned{cFKi}; % for convenience, always save the dcd file name
    for saveIndex=1:length(dataToSave)
        savedData(cFKi).(dataToSave{saveIndex}) = eval( dataToSave{saveIndex} );
    end
    
end
