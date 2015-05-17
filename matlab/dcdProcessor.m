%
% The main program to get the coordinate data from NAMD simulations
%

%
% Run the file that sets the parameters:
% dcdPath, dcds, dcdSettings, dcdFlags, dataToSave, optionalFunctions, dcdPruneStrings
%
dcdParameters; 

[ dcdPruned ]   = pruneDcdList( dcds,dcdPruneStrings ); % Prune the list according to patterns
clear savedData;
savedData(length(dcdPruned)) = struct(); % Initialize empty structure array to hold saved data

startfile=1;
endfile=length(dcdPruned);
for dcdi=startfile:endfile
    
    if dcdFlags == 'useLastDcd' && dcdi>1; display(sprintf('skipping dcd %i',dcdi)); continue; end
    if dcdFlags ~= 'useLastDcd'
        dcdFullFile = [paths.dcdPath '/' dcdPruned{dcdi}];
    
        %
        % Figure out important data from the dcd file name
        %
        [ namdFiles ]                                             = getNamdFileList( dcdFullFile, paths );
        [ pbcx, pbcy, pbcz ]                                      = getPbcs( namdFiles.xscf );
        [ timeParams ]                                            = timeDataFromNamd( namdFiles ); %timestep, expectedSteps, dcdFreq, framePeriodns, expectedSimLength
        [ dcdFnData ]                                             = parseDcdFn( dcdPruned{dcdi} );
        [ atomsPerFluidMol ]                                      = guessAtomsPerFluidMol( dcdFnData.fluidModelStr, namdFiles );
        [ atomsToGet ]                                            = fluidAtomIndexes ( dcdFnData, atomsPerFluidMol, paths );

        % 
        % Read in the data in the dcd file
        %
        [ xyzs ] = readdcd( namdFiles.dcdf, atomsToGet );
    end
    
    %
    % Process the xyz data
    %
    [ hostAtomsXyzs, reducedTimes ] = getHostAtomsTrajsAndReducet( xyzs, timeParams, atomsPerFluidMol, dcdSettings.shortTimeSteps ); % 3d (spatial dim, atom, timestep)
    [ oneAtomXyzs ] = reduceToSingleTrajectory( hostAtomsXyzs, dcdSettings.oneAtomStrategy );

    % TODO: unwrap the z coordinates using the PBCs

    % Sort trajectories on z-coordinate if requested
    if {dcdSettings.sortTrajectory} == 'sortz'
        hostAtomsXyzs=sort(hostAtomsXyzs,2); %TODO: this is a temporary space holder, it's pseudocode
    end
        
    
    oneAtomXs = oneAtomXyzs(1,:);
    oneAtomYs = oneAtomXyzs(2,:);
    oneAtomZs = oneAtomXyzs(3,:);
    oneAtomRs = sqrt( oneAtomXs.^2 + oneAtomYs.^2 );

    %
    % Modules for extra data processing.
    % External module scripts import data into the moduleData structure
    %
    if moduleList == 'fft'
        [ hostAtomsFullXyzs, fullTimes ] = getHostAtomsTrajsAndReducet( xyzs, timeParams, atomsPerFluidMol, 0 ); % 3d (spatial dim, atom, timestep)
        fftSettings = struct( 'smoothingWindow', 15 );
        fftFlags = { 'plotOn' };
        hostAtomsX=squeeze(hostAtomsFullXyzs(1,1,:));
        hostAtomsZ=squeeze(hostAtomsFullXyzs(3,1,:));
        [ moduleData(dcdi).ffts ] = computeFFT( fftSettings, fftFlags, fullTimes, hostAtomsZ, hostAtomsX ); 
        % moduleData(1).ffts.hostAtomsZ.smoothPower is the smoothed fft power of hostAtomsZ
        clear hostAtomsFullXyzs fullTimes hostAtomsX hostAtomsZ
    end

    if moduleList == 'makeMovie'

    end
    %
    % Save some data for easier data exploring, after this script is finished
    %
    savedData(dcdi).dcd = dcdFnData.simName; % for convenience, always save the dcd file name
    for saveIndex=1:length(dataToSave)
        savedData(dcdi).(dataToSave{saveIndex}) = eval( dataToSave{saveIndex} );
    end
    
end
