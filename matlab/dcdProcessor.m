%
% The main program to get the coordinate data from NAMD simulations
%

%
% Run the file that holds the parameters
%
dcdParameters;

[ dcdPruned ]   = pruneDcdList( dcds,dcdPruneStrings ); % Prune the list according to patterns
savedData(length(dcdPruned)) = struct(); % Initialize empty structure array to hold saved data

startfile=1;
endfile=length(dcdPruned);
for dcdi=startfile:endfile
    
    if dcds ~= [1] % Set dcds to [1] to skip reading in new data
        dcdFullFile = [dcdPath '/' dcdPruned{dcdi}];
    
        %
        % Figure out important data from the dcd file name
        %
        [ namdFiles ]                                             = getNamdFileList( dcdFullFile );
        [ pbcx, pbcy, pbcz ]                                      = getPbcs( namdFiles.xscf );
        [ timeData ]                                              = timeDataFromNamd( namdFiles ); %timestep, runsteps, dcdFreq, framePeriodns, simLength
        [ dcdFnData ]                                             = parseDcdFn( dcdFullFile );
        [ ~, simName, ~ ]                                         = fileparts( dcdFullFile );
        [ atomsPerSolventMol ]                                    = guessAtomsPerSolventMol( dcdFnData.solventModelStr, namdFiles );
        solventAtomFirst = (dcdFnData.cntL * 4 * dcdFnData.nm1) + 1;
        solventAtomLast  = solventAtomFirst + (dcdFnData.solventQuantity * atomsPerSolventMol) - 1;
        atomsToGet = solventAtomFirst:1:solventAtomLast;

        % 
        % Read in the data in the dcd file
        %
        [ xyzs ] = readdcd( namdFiles.dcdf, atomsToGet );
    end
    
    %
    % Process the xyz data
    %
    [ hostAtomsXyzs ] = getHostAtomsTrajsAndReducet( xyzs, atomsPerSolventMol, dcdSettings.shortTimeSteps ); % 3d (spatial dim, atom, timestep)
    [ oneAtomXyzs ] = reduceToSingleTrajectory( hostAtomsXyzs, dcdSettings.oneAtomStrategy );

    % TODO: unwrap the z coordinates using the PBCs

    % Sort trajectories on z-coordinate if requested
    if dcdSettings.sortTrajectory == 'sortz'
        hostAtomsXyzs=sort(hostAtomsXyzs,2); %TODO: this is a temporary space holder, it's pseudocode
    end
        
    
    oneAtomXs = oneAtomXyzs(1,:));
    oneAtomYs = oneAtomXyzs(2,:));
    oneAtomZs = oneAtomXyzs(3,:));
    oneAtomRs = sqrt( oneAtomXs.^2 + oneAtomYs.^2 );

    %
    % External module scripts import data into the moduleData structure
    %
    if optionalFunctions == 'fft'
        fftSettings = struct( 'smoothingWindow', 15 );
        fftFlags = { 'plotOn' };
        [ moduleData.ffts ] = computeFft( fftSettings, fftFlags, { oneAtomZs, oneAtomXs } );
    end

    %
    % Save some data for easier data exploring, after this script is finished
    %
    for saveIndex=1:length(toSave)
        savedData(dcdi).dcd = simName; % for convenience, always save the dcd file name
        savedData(dcdi).(toSave{saveIndex}) = eval( toSave{saveIndex} );
    end
    
end
