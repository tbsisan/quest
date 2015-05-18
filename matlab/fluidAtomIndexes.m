function [ atomIndexes ] = fluidAtomIndexes( fnData, atomsPerFluid, paths, pdbFullFile )
% Get the vmd indexes for all fluid atoms
    if (fnData.justNanotubeAndFluid)
        fluidAtomFirst = (fnData.cntL * 4 * fnData.nm1) + 1;
        fluidAtomLast  = fluidAtomFirst + (fnData.fluidQuantity * atomsPerFluid) - 1;
        atomIndexes = fluidAtomFirst:1:fluidAtomLast;
    else % Set the fluid atoms to be the non ring-bonded (non CA, non BA, non SA, non NA) atoms.  
        tclFullFile = [paths.scriptPath '/vmd/outputFluidIndexes.tcl'];        
        indexOutputFile = [paths.projectStor '/indexes/' fnData.simName '.oi'];
        [ status, outp ] = system(sprintf('vmd -dispdev text -pdb %s -e %s -args %s', pdbFullFile, tclFullFile, indexOutputFile));
        fileID = fopen('vmdindexoutput','w');
        fprintf(fileID,'%s',outp);
        fclose(fileID);
        disp(sprintf('\tReading coords from noninfinite system %s using index file .oi', indexOutputFile));
        atomIndexes=dlmread(indexOutputFile);
        disp(sprintf('\tNumber of fluid atoms to read in: %i',length(atomIndexes)));
        % TODO: remove comment: vmd -dispdev text -eofexit -e makeoi.tcl -args $coords $projStor/indexes/$canonicalLeaf.oi >> $vmdOutFile
    end
end
