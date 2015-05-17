function [ atomIndexes ] = fluidAtomIndexes( fnData, atomsPerFluid, paths )
% Get the vmd indexes for all fluid atoms
    if (fnData.justNanotubeAndFluid)
        fluidAtomFirst = (fnData.cntL * 4 * fnData.nm1) + 1;
        fluidAtomLast  = fluidAtomFirst + (fnData.fluidQuantity * atomsPerFluid) - 1;
        atomIndexes = fluidAtomFirst:1:fluidAtomLast;
    else % Set the fluid atoms to be the non ring-bonded (non CA, non BA, non SA, non NA) atoms.  
        tclFullFile = [paths.scriptPath '/vmd/outputFluidIndexes.tcl'];        
        indexOutputFile = [paths.projectStor '/indexes/' fnData.simName '.oi'];
        [status] = system(sprintf('vmd -dispdev text -pdb %s -e %s -args %s', pdbFullFile, tclFullFile, indexOutputFile),'-echo');
        % TODO: remove comment: vmd -dispdev text -eofexit -e makeoi.tcl -args $coords $projStor/indexes/$canonicalLeaf.oi >> $vmdOutFile
    end
end
