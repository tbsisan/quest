function [ fluidAtoms ] = guessFluidAtomsPerMol( fluidStr, justNanotubeAndFluidBool, namdFiles )
% gets number of fluid atoms per fluid molecule from the fluid string.  Fluid string should be obtained
% from another function or manually obtained.  The number of atoms is also predicted from the output of
% NAMD.  If the second method gives a different result, the fluidAtoms can be changed by +1 or -1, or else
% panics and sets fluidAtoms=1. A warning is output if the methods disagree.

disp('Guessing Fluid atoms per molecule');
% Hard code in the number of fluid atoms per molecule.
switch fluidStr
  case {'tip3p','tip3m','spe','tbs0p','ts26a','ts26b'}
    fluidAtoms=3;
  case 'tip42005p'
    fluidAtoms=4;
  case 'lj'
    fluidAtoms=1;
  case {'cgp0g','cgp1g','cgp2g','cgp3g','cgp4g','cgp5g','cgp6g'}
    fluidAtoms=1;
  otherwise
    fluidAtoms=3;
    display(sprintf('\tWARNING: fluid atom did not match any known type in guessAtomsPerFluidMol'));
end

%
% Guess fluid atoms per molecule.
%

% Get file name of the pdb file. First check the normal namd output location (namdFiles.outfStd), then the backup location (namdFiles.outfState).
[stat,str]=system(['sed -n "/^Info: COORDINATE PDB/"p <' namdFiles.outfStd]);
if (~stat && length(str)>0)
    pdbCell=regexp(str,'[a-z0-9_]+\.pdb','match');
    pdbFile=pdbCell{1};
else
    [stat,str]=system(['sed -n "/^Info: COORDINATE PDB/"p <' namdFiles.outfState]);
    if (~stat && length(str)>0)
        pdbCell=regexp(str,'[a-z0-9_]+\.pdb','match');
        pdbFile=pdbCell{1};
    end
end

% Count the number of atoms that are not carbon, silicon, boron, nitrogen
[stat, str] = system(['awk "!/ CA| BA| NA| SA/ {count++} END {print count}" <' namdFiles.pdbf]);
if (~stat)
    nonTubes = str2num(str);
    if rem(nonTubes,fluidAtoms)==0
        disp(sprintf('\tNumber of fluid atoms per mol confirmed by dcd name and pdb file: %i', fluidAtoms));
    else
        if ~justNanotubeAndFluidBool
	        disp(sprintf('\tAssuming a system with water in reservoirs, keeping fluid atoms per molecule: %i',fluidAtoms));
        else
	        fluidAtoms=fluidAtoms+1;
	        if rem(nonTubes,fluidAtoms)==0
	            disp(sprintf('\tFLUID ATOM DISCREPENCY: changing number of fluid Atoms by +1'));
	            disp(sprintf('\tNumber of fluids expected was %i but determined from non CAs was %i',fluidAtoms,nonTubes));
	        else
	            fluidAtoms=fluidAtoms-2;
	            if rem(nonTubes,fluidAtoms)==0
	                disp(sprintf('\tFLUID ATOM DISCREPENCY: changing number of fluid Atoms by -1 to %i', fluidAtoms));
	            else
	                disp(sprintf('\tFLUID ATOM DISCREPENCY: warning no good number of fluid atoms per mol, setting to 1'));
	                fluidAtoms=1;
	            end
	        end
        end
    end
else
    disp(sprintf('WARNING:Did not determine the number fluid atoms using awk of pdb file in %s',mfilename));
end

end

