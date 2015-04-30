function [ fluidAtoms ] = guessFluidAtomsPerMol( fluidStr, namdFiles )
%parsing the data from the name of the dcd file
%still a workin in progress

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
[stat, str] = system(['awk "!/ CA| BA| NA| SA/ {count++} END {print count}" <' pdbFile]);
if (~stat)
  nonTubes = str2num(str);
  if rem(nonTubes,fluidAtoms)==0
    disp(['Number of fluid atoms per mol confirmed by dcd name and pdb file ' num2Str(fluidAtoms)]);
  else
    fluidAtoms=fluidAtoms+1;
    if rem(nonTubes,fluidAtoms)==0
      disp(['FLUID ATOM DISCREPENCY: changing number of fluid Atoms by +1 to ' num2Str(fluidAtoms)]);
    else
      fluidAtoms=fluidAtoms-2;
      if rem(nonTubes,fluidAtoms)==0
        disp(['FLUID ATOM DISCREPENCY: changing number of fluid Atoms by -1 to ' num2Str(fluidAtoms)]);
      else
        disp('FLUID ATOM DISCREPENCY: warning no good number of fluid atoms per mol, setting to 1');
        fluidAtoms=1;
      end
    end
  end
end

end

