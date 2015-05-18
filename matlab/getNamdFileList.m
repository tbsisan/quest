function [ namdFiles ] = getNamdFileList( dcdf, paths )
% Parsing the data from the name of the dcd file.
% The fullnes of the paths to files is largely determined by the input dcdf parameter.
% It is intended that dcdf be input with an absolute path to the dcd file.
    namdFiles.dcdf=dcdf;
    namdFiles.veldcdf=regexprep(dcdf,'dcd$','veldcd');
    namdFiles.fdcdf=regexprep(dcdf,'dcd$','forcedcd');
    namdFiles.outfState=regexprep(dcdf,'dcd$','out');
    namdFiles.outfStd=regexprep(namdFiles.outfState,'namdState','namdStdOut');
    namdFiles.xscf=regexprep(dcdf,'dcd$','xsc');
    [~,psfFile] = system( ['awk "/Info: STRUCTURE FILE/ {print \$4; exit}" <' namdFiles.outfStd] );
    namdFiles.psff = strtrim([paths.projectHome '/psfs/' psfFile]);
    namdFiles.pdbf = regexprep(namdFiles.psff,'psf','pdb'); % needs to change psf in 2 places
end

