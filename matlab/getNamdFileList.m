function [ namdFiles ] = getNamdFileList( dcdf )
%parsing the data from the name of the dcd file
%still a workin in progress
    namdFiles.dcdf=dcdf;
    namdFiles.veldcdf=regexprep(dcdf,'dcd$','veldcd');
    namdFiles.fdcdf=regexprep(dcdf,'dcd$','forcedcd');
    namdFiles.outfState=regexprep(dcdf,'dcd$','out');
    namdFiles.outfStd=regexprep(namdFiles.outfState,'namdState','namdStdOut');
    namdFiles.xscf=regexprep(dcdf,'dcd$','xsc');

end

