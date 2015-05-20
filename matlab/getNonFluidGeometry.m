function [ nonFluidGeometry ] = getNonFluidGeometry( pdb, geometryFile, paths );
% Returnthe locations of the nanotube ends, and the graphene sheets

disp(sprintf('\tReading in system geometry of nonFluid atoms from geometry file %s',geometryFile));
tclFullFile = [paths.scriptPath '/vmd/outputNonFluidGeometry.tcl'];
[ stat, outp ] = system(sprintf('vmd -dispdev text -pdb %s -e %s -args %s',pdb,tclFullFile,geometryFile), '-echo');
geometryData = dlmread(geometryFile);
systemMiddle = mean(geometryData(:));
nonFluidGeometry = struct(  ...
                            'cntBtm', geometryData(1,1), ...
                            'cntTop', geometryData(1,2), ...
                            'membraneBtm', geometryData(2,1), ...
                            'membraneTop', geometryData(2,2), ...
                            'pistonBtm', geometryData(3,1), ...
                            'pistonTop', geometryData(3,2), ...
                            'systemMiddle', systemMiddle ...
                            );
