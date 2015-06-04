function [ reducedXyzs, reducedTimes ] = getHostAtomsTrajsAndCompress( xyzs, timeParams, atomsPerMolecule, neededTimeSteps )
% return 3d matrix of coordinates with dimensions (3_aka_XYandZ,numMolecules,timeSteps)
% input format
% t1: x1 y1 z1 x2 y2 z2 x3 y3 z3
% t2: x1 y1 z1 x2 y2 z2
% output format 3d matrix
% Assumes the Host atom is the first atom of each molecule
% x
% atom1: t1 t2 t3 t4 ...
% atom2: t1 t2 t3 t4 ...
% ...
% y
% atom1: t1 t2 t3 t4 ...
% atom2: t1 t2 t3 t4 ...
% ...
% z
% atom1: t1 t2 t3 t4 ...
% atom2: t1 t2 t3 t4 ...
% ...
timeSteps=size(xyzs,1);
atomCoords=size(xyzs,2);
%display(sprintf('\nsize matrix=%i,%i',timeSteps,atomCoords));
%display(sprintf('\natomsPerMolecules=%i',atomsPerMolecule));
display(sprintf('Reducing trajectory data to only save %i Host Atoms trajectories',atomCoords/3/atomsPerMolecule));
xyzM=reshape(xyzs',3,atomsPerMolecule,atomCoords/3/atomsPerMolecule,timeSteps);

%
% Reduce the number of time steps returned by giving an integer number to return, or
% by giving a fraction of the total
%
timeReductionStr='Reducing time data using strategy: %s';
originalTimes=timeParams.framePeriodns*[0:1:timeSteps-1];
[ reducedTimes, timeIndexes ] = reduceTimes( originalTimes, neededTimeSteps );
reducedXyzs = squeeze(xyzM(:,1,:,timeIndexes));
display(sprintf('\treduced data size: %i,%i,%i',size(reducedXyzs,1),size(reducedXyzs,2),size(reducedXyzs,3)));

end

