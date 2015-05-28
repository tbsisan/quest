function [ pistonsZ ] = getPistonsZ( pistonXyzs, nonFluidGeometry )
% Get average location of pistons versus time

disp(sprintf('\tConvertin piston xyz array into piston Z data for top and btm pistons'));
pistonZs = squeeze(pistonXyzs(3,:,:));
topPistonZsBool = pistonZs > nonFluidGeometry.systemMiddle;
btmPistonZsBool = pistonZs < nonFluidGeometry.systemMiddle;

topInds = all(topPistonZsBool == 1, 2);
btmInds = all(btmPistonZsBool == 1, 2);
pistonsZ.top = pistonZs( topInds, : );
pistonsZ.btm = pistonZs( btmInds, : );

