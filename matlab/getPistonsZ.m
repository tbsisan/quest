function [ pistonsZ ] = getPistonsZ( pistonXyzs, nonFluidGeometry )
% Get average location of pistons versus time

disp(sprintf('\tConvertin piston xyz array into piston Z data for top and btm pistons'));
pistonZs = squeeze(pistonXyzs(3,:,:));
topPistonZsBool = pistonZs > nonFluidGeometry.systemMiddle;
btmPistonZsBool = pistonZs < nonFluidGeometry.systemMiddle;

pistonsZ.top = pistonZs( all( topPistonZsBool == 1 ), : );
pistonsZ.btm = pistonZs( all( btmPistonZsBool == 1 ), : );

