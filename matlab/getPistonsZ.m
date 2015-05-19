function [ pistonsZ ] = getPistonsZ( pistonXyzs, nonFluidGeometry )
% Get average location of pistons versus time

pistonZs = pistonXyzs(:,3:3:end);
topPistonZsBool = pistonZs > nonFluidGeometry.systemMiddle;
btmPistonZsBool = pistonZs < nonFluidGeometry.systemMiddle;

pistonsZ.top = pistonZs( :, all( topPistonZsBool == 1 ) );
pistonsZ.btm = pistonZs( :, all( btmPistonZsBool == 1 ) );

