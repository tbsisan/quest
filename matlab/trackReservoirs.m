function [ reservoirData ] = trackReservoirs( fluidMoleculeZs, pistonsZs, reservoirTimes, cntReservoirGeometry )
% For a system with two reservoirs, return the difference in the number of water molecules in the two reservoirs
% Expects the z coordinate of the fluid molecule locations.  Usually this can be the location of the "central" atom in the fluid.

% Count up fluids in reservoirs.
topBool = fluidMoleculeZs > cntReservoirGeometry.membraneTop;
btmBool = fluidMoleculeZs < cntReservoirGeometry.membraneBtm;
reservoirData.topCount = sum(topBool);
reservoirData.btmCount = sum(btmBool);
%topReservoirNet=topReservoirCount-topReservoirCount(1);
%btmReservoirNet=btmReservoirCount-btmReservoirCount(1);

% Total water in the simulation is a constant
% Theoretically, topQ == -btmQ, but water is compressible and extras can squeeze in the channel (or empty from the channel).
% Averaging them together should smooth out this error, as shown below.
% Qestimate = ( topReservoirNet+(-btmReservoirNet)) / 2;

reservoirData.topPistonZ = mean(pistonsZs.top);
reservoirData.btmPistonZ = mean(pistonsZs.btm);
reservoirData.times = reservoirTimes;
