function [ reservoirSizes ] = trackReservoirs( fluidMoleculeZs, pistonsZs, cntReservoirGeometry )
% For a system with two reservoirs, return the difference in the number of water molecules in the two reservoirs
% Expects the z coordinate of the fluid molecule locations.  Usually this can be the location of the "central" atom in the fluid.

% Count up fluids in reservoirs.
topBool = fluidMoleculeZs > cntReservoirGeometry.membraneTop;
btmBool = fluidMoleculeZs < cntReservoirGeometry.membraneBtm;
reservoirSizes.topCount = sum(topBool);
reservoirSizes.btmCount = sum(btmBool);
%topReservoirNet=topReservoirCount-topReservoirCount(1);
%btmReservoirNet=btmReservoirCount-btmReservoirCount(1);

% Total water in the simulation is a constant
% Theoretically, topQ == -btmQ, but water is compressible and extras can squeeze in the channel (or empty from the channel).
% Averaging them together should smooth out this error, as shown below.
% Qestimate = ( topReservoirNet+(-btmReservoirNet)) / 2;

reservoirSizes.topPistonZ = mean(pistonsZs.top);
reservoirSizes.btmPistonZ = mean(pistonsZs.btm);
