function [ reservoirData ] = trackReservoirs( fluidMoleculeZs, pistonsZs, reservoirTimes, cntReservoirGeometry, settings )
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

if isfield(settings,'plot')
    r = reservoirData;
    tlabel='t (ns)';

    figure;
    plot(r.times,r.topCount+r.btmCount);
    gussy(settings.plot,tlabel,'Total Water in Reservoirs');

    figure;
    plot(r.times,r.topCount);
    hold on;
    plot(r.times,r.btmCount,'r');
    gussy(settings.plot, tlabel, 'Water in top and bottom reservoirs');
    legend('top','bottom');

    figure;
    netQ =(r.topCount-r.btmCount)/2); 
    plot(r.times,NetQ);
    gussy(settings.plot, tlabel, 'Net Flow Estimate');

    Qrate = diff(netQ) / (r.times(2)-r.times(1));
    Qrate = abs( Qrate );
    [ QrateSm, ~ ] = savitzyGolaySmoothing( 2, settings.smoothingWindow, Qrate, 1 );
    QrateSm(end+1:length(r.times))=0;
    plot(r.times,QrateSm);
    gussy(settings.plot, tlabel, 'chain hops per ns');
end

end
