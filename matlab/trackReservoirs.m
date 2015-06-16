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
    dt = r.times(2)-r.times(1);
    tlabel='t (ns)';

    figure;
    plot(r.times(1:end),r.topCount(1:end)+r.btmCount(1:end));
    labelFig(settings.plot,tlabel,'Total Water in Reservoirs',{});

    figure;
    plot(r.times(1:end),r.topCount(1:end));
    hold on;
    plot(r.times(1:end),r.btmCount(1:end),'r');
    labelFig(settings.plot, tlabel, 'Water in top and bottom reservoirs',{'top','bottom'});

    figure;
    netQ =(r.topCount-r.btmCount)/2;
    netQ=netQ-netQ(1);
    plot(r.times,netQ);
    hold on;
    [ netQsm, QdotSm ] = savitzyGolaySmoothing( 2, settings.smoothingWindow, netQ, 1 );
    disp('WARNING UNTESTED CHANGES DUE TO CHANGES in savitzyGolaySmoothing return value');
    %netQsm(end+1:length(r.times))=netQsm(end);
    integerFluxEventsr=sum(abs(diff(round(netQsm))))
    integerFluxEventsc=sum(abs(diff(ceil(netQsm))))
    flux=(integerFluxEventsr+integerFluxEventsc)/r.times(end)/2
    %QdotSm(end+1:length(r.times))=0;
    [ diffNetQsm, ~ ] = savitzyGolaySmoothing( 2, settings.smoothingWindow, diff(netQsm), 1 );
    %diffNetQsm(end:length(r.times))=0;
    plot(r.times,netQsm,'r');
    labelFig(settings.plot, tlabel, 'Net Flow Estimate',{});
    legend('avg diff of top and btm reservoirs',sprintf('smoothed and used for passage \nby distance a: Q=%3.2f/ns',flux));

    figure;
    plot(r.times,QdotSm);
    hold on;
    plot(r.times,diffNetQsm,'r');
    labelFig(settings.plot, tlabel, 'Derivative of net Flow',{});
    legend( 'savitzy Golay derivative','diff of sG' );

    figure;
    netQ=netQ(1:end);
    QperStep = diff(netQ); 

    % When one fluid particle leaves but a fluid doesn't enter at the opposite end it's not flow.
    oneEntrywayEvents = QperStep < 1;
    QperStep(oneEntrywayEvents) = 0;

    % When there is a flow even in one direction, immediately followed by a reverse event, it's a fluctuation not flow.
    candidateFluctuations = abs(diff(QperStep)) > 1;
    candidateFluctuations(end+1) = 0;
    singleQs = QperStep < 2;
    QperStep(candidateFluctuations & singleQs)=0;

    Qrate = QperStep / dt; 
    Qrate = abs( Qrate );
    Qrate(end+1)=Qrate(end);
    [ QrateSm, ~ ] = savitzyGolaySmoothing( 2, settings.smoothingWindow, Qrate, 1 );
    % QrateSm(end+1:length(r.times))=0;
    plot(r.times,QrateSm);
    hold on;
    plot(r.times,Qrate,'r');
    meanQrate = mean( Qrate );
    horplot( meanQrate, 'g--' );
    labelFig(settings.plot, tlabel, 'chain hops per ns',{});
    legend( sprintf('smoothing Window: %i',settings.smoothingWindow), 'raw Qrate', sprintf('average when excluding \nimmediate backflow and \nrequire flow at both ends: %3.2f/ns',meanQrate) );
end

end
