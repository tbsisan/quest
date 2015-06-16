function cFKplotEnergies( ts, E_Ks, hE, numSolitons, cFKaxes, cFKi, cFKsimParams, runList, labelFigs )

    axes(cFKaxes.kinetic);
    hold on;
    figTitle = sprintf('Energies at equilibrated temp of %.1e (K)', cFKsimParams.Temp);
    cFKplot(ts*1e9, E_Ks(:,1), cFKi, 1, '-', figTitle, '', 'Kinetic (K)', runList, labelFigs);
    axes(cFKaxes.spring);
    hold on;
    cFKplot(ts*1e9, E_Ks(:,2), cFKi, 1, '-', '', '', 'Spring (K)', {}, labelFigs);
    axes(cFKaxes.substrate);
    hold on;
    % cFKplot(ts*1e9, E_Ks(:,3), cFKi, 1, '-', '', '', 'Substrate (K)', {}, labelFigs);
    [ hEsmooth, ~ ] = savitzyGolaySmoothing( 2, 15, sum(hE,2), 1 );
    hEsmooth = sum(hE,2);
    cFKplot(ts*1e9, hEsmooth, cFKi, 1, '-', '', '', 'Substrate (K)', {}, labelFigs);
    axes(cFKaxes.total);
    hold on;
    cFKplot(ts*1e9, sum(E_Ks,2), cFKi, 1, '-', '', '', 'Total (K)', {}, labelFigs);
    axes(cFKaxes.solitons);
    hold on;
    cFKplot(ts*1e9, numSolitons, cFKi, 1, '-', '', 'time (ns)', '# solitons', {}, labelFigs);
    axis tight;

end
