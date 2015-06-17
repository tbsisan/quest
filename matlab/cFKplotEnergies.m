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
    TE=E_Ks(:,1)+E_Ks(:,2)+sum(hE,2);
    cFKplot(ts*1e9, TE, cFKi, 1, '-', '', '', 'Total (K)', {}, labelFigs);
    axes(cFKaxes.solitons);
    hold on;
    cFKplot(ts*1e9, numSolitons, cFKi, 1, '-', '', 'time (ns)', '# solitons', {}, labelFigs);
    tighty(cFKaxes.solitons);
    if (labelFigs)
        axes(cFKaxes.kinetic);
        text(1.05,0.2,sprintf(cFKsimParams.allStr),'Units','normalized'); 
    end
end

function tightx(ax)
    axes(ax)
    loose = axis;
    axis tight;
    tight = axis;
    axis([ tight(1) tight(2) loose(3) loose(4) ]);
end
function tighty(ax)
    axes(ax)
    loose = axis;
    axis tight;
    tight = axis;
    axis([ loose(1) loose(2) tight(3) tight(4) ]);
end
    
