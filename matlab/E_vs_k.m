
% Turn on energyVsTime in cFKprocessor
solEfig=figure; hold on;
sols=0:6;
for soli=sols
    labelFig=(soli==sols(end));
    if soli~=1; s='s'; else s=''; end
    Elegend{soli+1} = [ num2str(soli) ' soliton' s];
    cFKpattern=['x.sol' num2str(soli) '_0*_eqt0_Ti000_L350_N300_a1.05E+00_k*'];
    cFKprocessor;
    figure(solEfig);
    [ksort, sortInds] = sort( kValues );
    [ plotHandle ] = cFKplot(ksort,finalEnergy(sortInds),0,2,'-o','FK Polymer Potential Energies','g (dimensionless)','Energy (K)',Elegend,labelFig);
    if labelFig; text(0.02,0.68,{'PARAMETERS FROM ONE OF THE RUNS',sprintf(cFKsimParams.allStr)},'Units','normalized'); end
    hold on;
end
