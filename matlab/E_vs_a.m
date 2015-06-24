
% Turn on energyVsTime in cFKprocessor
cFKparameters;
solEfig=launchFigure(cFKflags,900,700); hold on;
sols=[-9:-1 1:9];
sols=[9:2:25];
sols=[1:2:25];
clear Elegend;
for soli=1:length(sols)
    labelFig=(soli==length(sols));
    if abs(sols(soli))~=1; s='s'; else s=''; end
    if sols(soli)<0; aFirstChar='9'; else aFirstChar='1'; end
    if sols(soli)<0; symbols='-s'; else symbols='-o'; end
    Elegend{soli} = [ num2str(sols(soli)) ' soliton' s];
    cFKpattern=['x.sol' num2str(sols(soli)) '_00_eqt0_Ti000_*N300_a' aFirstChar '*_k3.20E+01*'];
    cFKprocessor;
    figure(solEfig);
    [ aSort, sortInds ] = sort( aPercents );
    solSort = round(numSolitons(sortInds));
    finalEnergy = finalEnergy(sortInds);
    useables = (solSort ~= NaN);
    useables = (solSort == abs(sols(soli)));
    [ plotHandle ] = cFKplot(abs(1-aSort(useables)),finalEnergy(useables),0,2,symbols,'FK Polymer Potential Energies','a/lambda','Energy (K)',Elegend,labelFig);
    if labelFig; text(0.02,0.68,{'PARAMETERS FROM ONE OF THE RUNS',sprintf(cFKsimParams.allStr)},'Units','normalized'); end
    hold on;
end
