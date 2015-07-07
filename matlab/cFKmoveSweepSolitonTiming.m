% Sweep through cFK polymer runs to see if soliton forms when end atom moved various distances.
% Makes plot of both timeTillSoliton and percentageOfRunsShowingSoliton verus the movement distances listed in Mlist.
% A line is drawn for each value of the resting spring length shown in aList.

clear ensData;
T=2.5;
aList=[1.005 1.01 1.02 1.03 1.04 1.05];
Mlist=[0.65 0.675 0.7 0.725 0.75 0.775 0.8 0.85];
runID='Minspect'
legendLabels=cellfun(@num2str,num2cell(aList),'UniformOutput',false)
plotOn=1;

if plotOn
    [ numFig ] = launchFigure( cFKflags, 700, 900 );
    hold on;
    [ timeFig ] = launchFigure( cFKflags, 700, 900 );
    hold on;
end

for ai=1:length(aList)

    for ensi=0:19
        if ensi<10
            ensdigit='0';
        else
            ensdigit='';
        end
        cFKpattern=['x.' runID '_' ensdigit num2str(ensi) '_M*a' num2str(aList(ai)) '*k*h1.53*T' num2str(T) '*n5.00E+09*t*dat']
        cFKprocessor;
        ensData(ensi+1,:,ai) = solitonPresent*(ts(3)-ts(2));
    end

    if plotOn
        figure(timeFig); % side effect of making figure visible
        ensDataWceiling = squeeze(ensData(:,:,ai));
        ensDataWceiling(isnan(ensDataWceiling))=50e-9;
        cFKplot( Mlist, mean(ensDataWceiling), ai, 1, ai, ...
            {'Average Time till first soliton'; '20 50ns runs'}, 'Move By', 'Time (s)', ...
            legendLabels, ai==length(aList) );
        figure(numFig); % side effect of making figure visible
        cFKplot( Mlist, sum(ensData(:,:,ai)<50e-9), ai, 1, ai, ...
            {'Number out of 20 50ns runs showing soliton'; 'movement during 1-2ns'}, 'Move By', 'Number of runs', ...
            legendLabels, ai==length(aList) );
    end
end

save(['Mdata' runID num2str(T) '.dat'],'aList','Mlist','ensData');

ensRaw=ensData;
ensData(isnan(ensData))=ts(end);
ensData(ensData<2e-9)=2e-9;
