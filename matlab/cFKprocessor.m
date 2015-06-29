%
% The main program to get the simulation data from cFK simulations
%

%
% Run the file that sets the parameters:
% dcdPath, dcds, dcdSettings, dcdFlags, dataToSave, optionalFunctions, dcdPruneStrings
%
cFKparameters; 

clear savedData moduleData runList;
savedData(length(cFKpruned)) = struct(); % Initialize empty structure array to hold saved data

startfile=1;
endfile=length(cFKpruned);
runList=cell(1,endfile-startfile+1);
%runList{1}='';
%runList{endfile-startfile}='';

if amember(moduleList, 'plotOverview') 
    [ fh, cFKoverviewAxes ] = cFKoverviewFigure(900,700,cFKflags)
end
if amember(moduleList, 'energyVsTime') && amember(cFKflags,'doFigs')
    [ fh, cFKenergyAxes ] = cFKenergyFigure(900,700,cFKflags)
end

clear numSolitons numSolitonsInt aPercents finalEnergy kValues;

for cFKi=startfile:endfile
     
    % if dcdFlags == 'useLastDcd' && dcdi>1; display(sprintf('skipping dcd %i',dcdi)); continue; end
    % if dcdFlags ~= 'useLastDcd' || ~exist( 'xyzs', 'var' );

    cFKfullFile = [ paths.projectStor '/' cFKpruned{cFKi} ];
    cFKfiles    = getcFKfiles( cFKfullFile );
    
    [ cFKsimParams ]    = getcFKsimParams( cFKfiles.log );
    %runList{cFKi}       = cFKsimParams.ensStr;
     %runList{cFKi}       = sprintf('%.2f',cFKsimParams.kbar);
     runList{cFKi}       = sprintf('%.2f',cFKsimParams.a/cFKsimParams.lambda);
    % runList{cFKi}       = sprintf('%.1e',cFKsimParams.kbar);
    % runList{cFKi}       = sprintf('%.1e',cFKsimParams.ensStr);

    % 
    % Read in the data in the dcd file
    %
    % [ Uxs,  ~,  ~,  ~ ]  = readFortran( cFKfiles.Ux, 0 );
    [  xs, ts, ui, fn ]  = readFortran( cFKfiles.x, cFKsimParams.lambda );
    % [ reduced, reducedIndexes ] = cFKreduceVars( xs, ui, Uxs, ts, cFKsettings );
    
    %
    % Modules
    %
    if amember(moduleList,'energyVsTime')
        labelFigs=(cFKi==endfile);
        E_Ks = reduced.Uxs/1.38e-23/cFKsimParams.N*2;
        hE = cFKsimParams.h * (cos(reduced.xs*2*pi/(cFKsimParams.lambda))+1) / 1.38e-23 / cFKsimParams.N *2;
        numSolitons(cFKi) = abs(ui(end,end)-ui(end,1))/(cFKsimParams.lambda);
        solitonTrack = (reduced.ui(:,end)-reduced.ui(:,1))/(cFKsimParams.lambda);
        if amember(cFKflags,'doFigs')
            cFKplotEnergies( reduced.ts, E_Ks, hE, solitonTrack, cFKenergyAxes, cFKi, cFKsimParams, runList, labelFigs );
        end
        systemPE = E_Ks(:,2) + sum(hE,2);
        finalEnergy(cFKi) = systemPE(end);
        kValues(cFKi) = cFKsimParams.k;
        aPercents(cFKi) = cFKsimParams.a/cFKsimParams.lambda;
    end

    if amember(moduleList,'animate')
        reduced.temps = reduced.Uxs(:,1)/1.38e-23/cFKsimParams.N*2;
        [ animHandle, cFKmovie ] = cFKanimate( reduced.ts, reduced.ui, reduced.temps, cFKsimParams, cFKflags, paths );
    end
        
    if amember(moduleList, 'plotOverview')
        labelFigs=(cFKi==endfile);
        cFKplotOverview(ts, xs, Uxs, ui, cFKsimParams, cFKoverviewAxes, cFKi, runList, labelFigs);
    end

    if amember(moduleList, 'countSolitons')
        numSolitons(cFKi) = abs(ui(end,end)-ui(end,1))/(cFKsimParams.lambda);
        numSolitonInt(cFKi) = round(numSolitons(cFKi));
        moduleData.solitons.num(cFKi) = numSolitons(cFKi);
        moduleData.solitons.N(cFKi) = cFKsimParams.N;
        moduleData.solitons.aP(cFKi) = cFKsimParams.a/cFKsimParams.lambda;
        moduleData.solitons.g(cFKi) = cFKsimParams.kbar;
        moduleData.solitons.L(cFKi) = cFKsimParams.L;
    end

    if amember(moduleList, 'measureMotion')
        comxs = mean(xs,2);
        totalDist = comxs(end) - comxs(1);
        maxDist = max(comxs) - comxs(1);
        widths = squeeze(xs(:,end)-xs(:,1));
        moduleData.displacement.total(cFKi)=totalDist;
        moduleData.displacement.maxx(cFKi)=maxDist;
        moduleData.displacement.width(cFKi)=widths;
        moduleData.displacement.t=ts;
    end

    if amember(moduleList,'fft')
        % [ hostAtomsFullXyzs, fullTimes ] = getHostAtomsTrajsAndReducet( xyzs, timeParams, atomsPerFluidMol, 0 ); % 3d (spatial dim, atom, timestep)
        fftSettings = struct( 'smoothingWindow', 15 );
        fftFlags = { 'plotOn' };
        [ moduleData.ffts(cFKi) ] = computeFFT( fftSettings, fftFlags, ts, xs ); 
        % moduleData(1).ffts.hostAtomsZ.smoothPower is the smoothed fft power of hostAtomsZ
        % clear hostAtomsFullXyzs fullTimes hostAtomsX hostAtomsZ;
    end

    if amember(moduleList,'makeMovie')

    end


    %
    % Save some data for easier data exploring, after this script is finished
    %
    savedData(cFKi).fn = cFKpruned{cFKi}; % for convenience, always save the dcd file name
    for saveIndex=1:length(dataToSave)
        savedData(cFKi).(dataToSave{saveIndex}) = eval( dataToSave{saveIndex} );
    end
    
end

if (isOctave)
    % shrink fonts
    FS = findall(fh,'-property','FontSize');
    for i=1:length(FS)
        fsi=get(FS(i),'FontSize');
        set(FS(i),'FontSize',round(fsi*0.7));
    end
end
%text(0.05,0.8,sprintf(cFKsimParams.allStr),'Units','normalized'); 
%text(-0.5,1.05,'cFK Finite Chain Results','Units','normalized','FontSize',15,'FontWeight','bold')
%text(1,1,'a','Units','normalized');
