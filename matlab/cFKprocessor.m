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
    [ fh, cFKaxes ] = cFKfigure(800,600,cFKflags)
end

for cFKi=startfile:endfile
     
    % if dcdFlags == 'useLastDcd' && dcdi>1; display(sprintf('skipping dcd %i',dcdi)); continue; end
    % if dcdFlags ~= 'useLastDcd' || ~exist( 'xyzs', 'var' );

    cFKfullFile = [ paths.projectStor '/' cFKpruned{cFKi} ];
    cFKfiles    = getcFKfiles( cFKfullFile );
    
    [ cFKsimParams ]    = getcFKsimParams( cFKfiles.log );
    % runList{cFKi}       = sprintf('%.1e',cFKsimParams.ensStr);
    runList{cFKi}       = sprintf('%.1e',cFKsimParams.kbar);

    % 
    % Read in the data in the dcd file
    %
    [ Uxs,  ~,   ~,    ~,  ~ ]  = readFortran( cFKfiles.Ux, 1 );
    [  xs, ts, sol, solo, fn ]  = readFortran( cFKfiles.x, 1.228e-10 );
    [ reduced, reducedIndexes ] = cFKreduceVars( xs, Uxs, ts, cFKsettings );
    
    %
    % Modules
    %

    if amember(moduleList,'animate')
        solsRaw=mod(reduced.xs,cFKsimParams.lambda);
        sols=unbreakSol(solsRaw,cFKsimParams.lambda);
        particles=1:size(xs,2);
        ui=bsxfun(@minus,reduced.xs,particles*cFKsimParams.lambda+reduced.xs(1,1));
        reduced.temps = reduced.Uxs/1.38e-23/cFKsimParams.N*2;
        [ animHandle, cFKmovie ] = cFKanimate( reduced.ts, ui, reduced.temps, cFKsimParams, paths );
    end
        
    if amember(moduleList, 'plotOverview')
        labelFigs=(cFKi==endfile);
        cFKplotOverview(ts, xs, Uxs, sol, cFKsimParams, cFKaxes, cFKi, runList, labelFigs);
    end

    if amember(moduleList, 'countSolitons')
        numSolitons = (sol(end,end)-sol(end,1))/(cFKsimParams.WL/cFKsimParams.WLperN);
        numSolitonInt = round(numSolitons);
        moduleData.solitons.num(cFKi) = numSolitons;
        moduleData.solitons.N(cFKi) = cFKsimParams.N;
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
