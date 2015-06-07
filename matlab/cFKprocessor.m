%
% The main program to get the simulation data from cFK simulations
%

%
% Run the file that sets the parameters:
% dcdPath, dcds, dcdSettings, dcdFlags, dataToSave, optionalFunctions, dcdPruneStrings
%
cFKparameters; 

clear savedData;
savedData(length(cFKpruned)) = struct(); % Initialize empty structure array to hold saved data

startfile=1;
endfile=length(cFKpruned);
runList=cell(1,endfile-startfile);

[ fh, cFKaxes ] = cFKfigure(1000,800)

for cFKi=startfile:endfile
     
    % if dcdFlags == 'useLastDcd' && dcdi>1; display(sprintf('skipping dcd %i',dcdi)); continue; end
    % if dcdFlags ~= 'useLastDcd' || ~exist( 'xyzs', 'var' );

    cFKfullFile = [ paths.projectStor '/' cFKpruned{cFKi} ];
    cFKfiles    = getcFKfiles( cFKfullFile );
    
    [ cFKsimParams ]    = getcFKsimParams( cFKfiles.log );
    runList{cFKi}       = cFKsimParams.ensStr;

    % 
    % Read in the data in the dcd file
    %
    [ Uxs,  ~,   ~,    ~,  ~ ]  = readFortran( cFKfiles.Ux, 1 );
    [  xs, ts, sol, solo, fn ]  = readFortran( cFKfiles.x, 1.228e-10 );
    
    %
    % Process the xyz data
    %
    [ reducedTimes, timeIndexes ] = reduceTimes( ts, cFKsettings.shortTimeSteps );
    [ reducedXs ] = xs( timeIndexes, : );

    % TODO: unwrap the z coordinates using the PBCs

    % Sort trajectories on z-coordinate if requested
    % if {dcdSettings.sortTrajectory} == 'sortz'
    %     hostAtomsXyzs=sort(hostAtomsXyzs,2); %TODO: this is a temporary space holder, it's pseudocode
    % end
        
    labelFigs=(cFKi==endfile);

    axes(cFKaxes.temp); hold on;
    cFKplot(ts*1e9, Uxs(:,1)/1.38e-23/cFKsimParams.N*2, cFKi, 1, ':', 'Temperature', '', 'T(K)', runList, labelFigs);

    axes(cFKaxes.energy);hold on;
    hE = cFKsimParams.h * (sin(xs*2*pi/(cFKsimParams.WL/2))+1);
    Eratio=sum(hE,2)./Uxs(:,2);
    cFKplot(ts*1e9, Eratio, cFKi, 1, '', 'Energy ratio: E_h/E_{spring}', '', 'Ratio', '', labelFigs);
    horizontalLine(Eratio(end),'--', getaNiceColor(cFKi)); 

    axes(cFKaxes.x1);hold on;
    cFKplot(ts*1e9, xs(:,1)*1e10, cFKi, 1, '', 'Position of particle 1', '', ['x (' angstrom ')'], '', labelFigs);

    axes(cFKaxes.chainwidth); hold on;
    cFKplot(ts*1e9, (xs(:,end)-xs(:,1))*1e10, cFKi, 1, ':', 'Chain Length','time (ns)', ['length (' angstrom ')'], '', labelFigs);
    colorLinePlot(0,(xs(1,end)-xs(1,1))*1e10,1,1,'o');
    if (cFKi==endfile) 
        horizontalLine( (cFKsimParams.N-1)*cFKsimParams.a*1e10, '--', getaNiceColor(1)); 
    end

    axes(cFKaxes.sol); hold on;
    cFKplot(1:length(sol(end,:)), sol(end,:)*1e10, cFKi, 2, '', 'Soliton at end of cooling', 'particle i', ['u_i (' angstrom ')'], '', labelFigs);
    if (cFKi==1)
        text(0.05,0.8,sprintf(cFKsimParams.allStr),'Units','normalized'); 
        text(-0.5,1.05,'cFK Finite Chain Results','Units','normalized','FontSize',15,'FontWeight','bold')
    end

    %
    % Modules for extra data processing.
    % External module scripts import data into the moduleData structure
    %
    if amember(moduleList,'fft')
        % [ hostAtomsFullXyzs, fullTimes ] = getHostAtomsTrajsAndReducet( xyzs, timeParams, atomsPerFluidMol, 0 ); % 3d (spatial dim, atom, timestep)
        fftSettings = struct( 'smoothingWindow', 15 );
        fftFlags = { 'plotOn' };
        % oneAtomX=squeeze(hostAtomsFullXyzs(1,1,:));
        % oneAtomZ=squeeze(hostAtomsFullXyzs(3,1,:));
        [ moduleData(cFKi).ffts ] = computeFFT( fftSettings, fftFlags, ts, xs ); 
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
