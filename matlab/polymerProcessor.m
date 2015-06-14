
% Get list of polymer data files to process (produced from a vmd script measurePolymerBonds.tcl).
% Here, search only for the *.bonds files.  Files for angle and dihedral data will also be obtained 
% later from substituting the respective file suffixes.
dcdParameters
[ dcdPruned ]   = pruneDcdList( dcds,dcdPruneStrings ); % Prune the list according to patterns

for dcdi=1:length(dcdPruned)
    % Bonding data file names
    dcdFullFile = [paths.dcdPath '/' dcdPruned{dcdi}];
    bondDataFullFile=[paths.projectStor '/polymerData/' regexprep(dcds(dcdi).name,'dcd$','bonds')];
    angleDataFullFile=regexprep(bondDataFullFile,'bonds$','angles');
    dihedralDataFullFile=regexprep(bondDataFullFile,'bonds$','dihedrals');
    rDataFullFile=regexprep(bondDataFullFile,'bonds$','rs');

    % Process dcd files into bonding data files, if we haven't already done so.
    [ namdFiles ] = getNamdFileList( dcdFullFile, paths );
    if ~exist( rDataFullFile, 'file' )
        tclFullFile = '/home/tbs246/md/vmd/measurePolymerBonds.tcl';
        [status] = system(sprintf('vmd -dispdev text -psf %s -dcd %s -e %s -args %s %s %s %s', ...
                                   namdFiles.psff, dcdFullFile, tclFullFile, bondDataFullFile, angleDataFullFile, dihedralDataFullFile, rDataFullFile),'-echo');
    end

    % Read in bonding data.
    % Each line of data files is a new time step.
    % Rows = time steps, Columns = bonds
    bondData=dlmread( bondDataFullFile );
	angleData=dlmread( angleDataFullFile );
	dihedralData=dlmread( dihedralDataFullFile );
	dihedralData=mod(dihedralData,360);
    rData=dlmread( rDataFullFile );
    cooledInd=round(size(rData,1)*2/3);
    cooledRs=rData(end*2/3:end,:);

    % Average over all bonds for each time step.
	bonds_v_time=mean(bondData,2);
	angles_v_time=mean(angleData,2);
	dihedrals_v_time=mean(dihedralData,2);

    % Plot bond data vs time.
	figure;
	subplot(3,1,1)
	plot(bonds_v_time);
    horplot(1.53,'--');
    [ ~, bondName, ~ ] = fileparts( bondDataFullFile );
    labelFig(bondName,'','bond lengths',{});
	subplot(3,1,2);
	plot(angles_v_time);
    horplot(113.6,'--'); horplot(115.0,'--');
    ylabel('backbone angles')
	subplot(3,1,3);
	plot(dihedrals_v_time);
    horplot(180,'--');
    ylabel('backbone dihedrals')
    xlabel('time step')

    % Plot R data
    figure;
    hist(cooledRs(:),20);
    [ ~, rName, ~ ] = fileparts( rDataFullFile );
    labelFig(rName,['R (' angstrom ')'], 'Histogram of radial positions of polymer atoms',{});

    % Plot dihedral angle m+1 versus dihedral angle m
    figure;
    shortTimeArray=1:size(dihedralData,1);
    if size(dihedralData,1)>150
        shortTimeArray=round(linspace(1,size(dihedralData,1),100));
    end
    dihedralData1=dihedralData(shortTimeArray,1:end-1);
    dihedralData2=dihedralData(shortTimeArray,2:end);
    plot(dihedralData1,dihedralData2,'o');
    [ ~, dihedralName, ~ ] = fileparts( dihedralDataFullFile );
    labelFig(dihedralName,'dihedral angle i','dihedral angle i+1',{});
    horplot(180,'--');
    vertplot(180,'--');

    vmdMovieController = '/home/tbs246/md/vmd/movieController.tcl';
    vmdMovieCmd = sprintf('vmd -dispdev text -psf %s -dcd %s -e %s -args %s %s %s %s %s', ...
                           namdFiles.psff, dcdFullFile, vmdMovieController, '1', '1', '70', '/home/tbs246/Dropbox/LichterGroup/meetings/5.14.2015', '~/movs')
    [ status, cmdOut ] = system(vmdMovieCmd);
end
savefigs('/home/tbs246/Dropbox/LichterGroup/meetings/5.14.2015/','nm77_T2_polymerData',1);
