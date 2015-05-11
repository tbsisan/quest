
% Get list of polymer data files to process (produced from a vmd script measurePolymerBonds.tcl).
% Here, search only for the *.bonds files.  Files for angle and dihedral data will also be obtained 
% later from substituting the respective file suffixes.
dcdParameters
[ dcdPruned ]   = pruneDcdList( dcds,dcdPruneStrings ); % Prune the list according to patterns

for dcdi=1:length(dcdPruned)
    % Bonding data file names
    dcdFullFile = [dcdPath '/' dcdPruned{dcdi}];
    bondDataFullFile=['/projects/p20200/polymerData/' regexprep(dcds(dcdi).name,'dcd$','bonds')];
    angleDataFullFile=regexprep(bondDataFullFile,'bonds$','angles');
    dihedralDataFullFile=regexprep(bondDataFullFile,'bonds$','dihedrals');

    % Process dcd files into bonding data files, if we haven't already done so.
    if ~exist( bondDataFullFile, 'file' )
        [ namdFiles ] = getNamdFileList( dcdFullFile );
        [~,psfFile] = system( ['awk "/Info: STRUCTURE FILE/ {print \$4; exit}" <' namdFiles.outfStd] );
        psfFullFile = strtrim([projectPath '/psfs/' psfFile]);
        tclFullFile = '/home/tbs246/md/vmd/measurePolymerBonds.tcl';
        [status] = system(sprintf('vmd -dispdev text -psf %s -dcd %s -e %s', psfFullFile, dcdFullFile, tclFullFile),'-echo');
    end

    % Read in bonding data.
    % Each line of data files is a new time step.
    % Rows = time steps, Columns = bonds
    bondData=dlmread( bondDataFullFile );
	angleData=dlmread( angleDataFullFile );
	dihedralData=dlmread( dihedralDataFullFile );
	dihedralData=mod(dihedralData,360);

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
	title(strrep(bondName,'_','\_'));
    ylabel('bond lengths')
	subplot(3,1,2);
	plot(angles_v_time);
    horplot(113.6,'--'); horplot(115.0,'--');
    ylabel('backbone angles')
	subplot(3,1,3);
	plot(dihedrals_v_time);
    horplot(180,'--');
    ylabel('backbone dihedrals')
    xlabel('time step')

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
	title(strrep(dihedralName,'_','\_'));
    horplot(180,'--');
    vertplot(180,'--');
end
