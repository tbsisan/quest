
% Get list of polymer data files to process (produced from a vmd script measurePolymerBonds.tcl).
% Here, search only for the *.bonds files.  Files for angle and dihedral data will also be obtained 
% later from substituting the respective file suffixes.
fPath     = '/home/tbs246/XiEffects/changeh/projScience/polymerData';
fPattern  = '/cnt60_*langevin*bonds';
fns       = dir( [fPath fPattern] );

for fi=1:length(fns)
    % Get bonds, angles, and dihedrals data filenames.
    bondf=fns(fi).name;
    anglef=regexprep(bondf,'bonds$','angles');
    dihedralf=regexprep(bondf,'bonds$','angles');
    % Each line of data files is a new time step.
    % Rows = time steps, Columns = bonds
    bondData=dlmread([fPath '/' bondf]);
	angleData=dlmread([fPath '/' anglef]);
	dihedralData=dlmread([fPath '/' dihedralf]);
	dihedralData=mod(dihedralData,360);
	bonds_v_time=mean(bondData,2);
	angles_v_time=mean(angleData,2);
	dihedrals_v_time=mean(dihedralData,2);
	figure;
	subplot(3,1,1)
	plot(bonds_v_time);
	subplot(3,1,2);
	plot(angles_v_time);
	subplot(3,1,3);
	plot(dihedrals_v_time);
	title(bondf);
end
