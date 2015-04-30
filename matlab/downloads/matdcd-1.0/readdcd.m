function xyz = readdcd(filename, ind)

% xyz = readdcd(filename, indices)
% reads an dcd and puts the x,y,z coordinates corresponding to indices 
% in the rows of x,y,z

display(sprintf(['\nREADING DCD FILE:\n\t' filename]));
h = read_dcdheader(filename);
nsets = h.NSET;
natoms = h.N;
numind = length(ind);

%  if nsets>(h.endoffile/natoms/3/4)
%      nsets=(h.endoffile-212000)/natoms/3/4
%  end

x = zeros(natoms,1);
y = zeros(natoms,1);
z = zeros(natoms,1);

if nsets == 0
  xyz = zeros(1,3*numind);
  nsets = 99999;
else
  xyz = zeros(nsets, 3*numind);
end

waitStr=sprintf(['\t... starting loop to read %i steps'],nsets);
fprintf([waitStr ' Progress:  0%']);
%hBar = waitbar(0,waitStr);
%fprintf('\tProgress:  0%');
warnDone=0;
for i=1:nsets
  pos = ftell(h.fid);
  if pos == h.endoffile 
    break;
  end
  [x,y,z] = read_dcdstep(h);
  %TBS below 3 lines added
%  if length(x) ~= length(z)
%    xatbreak=size(x)
%    zatbreak=size(z)
%    z=zeros(size(x));
%    zatbreak=size(z)
    %break;
%  end
  if (max(ind)>length(z) && warnDone==0)
    display(['during nset ' num2str(i) ':']);
    display(['max atom index fed to readdcd:' num2str(max(ind)) ' is greater than obtained numatoms ' num2str(natoms)]);
    display(['length of z vector is ' num2str(length(z))]);
    warnDone=1;
  else
    xyz(i,1:3:3*numind) = x(ind)';
    xyz(i,2:3:3*numind) = y(ind)';
    xyz(i,3:3:3*numind) = z(ind)';
  end
  %waitbar(i/nsets,hBar);
  fprintf('\b\b\b\b%3d%%', round(i/nsets*100));
end

close_dcd(h);
display(sprintf(['\n\t... Finished reading file with atoms=%i and steps=%i'],natoms,nsets));
display(sprintf(['\t... data size = (%ix%i)'],size(xyz,1),size(xyz,2)));
