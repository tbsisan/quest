function [ files ] = getFilesWithPattern( paths, pattern )
files=struct();
if strfind( paths.remoteStor, '@' )
    %remote files
    disp('Downloading remote files from server (only works for cFK data).');
    scpStr = [ 'scp ' paths.remoteStor '/' pattern ' ' paths.projectStor ];
    [ status, outStr ] = system(scpStr);
    localFiles = [ paths.projectStor '/x' pattern ];
    files = dir( localFiles );
    fileLocation = paths.remoteStor;
else
    localFiles = [ paths.projectStor '/' pattern ];
    files = dir( localFiles );
    fileLocation = paths.projectStor;
end

if length(files)==0
    disp( [ 'No files match pattern: ' localFiles ]);
end
