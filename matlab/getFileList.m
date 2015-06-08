function [ files ] = getFileList( paths, pattern )
if strfind( paths.remoteStor, '@' )
    %remote files
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
    error( [ 'No files match pattern: ' localFiles ]);
end
