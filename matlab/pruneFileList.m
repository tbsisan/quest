function [ pruned ] = pruneFileList( infiles, patterns )
% Takes a list of file structures (infiles) as returned from dir() and excludes those whose filenames match a pattern (patterns).
pruned={};
for filei=1:length(infiles)
    if (any( regexp( infiles(filei).name, [ '(' strjoin( patterns,'|' ) ')' ] )));
        continue;
    end
    pruned{end+1}=infiles(filei).name;
end
end
