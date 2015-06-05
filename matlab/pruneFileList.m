function [ pruned ] = pruneFileList( infiles, patterns )
% Takes a list of file structures (infiles) as returned from dir() and excludes those whose filenames match a pattern (patterns).

pruned={};
for filei=1:length(infiles)
    skip=0;
    for prunei=1:length(patterns)
        if ( any(regexp( infiles(filei).name, patterns{prunei} )) )
            skip=1;
        else
            %do nothing
        end
    end
    if (skip==0)
        pruned{end+1}=infiles(filei).name;
    end
    %if (any( regexp( infiles(filei).name, [ '(' strjoin( patterns,'|' ) ')' ] )));
    %    continue;
    %end
end

end
