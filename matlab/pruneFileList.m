function [ prunedNames ] = pruneFileList( infiles, keepPatterns, prunePatterns )
% Takes a list of file structures (infiles) as returned from dir() and excludes those whose filenames match prunePatterns and don't match keepPatterns.
% As its often easier to specify and exclusion list, rather than a keep list, keppPatterns is often set to '.+', which matches all possible files in infiles.
% infiles is a structure as returned by the matlab builtin command dir.

prunedNames={};

onesToKeepLogical=logical(ones(1,length(infiles)));
onesToPruneLogical=logical(zeros(1,length(infiles)));
if (length(infiles)>0)
for keepi=1:length(keepPatterns)
    onesToKeep = regexp( {infiles.name}, keepPatterns{keepi} );
    % eg: { [1] [1] [] [] [1] }, for an infiles array that is five elements, where three of them match the current keepPattern
    onesToKeepLogical = onesToKeepLogical & logical( cellfun( 'length', onesToKeep ) );
end

for prunei=1:length(prunePatterns)
    onesToPrune = regexp( {infiles.name}, prunePatterns{prunei} );
    onesToPruneLogical = onesToPruneLogical | logical( cellfun( 'length', onesToPrune ) );
end
end

keepAndNotPrune = onesToKeepLogical & ~onesToPruneLogical;
prunedFiles = infiles(keepAndNotPrune);
prunedNames = {prunedFiles.name};
prunedNames'


end
