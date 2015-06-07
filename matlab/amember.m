function [ isamember ] = amember( fullGroup, possibleMembers )
% Determine if a string or list of strings (possibleMembers) are all present in a cell array, fullGroup.
% Returns true if all strings in possibleMembers are exactly present in fullGroup.
% possibleMembers can be a string, or a cell array of strings.

isnotamember=0;
% If possibleMembers is a string, turn it into a one element cell array.
if isa(possibleMembers,'char')
    possibleMembers={ possibleMembers };
end

%Check that each member of cell array possibleMembers is in fullGroup.
for memi=1:length(possibleMembers)
    isnotamember = ~any(strcmp(fullGroup, possibleMembers{memi}));
    if (isnotamember)
        break; % If you find one that isn't in fullGroup, stop everything and return false.
    end
end

isamember = ~isnotamember; 

end
