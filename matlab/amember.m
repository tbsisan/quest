function [ isamember ] = amember( fullGroup, possibleMembers, varargin )
% Determine if a string or list of strings (possibleMembers) are present in a cell array, fullGroup.
% By default, returns true if all strings in possibleMembers are exactly present in fullGroup.
% possibleMembers can be a string, or a cell array of strings.
% Optionally, a handle to an array function can be passed in.  For example, pass in @any, to test if any of possibleMembers is in fullGroup.

memberLogic = @all; % my default, memberLogic is a function handle to the all() function. 
                    % It will check that each element of possibleMembers is in fullGroup.
if length(varargin)>0
    memberLogic = varargin{1};
end

isnotamember=0;
% If possibleMembers is a string, turn it into a one element cell array.
if isa(possibleMembers,'char')
    possibleMembers={ possibleMembers };
end

%Check that some or all members of cell array possibleMembers is in fullGroup.
memberships = zeros(1,length(possibleMembers));
for memi=1:length(possibleMembers)
    memberships(memi) = any(strcmp(fullGroup, possibleMembers{memi})); % is the current possibleMember in fullGroup?
end
isamember = memberLogic(memberships);

end
