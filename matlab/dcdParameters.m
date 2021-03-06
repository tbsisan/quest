%
% Set dcd parameters before processing with dcdProcessor.m
%
setPaths;
setAvailableModules;
% dcdPattern  = '/cnt4*nm66*spe4*ens*run1*langevin*.dcd';


%
% Settings and flags control behavior in dcdProcessor.m
% Settings are for things which are noteworthy enough for always including. All available settings are always present.
% Flags are for things which are frequently not set, to avoid clutter.
% A list of possible flags is in flags.m (TODO)
%
dcdSettings = struct(   'sortTrajectory', 'no',     'oneAtomStrategy', 'trackCOM', ...
                        'sliceTimes', 100 );
dcdFlags    =       {   '~useLastDcd', 'plotPosition' }; % a ~ prepended to a flag, or, technically any change to the string, turns it off.

dataToSave  = { 'oneAtomZs' }; % Save this data for each dcd file processed

moduleList = { 'trackReservoirs' }; % List of optional data processing options. Full list in makeAvailableModules:
for module=moduleList; if availableModules~=module; error([ 'Module ' cell2mat(module) ' not available' ]); end; end

dcdPruneStrings = { 'aaaaaaa',  ...
                    'replaceMeWithAStringToMatchAdcdFileNameForExclusion',  ...
                    '9999999' }; % dcd files matching these pattern strings will be excluded from processing

if dcdFlags ~= 'useLastDcd'
    dcds        = dir( [paths.dcdPath '/' dcdPattern] ); % Just process the last dcd with: dcds = [1];
end
