%
% Set dcd parameters before processing with dcdProcessor.m
%
setPaths;
dcdPattern  = '/cnt8_*nm66*spe8*ens*run1*langevin*.dcd';
dcds        = dir( [paths.dcdPath dcdPattern] ); % Just process the last dcd with: dcds = [1];

%
% Settings and flags control behavior in dcdProcessor.m
% Settings are for things which are noteworthy enough for always including. All available settings are always present.
% Flags are for things which are frequently not set, to avoid clutter.
% A list of possible flags is in flags.m (TODO)
%
dcdSettings = struct(   'sortTrajectory', 'no',     'oneAtomStrategy', 'trackCOM', ...
                        'shortTimeSteps', 100                                  );
dcdFlags    =       {   'useLastDcd', 'plotPosition' };

dataToSave  = { 'oneAtomZs' }; % Save this data for each dcd file processed

moduleList = { 'trackReservoirs' }; % List of optional data processing options. Full list in TODO:

dcdPruneStrings = { 'aaaaaaa',  ...
                    'replaceMeWithAStringToMatchAdcdFileNameForExclusion',  ...
                    '9999999' }; % dcd files matching these pattern strings will be excluded from processing

