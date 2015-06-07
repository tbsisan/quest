% Some global variables.
global angstrom isOctave;
isOctave = exist('OCTAVE_VERSION', 'builtin') ~= 0;
%angstrom='{\AA}';
angstrom=char(197);

%
% Set dcd parameters before processing with dcdProcessor.m
%
setPaths;
setAvailableModules;

% cFKpattern need to be set before running cFKprocessor.  Here is an example:
% cFKpattern  = '/cnt4*nm66*spe4*ens*run1*langevin*.dcd';


%
% Settings and flags control behavior in dcdProcessor.m
% Settings are for things which are noteworthy enough for always including. All available settings are always present.
% Flags are for things which are frequently not set, to avoid clutter.
% A list of possible flags is in flags.m (TODO)
%
cFKsettings = struct(   'sortTrajectory', 'no',     'oneAtomStrategy', 'trackCOM', ...
                        'shortTimeSteps', 100 );
cFKflags    = { '~useLastcFK', 'plotPosition' }; % a ~ prepended to a flag, or, technically any change to the string, turns it off.

dataToSave  = { 'reducedXs', 'reducedTimes', 'sol' }; % Save this data for each dcd file processed

moduleList  = { 'trackReservoirs' }; % List of optional data processing options. Full list in makeAvailableModules:
if ~amember(availableModules, moduleList) error([ 'One of the modules in moduleList is not in availableModules' ]); end

% This is where the list of files to process is obtained. It is later processed with keepPatterns and prunePatterns.
if ~amember(cFKflags,'useLastcFK') 
    cFKs    = dir( [paths.projectStor '/' cFKpattern] );
end

keepPatterns    = { '.+' }; % Only keep files that match ALL keep patterns. To not filter based on this, use '.+'
prunePatterns   = { 'aaaaaaa',  ...
                    'replaceMeWithaPatternToMatchAFileNameForExclusion',  ...
                    'x.[01]',  ...
                    'x.20',  ...
                    '9999999' }; % Files matching these pattern strings will be excluded from processing.
[ cFKpruned ]   = pruneFileList( cFKs, keepPatterns, prunePatterns ); % Prune the list according to patterns

