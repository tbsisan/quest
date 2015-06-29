% Some global variables.
global angstrom isOctave;
isOctave = exist('OCTAVE_VERSION', 'builtin') ~= 0;
angstrom=char(197);
%angstrom='{\AA}'; % Older versions of matlab would need something like this.

%
% Set dcd parameters before processing with dcdProcessor.m
%
setPaths;
setAvailableModules;
if ~exist('cFKpattern'); cFKpattern='asdfasdf'; end

% cFKpattern need to be set before running cFKprocessor.  The pattern is searched in paths.projStor.
% Here is an example pattern:
% cFKpattern  = 'x.*eqt8*Ti600*N100*k0.30*h0.15*F0.00*t0.30*.dcd';


%
% Settings and flags control behavior in dcdProcessor.m
% Settings are for things which are noteworthy enough for always including. All available settings are always present.
% Flags are for things which are frequently not set, to avoid clutter.
% A list of possible flags is in flags.m (TODO)
%
cFKsettings = struct(   'sortTrajectory', 'no',     'oneAtomStrategy', 'trackCOM', ...
                        'sliceTimes', 0,        'cropTimes', [0.0/10.0 10.0/10.0] );
cFKflags    = { '~useLastcFK', 'doFigs', '~hideFigs', '~plotPosition' }; % a ~ prepended to a flag, or, technically any change to the string, turns it off.

dataToSave  = { 'ui', 'cFKsimParams' }; % Save this data for each dcd file processed

moduleList  = { '~energyVsTime', '~animate', '~plotOverview','countSolitons','~measureMotion' }; % List of optional data processing options. Full list in makeAvailableModules:
%if ~amember(availableModules, moduleList) error([ 'One of the modules in moduleList is not in availableModules' ]); end

% This is where the list of files to process is obtained. The list pruned below according to keepPatterns and prunePatterns.
if ~amember(cFKflags,'useLastcFK') 
    cFKs    = getFilesWithPattern( paths, cFKpattern );
end

keepPatterns    = { @or, 'test','phase' }; % Only keep files based on logic infirst cell element. To not filter based on this, use '.+'
prunePatterns   = { @or, 'aaaaaaa',  ...
                    'replaceMeWithaPatternToMatchAFileNameForExclusion',  ...
                    '9999999' }; % Files matching these pattern strings, with logic based on first element, will be excluded from processing.
[ cFKpruned ]   = pruneFileList( cFKs, keepPatterns, prunePatterns ); % Prune the list according to patterns

