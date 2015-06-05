%
% Set dcd parameters before processing with dcdProcessor.m
%
setPaths;
setAvailableModules;
% cFKpattern  = '/cnt4*nm66*spe4*ens*run1*langevin*.dcd';


%
% Settings and flags control behavior in dcdProcessor.m
% Settings are for things which are noteworthy enough for always including. All available settings are always present.
% Flags are for things which are frequently not set, to avoid clutter.
% A list of possible flags is in flags.m (TODO)
%
cFKsettings = struct(   'sortTrajectory', 'no',     'oneAtomStrategy', 'trackCOM', ...
                        'shortTimeSteps', 100 );
cFKflags    = {   '~useLastcFK', 'plotPosition' }; % a ~ prepended to a flag, or, technically any change to the string, turns it off.

dataToSave  = { 'reducedXs', 'reducedTimes', 'sol' }; % Save this data for each dcd file processed

moduleList  = { 'trackReservoirs' }; % List of optional data processing options. Full list in makeAvailableModules:
% for module=moduleList.cell; if availableModules~=module; error([ 'Module ' cell2mat(module) ' not available' ]); end; end

pruneStrings= { 'aaaaaaa',  ...
                    'replaceMeWithAStringToMatchAdcdFileNameForExclusion',  ...
                    '9999999' }; % dcd files matching these pattern strings will be excluded from processing

% This is where the list of files to process is obtained.
if ~amember(cFKflags,'useLastDcd') 
    cFKs     = dir( [paths.projectStor '/' cFKpattern] ); % Just process the last dcd with: dcds = [1];
end

global angstrom isOctave;
angstrom=char(197);
isOctave = exist('OCTAVE_VERSION', 'builtin') ~= 0;