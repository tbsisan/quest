function [ timeParams ] = timeDataFromNamd( namdFiles )
%UNTITLED4 Summary of this function goes here
%   Detailed explanation goes here

[~,timestep] = system( ['awk "/Info: TIMESTEP/ {print \$3; exit}" <' namdFiles.outfStd] ); % '>' datf]);
[~,dcdFreq]  = system( ['awk "/Info: DCD FREQ/ {print \$4; exit}" <' namdFiles.outfStd] );
[~,expectedSteps] = system( ['awk "/TCL: run time:/ {print \$4; exit}" <' namdFiles.outfStd] );

timeParams.timestep = str2num(timestep); %fs
timeParams.dcdFreq  = str2num(dcdFreq);
timeParams.expectedSteps = str2num(expectedSteps);

timeParams.framePeriodns=timeParams.timestep*timeParams.dcdFreq/1e6; %ns
timeParams.framePeriodps=timeParams.framePeriodns*1000;
timeParams.expectedSimLength=timeParams.expectedSteps*timeParams.timestep/1e6;

%% TODO: GET TIME RAN FROM THE END OF THE OUTPUT FILE
% system(tail --lines=100 namdFiles.outfStd | awk "/^WRITING ... AT STEP )/ {print $?; exit}")

end

