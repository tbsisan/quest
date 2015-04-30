function [ timeData ] = basicInfoFromNamd( namdFiles )
%UNTITLED4 Summary of this function goes here
%   Detailed explanation goes here

[~,timestep] = system( ['awk "/Info: TIMESTEP/ {print \$3; exit}" <' namdFiles.outfStd] ); % '>' datf]);
[~,dcdFreq]  = system( ['awk "/Info: DCD FREQ/ {print \$4; exit}" <' namdFiles.outfStd] );
[~,runsteps] = system( ['awk "/TCL: run time:/ {print \$4; exit}" <' namdFiles.outfStd] );

timeData.timestep = str2num(timestep); %fs
timeData.dcdFreq  = str2num(dcdFreq);
timeData.runsteps = str2num(runsteps);

timeData.framePeriodns=timeData.timestep*timeData.dcdFreq/1e6; %ns
timeData.framePeriodps=timeData.framePeriodns*1000;
timeData.simLength=timeData.runsteps*timeData.timestep/1e6;

%% TODO: GET TIME RAN FROM THE END OF THE OUTPUT FILE
% system(tail --lines=100 namdFiles.outfStd | awk "/^WRITING ... AT STEP )/ {print $?; exit}")

end

