function [ reducedTimes, timeIndexes ] = reduceTimes ( originalTimes, timeReduction )
timeReductionStr='Reducing time data using strategy: %s';
timeSteps = length(originalTimes);
if (timeReduction > 1 && timeReduction < timeSteps)
    timeIndexes = round(linspace( 1, timeSteps, timeReduction));
    display(sprintf(timeReductionStr,'absolute number of steps'));
    display(sprintf('\toriginal steps: %i, new steps: %i',timeSteps,timeReduction));
elseif (timeReduction <= 1 && timeReduction > 0)
    newSteps = round(timeSteps*timeReduction);
    timeIndexes = round( linspace( 1, timeSteps, newSteps ) );
    display(sprintf(timeReductionStr,'fraction of total number of steps'));
    display(sprintf('\toriginal steps: %i, new steps: %i',timeSteps,newSteps));
else
    timeIndexes = 1:1:timeSteps;
    display(sprintf(timeReductionStr,'Using all data'));
    display(sprintf('\toriginal steps: %i',timeSteps));
end
reducedTimes=originalTimes(timeIndexes);
