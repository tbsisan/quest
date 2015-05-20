function [ newTimeIndexes, newTimesPs ] = sliceAndCropTimes( fullTimes, timeParams, psSkip, cropRange )

nsTimes = fullTimes;
originalPsSkip = 1000*(nsTimes(2)-nsTimes(1));
if psSkip < originalPsSkip
    display('WARNING: cant increase time frequency of data');
    fraction = 1;
else
    fractionDataPts = originalSkip/psSkip;
    numberDataPts = length(nsTimes);
    numberNewDataPts = numberDataPts*fractionDataPts;
    newTimeIndexes = round(linspace(1,numberDataPts,numberNewDataPts));
    newTimesNs = nsTimes(newTimeIndexes);
end
newTimesLength = length(newTimesNs);
if ( any(cropRange<0 || cropRange>1) || length(cropRange)~=2 || cropRange(1)>=cropRange(2) )
    display('WARNING: crop range must be 2 element array [a b] with b>a and both between 0 and 1')
else
    minTimeIndex = round(cropRange(1)*newTimesLength);
    minTimeIndex(minTimeIndex == 0)=1; % a way to write: set minTimeIndex=1 if it equals zero;
    maxTimeIndex = round(cropRange(2)*newTimesLength);
    cropNewTimesNs = newTimesNs(minTimeIndex : maxTimeIndex);
    cropNewIndexes = newTimeIndexes(minTimeIndex : maxTimeIndex);
end
newTimeIndexes = cropNewIndexes;
newTimesPs = cropNewTimesNs*1000;
%timeParams.framePeriodps
%originalTimes=timeParams.framePeriodps*[0:1:timeSteps-1];
%psTimes

