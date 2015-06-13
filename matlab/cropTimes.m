function [ reducedTimes, timeIndexes ] = cropTimes ( originalTimes, cropRange )

allIndexes=1:length(originalTimes);
effectiveLength=length(originalTimes)-1;
if any(cropRange>1) | any(cropRange<0 | length(cropRange)~=2)
    reducedTimes=originalTimes;
    timeIndexes=allIndexes;
    disp('WARNING: cropRange ([lower upper]) must 2 element vector, both be between 0 and 1');
else
    disp( sprintf( '\tCropping times to range [%.2f, %.2f]', cropRange(1), cropRange(2) ) );
    cropRange=round(cropRange*effectiveLength)+1;
    timeIndexes=cropRange(1):cropRange(end);
    reducedTimes=originalTimes(timeIndexes);
    disp( sprintf( '\tCropping times from %i down to %i', length(originalTimes), length(reducedTimes) ) );
end

end
