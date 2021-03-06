function [ reduced, reducedIndexes ] = cFKreduceVars( xs, ui, Uxs, ts, cFKsettings );
    [ slicedTimes, sliceIndexes ] = reduceTimes( ts, cFKsettings.sliceTimes );
    [ croppedTimes, cropIndexes ] = cropTimes( ts, cFKsettings.cropTimes );
    reducedIndexes = intersect(sliceIndexes,cropIndexes);
    disp( sprintf( '\tReducing Variables from %i down to %i', length(ts), length(reducedIndexes) ) );
    reduced.xs = xs(reducedIndexes,:);
    reduced.ui = ui(reducedIndexes,:);
    reduced.Uxs = Uxs(reducedIndexes,:);
    reduced.ts = ts(reducedIndexes);
end
