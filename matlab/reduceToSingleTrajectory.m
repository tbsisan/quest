function [ oneXyzs ] = reduceToSingleTrajectory( xyzs, strategy )
% flatten trajectory of many particles into one trajectory
% strategy is either a cell array of strings (generally a list of config options) or a string with the reduction strategy
    if (strategy == 'trackCOM')
        oneXyzs = mean(xyzs,2);
    elseif (strategy == 'trackMiddle')
        oneXyzs = squeeze( centralAtomsXyzs( :, round(size(centralAtomsXyzs,2)/2), : ) );
    elseif (strategy == 'trackFirst')
        oneXyzs = squeeze( centralAtomsXyzs( :, 1, : ) );
    elseif (strategy == 'trackLast')
        oneXyzs = squeeze( centralAtomsXyzs( :, end, : ) );
    end

end
