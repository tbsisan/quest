function [ oneXyzs ] = reduceToSingleTrajectory( hostAtomsXyzs, strategy )
% flatten trajectory of many particles into one trajectory
% strategy is either a cell array of strings (generally a list of config options) or a string with the reduction strategy
    switch strategy
        case 'trackCOM'
            oneXyzs = mean(hostAtomsXyzs,2); %the second dimension should be the list of atoms
        case 'trackMiddle'
            oneXyzs = squeeze( hostAtomsXyzs( :, round(size(centralAtomsXyzs,2)/2), : ) );
        case 'trackFirst'
            oneXyzs = squeeze( hostAtomsXyzs( :, 1, : ) );
        case 'trackLast'
            oneXyzs = squeeze( hostAtomsXyzs( :, end, : ) );
    end

end
