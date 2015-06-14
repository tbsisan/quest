function [ ui ] = solitonTransform( xs, lambda )
    particles=1:size(xs,2);
    ui=bsxfun(@minus,xs-xs(1,1),particles*lambda);
end
