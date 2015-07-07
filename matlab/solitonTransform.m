function [ ui ] = solitonTransform( xs, lambda )
    particles=1:size(xs,2);
    %TBS ui=bsxfun(@minus,xs-xs(1,1),particles*lambda);
    ui=bsxfun(@minus,xs-lambda/2,particles*lambda);
end
