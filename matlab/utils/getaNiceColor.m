function [ rgb ] = getaNiceColor( varargin )
% Get a color that is different from other colors (generally for plotting).
% The list of rgb values below is in an order where colors plotted should be 
% roughly maximally distinct from previous colors for best visibility.
% I found this color list  online somewhere, perhaps in an mfile cplot.m.
rgbs=[...
    0         0    1.0000; ...% 1
    1.0000         0         0; ...% 2
         0    1.0000         0; ...% 3
         0         0    0.1724; ...% 4
    1.0000    0.1034    0.7241; ...% 5
    1.0000    0.8276         0; ...% 6
         0    0.3448         0; ...% 7
    0.5172    0.5172    1.0000; ...% 8
    0.6207    0.3103    0.2759; ...% 9
         0    1.0000    0.7586; ...% 10
         0    0.5172    0.5862; ...% 11
         0         0    0.4828; ...% 12
    0.5862    0.8276    0.3103; ...% 13
    0.9655    0.6207    0.8621; ...% 14
    0.8276    0.0690    1.0000; ...% 15
    0.4828    0.1034    0.4138; ...% 16
    0.9655    0.0690    0.3793; ...% 17
    1.0000    0.7586    0.5172; ...% 18
    0.1379    0.1379    0.0345; ...% 19
    0.5517    0.6552    0.4828; ...% 20
    0.9655    0.5172    0.0345; ...% 21
    0.5172    0.4483         0; ...% 22
    0.4483    0.9655    1.0000; ...% 23
    0.6207    0.7586    1.0000; ...% 24
    0.4483    0.3793    0.4828; ...% 25
    0.6207         0         0; ...% 26
         0    0.3103    1.0000; ...% 27
         0    0.2759    0.5862; ...% 28
    0.8276    1.0000         0; ...% 29
    0.7241    0.3103    0.8276; ...% 30
    0.2414         0    0.1034; ...% 31
    0.9310    1.0000    0.6897; ...% 32
]; 
if nargin==0
    if length(get(0,'CurrentFigure'))>0
        childPlots=get(gca,'Children');
        numPlots=size(childPlots,1);
        n=numPlots+1;
    else
        n=1;
    end
elseif nargin==1
    n=varargin{1};
else
    display(['Too many arguments to ' mfilename]);
end

if (n > 32)
    n=n-32;
    warn(['COLOR INDEX OUT OF RANGE in ' mfilename]);
elseif (n<1)
    n=1;
    warn(['COLOR INDEX OUT OF RANGE in ' mfilename]);
end

rgb = rgbs( n,: );
