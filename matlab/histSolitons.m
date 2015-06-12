function [ avgSol, medianSol, avgSolInt, stdSol, rmsqDev, runs, Ns ] = histSolitons( moduleData )
% Determine soliton density and the rms displacement as a function of number of cFK monomers.

md = moduleData;
Nlist = unique(md.solitons.N);
avgSol = zeros(1,length(Nlist));
stdSol = avgSol;
medianSol = avgSol;
avgSolInt = avgSol;
runs = avgSol;
Ns = avgSol;
stdDisp = avgSol;

for Ni=1:length(Nlist)
    NiLogicals = ( md.solitons.N==Nlist(Ni) );
    Ns(Ni) = Nlist(Ni);
    avgSol(Ni) = mean( md.solitons.num(NiLogicals)/Ns(Ni) );
    medianSol(Ni) = median( md.solitons.num(NiLogicals)/Ns(Ni) );
    avgSolInt(Ni) = mean( round(md.solitons.num(NiLogicals))/Ns(Ni) );
    stdSol(Ni) = std( md.solitons.num(NiLogicals)/Ns(Ni) );
    runs(Ni) = sum( NiLogicals );
    rmsqDev(Ni) = sqrt(mean( ( md.displacement.total(NiLogicals) ).^2));
end


