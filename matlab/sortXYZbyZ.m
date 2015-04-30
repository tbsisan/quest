[sxs, sys, szs] = sortXYZbyZ( xs,ys,zs )

XYZs=[xs ys zs];
[sXYZs]=sortrows(xyzs,3);
sxs=sXYZs(:,1);
sys=sXYZs(:,2);
szs=sXYZs(:,3);

end
