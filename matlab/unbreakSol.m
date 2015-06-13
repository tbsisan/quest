function [ solM ] = unbreakSol(solM,lambda)
% Patch up soliton function u_i (rows are timesteps) to be contiguous

for i=1:size(solM,1)
    sol=solM(i,:);
    soldiff=diff(sol);
    numsolbreaks=sum(abs(soldiff)>lambda/2);
    while numsolbreaks>0
        solbreak1=find(abs(soldiff)>lambda/2,1);
        if soldiff(solbreak1)<0
            sol=[sol(1:solbreak1) sol(solbreak1+1:end)+lambda];
        elseif soldiff(solbreak1)>0
            sol=[sol(1:solbreak1) sol(solbreak1+1:end)-lambda];
        end
        soldiff=diff(sol);
        numsolbreaks=sum(abs(soldiff)>lambda/2);
    end
    % sol=sol-min(sol);
    solM(i,:)=sol;
end

end
