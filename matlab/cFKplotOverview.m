function cFKplotOverview(ts, xs, Uxs, sol, cFKsimParams, cFKaxes, cFKi, runList, labelFigs)
% function cFKplotOverview(ts, xs, Uxs, sol, cFKsimParams, cFKaxes, cFKi, runList, labelFigs)
% Make a nice overview plot for cFK sims.
% Plots Temp, spring/substrate Energy ratio, displacement, chain width (relevant for finite length chains), and solitons.
    global angstrom;

    axes(cFKaxes.temp); hold on;
    cFKplot(ts*1e9, Uxs(:,1)/1.38e-23/cFKsimParams.N*2, cFKi, 1, ':.', 'Temperature', '', 'T(K)', runList, labelFigs);

    axes(cFKaxes.energy);hold on;
    hE = cFKsimParams.h * (cos(xs*2*pi/(cFKsimParams.WL/2))+1);
    Eratio=sum(hE,2)./Uxs(:,2);
    cFKplot(ts*1e9, Eratio, cFKi, 1, '', 'Energy ratio: E_h/E_{spring}', '', 'Ratio', '', labelFigs);
    horizontalLine(Eratio(end),'--', cFKi); 

    axes(cFKaxes.x1);hold on;
    ps=[1,size(xs,2),20,80]
    cFKplot(ts*1e9, xs(:,ps(1))*1e10, cFKi, 1, '.', 'Position of particles 1,N(scaled)', '', ['x (' angstrom ')'], '', 0);
    cFKplot(ts*1e9, (xs(:,ps(2))-xs(1,ps(2))+xs(1,ps(1)))*1e10, cFKi+16, 1, '-', 'Position of particles 1,N', '', ['x (' angstrom ')'], {'1','N'}, labelFigs);
    cFKplot(ts*1e9, (xs(:,ps(3))-xs(1,ps(3))+xs(1,ps(1)))*1e10, cFKi+17, 1, '-', 'Position of particles 1,N', '', ['x (' angstrom ')'], {'1','N','20'}, labelFigs);
    cFKplot(ts*1e9, (xs(:,ps(4))-xs(1,ps(4))+xs(1,ps(1)))*1e10, cFKi+18, 1, '-', 'Position of particles 1,N', '', ['x (' angstrom ')'], {'1','N','20','80'}, labelFigs);

    axes(cFKaxes.chainwidth); hold on;
    %solWidth=max(sol,[],2)-min(sol,[],2);
    %solWidth=solWidth-solWidth(3);
    width = xs(:,end)-xs(:,1);
    noSolitonWidth = (cFKsimParams.N-1)*cFKsimParams.lambda;
    width = (width - noSolitonWidth)/cFKsimParams.lambda;
    %cFKplot(ts*1e9, (xs(:,end)-xs(:,1))*1e10, cFKi, 1, '-', 'Chain Length','time (ns)', ['length (' angstrom ')'], '', labelFigs);
    cFKplot(ts*1e9, width, cFKi, 1, '-', 'Net expansion solitons','time (ns)', ['u_i width (' angstrom ')'], '', labelFigs);
    %colorLinePlot(0,(xs(1,end)-xs(1,1))*1e10,1,1,'o');
    if (labelFigs) 
        %horizontalLine( (cFKsimParams.N-1)*cFKsimParams.a*1e10, '--', cFKi); 
    end

    axes(cFKaxes.sol); hold on;
    solsToPlot=round(linspace(1,size(sol,1),10));
    solshift=bsxfun(@minus,sol,sol(:,1));
    cFKplot(1:length(sol(end,:)), solshift(end,:)*1e10, cFKi, 2, '.', 'Soliton at end of simulation', 'particle i', ['u_i (' angstrom ')'], '', labelFigs);
    if (labelFigs)
        text(0.05,0.7,sprintf(cFKsimParams.allStr),'Units','normalized'); 
        text(-0.6,1.05,'cFK Finite Chain Short Channel Results','Units','normalized','FontSize',15,'FontWeight','bold');
    end
end
