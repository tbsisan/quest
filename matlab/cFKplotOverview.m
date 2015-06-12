function cFKplotOverview(ts, xs, Uxs, sol, cFKsimParams, cFKaxes, cFKi, runList, labelFigs)
% Make a nice overview plot for cFK sims.
% Plots Temp, spring/substrate Energy ratio, displacement, chain width (relevant for finite length chains), and solitons.
    global angstrom;

    axes(cFKaxes.temp); hold on;
    cFKplot(ts*1e9, Uxs(:,1)/1.38e-23/cFKsimParams.N*2, cFKi, 1, ':o', 'Temperature', '', 'T(K)', runList, labelFigs);

    axes(cFKaxes.energy);hold on;
    hE = cFKsimParams.h * (sin(xs*2*pi/(cFKsimParams.WL/2))+1);
    Eratio=sum(hE,2)./Uxs(:,2);
    cFKplot(ts*1e9, Eratio, cFKi, 1, '', 'Energy ratio: E_h/E_{spring}', '', 'Ratio', '', labelFigs);
    horizontalLine(Eratio(end),'--', cFKi); 

    axes(cFKaxes.x1);hold on;
    cFKplot(ts*1e9, xs(:,1)*1e10, cFKi, 1, '', 'Position of particle 1', '', ['x (' angstrom ')'], '', labelFigs);

    axes(cFKaxes.chainwidth); hold on;
    cFKplot(ts*1e9, (xs(:,end)-xs(:,1))*1e10, cFKi, 1, ':', 'Chain Length','time (ns)', ['length (' angstrom ')'], '', labelFigs);
    colorLinePlot(0,(xs(1,end)-xs(1,1))*1e10,1,1,'o');
    if (labelFigs) 
        horizontalLine( (cFKsimParams.N-1)*cFKsimParams.a*1e10, '--', cFKi); 
    end

    axes(cFKaxes.sol); hold on;
    cFKplot(1:length(sol(end,:)), sol(end,:)*1e10, cFKi, 2, '.', 'Soliton at end of cooling', 'particle i', ['u_i (' angstrom ')'], '', labelFigs);
    if (labelFigs)
        text(0.05,0.8,sprintf(cFKsimParams.allStr),'Units','normalized'); 
        text(-0.5,1.05,'cFK Finite Chain Results','Units','normalized','FontSize',15,'FontWeight','bold');
    end
end
