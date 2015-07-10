function ui = uFvdM(u1_bc,uN_bc,halfi,l0,oneOverP0) 
    two=2;
    oneOverP0
    l0
    A=pi/two/l0/l0
    lambda=1.228e-10;
    kv=two*pi/lambda;
    N=100;
    ui=zeros(1,N);
    figure;
    hold on;
    if (u1_bc == -1.0)
          ui(1) = lambda/pi * (asin(l0*oneOverP0));
    else
          ui(1) = u1_bc;
    end
    eps = lambda*1/20/l0*sign(ui(1))
    ui(2) = ui(1) + lambda*( A*sin(kv*ui(1))+oneOverP0 );
    ui(2) = ui(1)/l0; % - eps; %+ lambda*( A*sin(kv*ui(1))+oneOverP0 );
    ui(2:end) = 0;
    %for i=3:halfi
    %    ui(i) = 2*ui(i-1)-ui(i-2) + lambda*( A*sin(kv*ui(i-1)) );
    %end
    % guess 5 more times 
    for guessi=1:3
    for i=2:halfi-1
        ui(i) =0.5* ( ui(i-1) + ui(i+1) - lambda*( A*sin(kv*ui(i-1)) ) );
    end
    end
    for guessi=1:30
        ui(1) = ( ui(2) - lambda*( A*sin(kv*ui(1)) ) +lambda*oneOverP0);
    for i=2:halfi-1
        ui(i) =0.5* ( ui(i-1) + ui(i+1) - lambda*( A*sin(kv*ui(i-1)) ) );
    end
    plot(1:length(ui),ui/lambda,'.-');
    end
    axis([0 length(ui) -1 1]);
    title(sprintf('TBS: 1/P0=%.3f, L0=%.3f, A=%.3f',oneOverP0,l0,A),'interpreter','tex');



    if ~isempty(uN_bc)
        if (uN_bc == -1.0)
              ui(N) = lambda/pi * asin(l0*oneOverP0);
        else
              ui(N) = uN_bc;
        end
        ui(N-1) = ui(N) - lambda*( A*sin(kv*ui(1))+oneOverP0 );
        for i = N-2:-1:halfi+1
            ui(i) = 2*ui(i+1)-ui(i+2) + lambda*( A*sin(kv*ui(i+1)) );
        end
    end

end
