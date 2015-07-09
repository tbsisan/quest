function ui = uFvdM(u1_bc,uN_bc,halfi,l0,oneOverP0) 
    two=2;
    oneOverP0
    l0
    A=pi/two/l0/l0
    lambda=1.228e-10;
    kv=two*pi/lambda
    N=100;
    ui=zeros(1,N);
    if (u1_bc == -1.0)
          ui(1) = -lambda/pi * (pi-asin(l0*oneOverP0));
    else
          ui(1) = u1_bc;
    end
    ui(2) = ui(1) + lambda*( A*sin(kv*ui(1))+oneOverP0 );
    for i=3:halfi
        ui(i) = 2*ui(i-1)-ui(i-2) + lambda*( A*sin(kv*ui(i-1)) );
    end

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
