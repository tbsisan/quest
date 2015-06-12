function [ cFKparams ] = getcFKsimParams( logfile )
global angstrom;

[~,hStr]=system(['awk "/ h:/ {print \$2; exit}" <' logfile]);
cFKparams.h=str2num(hStr);

[~,kStr]=system(['awk "/ k:/ {print \$2; exit}" <' logfile]);
cFKparams.k=str2num(kStr);

[~,TempStr]=system(['awk "/ Temp eq:/ {print \$3; exit}" <' logfile]);
cFKparams.Temp=str2num(TempStr);

[~,startTstr]=system(['awk "/ Temp start:/ {print \$3; exit}" <' logfile]);
cFKparams.startT=str2num(startTstr);

[~,Nstr]=system(['awk "/ particles N:/ {print \$3; exit}" <' logfile]);
cFKparams.N=str2num(Nstr);

[~,Lstr]=system(['awk "/ System length units:/ {print \$4; exit}" <' logfile]);
cFKparams.L=str2num(Lstr);

[~,Fstr]=system(['awk "/ G:/ {print \$2; exit}" <' logfile]);
cFKparams.F=str2num(Fstr);

[~,aStr]=system(['awk "/ ax:/ {print \$2; exit}" <' logfile]);
cFKparams.a=str2num(aStr);

[~,WLstr]=system(['awk "/ WL:/ {print \$2; exit}" <' logfile]);
cFKparams.WL=str2num(WLstr);

[~,WLperNstr]=system(['awk "/ WLperN:/ {print \$2; exit}" <' logfile]);
cFKparams.WLperN=str2num(WLperNstr);

%[~,eqTstr]=system(['awk "/ eq steps:/ {printf \"%f\", \$3; exit}" <' logfile]);
[~,eqTstr]=system(['awk "/ eq steps:/ {print \$3; exit}" <' logfile]);
cFKparams.eqtime=str2num(eqTstr);

[~,timeSteps]=system(['awk "/ Total steps:/ {print \$3; exit}" <' logfile]);
cFKparams.timeSteps=str2num(timeSteps);

[~,totalTime]=system(['awk "/ Total time \(s\):/ {print \$4; exit}" <' logfile]);
cFKparams.simSeconds=str2num(totalTime);

[~,ens]=system(['awk "/ ens:/ {print \$2; exit}" <' logfile]);
cFKparams.ens=int16(str2num(ens));
cFKparams.ensStr=['run ' num2str(cFKparams.ens)];

cFKparams.kbar=(cFKparams.WL/cFKparams.WLperN/2/pi)^2*cFKparams.k/cFKparams.h;
c=cFKparams;
%sprintfAng=regexprep(angstrom,'AA','\\AA');
cFKparams.allStr=[  'h: ' sprintf('%.1e',c.h/1.38e-23) ' K\n'...
                    'k: ' sprintf('%.1e',c.k/1.38e-23*1e-20) ' K/' angstrom '**2\n'...
                    'kbar: ' sprintf('%.1e',c.kbar) '\n'...
                    'N: ' num2str(c.N) '\n'...
                    'L: ' num2str(c.L) '\n'...
                    'a: ' num2str(c.a*1e10) ' ' angstrom '\n'...
                    'l: ' num2str(c.WL*1e10) ' ' angstrom '\n'...
                    'Force: ' num2str(c.F) '\n'...
                    'Temp start: ' num2str(c.startT) ' K\n'...
                    'Temp final: ' num2str(c.Temp) ' K\n'...
                    'cooldown in: ' sprintf('%.1e',c.eqtime) ' steps\n'...
                    'sim steps: ' sprintf('%.1e',c.timeSteps) '\n'...
                    ];
