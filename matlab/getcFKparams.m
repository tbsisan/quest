function [ cFKparams ] = getcFKparams( logfile )

[~,hStr]=system(['awk "/^h:/ {printf \"%f\", \$2; exit}" <' logfile]);
cFKparams.h=str2num(hStr);
[~,kStr]=system(['awk "/^k:/ {printf \"%f\", \$2; exit}" <' logfile]);
cFKparams.k=str2num(kStr);
[~,TempStr]=system(['awk "/^Temp eq:/ {printf \"%f\", \$3; exit}" <' logfile]);
cFKparams.Temp=str2num(TempStr);
[~,Nstr]=system(['awk "/^particles N:/ {printf \"%f\", \$3; exit}" <' logfile]);
cFKparams.N=str2num(Nstr);
[~,Lstr]=system(['awk "/^System length units:/ {printf \"%f\", \$4; exit}" <' logfile]);
cFKparams.L=str2num(Lstr);
[~,aStr]=system(['awk "/^ax:/ {printf \"%f\", \$2; exit}" <' logfile]);
cFKparams.a=str2num(aStr);
[~,WLstr]=system(['awk "/^WL:/ {printf \"%f\", \$2; exit}" <' logfile]);
cFKparams.WL=str2num(WLstr);
[~,eqTstr]=system(['awk "/^eq steps:/ {printf \"%f\", \$3; exit}" <' logfile]);
cFKparams.eqtime=str2num(eqTstr);
