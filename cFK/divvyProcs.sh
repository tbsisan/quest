#!/bin/bash
# note: a function exe() to echo commands is defined and exported in ~/.bashrc
module load intel
Nlist=$( seq 60 30 200 ); export Nlist
Nlist=$( seq 90 1 90 ); export Nlist
enslist=$( seq 1 1 1 ); export enslist
#klist=$( seq 1 1 5 ); export enslist
klist="1 1.46779926762207 2.15443469003188 3.16227766016838 4.64158883361278 6.81292069057961 10 14.6779926762207 21.5443469003188 31.6227766016838 46.4158883361278 68.1292069057961 100";
klist="3.2"; export klist
klist="60 80 100 120 140 160";
klist="100 130 160";
Flist="1.0e-17 2.5e-17 5.0e-17 1.00e-16 2.50e-16 5e-16 1.0e-15 2.5e-15 5e-15"; export Flist
Flist="1.00e-16 1.67e-16 2.78e-16 4.64e-16 7.74e-16 1.29e-15 2.15e-15 3.59e-15 5.99e-15 1.00e-14"; export Flist
Flist="0"; export Flist
./sweep.pl
for Ni in $Nlist
do
for ensi in $enslist
do
for ki in $klist
do
for Fi in $Flist
do
#INTEGER, PARAMETER :: N=208, Nsim=208,  channelWL=200
    sed "s/ N=\([0-9]\{2,\}\), Nsim=\([0-9]\{2,\}\), channelWL=/ N=$Ni, Nsim=$Ni, channelWL=/" <cFKdata.xy.base.f90 >cFKdata.xy.forquest.f90
    # sed "s/ N=\([0-9]\{2,\}\), Nsim=\([0-9]\{2,\}\), channelWL=\([0-9]\{2,\}\)/ N=$Ni, Nsim=$Ni, channelWL=$Ni/" <cFKdata.xy.base.f90 >cFKdata.xy.forquest.f90
    sed "s/sweep.in/sweep.ens${ensi}.k${ki}.F${Fi}.in/" <cFK.xy.f90 >cFK.xy.forquest.f90
    #sed "/ens = / s/ 0\{1,\}\([^0-9.]\)/ $ensi\1/g" <sweep.in >sweep.ens.in
    sed -r "/ens = [0 ]*\$/ s/ 0/ $ensi/g" <sweep.in >sweep.ens.in
    #sed -r "/k = / s/ 0\([^.]\)/ $ki\1/g" <sweep.ens.in >sweep.forquest.in
    # the -r is for extended regex.  If you trap matches with (), refer back with \\1, \\2, etc
    sed -r "/k = [0.e+ ]*\$/ s/ 0.00e\+00/ ${ki}/g" <sweep.ens.in >sweep.k.in
    sed -r "/G = [0.e+ ]*\$/ s/ 0.00e\+00/ ${Fi}/g" <sweep.k.in >sweep.forquest.in
    cp sweep.forquest.in sweep.ens${ensi}.k${ki}.F${Fi}.in
    sleep 0;
    # Update timestamps so make recompiles fortran source
    touch cFKdata.xy.forquest.f90
    touch cFK.xy.forquest.f90
    make ifort.forquest.out
    cp ifort.forquest.out questbatches/ifort.N${Ni}.ens$ensi.k${ki}.F${Fi}.out
    sed "s/ifort.out/ifort.N${Ni}.ens$ensi.k${ki}.F${Fi}.out/" <questbatch.sh >questbatches/runN${Ni}.ens${ensi}.k${ki}.F${Fi}.sh
    exe msub -N N${Ni}E${ensi}k${ki}F${Fi} -l procs=1,walltime=00:05:00 -joe -V -o seeout.log questbatches/runN${Ni}.ens${ensi}.k${ki}.F${Fi}.sh
done
done
done
done
