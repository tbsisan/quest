#!/bin/bash
# note: a function exe() to echo and execute commands is defined and exported in ~/.bashrc

source ../shell/randomInt.sh
myrand=$(getRandomInt);

if command -v module >/dev/null 2>&1; then
    module load intel
fi

if [ -z $1 ]; then
    compiler="ifort"
else
    compiler=$1
fi

set_parameters() {
    # Compiled into executables
    Nlist=$( seq 56 50 56 ); export Nlist
    Nlist="56 112 225 450 900";
    Nlist="300";
    L=350; export Nlist
    eqtimes="5e6"; export eqtimes
    eqList=($eqtimes); export eqList
    startTlist=$( seq 500 100 500 ); export startTlist

    # Read from paramList.in file
    enslist=$( seq 0 1 9 ); export enslist
    klist="20 25 30 40 50 60 80 120"; export klist
    klist="1 2 4 8 16 32 64 128"; export klist
    klist="30"; export klist
    Flist="1.00e-16 1.67e-16 2.78e-16 4.64e-16 7.74e-16 1.29e-15 2.15e-15 3.59e-15 5.99e-15 1.00e-14"; export Flist
    Flist="0"; export Flist
    Flist=($Flist);
}

make_cFKdata_precompile() {
    #sed "s/ N=\([0-9]\{2,\}\), Nsim=\([0-9]\{2,\}\), channelWL=/ N=$Ni, Nsim=$Ni, channelWL=/" <cFKdata.xy.base.f90 >cFKdata.xy.N.f90
    Li=$(($Ni+50))
    sed "s/ N=\([0-9]\{2,\}\), Nsim=\([0-9]\{2,\}\), channelWL=\([0-9]\{2,\}\)/ N=$Ni, Nsim=$Ni, channelWL=$Li/" <cFKdata.xy.base.f90 >cFKdata.xy.N.f90
    sed "s/coolDownSteps=0/coolDownSteps=$eqti/" <cFKdata.xy.N.f90 >cFKdata.xy.eqt.f90
    sed "s/Tstart=0/Tstart=$Ti/" <cFKdata.xy.eqt.f90 >cFKdata.xy.batch.f90
}

make_cFK_precompile() {
    sed "s/paramList.in/params\/paramList.$customRun.in/" <cFK.xy.f90 >cFK.xy.batch.f90
}

recompile() {
        # Update timestamps so make recompiles fortran source
        sleep 0;
        touch cFKdata.xy.batch.f90
        touch cFK.xy.batch.f90
        make ${compiler}.batch.out
}

make_paramList.in() {
    # This only happends for parameter values that were set to zero in paramList.in

    #sed "/ens = / s/ 0\{1,\}\([^0-9.]\)/ $ensi\1/g" <paramList.in >paramList.ens.in
    sed -r "/ens = [0 ]*\$/ s/ 0/ $ensi/g" <paramList.in >paramList.ens.in
    #sed -r "/k = / s/ 0\([^.]\)/ $ki\1/g" <paramList.ens.in >paramList.batch.in
    # the -r is for extended regex.  If you trap matches with (), refer back with \\1, \\2, etc
    sed -r "/k = [0.e+ ]*\$/ s/ 0.00e\+00/ ${ki}/g" <paramList.ens.in >paramList.k.in
    sed -r "/G = [0.e+ ]*\$/ s/ 0.00e\+00/ ${Fi}/g" <paramList.k.in >paramList.batch.in
    cp paramList.batch.in $paramListi
}

launch_jobs() {
    if command -v msub >/dev/null 2>&1; then
        questruncmd=questbatches/run.$customRun.sh
        sed "s/ifort.out/$fortexe/" <questbatch.sh >$questruncmd
        chmod 755 $questruncmd
        exe msub -N r${myrand}eq${eqti}Tst${Ti}N${Ni}E${ensi}k${ki}F${Fi} -l procs=1,walltime=00:30:00 -joe -V -o seeout.log $questruncmd
    else
        exe ./questbatches/$fortexe
    fi
}

set_parameters
./paramList.pl

for eqti in ${eqList[@]}
do
for Ti in $startTlist
do
for Ni in $Nlist
do
for ensi in $enslist
do
for ki in $klist
do
for Fi in ${Flist[@]}
do
    myrand=$(getRandomInt);
    echo "$eqti, $Ti, $Ni, $ensi, $ki, $Fi, $myrand";
    customRun=r${myrand}.eq${eqti}.Tst${Ti}.N${Ni}.ens$ensi.k${ki}.F${Fi}
    #INTEGER, PARAMETER :: N=208, Nsim=208,  channelWL=200
    cFKdataChanges=$(( ${#eqList[@]} * ${#startTlist[@]} * ${#Nlist[@]} ))
    cFKchanges=${#Flist[@]}
    make_cFKdata_precompile
    make_cFK_precompile
    fortexe=${compiler}.$customRun.out
    paramListi=params/paramList.$customRun.in
    if [ 1 ]; then #"$cFKchanges" -gt "1" -o "$cFKdataChanges" -gt "1" ]; then
        echo "cFK changes: $cFKchanges, cFKdata changes: $cFKdataChanges"
        recompile
        cp ${compiler}.batch.out questbatches/${fortexe}
    fi
    make_paramList.in
    launch_jobs
done
done
done
done
done
done
