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
    solList=$( seq 0 1 0 );
    Nlist=$( seq 56 50 56 ); export Nlist
    Nlist="56 112 225 450 900";
    Nlist="200";
    L=250; export Nlist
    eqtimes="5e5"; export eqtimes
    eqList=($eqtimes); export eqList
    startTlist=$( seq 0 1 0 ); export startTlist
    Tlist="77"; export Tlist
    Tlist="77 120 140"; export Tlist
    aList="0.91 0.92 0.93 0.94 0.95 0.96 0.97 0.98 0.99";
    aList="1.00 1.01 1.02 1.03 1.04 1.05 1.06 1.07 1.08 1.09"; export aList
    aList="1.10 1.12 1.14 1.16 1.18 1.20 1.22 1.25"; export aList
    aList="1.01 1.03 1.05 1.07 1.09 1.10 1.12 1.14 1.16 1.18 1.20 1.22 1.23 1.24 1.25 "; export aList
    aList="1.06 1.065 1.07 1.075 1.08 1.085 1.09 1.095 1.10 1.105"; export aList
    aList="0.98 0.96 0.95";
    aList="1.01 1.02 1.03 1.04 1.05";
    aList="1.13 1.14";
    aList="1.16 1.17 1.18 1.19 1.20"; export aList
    aList="1.05";
    aList="1.01 1.02 1.03 1.04 1.05 1.06 1.07 1.08 1.09 1.1 1.11 1.12 1.13 1.14 1.15 1.16 1.17 1.18 1.19 1.20"; export aList
    kTrapList="0";

    # Read from paramList.in file
    enslist=$( seq 0 1 0 ); export enslist
    klist="20 25 30 40 50 60 80 120"; export klist
    klist="10 100"; export klist
    klist="2 4 8 16 32 64 128"; export klist
    klist="2.5 5 10 20"; export klist
    klist="3 6 12 24 48 96"; export klist
    klist="160 192"
    klist="2 2.5 3 4 5 6 8 10 12 16 20 24 32 48 64 96 128 160 192"
    klist="0"
    Flist="1.00e-16 1.67e-16 2.78e-16 4.64e-16 7.74e-16 1.29e-15 2.15e-15 3.59e-15 5.99e-15 1.00e-14"; export Flist
    Flist="1.00e-11 1.82e-11 3.31e-11 6.03e-11 1.10e-10 2.00e-10 4.00e-10";
    Flist="2.01e-10"; export Flist
    Flist="0.00e+00";
    Flist=($Flist);
}

make_cFKdata_precompile() {
    #sed "s/ N=\([0-9]\{2,\}\), Nsim=\([0-9]\{2,\}\), channelWL=/ N=$Ni, Nsim=$Ni, channelWL=/" <cFKdata.xy.base.f90 >cFKdata.xy.N.f90
    Li=$(($Ni+50))
    sed "s/ N=\([0-9]\{2,\}\), Nsim=\([0-9]\{2,\}\), channelWL=\([0-9]\{2,\}\)/ N=$Ni, Nsim=$Ni, channelWL=$Li/" <cFKdata.xy.base.f90 >cFKdata.xy.N.f90
    sed "s/coolDownSteps=0/coolDownSteps=$eqti/" <cFKdata.xy.N.f90 >cFKdata.xy.eqt.f90
    sed "s/Tstart=0/Tstart=$stTi/" <cFKdata.xy.eqt.f90 >cFKdata.xy.T.f90
    sed "s/runID=''/runID='$runID'/" <cFKdata.xy.T.f90 >cFKdata.xy.ID.f90
    sed "s/aPercent=0/aPercent=$ai/" <cFKdata.xy.ID.f90 >cFKdata.xy.a.f90
    sed "s/kTrap = 0/kTrap = $kTrapi/" <cFKdata.xy.a.f90 >cFKdata.xy.batch.f90
}

make_cFK_precompile() {
    sed "s/paramList.in/params\/paramList.$customRun.in/" <cFK.xy.f90 >cFK.xy.params.f90
    sed "s/numSols = 0/numSols = $soli/" <cFK.xy.params.f90 >cFK.xy.batch.f90
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
    sed -r "/Temp = [0 ]*\$/ s/ 0/ ${Ti}/g" <paramList.k.in >paramList.T.in
    sed -r "/G = [0.e+ ]*\$/ s/ 0.00e\+00/ ${Fi}/g" <paramList.T.in >paramList.batch.in
    cp paramList.batch.in $paramListi
}

launch_jobs() {
    if command -v msub >/dev/null 2>&1; then
        questruncmd=questbatches/run.$customRun.sh
        sed "s/ifort.out/$fortexe/" <questbatch.sh >$questruncmd
        chmod 755 $questruncmd
        echo $questruncmd
        exe msub -N r${myrand}ID${runID}kTr${kTrapi}a${ai}eq${eqti}Tst${stTi}N${Ni}E${ensi}k${ki}F${Fi} -l procs=1,walltime=00:30:00 -joe -V -o seeout.log $questruncmd
    else
        exe ./questbatches/$fortexe
    fi
}

set_parameters
./paramList.pl

for kTrapi in $kTrapList
do
for soli in $solList
do
for ai in $aList
do
for eqti in ${eqList[@]}
do
for Ti in $Tlist
do
for stTi in $startTlist
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
    # runID="sol${soli}_mv23"; export runID
    runID="test"; export runID
    echo "T$Ti, kTr$kTrapi, a$ai, sol$soli, ID$runID, eq$eqti, stTi$stTi, N$Ni, ens$ensi, k$ki, F$Fi, $myrand";
    customRun=r${myrand}.T${Ti}.kTr${kTrapi}.sol${soli}.a${ai}.eq${eqti}.Tst${stTi}.N${Ni}.ens$ensi.k${ki}.F${Fi}
    #INTEGER, PARAMETER :: N=208, Nsim=208,  channelWL=200
    cFKdataChanges=$(( ${#eqList[@]} * ${#startTlist[@]} * ${#Nlist[@]} * ${#aList[@]} * ${#kTrapList[@]} ))
    cFKchanges=$(( ${#Flist[@]} * ${#solList[@]} ))
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
done
done
done
done
