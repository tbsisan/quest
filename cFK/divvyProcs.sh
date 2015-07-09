#!/bin/bash
# note: a function exe() to echo and execute commands is defined and exported in ~/.bashrc

source ../shell/randomInt.sh
myrand=$(getRandomInt);

# If the module command exists, then we are on Quest, and load the intel compiler.
if command -v module >/dev/null 2>&1; then
    module load intel
fi

# If this was run with at least one argument, then it sets the compiler to use. Otherwise assume intel.
if [ -z $1 ]; then
    compiler="ifort"
else
    compiler=$1
fi
echo "compiler is $compiler"

# If there is a second argument passed, then we won't submit an msub job, in other words we will run on the local processor, even if we are on quest.
if [ -z $2 ]; then
    if command -v msub >/dev/null 2>&1; then
        submitMsub=true
    else
        submitMsub=false
    fi
else
    submitMsub=false
fi
echo "Will we use msub to submit jobs? $submitMsub"

set_parameters() {
    # Compiled into executables
    solList=$( seq 0 1 0 );
    Nlist=$( seq 56 50 56 ); export Nlist
    Nlist="56 112 225 450 900";
    Nlist="100";
    L=150; export Nlist
    eqtimes="5e5"; export eqtimes
    eqList=($eqtimes); export eqList
    startTlist=$( seq 0 1 0 ); export startTlist
    Tlist="5 10 17.8 31.6 56.2 77 100"; export Tlist
    Tlist="25"; export Tlist
# Temp = 1 1.77827941003892 3.16227766016838 5.62341325190349 10 17.7827941003892 31.6227766016838 56.2341325190349 100
    aList="1.005 1.01 1.02 1.03 1.04 1.05";
    aList="1.07";
    kTrapList="0"; # The trap strength is currently hardcoded to be the interparticle spring constant
    moveByList="0.50 0.55 0.60 0.65 0.70 0.75 0.80 0.85 0.90 0.95"
    moveByList="0.65 0.675 0.70 0.725 0.75 0.775 0.80 0.85"
    moveByList="0.65 0.70 0.75 0.80"; # 

    # Read from paramList.in file
    enslist=$( seq 0 1 0 ); export enslist
    klist="2 2.5 3 4 5 6 8 10 12 16 20 24 32 48 64 96 128 160 192"
    klist="5"
    Flist="1.00e-16 1.67e-16 2.78e-16 4.64e-16 7.74e-16 1.29e-15 2.15e-15 3.59e-15 5.99e-15 1.00e-14"; export Flist
    Flist="1.00e-11 1.82e-11 3.31e-11 6.03e-11 1.10e-10 2.00e-10 4.00e-10";
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
    sed "s/kTrap = 0/kTrap = $ki/" <cFKdata.xy.a.f90 >cFKdata.xy.kTr.f90
    sed "s/positioningMultiplier = 0.00/positioningMultiplier = $moveByi/" <cFKdata.xy.kTr.f90 >cFKdata.xy.batch.f90
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
    if [ "$submitMsub" = true ]; then
        questruncmd=questbatches/run.$customRun.sh
        sed "s/ifort.out/$fortexe/" <questbatch.sh >$questruncmd
        chmod 755 $questruncmd
        echo $questruncmd
        exe msub -N $customRun -l procs=1,walltime=00:20:00 -joe -V -o seeout.log $questruncmd
    else
        # be nice to not bog down the system.
        exe nice ./questbatches/$fortexe >>fort.log &
    fi
}

set_parameters
./paramList.pl

loopvar=0;
for moveByi in $moveByList
do
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
    loopvar=$((loopvar+1))
    stTi=$Ti
    myrand=$(getRandomInt);
    # runID="sol${soli}_mv23"; export runID
    runID="startHold"; export runID
    echo "T$Ti, M$moveByi, kTr$kTrapi, a$ai, sol$soli, ID$runID, eq$eqti, stTi$stTi, N$Ni, ens$ensi, k$ki, F$Fi, $myrand";
    customRun=r${myrand}.T${Ti}.M${moveByi}.kTr${kTrapi}.sol${soli}.a${ai}.eq${eqti}.Tst${stTi}.N${Ni}.ens$ensi.k${ki}.F${Fi}
    #INTEGER, PARAMETER :: N=208, Nsim=208,  channelWL=200
    cFKdataChanges=$(( ${#eqList[@]} * ${#startTlist[@]} * ${#Nlist[@]} * ${#aList[@]} * ${#kTrapList[@]} * ${#moveByList[@]} ))
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
    if [ "$submitMsub" = true ]; then
        launch_jobs
    else
        numjobs=$(top -b -n1 | grep fort | wc -l | tr -d '\n')
        echo "numjobs:$numjobs, submitted jobs:$loopvar"
        while ([[ $numjobs -gt 8 ]]); do
            sleep 5
            numjobs=$(top -b -n1 | grep fort | wc -l)
        done
        sleep 1
        launch_jobs
    fi
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
done
