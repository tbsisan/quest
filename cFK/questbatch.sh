#!/bin/bash
#MSUB -A p20200

module load intel
cd $PBS_O_WORKDIR
echo "in compute node dir: $(pwd)"
./questbatches/ifort.out
