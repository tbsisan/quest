G95OPTIM=-O5 -ffast-math -ftrace=full -fbounds-check #-Wall
SUNOPTIM=-O5 -fast -ftrap=underflow #-ftrap=overflow, -ftrap=underflow, etc, to check for type of floating point error
GNUOPTIM=-Ofast -fno-range-check
INTOPTIM=-O3 -W0

all : g95.out f95.out gfort.out ifort.out

# Use g95 compiler

g95.out : cFK.xy.f95 g95/cFKdata.o g95/RNG.o g95/dataTypes.o g95/WallMod.o
	g95 $(G95OPTIM) g95/cFKdata.o g95/RNG.o g95/dataTypes.o g95/WallMod.o cFK.xy.f95 -Ig95 -fmod=g95 -o g95.out

g95/cFKdata.o : g95/dataTypes.o cFKdata.xy.f95
	g95 $(G95OPTIM) -c cFKdata.xy.f95 -fmod=g95 -o g95/cFKdata.o

g95/RNG.o : g95/WallMod.o g95/cFKdata.o RNG.f95
	g95 $(G95OPTIM) -c RNG.f95 -fmod=g95 -o g95/RNG.o

g95/dataTypes.o : dataTypes.f95
	g95 $(G95OPTIM) -c dataTypes.f95 -fmod=g95 -o g95/dataTypes.o

g95/WallMod.o : WallMod.f95
	g95 $(G95OPTIM) -c WallMod.f95 -fmod=g95 -o g95/WallMod.o

# Use f95 compiler SUN

f95.out : cFK.xy.f95 f95/cFKdata.o f95/RNG.o f95/dataTypes.o f95/WallMod.o
	f95 $(SUNOPTIM) f95/cFKdata.o f95/RNG.o f95/dataTypes.o f95/WallMod.o cFK.xy.f95 -If95 -moddir=f95 -o f95.out

f95/cFKdata.o : f95/dataTypes.o cFKdata.xy.f95
	f95 $(SUNOPTIM) -c cFKdata.xy.f95 -moddir=f95 -o f95/cFKdata.o

f95/RNG.o : f95/WallMod.o f95/cFKdata.o RNG.f95
	f95 $(SUNOPTIM) -c RNG.f95 -moddir=f95 -o f95/RNG.o

f95/dataTypes.o : dataTypes.f95
	f95 $(SUNOPTIM) -c dataTypes.f95 -moddir=f95 -o f95/dataTypes.o

f95/WallMod.o : WallMod.f95
	f95 $(SUNOPTIM) -c WallMod.f95 -moddir=f95 -o f95/WallMod.o

# Use gfortran compiler

gfort.out : cFK.xy.f95 gfort/cFKdata.o gfort/RNG.o gfort/dataTypes.o 
	gfortran $(GNUOPTIM) gfort/cFKdata.o gfort/RNG.o gfort/dataTypes.o gfort/WallMod.o cFK.xy.f95 -Igfort -Jgfort -o gfort.out

gfort/cFKdata.o : gfort/dataTypes.o cFKdata.xy.f95
	gfortran $(GNUOPTIM) -c cFKdata.xy.f95 -Jgfort -Igfort -o gfort/cFKdata.o

gfort/RNG.o : gfort/WallMod.o gfort/cFKdata.o RNG.f95
	gfortran $(GNUOPTIM) -c RNG.f95 -Jgfort -o gfort/RNG.o

gfort/dataTypes.o : dataTypes.f95
	gfortran $(GNUOPTIM) -c dataTypes.f95 -Jgfort -o gfort/dataTypes.o

gfort/WallMod.o : WallMod.f95
	gfortran $(GNUOPTIM) -c WallMod.f95 -Jgfort -o gfort/WallMod.o

# Use ifortran compiler

ifort.out : cFK.xy.f90 ifort/cFKdata.o ifort/RNG.o ifort/dataTypes.o 
	ifort $(INTOPTIM) ifort/cFKdata.o ifort/RNG.o ifort/dataTypes.o ifort/WallMod.o cFK.xy.f90 -Iifort -module ifort -o ifort.out
	@echo "Turn on warnings if you want them, currently suppressed with -w compiler flag"

ifort.forquest.out : cFK.xy.forquest.f90 ifort/cFKdata.forquest.o ifort/RNG.o ifort/dataTypes.o 
	ifort $(INTOPTIM) ifort/cFKdata.forquest.o ifort/RNG.o ifort/dataTypes.o ifort/WallMod.o cFK.xy.forquest.f90 -Iifort -module ifort -o ifort.forquest.out
	@echo "Turn on warnings if you want them, currently suppressed with -w compiler flag"

ifort/cFKdata.o : ifort/dataTypes.o cFKdata.xy.base.f90
	ifort $(INTOPTIM) -c cFKdata.xy.base.f90 -Iifort -module ifort -o ifort/cFKdata.o

ifort/cFKdata.forquest.o : ifort/dataTypes.o cFKdata.xy.forquest.f90
	ifort $(INTOPTIM) -c cFKdata.xy.forquest.f90 -Iifort -module ifort -o ifort/cFKdata.forquest.o

ifort/RNG.o : ifort/WallMod.o ifort/cFKdata.forquest.o RNG.f90
	ifort $(INTOPTIM) -c RNG.f90 -module ifort -o ifort/RNG.o

ifort/dataTypes.o : dataTypes.f90
	ifort $(INTOPTIM) -c dataTypes.f90 -module ifort -o ifort/dataTypes.o

ifort/WallMod.o : WallMod.f90
	ifort $(INTOPTIM) -c WallMod.f90 -module ifort -o ifort/WallMod.o

clean: 
	rm *.mod *.o g95/*.mod g95/*.o f90/*.o f90/*.mod gfort/*.mod gfort/*.o ifort/*.mod ifort/*.o
#f95.out : cFK.xy.f95 cFKdata.xy.f95o RNG.f95o
#	f95 -O5 cFKdata.xy.f95o RNG.f95o cFK.xy.f95 -o f95.out

#cFKdataf95.o : dataTypesf95.o cFKdata.F95
#	f95 -O5 -c dataTypesf95.o cFKdata.F95 -o cFKdataf95.o

#RNG.o : RNG.f90
#	f95 -O5 -c RNG.f90 -o RNG.o

#dataTypesf95.o : dataTypes.F95
#	f95 -O5 -c dataTypes.F95 -o dataTypesf95.o

#	f95 -O5 cFKdataf95.o RNG.o cFK.xy.f95 -o f95.out
