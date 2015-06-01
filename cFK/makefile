G95OPTIM=-O5 -ffast-math -ftrace=full -fbounds-check #-Wall
SUNOPTIM=-O5 -fast -ftrap=underflow #-ftrap=overflow, -ftrap=underflow, etc, to check for type of floating point error
GNUOPTIM=-Ofast -fno-range-check
INTOPTIM=-O3 -W0

all : g95.out f90.out gfort.out ifort.out

# Use g95 compiler

g95.out : cFK.xy.f90 g95/cFKdata.o g95/RNG.o g95/dataTypes.o g95/WallMod.o
	g95 $(G95OPTIM) g95/cFKdata.o g95/RNG.o g95/dataTypes.o g95/WallMod.o cFK.xy.f90 -Ig95 -fmod=g95 -o g95.out

g95/cFKdata.o : g95/dataTypes.o cFKdata.xy.base.f90
	g95 $(G95OPTIM) -c cFKdata.xy.base.f90 -fmod=g95 -o g95/cFKdata.o

g95/RNG.o : g95/WallMod.o g95/cFKdata.o RNG.f90
	g95 $(G95OPTIM) -c RNG.f90 -fmod=g95 -o g95/RNG.o

g95/dataTypes.o : dataTypes.f90
	g95 $(G95OPTIM) -c dataTypes.f90 -fmod=g95 -o g95/dataTypes.o

g95/WallMod.o : WallMod.f90
	g95 $(G95OPTIM) -c WallMod.f90 -fmod=g95 -o g95/WallMod.o

# Use f90 compiler SUN

f90.out : cFK.xy.f90 f90/cFKdata.o f90/RNG.o f90/dataTypes.o f90/WallMod.o
	f90 $(SUNOPTIM) f90/cFKdata.o f90/RNG.o f90/dataTypes.o f90/WallMod.o cFK.xy.f90 -If90 -moddir=f90 -o f90.out

f90/cFKdata.o : f90/dataTypes.o cFKdata.xy.base.f90
	f90 $(SUNOPTIM) -c cFKdata.xy.base.f90 -moddir=f90 -o f90/cFKdata.o

f90/RNG.o : f90/WallMod.o f90/cFKdata.o RNG.f90
	f90 $(SUNOPTIM) -c RNG.f90 -moddir=f90 -o f90/RNG.o

f90/dataTypes.o : dataTypes.f90
	f90 $(SUNOPTIM) -c dataTypes.f90 -moddir=f90 -o f90/dataTypes.o

f90/WallMod.o : WallMod.f90
	f90 $(SUNOPTIM) -c WallMod.f90 -moddir=f90 -o f90/WallMod.o

# Use gfortran compiler

gfort.out : cFK.xy.f90 gfort/cFKdata.o gfort/RNG.o gfort/dataTypes.o 
	gfortran $(GNUOPTIM) gfort/cFKdata.o gfort/RNG.o gfort/dataTypes.o gfort/WallMod.o cFK.xy.f90 -Igfort -Jgfort -o gfort.out

gfort/cFKdata.o : gfort/dataTypes.o cFKdata.xy.base.f90
	gfortran $(GNUOPTIM) -c cFKdata.xy.base.f90 -Jgfort -Igfort -o gfort/cFKdata.o

gfort/RNG.o : gfort/WallMod.o gfort/cFKdata.o RNG.f90
	gfortran $(GNUOPTIM) -c RNG.f90 -Jgfort -o gfort/RNG.o

gfort/dataTypes.o : dataTypes.f90
	gfortran $(GNUOPTIM) -c dataTypes.f90 -Jgfort -o gfort/dataTypes.o

gfort/WallMod.o : WallMod.f90
	gfortran $(GNUOPTIM) -c WallMod.f90 -Jgfort -o gfort/WallMod.o

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
#f90.out : cFK.xy.f90 cFKdata.xy.f90o RNG.f90o
#	f90 -O5 cFKdata.xy.f90o RNG.f90o cFK.xy.f90 -o f90.out

#cFKdataf90.o : dataTypesf90.o cFKdata.F95
#	f90 -O5 -c dataTypesf90.o cFKdata.F95 -o cFKdataf90.o

#RNG.o : RNG.f90
#	f90 -O5 -c RNG.f90 -o RNG.o

#dataTypesf90.o : dataTypes.F95
#	f90 -O5 -c dataTypes.F95 -o dataTypesf90.o

#	f90 -O5 cFKdataf90.o RNG.o cFK.xy.f90 -o f90.out
