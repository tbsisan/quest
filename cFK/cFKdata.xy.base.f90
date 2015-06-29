MODULE cFKdata

USE dataTypes
IMPLICIT NONE
INTEGER :: mod, floor
SAVE
CHARACTER(LEN=*), PARAMETER :: projDir='/projects/p20200/cFK/polymer'
CHARACTER(LEN=*), PARAMETER :: runID=''
REAL(KIND=BR), PARAMETER :: zero=0.0_BR
REAL(KIND=BR), PARAMETER :: onehalf=0.5_BR, one=1.0_BR, threehalf=1.5_BR, two=2.0_BR
REAL(KIND=BR), PARAMETER :: onethird =0.33333333333333333333333333333333_BR 
REAL(KIND=BR), PARAMETER :: twothirds=0.66666666666666666666666666666666_BR 
REAL(KIND=BR), PARAMETER :: onesixth =0.16666666666666666666666666666666_BR
REAL(KIND=BR), PARAMETER :: fivesixth=0.83333333333333333333333333333333_BR
REAL(KIND=BR), PARAMETER :: pi       =3.141592653589793238462643383_BR
REAL(KIND=BR), PARAMETER :: tworoot3 =0.288675134594812882254574390_BR
REAL(KIND=BR), PARAMETER :: iota =1.0e-80_BR
REAL(KIND=BR), PARAMETER :: kb=1.3806488e-23
INTEGER, PARAMETER :: N=50, Nsim=50, channelWL=150
REAL(KIND=BR), PARAMETER :: WL=2.456e-10 !the wavelength between waters in the units used
REAL(KIND=BR), PARAMETER :: WLperN=2.0 !should be integer but use real for easier math
!REAL(KIND=BR), PARAMETER :: a=0.523*WL!*REAL(channelWL)/REAL(Nsim)
REAL, PARAMETER :: aPercent=0
REAL(KIND=BR), PARAMETER :: a=aPercent*WL/WLperN!*REAL(channelWL)/REAL(Nsim)
REAL(KIND=BR), PARAMETER :: L=WL*real(channelWL,KIND=BR), Lc=a*(N-1)
REAL(KIND=BR), PARAMETER :: ax=a!L/real(Nsim,KIND=BR)!*REAL(channelWL)/REAL(Nsim)
REAL(KIND=BR), PARAMETER :: ay=WL/2.0!*REAL(channelWL)/REAL(Nsim)
INTEGER, PARAMETER :: pitch=0 ! how far does helix advance for each unit in x
REAL(KIND=BR), PARAMETER :: Ly=WL
!REAL(KIND=BR), PARAMETER :: a=2.0_BR, L=2.0_BR*pi*real(channelWL,KIND=BR), Lc=a*REAL((N-1),KIND=BR)
!REAL(KIND=BR), PARAMETER :: Lsim=2.0_BR*pi*real(WLsim,KIND=BR), LcSim=a*(Nsim-1)
REAL(KIND=BR), PARAMETER :: eps = 10.0_BR**INT(-PRECISION(L)+5.0_BR)

REAL, PARAMETER :: T=6.0e-9, dt=2.0*1.0000e-15
INTEGER, PARAMETER :: steps=T/dt
INTEGER, PARAMETER :: Gwait=1e0, Gramp=1e0
INTEGER, PARAMETER :: coolDownSteps=0, Tstart=0
INTEGER, PARAMETER :: positioningDuration = 1e6, positioningOn=1e6
INTEGER, PARAMETER :: positioningOff = positioningOn+positioningDuration
REAL, PARAMETER :: positioningMove = WL/WLperN*twothirds
REAL, PARAMETER :: positioningdx = positioningMove / real(positioningDuration)
REAL, PARAMETER :: kTrap = 0

LOGICAL :: D2 = .false. !TWO DIMENSIONS?
REAL(KIND=BR), DIMENSION(NSim) :: x,y,vx,vy,xTotal,a_old
REAL(KIND=BR):: KEx,KEy,PEx,PEy,hE= 0_BR
REAL(KIND=BR), PARAMETER :: ySpringScale = 0_BR
REAL(KIND=BR), DIMENSION(NSim) :: impingerX,impingerV
CHARACTER(LEN=1), DIMENSION(N) :: label='L'
INTEGER :: lastwrap=N
INTEGER :: Q=0,R=0
REAL(KIND=BR) :: crossCorrelation=zero

REAL(KIND=BR) :: hardcore=WL*real(N,KIND=BR)/real(channelWL,KIND=BR)/10.0_BR
REAL(KIND=BR), DIMENSION(2500) :: ens,k,h,eta,Temp,bgH,G,M,oneOverM


! Named Control Constants
!
INTEGER, PARAMETER :: INFINITECHAIN=1, CATERPILLARCHAIN=2
INTEGER, PARAMETER :: INFINITECOORD=1, CHANNELCOORD=2
INTEGER, PARAMETER :: SPRINGMODEL=1, RIGIDBARS=2, ONEPARTICLE=3, NONLINEAR=4
INTEGER, PARAMETER :: EULER=1, RUNGEKUTTA=2, PREDICTOR=3, SDEVERLET=4, VVERLET=5, EULERM=6
CHARACTER(LEN=35), DIMENSION(6) :: INTEGRATORstr = [character(len=35) :: &
        "Euler", "Runge-Kutta 4", "Predictor-Corrector", "Stochastic Verlet", "Velocity Verlet", &
        "Euler-Maruyama (Stochastic)"]
INTEGER, PARAMETER :: IDENTICALREPLACEMENT=1, IMMEDIATE=2, ACTIVATIONENERGY=3
INTEGER, PARAMETER :: UNIFORMLINE=0, IMPINGER=1, SHIFT1Y=2, HELIX=3
INTEGER, PARAMETER :: TWOKINKS=4, ONEBREATHER=5
INTEGER, PARAMETER :: NOBG=1, ENDCHANNEL2=2, WHOLECHANNEL5=3
INTEGER, PARAMETER :: ZIG=1, RU=2, BOX=3, AHRENS=4, POLAR=5, WALLACE=6, GRAND=7, SCILAB=8, VACUUM=9
CHARACTER(LEN=25), DIMENSION(9) :: GRNGstring = [character(len=25) :: &
        "Ziggurat", "Ratio of Uniforms", "Box-Muller", "Ahrens-Dieter", "Polar Rejection", "Wallace-Brent", &
        "Alg. 488 Comm. ACM", "Ahrens-Dieter Scilab", "Vacuum = 0.0"]
! VACUUM 9.8
! ZIG 12.9, WALLACE 12.2
! BOX 20.5, AHRENS 20.7, SCILAB 21, POLAR 23
! RU 25.5
! GRAND 30
      ! nanosecs/minute:
      ! BUILTIN:       135   37    46     56        50        100-140    50        54
      ! SHIFT:         140                          82
      ! KISS:          130   86    62               78                             93
		      !7.7/4   11/6  8.4  7.8
INTEGER, PARAMETER :: BUILTIN=1, KISS=2, TWIST=3, SHIFT=4, RAN2=5, RAN3=6, COMB88=7, MLAGFIB=8
CHARACTER(LEN=40), DIMENSION(8) :: URNGstring = [character(len=40) :: &
        "Builtin", "KISS", "Mersenne Twister", "3-shift shift-register", "Knuth Ran2 recursive+shuffle", &
        "Knuth ran3 subtractive lagged fib", "R4_UNI L'Ecuyer", "Multiplicative Lagged Fib"]
INTEGER, PARAMETER :: NOATOM=0, ALLATOM=1, LASTATOM=2, FIRSTATOM=3, LASTTRAP=4
!
!  The Control Parameters are Set to the Following:
!
LOGICAL, PARAMETER :: RUNTESTS = .FALSE.
LOGICAL, PARAMETER :: HOLDON = .FALSE.
INTEGER, PARAMETER :: FORCEAPPLY = NOATOM
INTEGER, PARAMETER :: POSITIONING = NOATOM
INTEGER, PARAMETER :: ICS = TWOKINKS
INTEGER, PARAMETER :: INTERACTIONMODEL = SPRINGMODEL
INTEGER, PARAMETER :: CHAINBC = CATERPILLARCHAIN
INTEGER, PARAMETER :: COORDBC = INFINITECOORD
INTEGER, PARAMETER :: INTEGRATOR = EULERM
INTEGER, PARAMETER :: REPLACEMENTMETHOD = IDENTICALREPLACEMENT
! need to add sanity checks for boundary forces (channelBCs?)
INTEGER, PARAMETER :: BGTYPE = NOBG


! Random Number Generators
INTEGER, PARAMETER :: GRNG = ZIG
INTEGER, PARAMETER :: URNG = KISS

INTEGER, PARAMETER :: printingWidthReals=12

LOGICAL, PARAMETER :: INITRANDOMX=.false., INITRANDOMY=.false.
LOGICAL, PARAMETER :: STOCHASTICS=.true., REPEATABLE=.false.
LOGICAL, PARAMETER :: HARDCOLLISIONS=.false., COLLISIONREPORTING=.false., CHECKBLOWTHROUGH=.false.
LOGICAL, PARAMETER :: QTRACKLABELSON=.false., VCORRELATIONS=.false., TIMEPARTICLES=.false.
LOGICAL, PARAMETER :: WRITETIMEDAT=.false., WRITEEVENTS=.false.
LOGICAL, PARAMETER :: WRITEX=.true.,WRITEV=.true.,WRITEU=.true.
LOGICAL, PARAMETER :: TIMESUBS=.false.
INTEGER, PARAMETER :: WRITESTATEWAIT=10000, corWindow=floor(1.0/(dt*1.0e9*0.513*10.0))
LOGICAL, PARAMETER :: ASCIIFORMAT=.false.
INTEGER, PARAMETER :: STATUSWAIT=steps/10-mod(steps/10,corWindow)
REAL(KIND=4), PARAMETER :: writeSize=(steps/WRITESTATEWAIT)*N*printingWidthReals + 10*(steps/WRITESTATEWAIT)


END MODULE cFKdata
