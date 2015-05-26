MODULE RNG
use dataTypes
use WallMod
use cFKdata, only: GRNG, ZIG, RU, BOX, POLAR, AHRENS, WALLACE, GRAND, SCILAB,VACUUM, &
                     URNG, BUILTIN, TWIST, RAN2, RAN3, KISS, COMB88, SHIFT, MLAGFIB, &
                     REPEATABLE
implicit none
private

! kiss params
INTEGER :: x=123456789, y=362436169, z=521288629, w=916191069

!Lagged FIbonacci params
INTEGER, PARAMETER :: lag1=1279,lag2=861
INTEGER(KIND=8), DIMENSION(lag1+1000) :: lagTable


!Lecuyer params
INTEGER :: s1=2147483563,s2=2147483398

! Ziggurat params
!INTEGER,  PARAMETER  ::  DP=SELECTED_REAL_KIND( 12, 60 )
REAL(KIND=BR), PARAMETER  ::  m1=2147483648.0_BR,   m2=2147483648.0_BR,half=0.5_BR
REAL(KIND=BR)             ::  dn=3.442619855899_BR, tn=3.442619855899_BR, &
                         vn=0.00991256303526217_BR,q,de=7.697117470131487_BR, &
                         te=7.697117470131487_BR,ve=0.003949659822581572_BR
INTEGER,  SAVE       ::  iz, jz, jsr=123456789, kn(0:127), hz
REAL(KIND=BR), SAVE       ::  wn(0:127), fn(0:127)
LOGICAL,  SAVE       ::  initialized=.FALSE.
REAL(KIND=BR), PARAMETER :: pi       =3.141592653589793238462643383_BR
    !------------------------------------------------------------------------------
    ! Mersene Twister Constants and State Type
    integer(INT32), parameter :: N = 624_INT32
    integer(INT32), parameter :: M = 397_INT32
    type mtprng_state
        integer(INT32)                   :: mti = -1
        integer(INT64), dimension(0:N-1) :: mt
    end type 
TYPE(mtprng_state), SAVE :: twisterState

public :: initRNGs, mtprng_state !, kisset, zigset, shr3seed, mtprng_init
public :: uniformDeviate, gaussianDeviate ! kiss, shr3, twister, ran3, ran2
!PUBLIC :: boxMullerGRNG, polarRejectionGRNG, AhrensDieterGRNG, ratioUniformsGRNG, &
!            wallaceGRNG, zigguratGRNG


CONTAINS
!
! Initialize RNGs
!
   SUBROUTINE initRNGs()
      !INTEGER, INTENT(IN) :: N
      INTEGER              :: k,firstSeed
      INTEGER, ALLOCATABLE :: seed(:)
      !TYPE(mtprng_state) :: MTstate
      REAL(KIND=BR) :: junkOut
      INTEGER(KIND=8) :: maxNeg=2147483648
      !REAL, DIMENSION(N) :: dummyFt

      CALL RANDOM_SEED(SIZE=k)
      ALLOCATE( seed(k) )

      IF (REPEATABLE) THEN
         seed=1
      ELSE
         open(89,file='/dev/urandom',access='stream',form='UNFORMATTED')
         read(89) seed
         close(89)
      END IF
      
      print*,'RANDOM SEED:',seed
      CALL RANDOM_SEED(PUT=seed) ! Purportedly Fortran uses KISS algorithm
      firstSeed=seed(1)
      print*,'FIRST RANDOM SEED:',firstSeed
      CALL mtprng_init(firstSeed,twisterState)
      CALL shr3seed(firstSeed)
      junkOut = ran2URNG(-abs(firstSeed))
      junkOut = ran3URNG(-abs(firstSeed))
      CALL kisset(firstSeed,shr3IRNG(),seed(3),seed(4))
      junkOut = wallaceGRNG(abs(firstSeed))
      CALL initMLFG() ! MUST FIRST INIT KISS GENERATOR BEFORE THIS CALL
      do while (s1 .gt.  2147483562 .or. s2 .gt.  2147483397 )
         s1=kissIRNG()+maxNeg+1
         s2=kissIRNG()+maxNeg+1
      end do
      junkOut = combLec88URNG(s1,s2)

      DEALLOCATE( seed )
      RETURN
   END SUBROUTINE initRNGs
!
! Interface Functions
!
   FUNCTION gaussianDeviate() RESULT(randomVariable)
      !INTEGER :: N
      !REAL, DIMENSION(N) :: FN
      REAL(KIND=BR) :: randomVariable
      SELECT CASE (GRNG)
      CASE (ZIG)
	 randomVariable=zigguratGRNG()
      CASE (RU)
	 randomVariable=ratioUniformsGRNG()
      CASE (BOX)
	 randomVariable=boxMullerGRNG()
      CASE (AHRENS)
	 randomVariable=AhrensDieterGRNG()
      CASE (POLAR)
	 randomVariable=polarRejectionGRNG()
      CASE (WALLACE)
	 randomVariable=wallaceGRNG()
      CASE (GRAND)
         randomVariable=grandGRNG()
      CASE (SCILAB)
         randomVariable=AhrensDieterScilabGRNG()
      CASE (VACUUM)
         randomVariable=0.0_BR
      CASE DEFAULT
         stop "No Gaussian Generator Selected"
      END SELECT
      RETURN
   END FUNCTION gaussianDeviate

   FUNCTION uniformDeviate()! RESULT (uniformDeviate)
      REAL(KIND=BR) :: uniformDeviate
      REAL(KIND=BR) :: multiplier64 = 5.4210108624275e-20_BR !2217003726400434970855712890625e-20_BR
      !     4.656613057E-10 is 1/M1  M1 is set in a data statement in
      !     random_large_integer
      !      and is currently 2147483563. If M1 changes, change this also.
      SELECT CASE (URNG)
      CASE (BUILTIN)
         CALL RANDOM_NUMBER(uniformDeviate) !random_large_integer()*4.656613057E-10
      CASE (KISS)
         uniformDeviate = half + 0.2328306e-9_BR * kissIRNG( )
      CASE (TWIST)
         uniformDeviate = twisterURNG(twisterState)
      CASE (SHIFT)
         uniformDeviate = half + 0.2328306e-9_BR * shr3IRNG( )
      CASE (RAN2)
         uniformDeviate = ran2URNG()
      CASE (RAN3)
         uniformDeviate = ran3URNG()
      CASE (COMB88)
         uniformDeviate = combLec88URNG(s1,s2)
      CASE (MLAGFIB)
         !print*,mulLagFibUI64RNG()
         uniformDeviate = mulLagFibUI64RNG() * multiplier64 !+ TINY(uniformDeviate)
      CASE DEFAULT
         stop "no uniform generator selected"
      END SELECT
      !print*,'done uniform'
      RETURN

   END FUNCTION uniformDeviate 

!
! GAUSSIAN RANDOM NUMBER GENERATORS
!
   FUNCTION polarRejectionGRNG() RESULT(harvest)
      ! Numerical Recipes routine for generating a single normal random deviate,
      ! adapted to use the compiler's random number generator.
      ! This is the Polar Rejection Method, derived from Box Muller.
      ! It should be faster than box-muller on many processors, depending on the speed
      ! of division versus sin/cos.
   
      IMPLICIT NONE
   
      ! Local variables
      REAL(KIND=BR) :: harvest
      REAL(KIND=BR)          :: rsq, v1, v2
      REAL(KIND=BR), SAVE    :: nextHarvest
      LOGICAL, SAVE :: gaus_stored = .false.
   
      IF (gaus_stored) THEN
         harvest = nextHarvest
         gaus_stored = .false.
      ELSE

         rsq=2.0
         DO WHILE (rsq .gt. 1.0 .or. rsq .eq. 0)
   	    v1=uniformDeviate()
   	    v2=uniformDeviate()
   	    v1 = 2.0*v1 - 1.0
   	    v2 = 2.0*v2 - 1.0
   	    rsq = v1**2 + v2**2
         END DO
         rsq = SQRT(-2.0*LOG(rsq)/rsq)
         harvest = v1*rsq
         nextHarvest = v2*rsq
         gaus_stored = .true.
      END IF
      RETURN
   END FUNCTION polarRejectionGRNG

   FUNCTION boxMullerGRNG( ) RESULT (harvest)
      ! May be faster than the polar rejection method on some processors with pipelining
      ! and branch prediction
      REAL(KIND=BR) :: u1, u2, s, harvest
      REAL(KIND=BR), SAVE :: nextHarvest
      INTEGER, SAVE :: calls=0

      IF (calls==0) THEN
         u1=uniformDeviate()
         u2=uniformDeviate()
         s = sqrt(-2.*log(u1))
         harvest = s * cos(2.*pi*u2)
         nextHarvest = s * sin(2.*pi*u2)
         calls=1
      ELSE
         calls=0
         harvest=nextHarvest
      END IF

      RETURN
   END FUNCTION boxMullerGRNG  


   FUNCTION ratioUniformsGRNG() RESULT(fn_val)
     
      ! This algorithm was published in 
      ! Leva, J. "A Fast Normal Random Number Generator"
      ! ACM Transactions on Mathematical Software. 18(4) 1992
       
      ! Adapted from the following Fortran 77 code
      !      ALGORITHM 712, COLLECTED ALGORITHMS FROM ACM.
      !      THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
      !      VOL. 18, NO. 4, DECEMBER, 1992, PP. 434-435.
      
      !  The function ratioUniforms() returns a normally distributed pseudo-random
      !  number with zero mean and unit variance.
      
      !  The algorithm uses the ratio of uniforms method of A.J. Kinderman
      !  and J.F. Monahan augmented with quadratic bounding curves.
      
      REAL(KIND=BR) :: fn_val
   
      !     Local variables
      REAL(KIND=BR)     :: s = 0.449871, t = -0.386595, a = 0.19600, b = 0.25472,    &
               r1 = 0.27597, r2 = 0.27846, u, v, x, y, q
   
      !     Generate P = (u,v) uniform in rectangle enclosing acceptance region
   
      DO
      !CALL RANDOM_NUMBER(u)
      !CALL RANDOM_NUMBER(v)
      u=uniformDeviate()
      v=uniformDeviate()
      v = 1.7156 * (v - half)
   
      !     Evaluate the quadratic form
      x = u - s
      y = ABS(v) - t
      q = x**2 + y*(a*y - b*x)
   
      !     Accept P if inside inner ellipse
      IF (q < r1) EXIT
      !     Reject P if outside outer ellipse
      IF (q > r2) CYCLE
      !     Reject P if outside acceptance region
      IF (v**2 < -4.0*LOG(u)*u**2) EXIT
      END DO
   
      !     Return ratio of P's coordinates as the normal deviate
      fn_val = v/u
      RETURN
   
   END FUNCTION ratioUniformsGRNG

   FUNCTION zigguratGRNG( ) RESULT( fn_val )
      REAL(BR)             ::  fn_val
   
      REAL(BR), PARAMETER  ::  r = 3.442620_BR
      REAL(BR)             ::  x, y
   
      IF( .NOT. initialized ) CALL zigset( )
      hz = kissIRNG( )
      iz = IAND( hz, 127 )
      IF( ABS( hz ) < kn(iz) ) THEN
         fn_val = hz * wn(iz)
      ELSE
         DO
            IF( iz == 0 ) THEN
               DO
                  x = -0.2904764_BR* LOG( uniformDeviate( ) )
                  y = -LOG( uniformDeviate( ) )
                  IF( y+y >= x*x ) EXIT
               END DO
               fn_val = r+x
               IF( hz <= 0 ) fn_val = -fn_val
               RETURN
            END IF
            x = hz * wn(iz)
            IF( fn(iz) + uniformDeviate( )*(fn(iz-1)-fn(iz)) < EXP(-half*x*x) ) THEN
               fn_val = x
               RETURN
            END IF
            hz = kissIRNG( )
            iz = IAND( hz, 127 )
            IF( ABS( hz ) < kn(iz) ) THEN
               fn_val = hz * wn(iz)
               RETURN
            END IF
         END DO
      END IF
      RETURN
   END FUNCTION zigguratGRNG


      FUNCTION AhrensDieterScilabGRNG() RESULT (snorm)
!**********************************************************************C
!                                                                      C
!                                                                      C
!     (STANDARD-)  N O R M A L  DISTRIBUTION                           C
!                                                                      C
!                                                                      C
!**********************************************************************C
!**********************************************************************C
!                                                                      C
!     FOR DETAILS SEE:                                                 C
!                                                                      C
!               AHRENS, J.H. AND DIETER, U.                            C
!               EXTENSIONS OF FORSYTHE'S METHOD FOR RANDOM             C
!               SAMPLING FROM THE NORMAL DISTRIBUTION.                 C
!               MATH. COMPUT., 27,124 (OCT. 1973), 927 - 937.          C
!                                                                      C
!     ALL STATEMENT NUMBERS CORRESPOND TO THE STEPS OF ALGORITHM 'FL'  C
!     (M=5) IN THE ABOVE PAPER     (SLIGHTLY MODIFIED IMPLEMENTATION)  C
!                                                                      C
!     Modified by Barry W. Brown, Feb 3, 1988 to use uniformDeviate instead of   C
!     SUNIF.  The argument IR thus goes away.                          C
!                                                                      C
!**********************************************************************C
!
!
!     THE DEFINITIONS OF THE CONSTANTS A(K), D(K), T(K) AND
!     H(K) ARE ACCORDING TO THE ABOVEMENTIONED ARTICLE
!
!     .. Local Scalars ..
      DOUBLE PRECISION aa,s,tt,u,ustar,w,y,snorm
      INTEGER i
!     ..
!     .. Local Arrays ..
      DOUBLE PRECISION a(32),d(31),h(31),t(31)
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC float,int
!     ..
!     .. Save statement ..
!     JJV added a Save statement for arrays initialized in Data statmts
      SAVE a,d,t,h
!     ..
!     .. Data statements ..
      DATA a/0.0,.3917609E-1,.7841241E-1,.1177699,.1573107,.1970991, &
          .2372021,.2776904,.3186394,.3601299,.4022501,.4450965, &
          .4887764,.5334097,.5791322,.6260990,.6744898,.7245144, &
          .7764218,.8305109,.8871466,.9467818,1.009990,1.077516, &
          1.150349,1.229859,1.318011,1.417797,1.534121,1.675940, &
          1.862732,2.153875/
      DATA d/5*0.0,.2636843,.2425085,.2255674,.2116342,.1999243, &
          .1899108,.1812252,.1736014,.1668419,.1607967,.1553497, &
          .1504094,.1459026,.1417700,.1379632,.1344418,.1311722, &
          .1281260,.1252791,.1226109,.1201036,.1177417,.1155119, &
          .1134023,.1114027,.1095039/
      DATA t/.7673828E-3,.2306870E-2,.3860618E-2,.5438454E-2, &
          .7050699E-2,.8708396E-2,.1042357E-1,.1220953E-1,.1408125E-1, &
          .1605579E-1,.1815290E-1,.2039573E-1,.2281177E-1,.2543407E-1, &
          .2830296E-1,.3146822E-1,.3499233E-1,.3895483E-1,.4345878E-1, &
          .4864035E-1,.5468334E-1,.6184222E-1,.7047983E-1,.8113195E-1, &
          .9462444E-1,.1123001,.1364980,.1716886,.2276241,.3304980, &
          .5847031/
      DATA h/.3920617E-1,.3932705E-1,.3950999E-1,.3975703E-1, &
          .4007093E-1,.4045533E-1,.4091481E-1,.4145507E-1,.4208311E-1, &
          .4280748E-1,.4363863E-1,.4458932E-1,.4567523E-1,.4691571E-1, &
          .4833487E-1,.4996298E-1,.5183859E-1,.5401138E-1,.5654656E-1, &
          .5953130E-1,.6308489E-1,.6737503E-1,.7264544E-1,.7926471E-1, &
          .8781922E-1,.9930398E-1,.1155599,.1404344,.1836142,.2790016, &
          .7010474/
!     ..
!     .. Executable Statements ..
!
   10 u = 1- uniformDeviate()
      s = 0.0
      IF (u.GT.0.5) s = 1.0
      u = u + u - s
   20 u = 32.0*u
      i = int(u)
      IF (i.EQ.32) i = 31
      IF (i.EQ.0) GO TO 100
!
!                                START CENTER
!
   30 ustar = u - float(i)
      aa = a(i)
   40 IF (ustar.LE.t(i)) GO TO 60
      w = (ustar-t(i))*h(i)
!
!                                EXIT   (BOTH CASES)
!
   50 y = aa + w
      snorm = y
      IF (s.EQ.1.0) snorm = -y
      RETURN
!
!                                CENTER CONTINUED
!
   60 u = uniformDeviate()
      w = u* (a(i+1)-aa)
      tt = (0.5*w+aa)*w
      GO TO 80

   70 tt = u
      ustar = uniformDeviate()
   80 IF (ustar.GT.tt) GO TO 50
   90 u = uniformDeviate()
      IF (ustar.GE.u) GO TO 70
      ustar = uniformDeviate()
      GO TO 40
!
!                                START TAIL
!
  100 i = 6
      aa = a(32)
      GO TO 120

  110 aa = aa + d(i)
      i = i + 1
  120 u = u + u
      IF (u.LT.1.0) GO TO 110
  130 u = u - 1.0
  140 w = u*d(i)
      tt = (0.5*w+aa)*w
      GO TO 160

  150 tt = u
  160 ustar = uniformDeviate()
      IF (ustar.GT.tt) GO TO 50
  170 u = uniformDeviate()
      IF (ustar.GE.u) GO TO 150
      u = uniformDeviate()
      GO TO 140

      END FUNCTION AhrensDieterScilabGRNG


FUNCTION AhrensDieterGRNG() RESULT (AhrensDieter)
!**********************************************************************C
!                                                                      C
!                                                                      C
!     (STANDARD-)  N O R M A L  DISTRIBUTION                           C
!                                                                      C
!                                                                      C
!**********************************************************************C
!**********************************************************************C
!                                                                      C
!     FOR DETAILS SEE:                                                 C
!                                                                      C
!               AHRENS, J.H. AND DIETER, U.                            C
!               EXTENSIONS OF FORSYTHE'S METHOD FOR RANDOM             C
!               SAMPLING FROM THE NORMAL DISTRIBUTION.                 C
!               MATH. COMPUT., 27,124 (OCT. 1973), 927 - 937.          C
!                                                                      C
!     ALL STATEMENT NUMBERS CORRESPOND TO THE STEPS OF ALGORITHM 'FL'  C
!     (M=5) IN THE ABOVE PAPER     (SLIGHTLY MODIFIED IMPLEMENTATION)  C
!                                                                      C
!     Modified by Barry W. Brown, Feb 3, 1988 to use                   C
!     uniformDeviate instead of                               C
!     SUNIF.  The argument IR thus goes away.                          C
!                                                                      C
!**********************************************************************C
!     THE DEFINITIONS OF THE CONSTANTS A(K), D(K), T(K) AND
!     H(K) ARE ACCORDING TO THE ABOVEMENTIONED ARTICLE
! ..
! .. Local Scalars ..
        REAL(KIND=BR) :: aa, s, tt, u, ustar, w, y
        INTEGER :: i
! ..
! .. Intrinsic Functions ..
        INTRINSIC FLOAT, INT, MIN
! ..
! .. Function Return Value ..
        REAL(KIND=BR) :: AhrensDieter
! ..
! .. Parameters ..
        REAL(KIND=BR), PARAMETER :: a(32) = (/ 0.0, 0.3917609E-1, 0.7841241E-1, &
          0.1177699, 0.1573107, 0.1970991, 0.2372021, 0.2776904, &
          0.3186394, 0.3601299, 0.4022501, 0.4450965, 0.4887764, &
          0.5334097, 0.5791322, 0.6260990, 0.6744898, 0.7245144, &
          0.7764218, 0.8305109, 0.8871466, 0.9467818, 1.009990, 1.077516, &
          1.150349, 1.229859, 1.318011, 1.417797, 1.534121, 1.675940, &
          1.862732, 2.153875/)
        REAL(KIND=BR), PARAMETER :: d(31) = (/ 0.0, 0.0, 0.0, 0.0, 0.0, 0.2636843, &
          0.2425085, 0.2255674, 0.2116342, 0.1999243, 0.1899108, &
          0.1812252, 0.1736014, 0.1668419, 0.1607967, 0.1553497, &
          0.1504094, 0.1459026, 0.1417700, 0.1379632, 0.1344418, &
          0.1311722, 0.1281260, 0.1252791, 0.1226109, 0.1201036, &
          0.1177417, 0.1155119, 0.1134023, 0.1114027, 0.1095039/)
        REAL(KIND=BR), PARAMETER :: h(31) = (/ 0.3920617E-1, 0.3932705E-1, &
          0.3950999E-1, 0.3975703E-1, 0.4007093E-1, 0.4045533E-1, &
          0.4091481E-1, 0.4145507E-1, 0.4208311E-1, 0.4280748E-1, &
          0.4363863E-1, 0.4458932E-1, 0.4567523E-1, 0.4691571E-1, &
          0.4833487E-1, 0.4996298E-1, 0.5183859E-1, 0.5401138E-1, &
          0.5654656E-1, 0.5953130E-1, 0.6308489E-1, 0.6737503E-1, &
          0.7264544E-1, 0.7926471E-1, 0.8781922E-1, 0.9930398E-1, &
          0.1155599, 0.1404344, 0.1836142, 0.2790016, 0.7010474/)
        REAL(KIND=BR), PARAMETER :: t(31) = (/ 0.7673828E-3, 0.2306870E-2, &
          0.3860618E-2, 0.5438454E-2, 0.7050699E-2, 0.8708396E-2, &
          0.1042357E-1, 0.1220953E-1, 0.1408125E-1, 0.1605579E-1, &
          0.1815290E-1, 0.2039573E-1, 0.2281177E-1, 0.2543407E-1, &
          0.2830296E-1, 0.3146822E-1, 0.3499233E-1, 0.3895483E-1, &
          0.4345878E-1, 0.4864035E-1, 0.5468334E-1, 0.6184222E-1, &
          0.7047983E-1, 0.8113195E-1, 0.9462444E-1, 0.1123001, 0.1364980, &
          0.1716886, 0.2276241, 0.3304980, 0.5847031/)
! ..
! .. Executable Statements ..

1       u = uniformDeviate()
        s = 0.0
        IF (u>0.5) s = 1.0
        u = u + u - s
        u = 32.0*u
        i = MIN(INT(u),31)
        IF (i==0) GO TO 50

!                                START CENTER

        ustar = u - FLOAT(i)
        aa = a(i)
10      IF (ustar<=t(i)) GO TO 20
        w = (ustar-t(i))*h(i)
	 CALL set_value()
        RETURN

!                                CENTER CONTINUED

20      u = uniformDeviate()
        w = u*(a(i+1)-aa)
        tt = (0.5*w+aa)*w
        GO TO 40

30      tt = u
        ustar = uniformDeviate()
40      IF (ustar>tt) THEN
	 CALL set_value()
          RETURN
        END IF
        u = uniformDeviate()
        IF (ustar>=u) GO TO 30
        ustar = uniformDeviate()
        GO TO 10

!                                START TAIL

50      i = 6
        aa = a(32)
        GO TO 70

60      if (i>31) goto 1  ! a TBS mod because sometimes i=32
	aa = aa + d(i)
        i = i + 1
70      u = u + u
        IF (u<1.0) GO TO 60
        u = u - 1.0
80      w = u*d(i)
        tt = (0.5*w+aa)*w
        GO TO 100

90      tt = u
100     ustar = uniformDeviate()
        IF (ustar>tt) THEN
	 CALL set_value()
          RETURN
        END IF
        u = uniformDeviate()
        IF (ustar>=u) GO TO 90
        u = uniformDeviate()
        GO TO 80
   CONTAINS
      SUBROUTINE set_value()
	 !REAL, INTENT(IN) :: aa, w, s
	 !REAL, INTENT(OUT) :: y, AhrensDieter
! ..
! .. Executable Statements ..

!                                EXIT   (BOTH CASES)
          y = aa + w
          AhrensDieter = y
          IF (s==1.0) AhrensDieter = -y
          RETURN

      END SUBROUTINE set_value

   END FUNCTION AhrensDieterGRNG

   FUNCTION grandGRNG() RESULT(grand)
!     ALGORITHM 488 COLLECTED ALGORITHMS FROM ACM.
!     ALGORITHM APPEARED IN COMM. ACM, VOL. 17, NO. 12,
!     P. 704.
! EXCEPT ON THE FIRST CALL GRAND RETURNS A
! PSEUDO-RANDOM NUMBER HAVING A GAUSSIAN (I.E.
! NORMAL) DISTRIBUTION WITH ZERO MEAN AND UNIT
! STANDARD DEVIATION.  THUS, THE DENSITY IS  F(X) =
! EXP(-0.5*X**2)/SQRT(2.0*PI). THE FIRST CALL
! INITIALIZES GRAND AND RETURNS ZERO.
! THE PARAMETER N IS DUMMY.
! GRAND CALLS A FUNCTION RAND, AND IT IS ASSUMED THAT
! SUCCESSIVE CALLS TO RAND(0) GIVE INDEPENDENT
! PSEUDO- RANDOM NUMBERS DISTRIBUTED UNIFORMLY ON (0,
! 1), POSSIBLY INCLUDING 0 (BUT NOT 1).
! THE METHOD USED WAS SUGGESTED BY VON NEUMANN, AND
! IMPROVED BY FORSYTHE, AHRENS, DIETER AND BRENT.
! ON THE AVERAGE THERE ARE 1.37746 CALLS OF RAND FOR
! EACH CALL OF GRAND.
! WARNING - DIMENSION AND DATA STATEMENTS BELOW ARE
!           MACHINE-DEPENDENT.
! DIMENSION OF D MUST BE AT LEAST THE NUMBER OF BITS
! IN THE FRACTION OF A FLOATING-POINT NUMBER.
! THUS, ON MOST MACHINES THE DATA STATEMENT BELOW
! CAN BE TRUNCATED.
! IF THE INTEGRAL OF SQRT(2.0/PI)*EXP(-0.5*X**2) FROM
! A(I) TO INFINITY IS 2**(-I), THEN D(I) = A(I) -
! A(I-1).
      REAL, PARAMETER, DIMENSION(60) :: D=(/ &
      0.674489750,0.475859630,0.383771164,&
      0.328611323,0.291142827,0.263684322,&
      0.242508452,0.225567444,0.211634166,&
      0.199924267,0.189910758,0.181225181,&
      0.173601400,0.166841909,0.160796729,&
      0.155349717,0.150409384,0.145902577,&
      0.141770033,0.137963174,0.134441762,&
      0.131172150,0.128125965,0.125279090,&
      0.122610883,0.120103560,0.117741707,&
      0.115511892,0.113402349,0.111402720,&
      0.109503852,0.107697617, &
      0.105976772,0.104334841,0.102766012,&
      0.101265052,0.099827234,0.098448282,&
      0.097124309,0.095851778,0.094627461,&
      0.093448407,0.092311909,0.091215482,&
      0.090156838,0.089133867,0.088144619,&
      0.087187293,0.086260215,0.085361834,&
      0.084490706,0.083645487,0.082824924,&
      0.082027847,0.081253162,0.080499844,&
      0.079766932,0.079053527,0.078358781,&
      0.077681899/)
! END OF MACHINE-DEPENDENT STATEMENTS
      REAL :: A,W,V,grand
! U MUST BE PRESERVED BETWEEN CALLS.
      REAL, SAVE :: U=0.0_RR
      INTEGER :: I
! INITIALIZE DISPLACEMENT A AND COUNTER I.
      A = 0.0_RR
      I = 0
! INCREMENT COUNTER AND DISPLACEMENT IF LEADING BIT
! OF U IS ONE.
   10 U = U + U
      IF (U.LT.1.0_RR) GO TO 20
      U = U - 1.0_RR
      if (U .eq. 1.0_RR) U=U-0.001_RR ! TBS
      I = I + 1
!      IF (I .gt. 60) THEN  ! TBS ADDED LINE BECAUSE SOMETIMES I>60
!         print*,U
!      if (U .eq. 1.0_RR) then
!         print*,'detected 1.0 condition'
!         U=U-0.000001_RR
!         write(*,'(ES47.37)') U
!         print*,U
!      ENDIF
!         !U = 0.0
!         GO TO 1
!      END IF
!      if (U .eq. 1.0_RR) then
!         print*,'detected 1.0 condition'
!         U=U-0.00001_RR
!      ENDIF
      A = A - D(I)
      GO TO 10
! FORM W UNIFORM ON 0 .LE. W .LT. D(I+1) FROM U.
   20 W = D(I+1)*U
! FORM V = 0.5*((W-A)**2 - A**2). NOTE THAT 0 .LE. V
! .LT. LOG(2).
      V = W*(0.5_RR*W-A)
! GENERATE NEW UNIFORM U.
   30 U = uniformDeviate()
! ACCEPT W AS A RANDOM SAMPLE IF V .LE. U.
      IF (V.LE.U) GO TO 40
! GENERATE RANDOM V.
      V = uniformDeviate()
! LOOP IF U .GT. V.
      IF (U.GT.V) GO TO 30
! REJECT W AND FORM A NEW UNIFORM U FROM V AND U.
      U = (V-U)/(1.0_RR-U)
      GO TO 20
! FORM NEW U (TO BE USED ON NEXT CALL) FROM U AND V.
   40 U = (U-V)/(1.0_RR-V)
! USE FIRST BIT OF U FOR SIGN, RETURN NORMAL VARIATE.
      U = U + U
      IF (U.LT.1.0_RR) GO TO 50
      U = U - 1.0_RR
      GRAND = W - A
      RETURN
   50 GRAND = A - W
      RETURN
      END FUNCTION grandGRNG



   FUNCTION wallaceGRNG(seed) RESULT(wallace1)
      INTEGER, PARAMETER :: NatAtime=250000, nwork=660000
      !INTEGER, INTENT(IN) :: N
      !REAL, DIMENSION(N) :: FN
      REAL, DIMENSION(nwork*2), SAVE :: dwork
      REAL, DIMENSION(NatAtime+10) :: normals
      REAL(KIND=BR) :: wallace1
      INTEGER, OPTIONAL :: seed
      INTEGER, SAVE :: ix=0,numleft=0
      
      IF (PRESENT(seed)) ix=abs(seed)

      IF (numleft .gt. 1) THEN
         !FN(1:N) = normals(-numleft:-numleft+N)
         wallace1 = normals(numleft)
         numleft=numleft-1
      ELSE
         CALL rannw(0.0E0,1.0E0,ix,normals,NatAtime,dwork,nwork)
         numleft=NatAtime
         !print*,'calling rannw'
      END IF
      RETURN
   END FUNCTION wallaceGRNG
!
!.....................................................................
!
!
! UNIFORM REALS
!
!

   FUNCTION twisterURNG(state) result(r)
      ! FUNCTION mtprng_rand_real2(state) result(r)
      !   Obtain a psuedorandom real number in the range [0,1), i.e., a number
      !   greater than or equal to 0 and less than 1.
      ! arguments
      type(mtprng_state), intent(inout) :: state
    
      ! return type
      real(IEEE64) :: r
        
      ! Local constant; precalculated to avoid division below
      real(IEEE64), parameter :: factor = 1.0_IEEE64 / 4294967296.0_IEEE64
        
      ! compute
      r = real(mtprng_rand64(state),IEEE64) * factor
        
   END FUNCTION twisterURNG


   FUNCTION combLec88URNG ( s1, s2 ) result(combLec88)
   
   !*****************************************************************************80
   !
   !! combLec88 returns a pseudorandom number between 0 and 1.
   ! Originally called R4_UNI
   !
   !  Discussion:
   !
   !    This function generates uniformly distributed pseudorandom numbers
   !    between 0 and 1, using the 32-bit generator from figure 3 of
   !    the article by L'Ecuyer.
   !
   !    The cycle length is claimed to be 2.30584E+18.
   !
   !  Modified:
   !
   !    08 July 2008
   !
   !  Author:
   !
   !    Pascal original version by Pierre L'Ecuyer
   !    FORTRAN90 version by John Burkardt
   !
   !  Reference:
   !
   !    Pierre LEcuyer,
   !    Efficient and Portable Combined Random Number Generators,
   !    Communications of the ACM,
   !    Volume 31, Number 6, June 1988, pages 742-751.
   !
   !  Parameters:
   !
   !    Input/output, integer ( kind = 4 ) S1, S2, two values used as the
   !    seed for the sequence.  On first call, the user should initialize
   !    S1 to a value between 1 and 2147483562;  S2 should be initialized
   !    to a value between 1 and 2147483398.
   !
   !    Output, real ( kind = 4 ) R4_UNI, the next value in the sequence.
   !
     implicit none
   
     integer ( kind = 4 ) k
     real    ( kind = BR ) combLec88
     integer ( kind = 4 ) s1
     integer ( kind = 4 ) s2
     integer ( kind = 4 ) z
   
     k = s1 / 53668
     s1 = 40014 * ( s1 - k * 53668 ) - k * 12211
     if ( s1 < 0 ) then
       s1 = s1 + 2147483563
     end if
   
     k = s2 / 52774
     s2 = 40692 * ( s2 - k * 52774 ) - k * 3791
     if ( s2 < 0 ) then
       s2 = s2 + 2147483399
     end if
   
     z = s1 - s2
     if ( z < 1 ) then
       z = z + 2147483562
     end if
   
     combLec88 = real ( z, kind = BR ) / 2147483563.0E+00
   
     return
   END FUNCTION combLec88URNG


   FUNCTION ran2URNG(seed) RESULT(ran2)
      ! is a long periode (> 2 x 10^18) random number generator of 
      ! L'Ecuyer and Bays-Durham shuffle and added safeguards.
      ! Initialize by passing a negative integer
      INTEGER, OPTIONAL :: seed
      INTEGER :: j,k
      REAL(KIND=BR) :: ran2
      INTEGER, PARAMETER :: IM1=2147483563,IM2=2147483399,IMM1=IM1-1, &
      		IA1=40014,IA2=40692,IQ1=53668,IQ2=52774,IR1=12211,IR2=3791, &
      		NTAB=32,NDIV=1+IMM1/NTAB
      REAL(KIND=BR), PARAMETER :: AM=1./REAL(IM1,KIND=BR), RNMX=1.-EPSILON(AM)
      INTEGER, SAVE :: idum2=123456789,idum=234567890, iy=0
      INTEGER, SAVE, DIMENSION(NTAB) :: iv=0

      if (PRESENT(SEED)) then
        idum=seed
        idum=max(-idum,1)
        idum2=idum
        do j=NTAB+8,1,-1

          k=idum/IQ1
          idum=IA1*(idum-k*IQ1)-k*IR1
          if (idum.lt.0) idum=idum+IM1
          if (j.le.NTAB) iv(j)=idum
        end do
        iy=iv(1)
      endif
      k=idum/IQ1
      idum=IA1*(idum-k*IQ1)-k*IR1
      if (idum.lt.0) idum=idum+IM1
      k=idum2/IQ2
      idum2=IA2*(idum2-k*IQ2)-k*IR2
      if (idum2.lt.0) idum2=idum2+IM2
      j=1+iy/NDIV
      iy=iv(j)-idum2
      iv(j)=idum
      if(iy.lt.1)iy=iy+IMM1
      ran2=min(AM*iy,RNMX)
      return
   END FUNCTION ran2URNG

   FUNCTION ran3URNG(seed) RESULT(ran3)
      ! Comes from Knuth, "Seminumerical algorithms", The Art of Computer Programming, 1981
      ! L'Ecuyer and Simard "TestU01: Library for Empirical Testing of RNGs", ACM 2007
      ! say this is a lagged fibonacci generator LFib(10^9,55,24,-).
      ! Press just calls it a subtractive generator, and calls it a good counterpart to LCG methods.
      IMPLICIT NONE
      REAL(KIND=BR) ::  ran3
      INTEGER, OPTIONAL :: seed
      INTEGER :: i,ii,k
      INTEGER :: mj,mk
      INTEGER, SAVE, DIMENSION(55) :: ma
      INTEGER, SAVE :: iff=0,inext,inextp
      INTEGER, PARAMETER :: mbig=1000000000,mseed=161803398,mz=0
      REAL(KIND=BR), PARAMETER :: fac=1./REAL(mbig)

      IF ( PRESENT(seed) .or. (iff == 0) ) THEN
         iff=1
         mj=mseed-IABS(seed)
         mj=MOD(mj,mbig)
         ma(55)=mj
         mk=1
         DO i=1,54
            ii=MOD(21*i,55)
            ma(ii)=mk
            mk=mj-mk
            IF(mk < mz)mk=mk+mbig
            mj=ma(ii)
         ENDDO
         DO k=1,4
            DO i=1,55
               ma(i)=ma(i)-ma(1+MOD(i+30,55))
               IF (ma(i) < mz)ma(i)=ma(i)+mbig
            ENDDO
         ENDDO
         inext=0
         inextp=31
      ENDIF
      inext=inext+1
      IF (inext == 56) inext=1
      inextp=inextp+1
      IF (inextp == 56) inextp=1
      mj=ma(inext)-ma(inextp)
      IF (mj < mz) mj=mj+mbig
      ma(inext)=mj
      ran3=mj*fac

   END FUNCTION ran3URNG

!
!  Random 32-bit integers
!

   FUNCTION mulLagFibUI64RNG() RESULT (mulLagFib)
      INTEGER(KIND=8) :: mulLagFib
      INTEGER, SAVE :: i=0,imlag1=1000,imlag2=lag1+1000-lag2-1
      i=mod(i,lag1+1000)+1
      imlag1=mod(imlag1,lag1+1000)+1
      imlag2=mod(imlag2,lag1+1000)+1
      !print*,i,'before:',lagTable(i)
      lagTable(i)=lagTable(imlag1)*lagTable(imlag2)
      mulLagFib=lagTable(i)
      !print*,t,i,imlag1,imlag2,'after:',lagTable(i)
      !print*,t,i,imlag1,imlag2,'after2:',mulLagFib
      !if (mulLagFib .eq. 0) then
      !   lagTable(i)=lagTable(imlag2)
      !   mulLagFib=lagTable(i)
      !   print*,lagTable(imLag1),lagTable(imLag2)
      !   print*, 'WEIRD 0 VALUE'
      !end if
      
      return
   END FUNCTION mulLagFibUI64RNG

   FUNCTION mulLagFibIRNG() RESULT (mulLagFib)
      INTEGER(KIND=8) :: mulLagFib8
      INTEGER(KIND=4) :: mulLagFib
      mulLagFib8 = mulLagFibUI64RNG()
      mulLagFib = ISHFT(mulLagFib8,32) 
   END FUNCTION mulLagFibIRNG

   SUBROUTINE initMLFG()
      INTEGER(KIND=8) :: maxNeg=2147483648
      INTEGER :: i
      do i=1,lag1+1000
         lagTable(i)=(shr3IRNG()+maxNeg)*(maxNeg-1)-1 !shr3IRNG()+maxNeg)
         if (mod(lagTable(i),INT(2,KIND=8)) .eq. 0) lagTable(i)=lagTable(i)+1
      end do
      !print*,"doneInit MLFG"
   END SUBROUTINE initMLFG
      
   FUNCTION kissIRNG () RESULT (kiss)
   ! The  KISS (Keep It Simple Stupid) random number generator. Combines:
   ! (1) The congruential generator x(n)=69069*x(n-1)+1327217885, period 2^32.
   ! (2) A 3-shift shift-register generator, period 2^32-1,
   ! (3) Two 16-bit multiply-with-carry generators, period 597273182964842497>2^59
   !  Overall period>2^123;  Default seeds x,y,z,w.
   !  Set your own seeds with statement i=kisset(ix,iy,iz,iw).
   !
      integer :: kiss
      x = 69069 * x + 1327217885
      y = m3(m3(m3(y, 13), - 17), 5)
      z = 18000 * iand (z, 65535) + ishft (z, - 16)
      w = 30903 * iand (w, 65535) + ishft (w, - 16)
      kiss = x + y + ishft (z, 16) + w

   END FUNCTION kissIRNG
   !   CONTAINS
         function m3(k, n)
            integer :: m3
            integer, intent(in) :: k, n
            m3 = ieor (k, ishft (k, n) )
         end function m3

   FUNCTION shr3IRNG( ) RESULT( ival )
      INTEGER  ::  ival
      INTEGER :: calls=0
      !REAL :: reverseI
   
      jz = jsr
      jsr = IEOR( jsr, ISHFT( jsr,  13 ) )
      jsr = IEOR( jsr, ISHFT( jsr, -17 ) )
      jsr = IEOR( jsr, ISHFT( jsr,   5 ) )
      ival = jz + jsr
      !CALL RANDOM_NUMBER(reverseI)
      !ival = INT(reverseI*(2147483647.0))
      RETURN
   END FUNCTION shr3IRNG

    !--------------------------------------------------------------------------
    !   Mersenne Twister: Obtain the next 32-bit integer in the psuedo-random sequence
    FUNCTION mtprng_rand64(state) result(r)
    
        ! arguments
        type(mtprng_state), intent(inout) :: state
    
        !return type
        integer(INT64) :: r

        ! internal constants
        integer(INT64), dimension(0:1), parameter :: mag01 = (/ 0_INT64, -1727483681_INT64 /)

        ! Period parameters
        integer(INT64), parameter :: UPPER_MASK =  2147483648_INT64
        integer(INT64), parameter :: LOWER_MASK =  2147483647_INT64

        ! Tempering parameters
        integer(INT64), parameter :: TEMPERING_B = -1658038656_INT64
        integer(INT64), parameter :: TEMPERING_C =  -272236544_INT64
        
        ! Note: variable names match those in original example
        integer(INT32) :: kk
        
        ! Generate N words at a time
        if (state%mti >= N) then
            ! The value -1 acts as a flag saying that the seed has not been set.
            if (state%mti == -1) call mtprng_init(4357_INT32,state)
            
            ! Fill the mt array
            do kk = 0, N - M - 1
                r = ior(iand(state%mt(kk),UPPER_MASK),iand(state%mt(kk+1),LOWER_MASK))
                state%mt(kk) = ieor(ieor(state%mt(kk + M),ishft(r,-1_INT64)),mag01(iand(r,1_INT64)))
            end do
            
            do kk = N - M, N - 2
                r = ior(iand(state%mt(kk),UPPER_MASK),iand(state%mt(kk+1),LOWER_MASK))
                state%mt(kk) = ieor(ieor(state%mt(kk + (M - N)),ishft(r,-1_INT64)),mag01(iand(r,1_INT64)))
            end do
            
            r = ior(iand(state%mt(N-1),UPPER_MASK),iand(state%mt(0),LOWER_MASK))
            state%mt(N-1) = ieor(ieor(state%mt(M-1),ishft(r,-1)),mag01(iand(r,1_INT64)))
            
            ! Start using the array from first element
            state%mti = 0
        end if
        
        ! Here is where we actually calculate the number with a series of
        !   transformations 
        r = state%mt(state%mti)
        state%mti = state%mti + 1
        
        r = ieor(r,ishft(r,-11))
        r = iand(4294967295_INT64,ieor(r,iand(ishft(r, 7),TEMPERING_B)))
        r = iand(4294967295_INT64,ieor(r,iand(ishft(r,15),TEMPERING_C)))
        r = ieor(r,ishft(r,-18))
        
    END FUNCTION mtprng_rand64

! SETUP FUNCTIONS

   SUBROUTINE kisset (ix, iy, iz, iw)
      integer ::  ix, iy, iz, iw
      x = ix
      y = iy
      z = iz
      w = iw
   END SUBROUTINE kisset


   SUBROUTINE shr3seed( jsrseed )
      INTEGER, INTENT(IN)  :: jsrseed
      !  Set the seed
      jsr = jsrseed
   END SUBROUTINE shr3seed

   SUBROUTINE zigset( )

      INTEGER  :: i

   !  Tables for ziggurat
      q = vn*EXP(half*dn*dn)
      kn(0) = (dn/q)*m1
      kn(1) = 0
      wn(0) = q/m1
      wn(127) = dn/m1
      fn(0) = 1.0_BR
      fn(127) = EXP( -half*dn*dn )
      DO  i = 126, 1, -1
      dn = SQRT( -2.0_BR * LOG( vn/dn + EXP( -half*dn*dn ) ) )
      kn(i+1) = (dn/tn)*m1
      tn = dn
      fn(i) = EXP(-half*dn*dn)
      wn(i) = dn/m1
      END DO

      initialized = .TRUE.
      RETURN
   END SUBROUTINE zigset


   SUBROUTINE mtprng_init(seed, state)
   !---------------------------------------------------------------------
   ! From the Algorithmic Conjurings of Scott Robert Ladd comes...
   !---------------------------------------------------------------------
   !
   !  mtprng.f90 (a Fortran 95 module)
   !
   !  An implementation of the Mersenne Twister algorithm for generating
   !  psuedo-random sequences.
    
        ! arguments
        integer(INT32),     intent(in)  :: seed
        type(mtprng_state), intent(out) :: state
        
        ! working storage
        integer :: i
        integer(INT64) :: s, b

        ! save seed        
        state%mt(0) = seed
        
        ! Set the seed using values suggested by Matsumoto & Nishimura, using
        !   a generator by Knuth. See original source for details.
        do i = 1, N - 1
            state%mt(i) = iand(4294967295_INT64,1812433253_INT64 * ieor(state%mt(i-1),ishft(state%mt(i-1),-30_INT64)) + i)
        end do
        
        state%mti = N

    END SUBROUTINE mtprng_init


END MODULE RNG
