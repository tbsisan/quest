program cFK

   USE cFKdata
   USE RNG
   ! function call dummy arguments are associated by position, unless assignment
   ! keywords are used.  For instant calling myfunction(a,b,c,dummyoptionalname=asdf) with a
   ! function definition SUBROUTINE myfunction(x,y,z,dummyoptionalname) will set
   ! x=a, y=b, z=c, dummyoptionalname=asdf in the function. Non-named dummy
   ! arguments are associated by position in the argument list.

   implicit none

   INTEGER, DIMENSION(8) :: timeArray
   INTEGER :: tstep,i,run,clock,startClock,myerr
   INTEGER :: iter
   REAL(KIND=BR) :: CM,myrand
   REAL(KIND=BR) :: C1,C2,maxpt,zeropt,maxF,angle,phaseShift
   CHARACTER(LEN=2) :: printingWidthRealsAsChar
   CHARACTER(LEN=5) :: formatReal
   CHARACTER(LEN=3) :: NinCharForm,LinCharForm
   CHARACTER(LEN=1) :: ans
   CHARACTER(LEN=4) :: runNumberChar
   CHARACTER(LEN=8) :: kChar,hChar,Gchar,Tchar,etaChar,runtimechar
   CHARACTER(LEN=2) :: ensChar
   CHARACTER(LEN=10) :: startClockChar
   REAL(KIND=BR), DIMENSION(size(Temp)) :: kbT, thermalStrength
   REAL(KIND=BR) :: scaledThermal
   REAL(KIND=BR), DIMENSION(7) :: lastrun, running
   INTEGER :: runsRan=0
   REAL(KIND=BR), DIMENSION(N) :: eigen
   REAL(KIND=BR) :: sigma, soundSpeed

   namelist /cFKconstants/ ens,k,eta,h,Temp,bgH,G,M
   namelist /xInit/ eigen

   tstep=0
   CM=Lc/two
   write(printingWidthRealsAsChar,'(I2)') printingWidthReals
   formatReal='F'//printingWidthRealsAsChar//'.4'
   write(NinCharForm,'(I3)') N
   write(LinCharForm,'(I3)') channelWL

   CALL SYSTEM_CLOCK(clock)
   startClock=clock
   write(startClockChar,'(I10)') startClock

   open(unit=100, file='sweep.in')
   open(unit=101, file='eigen.in')
   open(unit=3,file='collisions.dat')
   open(unit=4,file='RandomT.dat')
   open(unit=5,file='debug.dat')
   ! Skip 6, has special meaning for gfort, g95. ok
   open(unit=7,file='transitTimes.dat')
   open(unit=8,file='returnTimes.dat')
   open(unit=9,file='hoppingTimes.dat')
   open(unit=10,file='boundaryEvents.dat')
   open(unit=11,file='data.dat')
   
   !read(unit=101,nml=xInit)

   ! 
   ! Define the parameters listed in cFKconstants 
   !
   read(unit=100,nml=cFKconstants, iostat=myerr)

   kbT=0.0690_BR*Temp ! old units
   kbT=kb*Temp
   IF (STOCHASTICS) THEN
      thermalStrength=sqrt(two*eta*kbT*M/dt)
   ELSE
      thermalStrength=0.0_BR
   END IF
   DO run=1,size(M)
     IF (M(run) > iota) oneOverM(run)=1.0/M(run)
   END DO

!
! Random Number Initialization
! This is done later also, and may need to be done periodically during a large sweep of runs
!
   CALL initRNGs()

   IF (INTERACTIONMODEL .eq. ONEPARTICLE) THEN
      C1=one
      C2=zero
      DO i=1,(N-1)
         C1=C1+cos(i*(a))
         C2=C2+sin(i*(a))
         zeropt=atan(-C2/C1)
         maxpt=atan(C1/C2)
      END DO
      maxF = 0.0
      DO i=0,(N-1)
         angle=maxpt+i*(a)
         print*, angle
         maxF=maxF+sin(angle)
      END DO
      phaseShift=zeropt-CM
      IF (maxpt<zeropt) phaseShift=phaseShift-pi
      ! F = maxF*sin(x(1)+phaseShift)
      print*, 'zeropt:',zeropt
      print*, 'maxpt:',maxpt
      print*, 'maxF:',maxF
      stop
   END IF

   print*, 'system size is ',L

   lastrun=0.0
   print*, 'will do ',size(k),' runs'
   print*, 'STATUSWAIT: ',STATUSWAIT

   IF (RUNTESTS) THEN
      !constant energy
      !SELECT CASE(UNITTESTS)
      !   CASE (CONSTANTENERGY)
      !      eta(run)=0.0_BR
      !      h(run)=0.0_BR
      !   CASE (TEMPTEST)
      !      T(run)=0.01
      !END SELECT
      !testConstE
      
      !testTemp

      ! Test for INITRANDOMX, INITRANDOMY

      CALL initState()
      CALL Energy(0)
      !CALL printConclusions()
      !IF (WRITEV) write(2,'(I8,'//NinCharForm//formatReal//')') tstep,v
      run=1
      running=(/ens(run),k(run),h(run),eta(run),Temp(run),bgH(run),G(run)/)
      !write(*,'(A,6'//formatReal//')')'Running with constants (ens,k,h,eta,T,bgH,G):', running
      do tstep=1,50
         CALL advanceState()  
      end do
      CALL Energy(tstep)
      !CALL printConclusions()

   ELSE
!
! Iterate over parameter list in sweep.in
!
SWEEP: do run=1,size(ens)
      running=(/ens(run),k(run),h(run),eta(run),Temp(run),bgH(run),G(run)/)
      if (ALL(running == 0_BR)) CYCLE
      ! CURRENTLY, files corresponding to unused run numbers are not reset 
      ! so programs which access this data may be accessing old data
      write(runNumberChar,'(I0.4)') run
      write(kChar,'(E8.2)') k(run)
      write(hChar,'(E8.2)') h(run)
      write(Tchar,'(E8.2)') Temp(run)
      write(etaChar,'(E8.2)') eta(run)
      write(Gchar,'(E8.2)') G(run)
      write(runtimechar,'(E8.2)') T
      write(ensChar,'(I0.2)') INT(ens(run))
      print*, 'ens:',ensChar
      open(unit=9999,file='log.dat',position='APPEND')
      write(9999,*)running
      close(unit=9999)
      open(unit=1010,file='events/events'//runNumberChar//'.dat')
      IF (ASCIIFORMAT) THEN
         open(unit=1,file=projDir//'/x.'//runNumberChar//'.dat')
         open(unit=2,file=projDir//'/vx.'//runNumberChar//'.dat')
         open(unit=33,file=projDir//'/Ux.'//runNumberChar//'.dat')
         IF (D2) THEN
            open(unit=1111,file=projDir//'/y.'//runNumberChar//'.dat')
            open(unit=2222,file=projDir//'/vy.'//runNumberChar//'.dat')
            open(unit=3333,file=projDir//'/Uy.'//runNumberChar//'.dat')
         ENDIF
      ELSE
         open(unit=1,file=projDir//'/x.'//ensChar//'_L'//LinCharForm//'_N'//NinCharForm//'_k'//kChar//'_h'//hChar//'_T'//Tchar//'_n'//etaChar//'_F'//Gchar//'_t'//runtimechar//'.dat',form='unformatted')
         open(unit=2,file=projDir//'/vx.'//ensChar//'_L'//LinCharForm//'_N'//NinCharForm//'_k'//kChar//'_h'//hChar//'_T'//Tchar//'_n'//etaChar//'_F'//Gchar//'_t'//runtimechar//'.dat',form='unformatted')
         open(unit=33,file=projDir//'/Ux.'//ensChar//'_L'//LinCharForm//'_N'//NinCharForm//'_k'//kChar//'_h'//hChar//'_T'//Tchar//'_n'//etaChar//'_F'//Gchar//'_t'//runtimechar//'.dat',form='unformatted')
         open(unit=3854,file=projDir//'/units.'//ensChar//'_L'//LinCharForm//'_N'//NinCharForm//'_k'//kChar//'_h'//hChar//'_T'//Tchar//'_n'//etaChar//'_F'//Gchar//'_t'//runtimechar//'.dat')
         IF (D2) THEN
            !open(unit=1111,file='/projects/p20200/cFK/nm44checkT/y.'//runNumberChar//'.dat',form='unformatted')
            open(unit=1112,file=projDir//'/xsep.'//runNumberChar//'.dat',form='unformatted')
            open(unit=1113,file=projDir//'/ysep.'//runNumberChar//'.dat',form='unformatted')
            !open(unit=2222,file='/projects/p20200/cFK/nm44checkT/vy.'//runNumberChar//'.dat',form='unformatted')
            open(unit=1111,file=projDir//'/y.'//ensChar//'_L'//LinCharForm//'_N'//NinCharForm//'_k'//kChar//'_h'//hChar//'_T'//Tchar//'_n'//etaChar//'_F'//Gchar//'_t'//runtimechar//'.dat',form='unformatted')
            open(unit=2222,file=projDir//'/vy.'//ensChar//'_L'//LinCharForm//'_N'//NinCharForm//'_k'//kChar//'_h'//hChar//'_T'//Tchar//'_n'//etaChar//'_F'//Gchar//'_t'//runtimechar//'.dat',form='unformatted')
            open(unit=3333,file=projDir//'/Uy.'//ensChar//'_L'//LinCharForm//'_N'//NinCharForm//'_k'//kChar//'_h'//hChar//'_T'//Tchar//'_n'//etaChar//'_F'//Gchar//'_t'//runtimechar//'.dat',form='unformatted')
         ENDIF
      END IF
      open(unit=999,file=projDir//'/log.'//ensChar//'_L'//LinCharForm//'_N'//NinCharForm//'_k'//kChar//'_h'//hChar//'_T'//Tchar//'_n'//etaChar//'_F'//Gchar//'_t'//runtimechar//'.dat')
      !if (ALL(running == lastrun)) CYCLE
      !IF (ALL(running == 0.0)) THEN
      !	 write(1010,*) ''
      !	 close(unit=1010)
      !	 CYCLE
      !END IF
      
      write(999,*) 'thermalStren:',thermalStrength(run)
      write(999,*) 'ens:',ens(run)
      write(999,*) 'h:',h(run)
      write(999,*) 'G:',G(run)
      write(999,*) 'k:',k(run)
      write(999,*) 'eta:',eta(run)
      write(999,*) 'iter:',iter
      soundSpeed=sqrt(k(run)*oneOverM(run))*WL
      write(999,*) 'sound Speed (m/s):',soundSpeed

!
! State Initialization
!
      Q=0
      R=0
      lastwrap=N
      CALL initState()
      write(999,*) 'starting xs after initState():',x(1:9)
      write(999,*) 'starting vxs after initState():',vx(1:9)
      CALL printStartup()
      write(999,*) 'starting xs after printStartup():',x(1:9)
      write(999,*) 'starting vxs after printStartup():',vx(1:9)

      ! Check that rigid chains are all same length
      IF (CHAINBC .eq. INFINITECHAIN .AND. &
         INTERACTIONMODEL .eq. RIGIDBARS) THEN
      CALL rigidSanityCheck()
      END IF
         
      IF (ICS .eq. IMPINGER .and. COORDBC .ne. INFINITECOORD) THEN
         write(999,*) 'CHOSEN ICS SHOULD HAVE INFINITE COORD'
         !stop
      END IF
   

!
! Main Simulation Loop
!
      CALL Energy(0)
      CALL writeState()

      scaledThermal=thermalStrength(run)
      sigma=sqrt(dt)*oneOverM(run)*scaledThermal ! For stochastic integrators
      do tstep=1,steps
         IF ( (tstep == 1 .or. mod(tstep,100)==0) .and. tstep <= coolDownSteps) THEN
           scaledThermal=currentTemp(tstep,coolDownSteps,thermalStrength(run),Temp(run))
           sigma=sqrt(dt)*oneOverM(run)*scaledThermal ! For stochastic integrators
         END IF
         CALL advanceState()  
      end do
      CALL Energy(steps)

      CALL printConclusions()

      runsRan=runsRan+1
      lastrun=running
      close(unit=1010)
      close(unit=1)
      close(unit=2)
      close(unit=33)

end do SWEEP

END IF

CALL SYSTEM_CLOCK(clock)
write(999,*) T*runsRan*0.513e-12*1e9/((clock-startClock)/1e3/60),'nanoseconds per minute'
!CALL system("beep -f 500 -n -f 600 -n -f 700 -n -f 800 -n -f 900 -n -f 1000")
write(999,*) 'DONE'

!
! Subroutines
!

CONTAINS
    FUNCTION currentTemp(tstep,coolDownSteps,eqThermalStrength,eqTemp) RESULT(scaledThermal)
      REAL(KIND=BR), INTENT(IN) :: eqThermalStrength, eqTemp
      INTEGER, INTENT(IN) :: tstep, coolDownSteps
      REAL :: eqCompletionPct, newtargetT, scaledThermal
           eqCompletionPct = real(tstep)/real(coolDownSteps)
           newtargetT = real(Tstart) * (1_BR-eqCompletionPct) + eqTemp*eqCompletionPct
           scaledThermal=eqThermalStrength * sqrt(newTargetT/eqTemp)
    END FUNCTION currentTemp
!
! Initialize
! State Variables
!
!   SUBROUTINE init_random_seed()
!            INTEGER :: i, n=1, clock
!            INTEGER :: seed
          
!            CALL RANDOM_SEED(size = n)
          
!            CALL SYSTEM_CLOCK(COUNT=clock)
          
!            seed = clock !+ 37 * (/ (i - 1, i = 1, n) /)
!            CALL RANDOM_SEED(PUT = seed)
          
!   END SUBROUTINE

   SUBROUTINE Energy(tstep)
      INTEGER, INTENT(IN) :: tstep
      REAL(KIND=BR), DIMENSION(NSim) :: rdiffx,ldiffx,rdiffy,ldiffy
      CALL rightLeft(x,rdiffx,ldiffx)
      CALL upDown(y,rdiffy,ldiffy)
      !CALL findWrapPt(rdiffx)
      !CALL fixWrapPtDiff(rdiffx)
      PEx = sum(onehalf*k(run)*(rdiffx-ax)**2)
      PEy = sum(onehalf*k(run)*rdiffy**2)
      KEx = sum(onehalf*M(run)*vx**2)
      KEy = sum(onehalf*M(run)*vy**2)
      !write(999,*) "Time Step:",tstep
      !write(999,*) "Ys:",y
      !write(999,*) "VYs:",vy
      !write(999,*) "k:",k(1)
      !write(999,*) "KEx:",KEx,"KEy:",KEy
      !write(999,*) "PEx:",PEx,"PEy:",PEy
   END SUBROUTINE Energy

   SUBROUTINE nondimensionalize()
      REAL(KIND=BR) :: hSI,kSI,lambdaSI,aSI,etaSI,TSI,mSI

!      SELECT CASE (NONDIMENTYPE)
!      CASE (HTYPE)
!	 IF (h(run) .ne. 1) THEN
!	    write(999,*) "h has to be 1 when, nondimensionalizing that way"
!	    STOP
!	 END IF
!      CASE (KTYPE)
!	 IF (k(run) .ne. 1) THEN
!	    write(999,*) "k has to be 1 when, nondimensionalizing that way"
!	    STOP
!	 END IF
!      END SELECT

   END SUBROUTINE nondimensionalize


   SUBROUTINE initState()
      REAL(KIND=BR) :: chainL, particleSep, center1, center2, width, vMean
      REAL(KIND=BR) :: charge=-1.0_BR
      REAL(KIND=BR), DIMENSION(2) :: uv1, uv2
      INTEGER :: i
      chainL=(real(N,KIND=BR)-one)*a
      x=(/ (i,i=0,size(x)-1) /)
      IF (CHAINBC .ne. CATERPILLARCHAIN) THEN
        particleSep=L/REAL(N,KIND=BR)
      ELSE
        particleSep=a
      END IF
      x=x*particleSep+particleSep/8.0
      y=1.0_BR
      vy=0.0_BR
      vx=0.0_BR
      write(999,*) 'System width:',L
      write(999,*) 'starting xs: from within initState() before random',x(1:9)
      write(999,*) 'starting vxs:fromwithininitState() before random',vx(1:10)
      vMean=sqrt(kb*300.0*oneOverM(run))
      SELECT CASE(ICS)
         CASE (UNIFORMLINE)
            ! TBS: the below was +(L-chainL)/2.0
            x=x+(L-chainL)/2.0
            DO i=1,N
               vx(i)=vMean*(1+handleRandom())
            ENDDO
         CASE (TWOKINKS)
            center1=15.0_BR
            center2=30.0_BR
            width=8.0_BR
            SELECT CASE(N)
               CASE (int(L)+1)
                  charge=1.0_BR
               CASE (int(L)-1)
                  charge=-1.0_BR
            END SELECT
            DO i=1,N
               uv1=uv(i,center1,width,0.5_BR)
               uv2=uv(i,center2,width,0.5_BR)
               x(i)=-WL/WLperN/2.0_BR+i*WL-charge*uv1(1)-charge*uv2(1)
               vx(i)=-charge*uv1(2)-charge*uv2(2)
            ENDDO
         CASE (ONEBREATHER)
            center1=20.0_BR
            center2=40.0_BR
            width=7.8808_BR ! at v=0.8, width=13.13_BR
            DO i=1,N
               uv1=uv(i,center1,width,0.00_BR)
               uv2=uv(i,center2,width,0.00_BR)
               x(i)=-WL/WLperN/2.0_BR+i*WL-uv1(1)+uv2(1)
               vx(i)=uv1(2)+uv2(2)+vMean*(1+handleRandom())
            ENDDO
         CASE (IMPINGER)
            x=x-0.0_BR
            vx=0.0_BR
            ! i don't understand why a dummy argument i was needed
            vx=sqrt(kbT(run))*(/ (handleRandom(i), i=1,N) /)
            vx=0.0_BR
            !x=x*( (/ (0.1_BR*handleRandom(i), i=1,N) /) + 1.0_BR )
            !x=x + (/ (0.01_BR*handleRandom(i), i=1,N) /)
            !write(999,*) 'perturbed x',x
            x = x - (x(N)-L) - 1.0
            write(999,*) 'shifted x',x
            !x(1) = 0.0
            vx(1)=4.0_BR * (1+0.25*handleRandom())
            write(999,*) 'initial velocities:',vx
            write(999,*) 'initial positions:',x
         CASE (SHIFT1Y)
            y(1)=Ly/100.0_BR
         CASE (HELIX)
            y=-1.0_BR/real(pitch,KIND=BR)*WLperN*x+pi
            DO i=1,3*N
               CALL applyBC()
            ENDDO
      END SELECT
      IF (INITRANDOMX) THEN
         DO i=1,N
            x(i)=x(i)*(1.0+0.0002*handleRandom())
            vx(i)=vx(i)*(1.0+0.0002*handleRandom())
         END DO
      END IF
      IF (INITRANDOMY) THEN
         DO i=1,N
            y(i)=y(i)*(1.0+0.01*handleRandom())
            vy(i)=vy(i)*(1.0+0.01*handleRandom())
         END DO
      END IF
      CM = sum(x)/N
      label(:(N/2))='L'
      label(N/2+1:)='R'
      write(999,*) 'starting xs from within initState() after random:',x(1:9)
      write(999,*) 'starting vxs from within initState() after random:',vx(1:10)
      write(999,*) 'ending xs:',x(-16),x(-15)
      CALL writeState()

   END SUBROUTINE initState
!
   SUBROUTINE eigenStretch()
      REAL(KIND=BR) :: chainL
      INTEGER :: i
      chainL=(real(N,KIND=BR)-one)*a
      x=(/ (i,i=0,size(x)-1) /)
      x=x*a
      x=x+(L-chainL)/2.0_BR
      ! TBS: commented out below because causing errors
      !x=x+4.0_BR*eigen
      CM = sum(x)/N
      vx=0.0_BR
      label(:(N/2))='L'
      label(N/2+1:)='R'
   END SUBROUTINE eigenStretch
!
! Do One 
! Time Step
!
   SUBROUTINE advanceState()
      !REAL(KIND=BR), SAVE, DIMENSION(N) :: rdiff=0.0,ldiff=0.0
      REAL(KIND=8), SAVE :: runningTime=0.0

      IF (TIMESUBS) THEN
         CALL startTimer()
      END IF

      IF( QTRACKLABELSON ) THEN 
         CALL flowTrack()
      END IF

      SELECT CASE (COORDBC)
         CASE (CHANNELCOORD) 
            CALL applyBC()
         CASE DEFAULT
      END SELECT


      !IF (INTERACTIONMODEL .ne. RIGIDBARS) THEN
      IF (VCORRELATIONS) THEN
         IF (mod(tstep,CorWindow)==0) CALL particleCorrelations()
      END IF
      !CALL doubleCheckCorrelations()
      !END IF

      CALL particleDynamics()

      IF (mod(tstep,WRITESTATEWAIT)==0) CALL writeState()

      IF (TIMESUBS) THEN
         runningTime=runningTime+elapsedTime()
         IF (mod(tstep,STATUSWAIT)==0) write(999,*) 'runningTime:',runningTime
      END IF

   END SUBROUTINE advanceState
!
!
!
!
   SUBROUTINE applyBC()
      INTEGER :: i
      !x=modulo(x,L)
      DO i=1,N
         IF (x(i)>L) THEN
            x(i)=x(i)-L
         ELSE IF (x(i)<zero) THEN
            x(i)=x(i)+L
         END IF
      END DO
      IF (D2) THEN
         WHERE (y > Ly) 
            y=y-Ly
         ELSEWHERE (y < 0)
            y=y+Ly
         END WHERE
      END IF
      !write(999,*) tstep,MAXVAL(y)

   END SUBROUTINE applyBC
!
!
!
!
   SUBROUTINE particleDynamics()
      REAL(KIND=BR), DIMENSION(NSim) :: dx,dy
      !REAL(KIND=BR) :: dxCorrect
      REAL(KIND=8), SAVE :: runningTime=0.0

      IF (TIMESUBS) THEN
         CALL startTimer()
      END IF

      SELECT CASE (INTEGRATOR)
         CASE (EULER)
            dx = euler1(1,x,vx)
            IF (D2) dy = euler1(2,y,vy)
         CASE (EULERM)
            dx = eulerMaruyama(1,x,vx)
            IF (D2) dy = eulerMaruyama(2,y,vy)
         CASE (RUNGEKUTTA)
            dx = rungekutta4(1,x,vx)
            IF (D2) dy = rungekutta4(2,y,vy)
         CASE (PREDICTOR)
            dx = predictorCorrector(1,x,vx)
            IF (D2) dy = predictorCorrector(2,y,vy)
         CASE (SDEVERLET)
            dx = sdeVerlet21(1,x,vx)
            IF (D2) dy = sdeVerlet21(2,y,vy)
         CASE (VVERLET)
            dx = Verlet1(1,x,vx)
            IF (D2) dy = Verlet1(2,y,vy)
         CASE DEFAULT
            write(999,*) 'No integrator of that method'
            STOP
      END SELECT

      IF (COORDBC .eq. CHANNELCOORD) xTotal=xTotal+dx

      IF (INTERACTIONMODEL .eq. RIGIDBARS) THEN
         ! I don't need the below anymore because now every particle is guaranteed to move the same amount
         !dxCorrect = sum(dx)/REAL(N,KIND=BR)
         !x=x-dx+dxCorrect
         CM=CM+dx(1)
         IF (COORDBC .eq. CHANNELCOORD) THEN
            CM=modulo(CM,L)
         !   x=modulo(x,L)
         END IF
         IF (x(N)-(CM+Lc*onehalf)>eps) THEN
            write(999,*) 'something not right at step',tstep
            write(999,*) 'L_c',Lc
            write(999,*) 'xN',x(N)
            write(999,*) 'CM',CM
            write(999,*) 'eps',eps
            write(999,*) 'x-cm/2',(x(N)-(CM+Lc*onehalf))
            write(999,*) '1-N',x(N)-x(1)
         END IF
      END IF

      IF (TIMESUBS) THEN
         runningTime=runningTime+elapsedTime()
         IF (mod(tstep,STATUSWAIT)==0) write(999,*) 'runningTime Integrator:',runningTime
      END IF
   END SUBROUTINE particleDynamics
!
!
   FUNCTION uv(i,center,width,velocity) RESULT(uvi)
      ! should return results in SI units
      REAL(KIND=BR) :: center, width, velocity, width_eff, e_x, sol_A
      REAL(KIND=BR), DIMENSION(2) :: uvi
      INTEGER :: i
      width_eff=width*sqrt(1-velocity**2)
      e_x=exp((i-center)/width_eff)
      sol_A=4*(WL/WLperN)/(2.0*pi)
      uvi(1)=sol_A*atan(e_x)
      uvi(2)=sol_A*velocity*soundSpeed/width_eff*e_x/(1+e_x**2)/2.456e-10
   END FUNCTION uv

   FUNCTION handleRandom(dum1) RESULT(randomVariable)
      REAL(KIND=BR) :: randomVariable
      INTEGER, OPTIONAL :: dum1
      LOGICAL, SAVE :: firstCall=.true.
      ! if temp is zero, i should just return zeros
      IF (firstCall .and. tstep .eq. 1) THEN
         CALL initRNGs()
         firstCall=.false.
      END IF
      randomVariable = gaussianDeviate()
      RETURN
      
   END FUNCTION handleRandom
!
!
   FUNCTION Verlet1(currentDim,xSub,vSub) RESULT (dx)
      REAL(KIND=BR), SAVE, DIMENSION(NSim) :: Ft=0
      REAL(KIND=BR), SAVE, DIMENSION(2,2,NSim) :: F=zero
      REAL(KIND=BR), DIMENSION(NSim) :: rdiffSub1,ldiffSub1
      REAL(KIND=BR), DIMENSION(NSim) :: dx,dv
      REAL(KIND=BR), INTENT(INOUT), DIMENSION(NSim) :: xSub,vSub
      REAL(KIND=BR), SAVE :: dt2 = dt**2
      INTEGER, INTENT(IN) :: currentDim
      IF (STOCHASTICS) THEN
         Ft=scaledThermal*(/ (handleRandom(), i=1,N) /)
      END IF
      IF (tstep .eq. 1) THEN
         F(currentDim,1,:)=cFKa(currentDim,xSub,vSub,Ft,rdiffSub1,ldiffSub1)
         write(999,*) "WARNING VVERLET HAS NO BLOWTHROUGH DETECTION"
      ENDIF
      dx=dt*vSub+onehalf*dt2*F(currentDim,1,:)
      xSub=xSub+dx
      F(currentDim,2,:)=cFKa(currentDim,xSub,vSub,Ft,rdiffSub1,ldiffSub1)
      dv=onehalf*dt*(F(currentDim,1,:)+F(currentDim,2,:))
      vSub=vSub+dv
      F(currentDim,1,:)=F(currentDim,2,:)
      !CALL writeSeps(currentDim,rdiffSub1)
      
   END FUNCTION Verlet1

   FUNCTION Verlet2(currentDim,xSub,vSub) RESULT (dx)
      REAL(KIND=BR), SAVE, DIMENSION(NSim) :: Ft=0
      REAL(KIND=BR), SAVE, DIMENSION(2,2,NSim) :: F=zero
      REAL(KIND=BR), DIMENSION(NSim) :: rdiffSub1,ldiffSub1
      REAL(KIND=BR), DIMENSION(NSim) :: dx,dv
      REAL(KIND=BR), INTENT(INOUT), DIMENSION(NSim) :: xSub,vSub
      REAL(KIND=BR), SAVE :: dt2 = dt**2
      INTEGER, INTENT(IN) :: currentDim
      IF (tstep .eq. 1) THEN
         F(currentDim,1,:)=cFKa(currentDim,xSub,vSub,Ft,rdiffSub1,ldiffSub1)
         write(999,*) "WARNING VVERLET HAS NO BLOWTHROUGH DETECTION"
      ENDIF
      IF (STOCHASTICS) THEN
         Ft=scaledThermal*(/ (handleRandom(), i=1,N) /)
      END IF
      dx=dt*vSub+onehalf*dt2*F(currentDim,1,:)
      xSub=xSub+dx
      dv=onehalf*dt*(F(currentDim,1,:))
      vSub=vSub+dv
      F(currentDim,2,:)=cFKa(currentDim,xSub,vSub,Ft,rdiffSub1,ldiffSub1)
      dv=onehalf*dt*(F(currentDim,2,:))
      vSub=vSub+dv
      F(currentDim,1,:)=F(currentDim,2,:)
      !CALL writeSeps(currentDim,rdiffSub1)
      
   END FUNCTION Verlet2

   FUNCTION sdeVerlet21(currentDim,xSub,vSub) RESULT (dx)
      ! from Vanden-Eijnden & Ciccotti (2006)
      ! Equation (21)
      REAL(KIND=BR), SAVE, DIMENSION(2,NSim) :: Rt=0
      REAL(KIND=BR), SAVE, DIMENSION(2,2,NSim) :: F=zero
      REAL(KIND=BR), DIMENSION(NSim) :: rdiffSubSde,ldiffSubSde
      REAL(KIND=BR), DIMENSION(NSim) :: dx,dv
      REAL(KIND=BR), INTENT(INOUT), DIMENSION(NSim) :: xSub,vSub
      REAL(KIND=BR), DIMENSION(NSim) :: A
      REAL(KIND=BR), SAVE :: dt2 = dt**2, dt32
      INTEGER, INTENT(IN) :: currentDim
      LOGICAL :: blowthrough=.false.
      dt32=dt*sqrt(dt)
      IF (STOCHASTICS) THEN
         Rt(1,:)=(/ (handleRandom(), i=1,N) /)
         Rt(2,:)=(/ (handleRandom(), i=1,N) /)
      ELSE
         IF (tstep<10) write(999,*) 'using SDE with no temperature!!!!'
      END IF
      A = onehalf*dt2*(F(currentDim,1,:)-eta(run)*vSub)+ &
         sigma*dt32*(onehalf*Rt(1,:)+tworoot3*Rt(2,:))
      !write(*,'(A,2E12.4E2)')' Thermal Forces:',Rt(1,1:2)
      !write(*,'(A,2E12.4E2)')' Old Total Forces:',F(1,1,1:2)
      dx = vSub*dt+A
      !write(999,*) 'v*dt:',v*dt
      xSub = xSub + dx
      F(currentDim,2,:)=cFKa(currentDim,xSub,rdiffSub=rdiffSubSde,ldiffSub=ldiffSubSde)
      !CALL writeSeps(currentDim,rdiffSubSde)
      IF (  INTERACTIONMODEL .ne. RIGIDBARS .AND. &
      INTERACTIONMODEL .ne. ONEPARTICLE .AND. &
      CHECKBLOWTHROUGH) THEN
         blowthrough = blowthroughDetector(rdiffSubSde)
      END IF
      IF (blowthrough) THEN
         dx=-threehalf*vSub*dt
         xSub=xSub+dx
         blowthrough=.false.
         STOP
      END IF
      CALL hardCoreCorrection(rdiffSubSde,ldiffSubSde)
      !write(999,*) 'F:',F(2,:)
      dv = onehalf*dt*(F(currentDim,2,:)+F(currentDim,1,:))-dt*eta(run)*vSub+ &
         sqrt(dt)*sigma*Rt(1,:)-eta(run)*A
      !write(999,*) 'dv:',dv
      F(currentDim,1,:)=F(currentDim,2,:)
      vSub = vSub + dv
      !if (tstep.eq.2) STOP
   END FUNCTION sdeVerlet21
!
!
   FUNCTION euler1(currentDim,xSub,vSub) RESULT (dx)
      REAL(KIND=BR), SAVE, DIMENSION(NSim) :: Ft=0
      REAL(KIND=BR), DIMENSION(NSim) :: rdiffSub,ldiffSub
      REAL(KIND=BR), DIMENSION(NSim) :: dx,dv
      REAL(KIND=BR), INTENT(INOUT), DIMENSION(NSim) :: xSub,vSub
      INTEGER, INTENT(IN) :: currentDim
      LOGICAL :: blowthrough
      blowthrough=.false.
      IF (STOCHASTICS) THEN
         Ft=scaledThermal*(/ (handleRandom(), i=1,N) /)
      END IF
      dv = dt*cFKa(currentDim,xSub,vSub,Ft,rdiffSub,ldiffSub)
      !F(currentDim,2,:)=cFKa(currentDim,xSub,rdiffSub=rdiffSubSde,ldiffSub=ldiffSubSde)
      !CALL writeSeps(currentDim,rdiffSub)
      !F=cFKa(x,Ft=Ft,rdiffSub=rdiffSub,ldiffSub=ldiffSub)
      !write(999,*) 'F:',F
      !F=cFKa(x,v,Ft,rdiffSub,ldiffSub)
      !write(999,*) 'Fnorm:',F
      IF (  INTERACTIONMODEL .ne. RIGIDBARS .AND. &
         INTERACTIONMODEL .ne. ONEPARTICLE .AND. &
         CHECKBLOWTHROUGH) THEN
         blowthrough = blowthroughDetector(rdiffSub)
      END IF
      IF (blowthrough) THEN
         dx=-threehalf*vSub*dt
         xSub=xSub+dx
      ELSE
         vSub=vSub+dv
         CALL hardCoreCorrection(rdiffSub,ldiffSub)
         dx=vSub*dt
         xSub=xSub+dx
      END IF
   END FUNCTION euler1
!
   FUNCTION eulerMaruyama(currentDim,xSub,vSub) RESULT (dx)
      ! from Vanden-Eijnden & Ciccotti (2006)
      ! Equation (5)
      REAL(KIND=BR), SAVE, DIMENSION(NSim) :: Rt=0
      REAL(KIND=BR), SAVE, DIMENSION(2,NSim) :: F=zero
      REAL(KIND=BR), DIMENSION(NSim) :: rdiffSubSde,ldiffSubSde
      REAL(KIND=BR), DIMENSION(NSim) :: dx,dv
      REAL(KIND=BR), INTENT(INOUT), DIMENSION(NSim) :: xSub,vSub
      INTEGER, INTENT(IN) :: currentDim
      LOGICAL :: blowthrough=.false.

      IF (STOCHASTICS) THEN
         Rt=(/ (handleRandom(), i=1,N) /)
      ELSE
         IF (tstep<10) write(999,*) 'using stochastic integrator with no Temp!!!'
      END IF
      !write(*,'(A,2E12.4E2)')' Thermal Forces:',Rt(1:2)
      !write(*,'(A,2E12.4E2)')' Old Total Forces:',F(1,1:2)

      dx = vSub*dt
      xSub = xSub + dx

      F(currentDim,:)=cFKa(currentDim,xSub,rdiffSub=rdiffSubSde,ldiffSub=ldiffSubSde)

      !CALL writeSeps(currentDim,rdiffSubSde)
      IF (  INTERACTIONMODEL .ne. RIGIDBARS .AND. &
      INTERACTIONMODEL .ne. ONEPARTICLE .AND. &
      CHECKBLOWTHROUGH) THEN
         blowthrough = blowthroughDetector(rdiffSubSde)
      END IF
      IF (blowthrough) THEN
         dx=-threehalf*vSub*dt
         xSub=xSub+dx
         blowthrough=.false.
         STOP
      END IF
      CALL hardCoreCorrection(rdiffSubSde,ldiffSubSde)

      dv = dt*F(currentDim,:)-dt*eta(run)*vSub+sqrt(dt)*sigma*Rt
      vSub = vSub + dv

   END FUNCTION eulerMaruyama
!
!
!
   FUNCTION predictorCorrector(currentDim,xSub,vSub) RESULT (dx)
      REAL(KIND=BR), DIMENSION(NSim) :: rdiffSub,ldiffSub
      REAL(KIND=BR), SAVE, DIMENSION(2,3,NSim) :: dv
      REAL(KIND=BR), DIMENSION(NSim) :: dx, vGuess
      REAL(KIND=BR), SAVE, DIMENSION(NSim) :: Ft=0
      REAL(KIND=BR), INTENT(INOUT), DIMENSION(NSim) :: xSub,vSub
      INTEGER, INTENT(IN) :: currentDim
      IF (STOCHASTICS) THEN
         Ft=scaledThermal*(/ (handleRandom(), i=1,N) /)
      END IF
      dv(currentDim,2,:)=dt*cFKa(currentDim,xSub,vSub,Ft,rdiffSub,ldiffSub)       ! the new a
      vGuess=vSub+threehalf*dv(currentDim,1,:)-onehalf*dv(currentDim,2,:)       ! guess at the new v
      dx=vSub*dt+twothirds*dv(currentDim,2,:)*dt-onesixth*dv(currentDim,1,:)*dt       ! the new x
      dv(currentDim,3,:)=dt*cFKa(currentDim,xSub+dx,vGuess,Ft,rdiffSub,ldiffSub)       ! guess at the next a
      vSub=vSub+onethird*dv(currentDim,3,:)+fivesixth*dv(currentDim,2,:)-onesixth*dv(currentDim,1,:)  ! the new v
      xSub=xSub+dx
      dv(currentDim,1,:)=dv(currentDim,2,:)       ! save the new a as the old a
      !CALL writeSeps(currentDim,rdiffSub)
   END FUNCTION predictorCorrector
!
!
   FUNCTION rungekutta4(currentDim,xSub,vSub) RESULT (dxf)
      REAL(KIND=BR), DIMENSION(NSim) :: rdiffSub,ldiffSub
      REAL(KIND=BR), DIMENSION(NSim) :: rdiffLater,ldiffLater
      REAL(KIND=BR), DIMENSION(NSim) :: xLater,vLater,dxf
      REAL(KIND=BR), DIMENSION(4,NSim) :: dv,dx
      REAL(KIND=BR), SAVE, DIMENSION(NSim) :: Ft=0
      REAL(KIND=BR), INTENT(INOUT), DIMENSION(NSim) :: xSub,vSub
      INTEGER, INTENT(IN) :: currentDim
      INTEGER :: i
      LOGICAL :: blowthrough
      blowthrough=.false.

      IF (STOCHASTICS) THEN
         Ft=scaledThermal*(/ (handleRandom(), i=1,N) /)
      END IF
      !write(999,*) 'Ft:',Ft(1:5)
      dx(1,:)=dt*vSub
      dv(1,:)=dt*cFKa(currentDim,xSub,vSub,Ft,rdiffSub,ldiffSub)   !Need to set rdiffSub, before doing blowthroughdetector

      IF (INTERACTIONMODEL .ne. RIGIDBARS) THEN
         IF (CHECKBLOWTHROUGH) THEN
            blowthrough = blowthroughDetector(rdiffSub)
         END IF
         IF (blowthrough) THEN
            dxf=-one*vSub*dt
         ELSE
            CALL hardCoreCorrection(rdiffSub,ldiffSub)
         END IF
      END IF

      xLater=xSub+dx(1,:)*onehalf
      vLater=vSub+dv(1,:)*onehalf
      dx(2,:)=dt*(vLater)
      dv(2,:)=dt*cFKa(currentDim,xLater,vLater,Ft,rdiffLater,ldiffLater)

      xLater=xSub+dx(2,:)*onehalf
      vLater=vSub+dv(2,:)*onehalf
      dx(3,:)=dt*(vLater)
      dv(3,:)=dt*cFKa(currentDim,xLater,vLater,Ft,rdiffLater,ldiffLater)

      xLater=xSub+dx(3,:)
      vLater=vSub+dv(3,:)
      dx(4,:)=dt*(vLater)
      dv(4,:)=dt*cFKa(currentDim,xLater,vLater,Ft,rdiffLater,ldiffLater)

      IF (.NOT.blowthrough) THEN
         dxf = (dx(1,:) + two*dx(2,:) + two*dx(3,:) + dx(4,:))*onesixth
      END IF

      xSub = xSub + dxf
      vSub = vSub + (dv(1,:) + two*dv(2,:) + two*dv(3,:) + dv(4,:))*onesixth
      !CALL writeSeps(currentDim,rdiffLater)

   END FUNCTION rungekutta4
!
!
!
!
   SUBROUTINE writeSeps(currentDim,rdiffSub)
      REAL(KIND=BR), INTENT(IN), DIMENSION(NSim) :: rdiffSub
      INTEGER, INTENT(IN) :: currentDim
      
      IF (mod(tstep,WRITESTATEWAIT)==0 .and. currentDim==1) write(1112) REAL(tstep*dt,KIND=SMREAL),REAL(rdiffSub,KIND=SMREAL)
      IF (mod(tstep,WRITESTATEWAIT)==0 .and. currentDim==2) write(1113) REAL(tstep*dt,KIND=SMREAL),REAL(rdiffSub,KIND=SMREAL)
   END SUBROUTINE writeSeps

   FUNCTION cFKa(currentDim,rSub,vSub,Ft,rdiffSub,ldiffSub)
      ! a function that can be run for either x or y
      REAL(KIND=BR), INTENT(OUT), DIMENSION(NSim) :: rdiffSub, ldiffSub
      REAL(KIND=BR), INTENT(IN), DIMENSION(NSim) :: rSub
      REAL(KIND=BR), INTENT(IN), OPTIONAL, DIMENSION(NSim) :: vSub
      REAL(KIND=BR), INTENT(IN), OPTIONAL, DIMENSION(N) :: Ft
      REAL(KIND=BR) :: phaseFactor
      REAL(KIND=BR), DIMENSION(NSim) :: cFKa
      REAL(KIND=BR), DIMENSION(NSim) :: Ftube, Fwash, Fbg=0.0_BR, Ffric, FG=0_BR
      REAL(KIND=BR), DIMENSION(NSim) :: yPhase,xPhase
      REAL(KIND=BR), DIMENSION(N) :: Fnear
      REAL(KIND=BR), DIMENSION(N) :: FtSub
      INTEGER, INTENT(IN) :: currentDim
      LOGICAL :: outputForces=.false.

      phaseFactor=2*pi*WLperN/WL
      IF (D2) THEN
         yPhase = real(pitch,KIND=BR)*y
         xPhase = WLperN*x*WL
      ELSE
         yPhase = 0.0_BR
         xPhase = 0.0_BR
      ENDIF

      IF (PRESENT(Ft)) THEN
         FtSub=Ft
      ELSE
         FtSub=zero
      ENDIF

      IF (PRESENT(vSub) .and. eta(run) .ne. 0.0 ) THEN
         Ffric=frictionF(vSub)
      ELSE
         Ffric=zero
      END IF

      !IF (G(run) .ne. 0_BR) FG=G(run)
      IF (currentDim==1 .and. tstep>Gwait) FG=G(run)
      !IF (tstep==200) write(999,*) 'tstep:',tstep,' FG:',FG

      IF (BGTYPE .ne. NOBG) THEN
         write(999,*) "doing background force"
         IF (currentDim==1) Fbg = bgForce(rSub)
      END IF

      IF (tstep .eq. 1) write(999,*) 'TBS: there is a factor of',WLperN,' for sinusoidal potential'
      IF (tstep .lt. 2) write(999,*) 'TBS: THERE MAY BE AN ERROR WHEN ONEPARTICLE IS &
      & ACTIVE; RIGIDBARS and ONEPARTICLE not set up right'
      SELECT CASE (INTERACTIONMODEL)
         CASE (RIGIDBARS) 
            Fwash=sum(h(run)*sin(phaseFactor*rSub))/REAL(N,KIND=BR)
            cFKa = Fwash + Ffric + sum(FtSub)/REAL(N,KIND=BR) + Fbg + FG
         CASE (ONEPARTICLE) 
            Fwash=maxF*sin((rSub+phaseShift)*WLperN/WL)
            cFKa = Fwash + Ffric + FtSub(1)/sqrt(REAL(N,KIND=BR)) + Fbg + FG
         CASE DEFAULT
            IF (currentDim==1) THEN
               CALL rightLeft(rSub,rdiffSub,ldiffSub)
               Fnear=particleParticle(rdiffSub,ldiffSub)
            ELSE
               CALL upDown(rSub,rdiffSub,ldiffSub)
               Fnear=ySpringScale*particleParticle(rdiffSub,ldiffSub)
            ENDIF
             !write(999,*) rdiffSub
             !write(999,*) ldiffSub
            IF (currentDim==1) THEN
               Fwash=h(run)*phaseFactor*sin(phaseFactor*rSub+yPhase)
               Ftube=0.0
            ELSE
               Fwash=h(run)*pitch*sin(rSub*pitch+xPhase)
               Ftube=0.0
            ENDIF
            !if (tstep .ge. steps-0) then
            !write(999,*) Fnear(1), '+', Fwash(1), '+', Ffric(1), '+', FtSub(1), '+', Fbg(1)
            !write(999,*) Fnear(2), '+', Fwash(2), '+', Ffric(2), '+', FtSub(2), '+', Fbg(2)
            !write(999,*) Fnear(3), '+', Fwash(3), '+', Ffric(3), '+', FtSub(3), '+', Fbg(3)
            !write(999,*) Fnear(4), '+', Fwash(4), '+', Ffric(4), '+', FtSub(4), '+', Fbg(4)
            !write(999,*) Fnear(5), '+', Fwash(5), '+', Ffric(5), '+', FtSub(5), '+', Fbg(5)
            !write(999,*) Fnear(6), '+', Fwash(6), '+', Ffric(6), '+', FtSub(6), '+', Fbg(6)
            !end if
            cFKa = oneOverM(run)*(Fnear + Fwash + Ffric + FtSub + Fbg + Ftube + FG)
      END SELECT

      IF (outputForces) THEN
         write(999,*) 'tstep:',tstep
         write(999,*) 'xs:',rSub(1:3)
         !write(999,*) 'vs:',vSub
         !write(*,'(A,2E12.4E2)')' Old Total Forces:',F(1,1,1:2)
         write(999,*) 'Fnear:',Fnear(1:5)
         write(999,*) 'Fwash:',Fwash(1:5)
         write(999,*) 'Ffric:',Ffric(1:5)
         write(999,*) 'Ffric:',Ffric(1:5)
         !write(999,*) 'FG:',FG
      ENDIF

   END FUNCTION cFKa

!
! Background Force
!
!
   FUNCTION bgForce(xSub) RESULT(Fbg)
      REAL(KIND=BR), INTENT(IN), DIMENSION(NSim) :: xSub
      REAL(KIND=BR), DIMENSION(NSim) :: Fbg
      REAL(KIND=BR), SAVE :: bgFA

      Fbg = 0.0_BR

      IF (tstep .eq. 1) THEN
         SELECT CASE(BGTYPE)
            CASE (WHOLECHANNEL5)
               bgFA=bgH(run)*6.0_BR*(two/L)**6
            CASE (ENDCHANNEL2)
               bgFA=bgH(run)*3.0_BR*(two/L)**3
         END SELECT
      ENDIF

      SELECT CASE (BGTYPE)
         CASE (WHOLECHANNEL5)
            Fbg=-bgFA*(xSub-L*onehalf)**5
         CASE (ENDCHANNEL2)
            ! only suitable for infinite channel case
            if (xSub(1) .lt. 0.0_BR) THEN
               Fbg(1) = bgFA * xSub(1)**2
            END IF
            IF (xSub(N) .gt. L) THEN
               Fbg(N) = -bgFA * (xSub(N)-L)**2
               !write(999,*) tstep,' Fbg:',Fbg, 'FricN:',frictionF(v)
            END IF
            !write(999,*) Fbg
            !write(999,*) L
            !write(999,*) xSub
            !stop
      END SELECT
   END FUNCTION bgForce

!
! Substrate Frictional Force
!
!
   FUNCTION frictionF(vSub)
      REAL(KIND=BR), INTENT(IN), DIMENSION(NSim) :: vSub
      REAL(KIND=BR), DIMENSION(N) :: frictionF
      SELECT CASE (INTERACTIONMODEL)
         CASE (RIGIDBARS)
            frictionF=sum(-eta(run)*vSub)/REAL(N,KIND=BR)
         CASE DEFAULT
            frictionF=-eta(run)*M(run)*vSub
      END SELECT
   END FUNCTION frictionF
!
!
!
!
   FUNCTION particleParticle(rdiffSub,ldiffSub)
      REAL(KIND=BR), DIMENSION(:), INTENT(IN) :: rdiffSub, ldiffSub
      REAL(KIND=BR), DIMENSION(size(rdiffSub)) :: particleParticle

      SELECT CASE (INTERACTIONMODEL)
         CASE (SPRINGMODEL)
            particleParticle=k(run)*(rdiffSub-ldiffSub)
         CASE (NONLINEAR)
            particleParticle=k(run)*(rdiffSub)**3-k(run)*(ldiffSub)**3
         CASE (RIGIDBARS)
            write(999,*) 'never should be here. Don''t have particle interactions with rigid connections.'
            STOP
         CASE DEFAULT
            write(999,*) 'Error: particle-particle interaction model Not Defined'
            STOP
      END SELECT

   END FUNCTION particleParticle
!
!
!
!
   SUBROUTINE hardCoreCorrection(rdiff,ldiff)
      REAL(KIND=BR), INTENT(IN), DIMENSION(N) :: rdiff,ldiff
      REAL(KIND=8), SAVE :: runningTime=0.0
      REAL(KIND=8) :: oldv
      INTEGER :: i

      IF (TIMESUBS) THEN
         CALL startTimer()
      END IF

      IF (HARDCOLLISIONS) then
         !WHERE(rdiff<hardcore)
         !   v=-abs(v)
         !end WHERE
         !WHERE(ldiff<hardcore)
         !   v=abs(v)
         !end WHERE
         DO i=1,N-1
            if (rdiff(i)<hardcore) then
               IF (COLLISIONREPORTING) then
                write(999,*) 'hard collision at time step',tstep
               END IF
               oldv=vx(i)
               vx(i)=vx(i+1)
               vx(i+1)=oldv
            end if
         END DO
         if (CHAINBC .ne. CATERPILLARCHAIN) then
            if (rdiff(N)<hardcore) then
               IF (COLLISIONREPORTING) then
                  write(999,*) 'hard collision at time step',tstep
               END IF
               oldv=vx(N)
               vx(N)=vx(1)
               vx(1)=oldv
            end if
         end if

      END IF

      IF (TIMESUBS) THEN
         runningTime=runningTime+elapsedTime()
         IF (mod(tstep,STATUSWAIT)==0) write(999,*) 'runningTime hardCoreCorrection:',runningTime
      END IF
   END SUBROUTINE hardCoreCorrection
!
!
!
!
   SUBROUTINE doubleCheckCorrelations()
      REAL(KIND=BR), DIMENSION(N) :: avgV,selfC
      REAL(KIND=BR), DIMENSION(N), SAVE :: sumV=zero, runSelfC=zero
      REAL(KIND=BR), SAVE :: runEndC=zero
      REAL(KIND=BR) :: endC, crossC
      sumV=sumV+vx
      IF (mod(tstep,CorWindow)==0) THEN
         avgV=sumV/REAL(corWindow,BR)
         selfC=avgV*avgV*t**2
         endC=avgV(1)*avgV(N)*t**2
         runSelfC=runSelfC+selfC
         runEndC=runEndC+endC
         crossC=runEndC/sqrt(runSelfC(1)*runSelfC(N))
         sumV=zero
      END IF
   
      IF (mod(tstep,STATUSWAIT) .eq. 0) THEN
         write(*,'(A,10F8.4)')' Cross Correlation Check:',crossC
      END IF

   END SUBROUTINE doubleCheckCorrelations


   SUBROUTINE particleCorrelations()
      REAL(KIND=BR), SAVE, DIMENSION(N) :: xSave,xTotalSave
      REAL(KIND=BR), SAVE, DIMENSION(N) :: selfCorrelations=zero
      REAL(KIND=BR), DIMENSION(N) :: avgV
      INTEGER, SAVE :: callNumber = 1
      REAL(KIND=BR), SAVE :: endCorrelation=zero
      REAL(KIND=8), SAVE :: runningTime=zero

      IF (TIMESUBS) THEN
         CALL startTimer()
      END IF

      IF (tstep .eq. CorWindow) THEN
         CALL RANDOM_NUMBER(xSave)
         xSave=x-0.001*xSave+0.002
         endCorrelation=zero
         crossCorrelation=zero
         selfCorrelations=zero
         runningTime=zero
         callNumber=1
         xTotalSave=xSave
      END IF

      avgV=x-xSave

      !
      ! If particles sometimes move more than L/2 in
      ! the timing window, then the below block won't work right.
      !
      IF (COORDBC==CHANNELCOORD) THEN
         WHERE (avgV>(1.0*L/5.0))
            avgV=xTotal-xTotalSave
         ELSEWHERE (avgV<(-1.0*L/5.0))
            avgV=xTotal-xTotalSave
         END WHERE
      END IF

      selfCorrelations=((callNumber-1)*selfCorrelations+avgV*avgV/REAL(CorWindow**1,BR))/callNumber
      endCorrelation=((callNumber-1)*endCorrelation+avgV(1)*avgV(N)/REAL(CorWindow**1,BR))/callNumber
      !selfCorrelations=selfCorrelations+avgV*avgV
      !endCorrelation=endCorrelation+avgV(1)*avgV(N)

      IF (mod(tstep,STATUSWAIT) .eq. 0) THEN
         crossCorrelation=endCorrelation/SQRT(selfCorrelations(1)*selfCorrelations(N))
         write(*,'(A,10F8.4)')' Cross Correlation:',crossCorrelation
        ! write(999,*) ''
        !write(*,'(A,10F8.4)')'Typical avgV:     ',avgV
        !write(*,'(A,10F8.4)')'Typical endCor:   ',endCorrelation
        !write(*,'(A,10F8.4)')'Typical selfCors: ',selfCorrelations
      END IF

      xSave = x
      xTotalSave = xTotal
      callNumber=callNumber+1

      IF (TIMESUBS) THEN
         runningTime=runningTime+elapsedTime()
         IF (mod(tstep,STATUSWAIT) .eq. 0) write(999,*) 'runningTime Correlations:',runningTime
      END IF
   END SUBROUTINE particleCorrelations
!
!
!
!
   SUBROUTINE flowTrack()
      ! This could be made more efficient.
      ! If rigidbars, then there can be only one flow/return event per time step
      ! If not rigid bars, then there can only be two
      ! This should be easier (See resetCoordAndFlowTrack())
      REAL(KIND=BR), SAVE, DIMENSION(N) :: entryPt, exitPt
      INTEGER :: i
      REAL(KIND=8), SAVE :: runningTime=zero
      REAL(KIND=8), SAVE :: lastx=zero, particleHop
      INTEGER, SAVE :: lasttime=one
      INTEGER, SAVE, DIMENSION(N) :: entryTime=1

      IF (TIMESUBS) THEN
         CALL startTimer()
      END IF

      ! INF COORD
      IF (tstep .eq. 1) THEN
         WHERE (label .eq. 'L') 
            entryPt=zero
            exitPt=L
         ELSEWHERE
            entryPt=L
            exitPt=zero
         END WHERE
      END IF

      IF (TIMEPARTICLES) THEN
         particleHop=x(3)-lastx
         if (abs(particleHop) .gt. L/2) then
            particleHop=L-abs(particleHop)
         end if
         if (abs(particleHop) .gt. a) then
            IF (WRITETIMEDAT) write(9,*) lasttime,tstep
            lastx=x(3)
            lasttime=tstep
         end if
      END IF

      DO i=1,N
         IF (label(i) .eq. 'L') THEN

            IF  (x(i) > exitPt(i) ) THEN
               IF (TIMEPARTICLES .and. WRITEEVENTS) CALL writeEvent(1,1,i)

               Q=Q+1
               IF (COORDBC .eq. INFINITECOORD) THEN
             entryPt(i)=exitPt(i)
             exitPt(i)=entryPt(i)+L
               END IF
               IF (TIMEPARTICLES) THEN
             IF (WRITETIMEDAT) write(7,*) entryTime(i),tstep,1
             IF (REPLACEMENTMETHOD .lt. ACTIVATIONENERGY) THEN
                entryTime(i) = tstep
             END IF
               END IF

            ELSEIF  (x(i) < entryPt(i)) THEN
               IF (TIMEPARTICLES .and. WRITEEVENTS) CALL writeEvent(0,-1,i)

               R=R+1
               label(i)='R'
               IF (COORDBC .eq. INFINITECOORD) THEN
             exitPt(i)=entryPt(i)-L
               ELSE
             exitPt(i)=zero
             entryPt(i)=L
               END IF
               IF (TIMEPARTICLES) THEN
             IF (WRITETIMEDAT) write(8,*) entryTime(i),tstep,1
             IF (REPLACEMENTMETHOD .lt. ACTIVATIONENERGY) THEN
                entryTime(i) = tstep
             END IF
               END IF

            ELSE
               ! Do nothing.  The particle did not exit the channel
            END IF

         ELSE 

            IF  (x(i) < exitPt(i) ) THEN
               IF (TIMEPARTICLES .and. WRITEEVENTS) CALL writeEvent(1,-1,i)

               Q=Q+1
               IF (COORDBC .eq. INFINITECOORD) THEN
             entryPt(i)=exitPt(i)
             exitPt(i)=entryPt(i)-L
               END IF
               IF (TIMEPARTICLES) THEN
             IF (WRITETIMEDAT) write(7,*) entryTime(i),tstep,-1
             IF (REPLACEMENTMETHOD .lt. ACTIVATIONENERGY) THEN
                entryTime(i) = tstep
             END IF
               END IF

            ELSEIF  (x(i) > entryPt(i)) THEN
               IF (TIMEPARTICLES .and. WRITEEVENTS) CALL writeEvent(0,1,i)

               R=R+1
               label(i)='L'
               IF (COORDBC .eq. INFINITECOORD) THEN
             exitPt(i)=entryPt(i)+L
               ELSE
             entryPt(i)=zero
             exitPt(i)=L
               END IF
               IF (TIMEPARTICLES) THEN
             IF (WRITETIMEDAT) write(8,*) entryTime(i),tstep,-1
             IF (REPLACEMENTMETHOD .lt. ACTIVATIONENERGY) THEN
                entryTime(i) = tstep
             END IF
               END IF

            ELSE
               ! Do nothing.  The particle did not exit the channel
            END IF

         END IF
      END DO
      
      IF (TIMESUBS) THEN
         runningTime=runningTime+elapsedTime()
         IF (mod(tstep,STATUSWAIT) .eq. 0) write(999,*) 'runningTime FlowTrack:',runningTime
      END IF

   END SUBROUTINE flowTrack

   SUBROUTINE writeEvent(eventType,exitSide,particle)
      INTEGER, INTENT(IN) :: eventType,exitSide,particle
      IF (REPLACEMENTMETHOD .lt. ACTIVATIONENERGY) THEN
         write(1010,*) tstep,eventType,exitSide,particle
      END IF
   END SUBROUTINE


!
! Particle Separation
! Distances
!
   SUBROUTINE upDown(yin,rdiffSub,ldiffSub)
      !The space to the t0p of each particle
      REAL(KIND=BR), INTENT(OUT), DIMENSION(N) :: rdiffSub,ldiffSub
      REAL(KIND=BR), INTENT(IN), DIMENSION(N) :: yin
      INTEGER :: i
      REAL(KIND=8), SAVE :: runningTime=0.0
      REAL(KIND=BR), SAVE :: halfSizeY = Ly/2.0_BR


      ! Cant's assign yin, because the calling function has the parameter as an
      ! input parameter, so it cant be INTENT(INOUT) here
      !WHERE (yin > Ly)
      !   yin = yin - Ly
      !ELSEWHERE (yin < 0)
      !   yin = yin + Ly
      !END WHERE

      rdiffSub(1:Nsim-1)=yin(2:)-yin(1:Nsim-1)
      rdiffSub(Nsim)=yin(1)-yin(Nsim)

      !wiki example: if (abs(dx) > x_size * 0.5) dx = dx - sign(x_size, dx)
      WHERE (rdiffSub  .gt.  halfSizeY) 
         rdiffSub=rdiffSub - Ly
      ELSEWHERE (rdiffSub  .lt. -halfSizeY) 
         rdiffSub=rdiffSub + Ly
      END WHERE

      ldiffSub=CSHIFT(rdiffSub,SHIFT=-1)

   END SUBROUTINE upDown

   SUBROUTINE rightLeft(xin,rdiffSub,ldiffSub)
      !The space to the right of each particle
      REAL(KIND=BR), INTENT(OUT), DIMENSION(N) :: rdiffSub,ldiffSub
      REAL(KIND=BR), INTENT(IN), DIMENSION(N) :: xin
      !LOGICAL, INTENT(IN) :: REALITY
      !LOGICAL, INTENT(OUT) :: blowthrough
      INTEGER :: i
      REAL(KIND=8), SAVE :: runningTime=0.0

      IF (TIMESUBS) THEN
         CALL startTimer()
      END IF
      !
      ! rdiff the particles except particle N
      ! The short, but slow (in g95 at least) way to do all values
      ! including N
      ! rdiff=CSHIFT(x,SHIFT=1)-x
      !
      DO i=1,N-1
         rdiffSub(i)=xin(i+1)-xin(i)
      END DO
      !
      ! Now do BC dependent part of rdiff
      ! I think I was being to clever.  Problems developed with a specific series of wrap pts.
      ! like if the wrap pt went from N to N-1 to N, the wrap pt wasn't being reset to N for the cat chain.
      ! so the below is commented out except for the one line under INFINITECHAIN
      SELECT CASE (CHAINBC)

         CASE (INFINITECHAIN)
            rdiffSub(N)=xin(1)-xin(N)

         CASE (CATERPILLARCHAIN)
            rdiffSub(N)=a

      END SELECT


      !
      ! Fix negative rdiff at coordinate BC for finite channel
      ! An inefficient but short and understandable way to do it is...
      ! WHERE(rdiffSub<0.0) rdiffSub=rdiffSub+L
      ! but we do it faster ...
      !
      IF (COORDBC==CHANNELCOORD) THEN
         CALL findWrapPt(rdiffSub)
         CALL fixWrapPtDiff(rdiffSub)
      ELSE  !INFINITECOORDS
         SELECT CASE (CHAINBC)
            CASE (INFINITECHAIN)
               rdiffSub(N)=rdiffSub(N)+L
            CASE (CATERPILLARCHAIN)
               rdiffSub(N)=a
         END SELECT
      END IF

      ldiffSub=CSHIFT(rdiffSub,SHIFT=-1)

      !SELECT CASE (TESTCASE)
      !	 CASE (IMPINGER)
      !	    ldiffSub(1) = x(1)-impingerX
      !END SELECT

      !
      ! Also, very slow in g95 is
      !    rdiff = (/ (x(i+1)-x(i), i=1,N) /)
      !    rdiff(N)=x(1)+L-x(N)
      ! Untested is 
      ! rdiff(1:N-1) = x(2:N)-x(1:N-1)
      !
      IF (TIMESUBS) THEN
         runningTime=runningTime+elapsedTime()
         IF (mod(tstep,STATUSWAIT) .eq. 0) write(999,*) 'runningTime rightLeft & blowThrough:',runningTime
      END IF
   END SUBROUTINE rightLeft
!
! Blowthrough
! Detection
!
   FUNCTION blowthroughDetector(rdiffSub)
      ! CALL From rightLeft
      REAL(KIND=BR), INTENT(IN), DIMENSION(N) :: rdiffSub
      INTEGER, SAVE :: expectedNegatives=0
      LOGICAL :: blowthroughDetector
    
      IF (tstep .eq. 1) THEN
         IF (CHAINBC==CATERPILLARCHAIN .AND. COORDBC==INFINITECOORD) expectedNegatives=0
      END IF
      !
      ! Checking for blowthrough would be more efficient when determining 
      ! rdiff (I think, maybe not)
      blowthroughDetector=.false.
      IF (count(rdiffSub<0)>expectedNegatives) then
         !write(999,*) x
         write(999,*) 'Blowthrough occured at step: ', tstep
         !write(999,*) rdiffSub
         blowthroughDetector=.true.
      END if

   END FUNCTION blowthroughDetector
!   
! Particle N->1
! Difference
!
   SUBROUTINE fixWrapPtDiff(rdiffSub)
      REAL(KIND=BR), INTENT(INOUT), DIMENSION(N) :: rdiffSub
      SELECT CASE (CHAINBC)
         CASE (CATERPILLARCHAIN)
            ! what about lastwrap = 1?
            rdiffSub(N)=a
            IF (lastwrap .ne. N) THEN
               rdiffSub(lastwrap)=rdiffSub(lastwrap)+L
            END IF
         CASE (INFINITECHAIN)
            rdiffSub(lastwrap)=rdiffSub(lastwrap)+L
      END SELECT
   END SUBROUTINE fixWrapPtDiff
!
!
!
!
   SUBROUTINE findWrapPt(rdiffSub)
      ! CALL from rightLeft
      ! At the periodic BC, the rdiff will be negative.
      ! Need to add L to rdiff for this array element
      REAL(KIND=BR), INTENT(INOUT), DIMENSION(N) :: rdiffSub
      INTEGER :: leftward, rightward
      INTEGER :: i
      INTEGER, DIMENSION(1) :: wrapInd

      ! IF I AM IN THIS SUB, WE SHOLD NOT HAVE INFINITECOORD 's

      ! shouldn't wrap depend on (x(n+1)<x(n)) is this easier?
      !IF (CHAINBC .eq. CATERPILLARCHAIN) THEN
      !	 IF (X(N)>X(1)) !lastwrap=N
      !END IF
      
      ! I need this because no rdiff is degative when caterpillarchain is on
      ! and the chain is within the channel
      ! At high enough Temps, each wrapt pt is equally likely, this doesn't speed anything up.
      ! I am commenting it out
      !IF (CHAINBC .ne. CATERPILLARCHAIN) THEN
!	 IF (x(N) > x(1)) THEN
!	    lastwrap=N
!	    if (tstep .gt. 2515548) then
!	    write(999,*) 'findWraprdiffs:',rdiffSub
!	    write(999,*) 'findWrapxs:',x
!	    write(999,*) 'lastwrap:',lastwrap
!	    end if
!	    RETURN
!	 END IF
!      END IF

      IF (N>=100) THEN
         ! This way is slightly faster when N > ~10 or so.
         leftward=lastwrap-1
         rightward=lastwrap+1
         ! don't check these unless we're sure they are the only options
         !SELECT CASE (lastwrap)
         !   CASE (1)
         !      leftward=N
         !   CASE (N)
         !      rightward=1
         !END SELECT
         ! what about for caterpillar chain when there is no wrap pt?
         IF (rdiffSub(lastwrap)<0.0) THEN
         ELSEIF (leftward .ne. 0 .and. rdiffSub(leftward)<0.0) THEN
            lastwrap=leftward
         ELSEIF (rightward .ne. (N+1) .and. rdiffSub(rightward)<0.0) THEN
            lastwrap=rightward
         ELSE
            ! below line seemed unnecessary
            ! lastwrap=N
            write(999,*) "uh oh, a wrap plus a blowthrough or 2 particles wrapped in one time step?"
            write(999,*) "searching for max pt."
            !write(999,*) 'x''es:',x
            !write(999,*) 'rdiffs:',rdiffSub 
            wrapInd=maxloc(x)
            lastwrap=wrapInd(1)
            if (rdiffSub(1) .lt. zero .and. lastwrap .ne. 1) then
               lastwrap=1
               CALL fixWrapPtDiff(rdiffSub)
               lastwrap=wrapInd(1)
            end if
            write(999,*) 'assigned lastwrap:',lastwrap
         END IF

      ELSE
      !
      ! slightly slower for many particles is to dumbly cycle 
      ! from particle index 1 through the particles sequentially
      ! until finding the one where rdiff is negative, then exit the loop.
      ! THIS  REQUIRES lastwrap=N to have ALREADY BEEN CHECKED (DOES IT?).
         DO i=1,N
            IF (rdiffSub(i)<0.0) THEN
               lastwrap=i
               ! write(999,*) tstep,":",i
               EXIT
            END IF
         END DO
         IF (lastwrap .eq. 1) THEN
            ! check for the tricky double wrap scenario
            IF (rdiffSub(N-1) .lt. zero) THEN
               CALL fixWrapPtDiff(rdiffSub)
               lastwrap=N-1
            END IF
         END IF

      END IF
   END SUBROUTINE findWrapPt
!
!
!
!
   SUBROUTINE startTimer()
      REAL(KIND=8) :: startTime
      CALL date_and_time(VALUES=timeArray)
      startTime=3600*timeArray(5)+60*timeArray(6)+timeArray(7)+0.001*timeArray(8)
      startTime=timeStack(startTime)
   END SUBROUTINE startTimer
   REAL(KIND=8) FUNCTION elapsedTime()
      REAL(KIND=8) :: endTime
      CALL date_and_time(VALUES=timeArray)
      endTime=3600*timeArray(5)+60*timeArray(6)+timeArray(7)+0.001*timeArray(8)
      elapsedTime = endTime-timeStack()
   END FUNCTION elapsedTime    
   REAL(KIND=8) FUNCTION timeStack(starting)
      INTEGER, SAVE :: topStack=0
      REAL(KIND=8), SAVE, DIMENSION(50) :: timeArray
      REAL(KIND=8), INTENT(IN), OPTIONAL :: starting
      IF (PRESENT(starting)) THEN
         topStack=topStack+1
         timeArray(topStack) = starting
         timeStack=starting
      ELSE
         timeStack=timeArray(topStack)
         topStack=topStack-1
      END IF
   END FUNCTION timeStack
!
!
!
!
   SUBROUTINE writeState()
      REAL(KIND=8), SAVE :: runningTime=0.0

      IF (TIMESUBS) THEN
         CALL startTimer()
      END IF
      !write(999,*) tstep,MAXVAL(x),MAXVAL(y)
      IF (WRITEU) CALL Energy(tstep)

      IF (ASCIIFORMAT) THEN
         IF (WRITEX) write(1,'(F12.4,'//NinCharForm//formatReal//')') tstep*dt,x
         IF (WRITEV) write(2,'(F12.4,'//NinCharForm//formatReal//')') tstep*dt,vx
         IF (WRITEU) write(33,'(F12.4,'//NinCharForm//formatReal//')') tstep*dt,KEx,PEx
         IF (D2) THEN
            IF (WRITEX) write(1111,'(F12.4,'//NinCharForm//formatReal//')') tstep*dt,y
            IF (WRITEV) write(2222,'(F12.4,'//NinCharForm//formatReal//')') tstep*dt,vy
            IF (WRITEU) write(3333,'(F12.4,'//NinCharForm//formatReal//')') tstep*dt,KEy,PEy
         ENDIF
      ELSE
         IF (WRITEX) write(1) REAL(tstep*dt,KIND=SMREAL),REAL(x,KIND=SMREAL)
         IF (WRITEV) write(2) REAL(tstep*dt,KIND=SMREAL),REAL(vx,KIND=SMREAL)
         IF (WRITEU) write(33) REAL(tstep*dt,KIND=SMREAL),REAL(KEx,KIND=SMREAL),REAL(PEx,KIND=SMREAL)
         IF (D2) THEN
            IF (WRITEX) write(1111) REAL(tstep*dt,KIND=SMREAL),REAL(y,KIND=SMREAL)
            IF (WRITEV) write(2222) REAL(tstep*dt,KIND=SMREAL),REAL(vy,KIND=SMREAL)
            IF (WRITEU) write(3333) REAL(tstep*dt,KIND=SMREAL),REAL(KEy,KIND=SMREAL),REAL(PEy,KIND=SMREAL)
         ENDIF
      END IF
      !write(999,*) vx

      IF (TIMESUBS) THEN
         runningTime=runningTime+elapsedTime()
         IF (mod(tstep,STATUSWAIT)==0) write(999,*) 'runningTime WriteState:',runningTime
      END IF
  END SUBROUTINE writeState
!
!
!
!
   SUBROUTINE printStartup()
      CHARACTER(LEN=1) :: runningSize
      write(999,*) ' '
      write(999,*) ' '
      
      write(runningSize,'(I1)') SIZE(running)
      write(*,'(A,I4)')'Run #: ',run
      write(*,'(A,'//NinCharForm//formatReal//')')'Starting Positions:',x
      write(999,*) 'starting xs from within printStartup():',x(1:9)
      write(999,*) 'starting vxs from within printStartup():',vx(1:9)
      !IF (WRITEV) write(2,'(I8,'//NinCharForm//formatReal//')') tstep,v
      write(*,'(A,'//runningSize//formatReal//')')'Running with constants (k,h,eta,T,bgH,G):', running
      write(*,'(A,'//formatReal//')')'hardcore radius:',hardcore
      write(*,'(A,'//NinCharForm//'A)')'labels:',label
      write(*,'(A,I10)')'status update every (steps):',STATUSWAIT
      write(*,'(A,F12.0)')'Max Write Size:',writeSize
      write(*,'(A,I10)')'corWindow:',corWindow
      write(*,'(A,I10)')'Steps for each run:',steps
      write(999,*) ' '
      IF (writeSize>5e8 .AND. (WRITEX .OR. WRITEV) .AND. run .eq. 1) THEN
         write(999,*) 'Write size too large, continue [Y,n]?'
         read*,ans
         IF (ans .ne. 'Y') STOP
      END IF
      SELECT CASE (ICS)
         CASE (IMPINGER)
            write(*,'(A)') 'RUNNING THE IMPINGER TEST CASE'
      END SELECT
   END SUBROUTINE printStartup
!
!
!
!
   SUBROUTINE rigidSanityCheck()
      IF (a<(2*pi*L/REAL(N,KIND=BR))) THEN
         write(999,*) 'Model setup does not make sense.  Rigid bars are different lengths. ' 
         write(999,*) 'Might as well use CATERPILLARCHAIN'
         write(999,*) 'Continue [Y,n]?'
         read*,ans
         IF (ans .ne. 'Y') STOP
      END IF
   END SUBROUTINE rigidSanityCheck
!
!
!
!
   SUBROUTINE printConclusions()
      REAL :: kbar_SLY_Roxin_1, kbar_SLY_Roxin_2, kbar_orighere, lo_FvdM
      REAL :: width_Braun_1, width_Braun_2, hbar_1, hbar_2, hK, Wd_FvdM
      write(999,*) ' '
      !write(999,*) 'Q:',Q
      !write(999,*) 'R:',R
      !write(999,*) 'Q/tau:',Q/T
      !write(999,*) 'R/tau:',R/T
      !write(999,*) 'Q/nanosec:',Q/T/0.513e-12*1e-9
      !write(999,*) 'R/nanosec:',R/T/0.513e-12*1e-9
      !write(11,'(2F8.4)') Q/T/0.513e-12*1e-9,crossCorrelation
      write(999,*) 'Sim ran for about: ',T*0.5e-12/1e-9,' nanoseconds'
      !write(999,*) 'Vx',sum(vx**2)
      !write(999,*) 'Vy',sum(vy**2)
      kbar_SLY_Roxin_1=k(run)/h(run)*(WL/two/pi)**2
      kbar_SLY_Roxin_2=k(run)/h(run)*(WL/WLperN/two/pi)**2
      write(999,*) 'nondimensional g:', kbar_SLY_Roxin_2
      kbar_orighere=k(run)/h(run)*(WL/WLperN/2.0/pi)**2/h(run)
      lo_FvdM=sqrt(k(run)*a*a/4/h(run))
      width_Braun_1=sqrt(kbar_SLY_Roxin_1)
      width_Braun_2=sqrt(kbar_SLY_Roxin_2)
      hbar_1=1/kbar_SLY_Roxin_1
      hbar_2=1/kbar_SLY_Roxin_2
      hK=h(run)/kb
      Wd_FvdM=8*hK*lo_FvdM/pi
      write(3854,*) 'kbar1, kbar2, kbarHere, lo, w_braun1, w_braun2, hbar_1, hbar2, hK, Wd '
      write(3854,*) kbar_SLY_Roxin_1, kbar_SLY_Roxin_2, kbar_orighere, lo_FvdM,&
        width_Braun_1, width_Braun_2, hbar_1, hbar_2, hK, Wd_FvdM
      close(3854)
   END SUBROUTINE printConclusions

end program cFK
    
  !REAL(KIND=BIGREAL) :: x_bigreal
  !REAL :: x_real
  !REAL(KIND=BR) :: x_10


  !x_bigreal=1.111111111111111111111111111111
  !x_real=1.111111111111111111111111111111
  !x_10=1.111111111111111111111111111111

  !write(999,*) x_bigreal
  !write(999,*) x_real
  !write(999,*) x_10

!
! Make Coords
! Less Than L
!
!   SUBROUTINE resetCoordAndFlowTrack()
!      ! 
!      ! This is for systems in a finite channel, ie with COORDBC=CHANNELCOORD
!      !
!      INTEGER :: i
!      !x=modulo(x,L)
!      DO i=1,N
!         IF (x(i)>exitPt(i)) THEN
!
!            IF (label(i) .eq. 'L') THEN
!               Q=Q+1
!
!            ELSE 
!               R=R+1
!               label(i)='L'
!
!            END IF
!
!         ELSE IF (x(i)<entryPt(i)) THEN
!
!            IF (label(i) .eq. 'L') THEN
!               R=R+1
!               label(i)='R'
!
!            ELSE
!               Q=Q+1
!
!            END IF
!
!         END IF
!      END DO
!   END SUBROUTINE resetCoordAndFlowTrack
