program main
  implicit none

  integer, parameter :: i = 3, j = 4
  integer :: cr,cm
  real :: x = 3.14, rate
  CHARACTER(len=255) :: cmd
  CHARACTER(len=32) :: arg
  CALL system_clock(count_rate=cr)
  CALL system_clock(count_max=cm)
  rate = REAL(cr)
  WRITE(*,*) "system_clock rate ",rate

      print*, real(i)/real(j)
      CALL get_command(cmd)
      CALL getarg(1, arg)
      WRITE (*,*) TRIM(cmd)
      WRITE (*,*) TRIM(arg)
      WRITE (*,*) LEN(TRIM(arg))

end program main
