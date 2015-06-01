program main
  implicit none

  integer, parameter :: i = 3, j = 4
  real :: x = 3.14
  CHARACTER(len=255) :: cmd
  CHARACTER(len=32) :: arg

      print*, real(i)/real(j)
      CALL get_command(cmd)
      CALL getarg(1, arg)
      WRITE (*,*) TRIM(cmd)
      WRITE (*,*) TRIM(arg)
      WRITE (*,*) LEN(TRIM(arg))

end program main
