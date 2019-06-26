program hello
  implicit none

  integer :: i, j
  real :: a, b
  complex :: c
  character(len=80) :: t

  a = 1.2
  b = 2.5
  i = 100
  j = 11
  c = (1.4,0.1)
  t = "Hello world from Fortran!"
  
  write (*,*) t

  write(*,*) a*i
  write(*,*) b**j
  write(*,*) c*a
end program hello
