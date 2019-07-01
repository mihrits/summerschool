program vectorsum
  implicit none
  integer, parameter :: rk = kind(1d0)
  integer, parameter :: ik = selected_int_kind(9)
  integer, parameter :: nx = 102400

  real(kind=rk), dimension(nx) :: vecA, vecB, vecC
  real(kind=rk)    :: sum
  integer(kind=ik) :: i
  real(rk) :: t1, t2

  ! Initialization of vectors
  do i = 1, nx
     vecA(i) = 1.0_rk/(real(nx - i + 1, kind=rk))
     vecB(i) = vecA(i)**2
  end do

  call cpu_time(t1)

  ! TODO:
  !   Implement here the parallelized version of vector addition,
  !   vecC = vecA + vecB
  !$omp parallel do schedule(dynamic)
  do i = 1, nx
    vecC(i) = vecA(i) + vecB(i)
  end do
  !$omp end parallel do

  call cpu_time(t2)
  ! Compute the check value
  write(*,*) 'Reduction sum: ', sum(vecC), '. Calculation time: ', t2-t1

end program vectorsum
