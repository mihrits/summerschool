program vectorsum
  use iso_fortran_env, only: int64
  implicit none
  integer, parameter :: ik = int64
  integer(kind=ik), parameter :: nx = 102400_ik

  integer(kind=ik), dimension(nx) :: vecA
  integer(kind=ik) :: sum, psum, sumex
  integer(kind=ik) :: i

  ! Initialization of vector
  do i = 1, nx
     vecA(i) = i
  end do

  sumex = nx*(nx+1_ik)/2_ik
  write(*,'(A40,I15)') 'Arithmetic sum formula (exact):                  ', sumex

  sum = 0
  ! TODO: Parallelize the computation
  !$omp parallel do schedule(dynamic) private(i) reduction(+:sum)
  do i = 1, nx
     sum = sum + vecA(i)
  end do
  !$omp end parallel do
  write(*,'(A40,I15)') 'Sum:                                                        ', sum
end program vectorsum
