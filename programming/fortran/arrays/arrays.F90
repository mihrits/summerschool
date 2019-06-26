program arrays
  implicit none
  ! TODO: Define the array A
  real, allocatable :: A(:,:)
  real :: dx, dy
  integer :: nx, ny, i, j, alloc_stat

  write (*,*) 'Give number of rows and columns for matrix A:'
  read (*,*) nx, ny
  dx = 1.0/real(nx)
  dy = 1.0/real(ny)

  ! TODO: allocate the array A
  allocate (A(ny,nx), stat = alloc_stat)
  if (alloc_stat /= 0) stop

  ! TODO: initalize the array A
  do j = 1, ny
    do i = 1, nx
      A(j,i) = (j*dy)**2 + (i*dx)**2
    end do
  end do

  ! TODO: Print out the array
  do j = 1, ny
    write(*,'(12F10.6)') A(j,:)
  end do

end program arrays
