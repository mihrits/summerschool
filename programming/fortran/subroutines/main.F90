program subroutines
  use laplacian_mod
  implicit none
  ! TODO: define the arrays
  real, dimension(:,:), allocatable :: previous, current
  integer :: nx, ny

  write (*,*) 'Give number of rows and columns for matrix A:'
  read (*,*) nx, ny

  allocate(previous(ny,nx), current(ny-2,nx-2))

  ! initialize the array
  call initialize(previous)

  write(*,*) "Initial array:"
  call write_field(previous)

  ! compute the Laplacian
  call laplacian(current, previous)

  ! print the result array
  write(*,*) "Laplacian of the inital array:"
  call write_field(current)

end program subroutines
