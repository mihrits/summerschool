! Main solver routines for heat equation solver
module core

contains

  ! Compute one time step of temperature evolution
  ! Arguments:
  !   curr (type(field)): current temperature values
  !   prev (type(field)): values from previous time step
  !   a (real(dp)): update equation constant
  !   dt (real(dp)): time step value
  subroutine evolve(curr, prev, a, dt)

    use heat
    implicit none

    type(field), intent(inout) :: curr, prev
    !real(dp), allocatable :: temp(:,:)
    real(dp) :: a, dt
    integer :: i, j, nx, ny

    nx = curr%nx
    ny = curr%ny

    ! Allocation of temporary array for periodic borderconditions
    !allocate(temp(ny+2, nx+2))
    !temp = 0
    !temp(2:ny-1, 2:nx-1) = prev%data
    !temp(1, 2:nx-1) = prev%data(ny,:)
    !temp(ny+1, 2:nx-1) = prev%data(1,:)
    !temp(2:ny-1, 1) = prev%data(:, nx)
    !temp(2:ny-1, nx+1) = prev%data(:, 1)

    ! Calculating the next step of heat equation
    do i = 1, ny
      do j = 1, nx
        curr%data(i, j) = prev%data(i, j) + dt * a &
                        * ((prev%data(i-1,j) - 2*prev%data(i,j) + prev%data(i+1,j))/curr%dy**2 &
                        +  (prev%data(i,j-1) - 2*prev%data(i,j) + prev%data(i,j+1))/curr%dx**2)
      end do
    end do
  end subroutine evolve

end module core
