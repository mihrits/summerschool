! Main solver routines for heat equation solver
module core
  use heat

contains

  ! Exchange the boundary data between MPI tasks
  ! part 1: start communication
  subroutine exchange_init(field0, parallel)
    use mpi_f08

    implicit none

    type(field), intent(inout) :: field0
    type(parallel_data), intent(inout) :: parallel
    integer :: ierr
    ! TODO
    ! Receive from left and right
    call mpi_irecv(field0%data(:,0), field0%nx, mpi_double_precision, parallel%nleft, mpi_any_tag, &
                &  mpi_comm_world, parallel%requests(1), ierr)
    call mpi_irecv(field0%data(:,field0%ny+1), field0%nx, mpi_double_precision, &
                &  parallel%nright, mpi_any_tag, mpi_comm_world, parallel%requests(2), ierr)

    ! Send to left and right
    call mpi_isend(field0%data(:,1), field0%nx, mpi_double_precision, parallel%nleft, 1, &
                &  mpi_comm_world, parallel%requests(3), ierr)
    call mpi_isend(field0%data(:,field0%ny), field0%nx, mpi_double_precision, parallel%nright, 1, &
                &  mpi_comm_world, parallel%requests(4), ierr)

  end subroutine exchange_init

  ! Compute one time step of temperature evolution
  ! Arguments:
  !   curr (type(field)): current temperature values
  !   prev (type(field)): values from previous time step
  !   a (real(dp)): update equation constant
  !   dt (real(dp)): time step value
  ! Update only the border-independent part of the field
  subroutine evolve_interior(curr, prev, a, dt)
    implicit none
    type(field), intent(inout) :: curr, prev
    real(dp) :: a, dt
    integer :: i, j, nx, ny

    nx = curr%nx
    ny = curr%ny

    ! TODO
    do j = 2, ny-1
      do i = 1, nx
        curr%data(i, j) = prev%data(i, j) + a * dt * &
              & ((prev%data(i-1, j) - 2.0 * prev%data(i, j) + &
              &   prev%data(i+1, j)) / curr%dx**2 + &
              &  (prev%data(i, j-1) - 2.0 * prev%data(i, j) + &
              &   prev%data(i, j+1)) / curr%dy**2)
      end do
    end do

  end subroutine evolve_interior

  ! Finalize the non-blocking communication
  subroutine exchange_finalize(parallel)
    use mpi_f08
    implicit none
    type(parallel_data), intent(inout) :: parallel
    type(mpi_status) :: statuses(4)
    integer :: ierr

    ! TODO
    call mpi_waitall(4, parallel%requests, statuses, ierr)
  end subroutine exchange_finalize

  ! Compute one time step of temperature evolution
  ! Arguments:
  !   curr (type(field)): current temperature values
  !   prev (type(field)): values from previous time step
  !   a (real(dp)): update equation constant
  !   dt (real(dp)): time step value
  ! Update only the border-dependent part
  subroutine evolve_edges(curr, prev, a, dt)

    implicit none

    type(field), intent(inout) :: curr, prev
    real(dp) :: a, dt
    integer :: i, j, nx, ny

    nx = curr%nx
    ny = curr%ny

    ! TODO
    do j = 1, ny, ny-1
        do i = 1, nx
           curr%data(i, j) = prev%data(i, j) + a * dt * &
                & ((prev%data(i-1, j) - 2.0 * prev%data(i, j) + &
                &   prev%data(i+1, j)) / curr%dx**2 + &
                &  (prev%data(i, j-1) - 2.0 * prev%data(i, j) + &
                &   prev%data(i, j+1)) / curr%dy**2)
        end do
     end do

  end subroutine evolve_edges

end module core
