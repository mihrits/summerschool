program hello
  use omp_lib
  use mpi_f08
  implicit none
  integer :: my_id, tid, rc
  integer :: provided, required=MPI_THREAD_FUNNELED

  ! TODO: Initialize MPI with thread support.
  call mpi_init_thread(required, provided, rc)

  call mpi_comm_rank(mpi_comm_world, my_id, rc)

  write(*,*) 'Hello from the rank ', my_id, '!'
  !$omp parallel private(tid)
    

  ! TODO: Find out the MPI rank and thread ID of each thread and print
  !       out the results.
  tid  = omp_get_thread_num()
  write(*,*) 'Hello from process ', my_id, ', thread ', tid

  ! TODO: Investigate the provided thread support level.
  write(*,*) 'Support level is ', provided
  !$omp end parallel
  call MPI_Finalize(rc)
end program hello
