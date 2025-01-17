program basic
  use mpi_f08
  use iso_fortran_env, only : REAL64

  implicit none
  integer, parameter :: msgsize = 10000000
  integer :: rc, myid, ntasks, i
  integer :: message(msgsize)
  integer :: receiveBuffer(msgsize)
  type(mpi_status) :: status(2)

  real(REAL64) :: t0, t1

  integer :: source, destination
  integer :: count
  type(mpi_request) :: requests(2)

  call mpi_init(rc)
  call mpi_comm_rank(MPI_COMM_WORLD, myid, rc)
  call mpi_comm_size(MPI_COMM_WORLD, ntasks, rc)

  message = myid

  ! Set source and destination ranks
  if (myid < ntasks-1) then
     destination = myid + 1
  else
     destination = MPI_PROC_NULL
  end if
  if (myid > 0) then
     source = myid - 1
  else
     source = MPI_PROC_NULL
  end if

  ! Start measuring the time spent in communication
  call mpi_barrier(mpi_comm_world, rc)
  t0 = mpi_wtime()


  requests(1) = mpi_request_null
  requests(2) = mpi_request_null

  ! Bonus exercise to use presistent communication

  call mpi_recv_init(receiveBuffer, msgsize, mpi_integer, source, myid, mpi_comm_world, requests(2), rc)
  call mpi_send_init(message, msgsize, mpi_integer, destination, myid+1, &
                     mpi_comm_world, requests(1), rc)
  do i = 1,100
    call mpi_start(requests(2), rc)
    call mpi_start(requests(1), rc)
    

    call mpi_wait(requests(1), mpi_status_ignore, rc)
    call mpi_wait(requests(2), mpi_status_ignore, rc)
  end do 

  call mpi_get_count(status(1), MPI_INTEGER, count, rc)
  write(*,'(A10,I3,A20,I8,A,I3,A,I3)') 'Sender: ', myid, &
       ' Sent elements: ', count,  &
       '. Tag: ', myid + 1, '. Receiver: ', destination



  call mpi_get_count(status(2), MPI_INTEGER, count, rc)
  write(*,'(A10,I3,A20,I8,A,I3,A,I3)') 'Receiver: ', myid, &
       'received elements: ', count, &
       '. Tag: ', status(2)%MPI_TAG, &
       '. Sender:   ', status(2)%MPI_SOURCE

  ! Finalize measuring the time and print it out
  t1 = mpi_wtime()
  call mpi_barrier(mpi_comm_world, rc)
  call flush(6)

  write(*, '(A20, I3, A, F6.3)') 'Time elapsed in rank', myid, ':', t1-t0

  call mpi_request_free(requests(1), rc)
  call mpi_request_free(requests(2), rc)




  call mpi_finalize(rc)
end program basic
