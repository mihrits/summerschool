program datatype1
  use mpi_f08
  implicit none

  integer, dimension(8,8) :: array
  integer :: rank, ierr
  ! declare variable for datatype
  type(mpi_datatype) :: rowtype, crazytype
  integer :: i, j

  call mpi_init(ierr)
  call mpi_comm_rank(MPI_COMM_WORLD, rank ,ierr)

  ! initialize arrays
  if (rank == 0) then
     do i=1,8
        do j=1,8
           array(i,j) = i*10 + j
        end do
     end do
  else
     array(:,:) = 0
  end if

  if (rank == 0) then
     write(*,*) 'Data in rank 0'
     do i=1,8
        write(*,'(8I3)') array(i, :)
     end do
  end if


  !TODO: create datatype describing one row, use mpi_type_vector
  call mpi_type_vector(8, 1, 8, MPI_INTEGER, rowtype, ierr)
  call mpi_type_commit(rowtype, ierr)
  !TODO: send first row of marix from rank 0 to 1

  if (rank == 0) then
    call mpi_send(array(2,1), 1, rowtype, 1, 1, MPI_COMM_WORLD, ierr)
  else if (rank == 1) then
    call mpi_recv(array(2,1), 1, rowtype, 0, 1, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
  end if

  ! Print out the result
  if (rank == 1) then
     write(*,*) 'a) Received data'
     do i=1,8
        write(*,'(8I3)') array(i, :)
     end do
  end if

  !TODO free datatype
  call mpi_type_free(rowtype, ierr)

  ! Crazytype (excercise b)

  call mpi_type_indexed(4, [1,2,3,4], [0, 16*1+1, 16*2+2, 16*3+3], MPI_INTEGER, crazytype, ierr)
  call mpi_type_commit(crazytype, ierr)

  if (rank == 1) then
    array = 0
  end if

  if (rank == 0) then
    call mpi_send(array, 1, crazytype, 1, 1, MPI_COMM_WORLD, ierr)
  else if (rank == 1) then
    call mpi_recv(array, 1, crazytype, 0, 1, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
  end if

  if (rank == 1) then
     write(*,*) 'b) Received data'
     do i=1,8
        write(*,'(8I3)') array(i, :)
     end do
  end if


  call mpi_finalize(ierr)

end program datatype1
