program pario
  use mpi_f08
  use, intrinsic :: iso_fortran_env, only : error_unit, output_unit
  implicit none

  integer, parameter :: datasize = 64, writer_id = 0
  integer :: rc, my_id, ntasks, localsize, i
  integer, dimension(:), allocatable :: localvector
  integer, dimension(datasize) :: fullvector
  character(len=30) :: filename

  call mpi_init(rc)
  call mpi_comm_size(mpi_comm_world, ntasks, rc)
  call mpi_comm_rank(mpi_comm_world, my_id, rc)

  if (ntasks > 64) then
     write(error_unit, *) 'Maximum number of tasks is 64!'
     call mpi_abort(MPI_COMM_WORLD, -1, rc)
  end if

  if (mod(datasize, ntasks) /= 0) then
     write(error_unit,*) 'Datasize (64) should be divisible by number of tasks'
     call mpi_abort(MPI_COMM_WORLD, -1, rc)
  end if

  localsize = datasize / ntasks
  allocate(localvector(localsize))

  localvector = [(i + my_id * localsize, i=1,localsize)]

  call single_writer()

  deallocate(localvector)
  call mpi_finalize(rc)

contains

  subroutine single_writer()
    implicit none

    ! TODO: Implement a function that writers the whole array of elements
    !       to a file so that single process is responsible for the file io
    !if (my_id /= 0) then
    !  call mpi_send(localvector, localsize, mpi_integer, 0, 0, mpi_comm_world, rc)
    !else
    !  fullvector(1:localsize) = localvector
    !  do i = 1, ntasks-1
    !    call mpi_recv(fullvector(i*localsize+1:(i+1)*localsize+1), localsize, mpi_integer, &
    !                & i, mpi_any_tag, mpi_comm_world, mpi_status_ignore, rc)
    !  end do

    !  open(10, file='output.txt', status='new', action='write')
    !  write(10,*) fullvector
    !  close(10)
    !end if

    call mpi_gather(localvector, localsize, mpi_integer, fullvector, localsize, mpi_integer, &
                &   0, mpi_comm_world, rc)
    if (my_id == 0) then
      open(20, file='output.txt', status='new', action='write')
      write(20,*) fullvector
      close(20)
    end if

    write(filename, '(A23,I2.2,A4)') 'mass_output/output_rank', my_id, '.txt'
    !open(30+my_id, file='mass_output/output_'//my_id//'.txt', status='new', action='write')
    open(30+my_id, file=filename, status='new', action='write')
    write(30+my_id,*) localvector
    close(30+my_id)




  end subroutine single_writer

end program pario
