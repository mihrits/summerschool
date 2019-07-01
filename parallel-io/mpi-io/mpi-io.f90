program pario
  use mpi_f08
  use, intrinsic :: iso_fortran_env, only : error_unit, output_unit
  implicit none

  integer, parameter :: datasize = 64, writer_id = 0
  integer :: rc, my_id, ntasks, localsize, i
  integer, dimension(:), allocatable :: localvector

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

  !localvector = [(i + my_id * localsize, i=1,localsize)]

  call mpiio_reader()
  call mpiio_writer()

  deallocate(localvector)
  call mpi_finalize(rc)

contains

  subroutine mpiio_writer()
    implicit none
    integer :: rc, dsize
    type(mpi_file) :: fh
    integer(kind=MPI_OFFSET_KIND) :: offset;

    call mpi_type_size(MPI_INTEGER, dsize, rc)

    ! TODO: write the output file "mpiio.dat" using MPI IO. Each
    !       rank should write their own local vectors to correct
    !       locations in the output file.

    call mpi_file_open(mpi_comm_world, 'mpiio.dat', mpi_mode_create+mpi_mode_wronly, &
                    &  mpi_info_null, fh, rc)

    offset = my_id * localsize * dsize
    call mpi_file_seek(fh, offset, mpi_seek_set, rc)

    call mpi_file_write_all(fh, localvector, localsize, mpi_integer, mpi_status_ignore, rc)
                
    call mpi_file_close(fh, rc)

  end subroutine mpiio_writer

  subroutine mpiio_reader()
    implicit none
    integer :: rc, dsize
    type(mpi_file) :: fh
    integer(kind=MPI_OFFSET_KIND) :: offset;

    call mpi_type_size(MPI_INTEGER, dsize, rc)

    ! TODO: write the output file "mpiio.dat" using MPI IO. Each
    !       rank should write their own local vectors to correct
    !       locations in the output file.

    call mpi_file_open(mpi_comm_world, 'input_mpiio.dat', mpi_mode_rdonly, &
                    &  mpi_info_null, fh, rc)

    offset = my_id * localsize * dsize
    call mpi_file_seek(fh, offset, mpi_seek_set, rc)

    call mpi_file_read_all(fh, localvector, localsize, mpi_integer, mpi_status_ignore, rc)
                
    call mpi_file_close(fh, rc)

  end subroutine mpiio_reader
end program pario
