! Hello world program for MPI
program helloworld
  use mpi_f08
  implicit none

  integer :: rc, rank, size

  call mpi_init(rc)
  call mpi_comm_rank(MPI_COMM_WORLD, rank)
  call mpi_comm_size(MPI_COMM_WORLD, size)
  print '(2(a,i2),a)', "Hello parallel world from process nr", rank, " out of", size, "!"
  call mpi_finalize()
end program helloworld
