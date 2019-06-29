program datatype_struct
  use mpi_f08
  use iso_fortran_env, only : real64
  implicit none

  type particle
     real :: coords(3)
     integer :: charge
     character(len=2) :: label
  end type particle

  integer, parameter :: n = 1000
  integer :: i, ierror,  myid,  ntasks, psize
  type(particle) :: particles(n)

  integer, parameter :: cnt = 3
  type(mpi_datatype) :: particle_mpi_type, temp_type, types(cnt)
  integer :: blocklen(cnt)
  integer(KIND=MPI_ADDRESS_KIND) :: disp(cnt), disp_particle(2)
  integer(KIND=MPI_ADDRESS_KIND) :: lb, extent, true_extent
  real(real64) :: t1, t2

  call MPI_INIT(ierror)
  call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierror)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, ntasks, ierror)

  ! insert some data for the particle struct
  if(myid == 0) then
     do i = 1, n
        call random_number(particles(i)%coords)
        particles(i)%charge = 54
        particles(i)%label = 'Xe'
     end do
  end if

  ! Sending as mpi_datatype (excercise c)
  ! define the datatype for type particle

  blocklen = (/ 3,1,1 /)
  types = (/ MPI_REAL, MPI_INTEGER, MPI_CHARACTER /)
  
  call mpi_get_address(particles(1)%coords, disp(1), ierror)
  call mpi_get_address(particles(1)%charge, disp(2), ierror)
  call mpi_get_address(particles(1)%label, disp(3), ierror)
  disp(3) = disp(3) - disp(1)
  disp(2) = disp(2) - disp(1)
  disp(1) = 0

  call mpi_type_create_struct(cnt, blocklen, disp, types, particle_mpi_type, ierror)
  call mpi_type_commit(particle_mpi_type, ierror)

  ! Check extent.
  ! (Not really neccessary on most systems.)
  call mpi_type_get_extent(particle_mpi_type, lb, extent)
  ! resize the particle_mpi_type if needed
  call mpi_get_address(particles(1), disp_particle(1), ierror)
  call mpi_get_address(particles(2), disp_particle(2), ierror)
  true_extent = disp_particle(2) - disp_particle(1)
  if (extent /= true_extent) then
     ! resize the particle_mpi_type if needed
     call mpi_type_create_resized(particle_mpi_type, lb, true_extent, particle_mpi_type, ierror)
  end if

  t1 = MPI_WTIME()
  if(myid == 0) then
     do i = 1, 1000
        call MPI_SEND(particles, n, particle_mpi_type, 1, i, &
             & MPI_COMM_WORLD, ierror)
     end do
  else if(myid == 1) then
     do i = 1, 1000
        call MPI_RECV(particles, n, particle_mpi_type, 0, i, &
             & MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
     end do
  end if
  t2 = MPI_WTIME()

  write(*,*) "Time: ", myid, (t2-t1) / 1000d0
  write(*,*) "Check:", myid, particles(n)%coords(1)

  ! Sending as bytes (excercise d)

  psize = sizeof(particles(1))

  t1 = MPI_WTIME()
  if(myid == 0) then
     do i = 1, 1000
        call MPI_SEND(particles, n*psize, MPI_BYTE, 1, i, &
             & MPI_COMM_WORLD, ierror)
     end do
  else if(myid == 1) then
     do i = 1, 1000
        call MPI_RECV(particles, n*psize, MPI_BYTE, 0, i, &
             & MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
     end do
  end if
  t2 = MPI_WTIME()

  write(*,*) "Time: ", myid, (t2-t1) / 1000d0
  write(*,*) "Check:", myid, particles(n)%coords(1)


  call MPI_TYPE_free(particle_mpi_type, ierror)
  call MPI_FINALIZE(ierror)

end program datatype_struct
