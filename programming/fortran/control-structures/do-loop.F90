program loops
  implicit none
  ! TODO define parameters nx and ny
  integer, parameter :: nx = 10, ny = 10
  ! TODO: define real-valued array A
  real, dimension(nx, ny) :: A
  integer :: i, j, x, fi, fib

  ! TODO initialize array A here
  do i = 1, ny
    do j = 1, nx
      A(i,j) = (real(i)/ny)**2 + (real(j)/nx)**2
    end do
  end do

  !--------------------------------------------------
  ! Print out the array
  ! the ':' syntax means the whole row, see the Fortran arrays lecture
  do i = 1, nx
     write(*, '(12F6.3)') A(i,:)
  end do


  ! b) control structure checking integer status

  do
    write(*,*) "Insert an integer."
    read(*,*) x
    if (x == 9999) then
        write(*,*) "Thank you for saving me!"
        exit
    else if (x < 0) then
        write(*,*) "Given integer is negative."
    else if (x == 0) then
        write(*,*) "Given integer is 0."
    else if (x > 100) then
        write(*,*) "Given integer is greater than 100."
    else
        write(*,*) "Given integer is positive and less than 100."
    end if
  end do

  ! c) Fibonacci
  fi = 0
  fib = 1
  write(*,*) "Fibonacci numbers are:"
  write(*,*) fi
  write(*,*) fib
  do while (fib < 100)
    write(*,*) fib
    fib = fi + fib
    fi = fib - fi
  end do 

end program loops
