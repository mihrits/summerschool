module laplacian_mod
  implicit none
  real, parameter :: dx = 0.01, dy = 0.01

contains

  subroutine initialize(field0)
    ! TODO: implement a subroutine that initializes the input array
    integer :: nx, ny 
    integer :: i, j, k
    real :: y, x, dy, dx
    real, dimension(:,:), intent(out) :: field0
    
    ! Value of power
    k = 2

    ! Size of the array
    nx = size(field0, 2)
    ny = size(field0, 1)

    ! Grid spacing
    dx = 1.0/real(nx-1)
    dy = 1.0/real(ny-1)

    ! Initializing field0 as f(x,y) = x**k + y**k
    y = 0.0
    do j = 1, ny
      x = 0.0
      do i = 1, nx
         field0(i,j) =  x**k + y**k
         x = x + dx
      end do
      y = y + dy
    end do
  end subroutine initialize

  subroutine laplacian(curr, prev)
    ! TODO: insert a subroutine that computes a laplacian of the
    ! array "prev" and returns it as an array "curr"
    integer :: i, j, nx, ny
    real :: dx, dy
    real, dimension(:,:), intent(out) :: curr
    real, dimension(:,:), intent(in) :: prev
    real, dimension(:,:), allocatable :: temp

    ! Size of the array
    nx = size(prev, 2)
    ny = size(prev, 1)
 
    ! Grid spacing
    dx = 1.0/real(nx-1)
    dy = 1.0/real(ny-1)

    allocate(temp(1:ny+1,1:nx+1))
    temp = 0.
    temp(2:ny,2:nx) = prev

    do j = 1, ny
      do i = 1, nx
        select case (i)
        
        ! TODO: Implement embedding with zeroes

        curr(i-1,j-1) = (temp(i-1,j)-2*temp(i,j)+prev(i+1,j))/dx**2 &
                      + (prev(i,j-1)-2*prev(i,j)+prev(i,j+1))/dy**2
      end do
    end do
  end subroutine laplacian

  subroutine write_field(array)
    ! TODO: write a subroutine that prints "array" on screen
    integer :: i
    real, dimension(:,:), intent(in) :: array

    do i = 1, size(array,1)
      write(*,'(12F9.4)') array(i,:)
    end do
  end subroutine write_field

end module laplacian_mod
