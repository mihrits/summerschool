module vector_algebra
  use iso_fortran_env, only : REAL64
  implicit none
  real(REAL64) :: a
  type vector_t
     real(REAL64) :: x, y, z
  end type vector_t

  ! TODO: overload operators needed by the parser
  
  interface abs
    module procedure abs_vec
  end interface abs

  interface operator(+)
    module procedure add_vec
  end interface operator(+)

  interface operator(-)
    module procedure substract_vec
  end interface operator(-)

  interface operator(*)
    module procedure dotmultip_vec
  end interface operator(*)

  interface operator(.x.)
    module procedure crossmultip_vec
  end interface operator(.x.)

contains
  ! TODO: implement the corresponding functions
  function abs_vec(v1) result(a)
    type(vector_t), intent(in) :: v1
    real(REAL64) :: a
    
    a = sqrt(v1 % x**2 + v1 % y**2 + v1 % z**2)
  end function abs_vec

  function add_vec(v1, v2) result(v3)
    type(vector_t), intent(in) :: v1, v2
    type(vector_t) :: v3

    v3 % x = v1 % x + v2 % x
    v3 % y = v1 % y + v2 % y
    v3 % z = v1 % z + v2 % z
  end function add_vec

  function substract_vec(v1, v2) result(v3)
    type(vector_t), intent(in) :: v1, v2
    type(vector_t) :: v3   

    v3 % x = v1 % x - v2 % x
    v3 % y = v1 % y - v2 % y
    v3 % z = v1 % z - v2 % z
  end function substract_vec

  function dotmultip_vec(v1, v2) result(a)
    type(vector_t), intent(in) :: v1, v2
    real(REAL64) :: a

    a = v1 % x * v2 % x + v1 % y * v2 % y + v1 % z * v2 % z
  end function dotmultip_vec

  function crossmultip_vec(v1, v2) result(v3)
    type(vector_t), intent(in) :: v1, v2
    type(vector_t) :: v3   

    v3 % x = v1 % y * v2 % z - v1 % z * v2 % y
    v3 % y = v1 % z * v2 % x - v1 % x * v2 % z
    v3 % z = v1 % x * v2 % y - v1 % y * v2 % x
  end function crossmultip_vec

end module vector_algebra
