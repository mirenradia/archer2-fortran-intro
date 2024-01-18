program exercise

  use mod_trapezium
  use iso_fortran_env
  implicit none

  real (real64)   :: a
  real (real64)   :: b
  integer (int64), dimension(3) :: n_arr
  integer (int64) :: i

  a = 0.0d0
  b = 2.0d0*atan(1.0d0) ! pi/2

  n_arr = [10, 100, 1000]

  do i = 1, size(n_arr)
    print *, "For n = ", n_arr(i), "integral = ", trapezium(a, b, n_arr(i), my_func)
  end do

  contains

    function my_func(x) result(y)
      implicit none
      real (real64), intent(in) :: x
      real (real64) :: y

      y = cos(x) * sin(x)

    end function my_func

  end program exercise

