module mod_trapezium

  use iso_fortran_env
  implicit none

contains

  function trapezium(a, b, n, f) result(out)
    real (real64), intent(in) :: a
    real (real64), intent(in) :: b
    integer (int64), intent(in) :: n
    interface
      function f(x) result(y)
        use iso_fortran_env
        real (real64), intent(in) :: x
        real (real64) :: y
      end function f
    end interface
    real (real64) :: out

    real (real64) :: h, sum
    integer :: k

    h = (b - a) / real(n, real64)

    sum = f(a) + f(b)
    do k = 1, n - 1
      sum = sum + 2.0_real64 * f(a + real(k, real64) * h)
    end do

    out = 0.5_real64 * h * sum

  end function trapezium

end module mod_trapezium
