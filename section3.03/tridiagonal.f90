module tridiagonal

  ! Tri-diagonal matrix problem via Thomas' algorithm
  ! See https://en.wikipedia.org/wiki/Tridiagonal_matrix_algorithm

  ! The following values are used as a test:
  !   a =      [1.0, 1.0, 1.0]                - lower diagonal
  !   b = [4.0, 4.0, 4.0, 4.0]                - diagonal
  !   c = [2.0, 2.0, 2.0]                     - upper diagonal
  !   d = [1.0, 4.0, 5.0, 6.0]                - rhs
  ! which should give a solution approx. x = [-0.195, 0.890, 0.317, 1.42]

  use iso_fortran_env
  implicit none

  integer, parameter :: mykind = real32

contains

  subroutine tridiagonal_solve(b, a, c, rhs, x)
    real (mykind), dimension(:), intent(in)      :: b
    real (mykind), dimension(2:), intent(in)      :: a
    real (mykind), dimension(:), intent(in)      :: c
    real (mykind), dimension(:), intent(in)      :: rhs
    real (mykind), dimension(:), intent(out)     :: x

    integer :: nmax
    real (mykind), dimension(size(rhs)) :: b_tmp, rhs_tmp
    real (mykind) :: w
    integer :: i

    nmax = size(rhs)
    b_tmp = b
    rhs_tmp = rhs

    ! Set up the matrix here: all elements; diagonal elements, then
    ! off-diagonal elements

    ! Solve via Thomas' algorithm
    ! Note b(:) and d(:) are destroyed

    do i = 2, nmax
      w = a(i) / b_tmp(i-1)
      b_tmp(i) = b_tmp(i) - w*c(i-1)
      rhs_tmp(i) = rhs_tmp(i) - w*rhs_tmp(i-1)
    end do

    x(:) = rhs_tmp(:)/b_tmp(:)

    do i = nmax-1, 1, -1
      x(i) = (rhs_tmp(i) - c(i)*x(i+1))/b_tmp(i)
    end do

  end subroutine tridiagonal_solve

end module tridiagonal
