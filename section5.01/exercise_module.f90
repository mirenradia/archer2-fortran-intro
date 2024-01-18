module exercise_module

  ! Tri-diagonal matrix problem via Thomas' algorithm
  ! See https://en.wikipedia.org/wiki/Tridiagonal_matrix_algorithm

  use iso_fortran_env
  implicit none

  integer, parameter :: mykind = real32

  type, public :: tridiagonal_matrix
    integer :: nmax
    real (mykind), dimension(:), allocatable :: b
    real (mykind), dimension(:), allocatable :: a
    real (mykind), dimension(:), allocatable :: c
  end type tridiagonal_matrix

contains

  function create_tridiagonal_matrix(nmax, b, a, c) result(tri_mat)
    integer, intent(in) :: nmax
    real (mykind), dimension(1:nmax), intent(in) :: b
    real (mykind), dimension(2:nmax), intent(in) :: a
    real (mykind), dimension(1:nmax-1), intent(in) :: c
    type (tridiagonal_matrix) :: tri_mat

    tri_mat%nmax = nmax
    allocate(tri_mat%b(1:nmax))
    allocate(tri_mat%a(2:nmax))
    allocate(tri_mat%c(1:nmax-1))
    tri_mat%b = b
    tri_mat%a = a
    tri_mat%c = c

  end function create_tridiagonal_matrix

  subroutine tridiagonal_solve(tri_mat, rhs, x)

    ! Routine to solve system for
    !   b(1:nmax)                  diagonal elements
    !   a(2:nmax)                  lower diagonal elements
    !   c(1:nmax-1)                upper diagonal elements
    !   rhs(1:nmax)                right-hand side
    !   x(1:nmax)                  solution on exit
    type (tridiagonal_matrix), intent(in)     :: tri_mat
    real (mykind), dimension(1:), intent(in)  :: rhs
    real (mykind), dimension(1:), intent(out) :: x

    real (mykind), dimension(tri_mat%nmax) :: blocal       ! local copy of b
    real (mykind), dimension(tri_mat%nmax) :: rlocal       ! local copy of rhs

    integer :: i
    real (mykind) :: w

    blocal(1:tri_mat%nmax) = tri_mat%b(1:tri_mat%nmax)
    rlocal(1:tri_mat%nmax) = rhs(1:tri_mat%nmax)

    ! Solve via Thomas' algorithm

    do i = 2, tri_mat%nmax
      w = tri_mat%a(i) / blocal(i-1)
      blocal(i) = blocal(i) - w*tri_mat%c(i-1)
      rlocal(i) = rlocal(i) - w*rlocal(i-1)
    end do

    x(:) = rlocal(:)/blocal(:)

    do i = tri_mat%nmax-1, 1, -1
      x(i) = (rlocal(i) - tri_mat%c(i)*x(i+1))/blocal(i)
    end do

  end subroutine tridiagonal_solve

end module exercise_module
