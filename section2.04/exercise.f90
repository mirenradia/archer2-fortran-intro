program exercise

  ! Tri-diagonal matrix problem via Thomas' algorithm
  ! See https://en.wikipedia.org/wiki/Tridiagonal_matrix_algorithm

  ! We will write a program to solve the tri-diagonal matrix problem
  !   a_i x_i-1 + b_i x_i + c_i x_i+1 = d_i
  ! Follow the description at the web page above.

  ! Consider a small problem with n = 4
  ! The following values can be used as a test:
  !   a =      [1.0, 1.0, 1.0]
  !   b = [4.0, 4.0, 4.0, 4.0]
  !   c = [2.0, 2.0, 2.0]
  !   d = [1.0, 4.0, 5.0, 6.0]
  ! which should give a solution approx. x = [-0.195, 0.890, 0.317, 1.42]

  ! 1. Implement the algorithm following the pseudocode.
  ! 2. Check the answer
  ! 3. Construct the full matrix and use the intrinsic function
  !    matmul(a, x) to check the answer for a different d. The
  !    arguments a and x may be an appropriate combination of
  !    matrix and vector.
  ! 4. The above reference states that the algorithm is stable if
  !    the matrix is diagonally dominant. Check this is the case
  !    before entering the solution phase.
  !    See also https://en.wikipedia.org/wiki/Diagonally_dominant_matrix

  use iso_fortran_env
  implicit none

  integer, parameter :: n = 4
  real (real64), dimension(n) :: a = [0.0d0, 1.0d0, 1.0d0, 1.0d0]
  real (real64), dimension(n) :: b = [4.0d0, 4.0d0, 4.0d0, 4.0d0]
  real (real64), dimension(n) :: c = [2.0d0, 2.0d0, 2.0d0, 0.0d0]
  real (real64), dimension(n) :: d = [1.0d0, 8.0d0, 5.0d0, 6.0d0]
  real (real64), dimension(n) :: x, d_original, error
  real (real64) :: w
  real (real64), dimension(n,n) :: m
  integer :: i


  d_original = d
  m = 0.0d0

  m(2,1) = a(2)
  m(1,1) = b(1)
  m(1,2) = c(1)
  do i = 2,n-1
    m(i+1,i) = a(i+1)
    m(i,i) = b(i)
    m(i,i+1) = c(i)
  end do
  m(n,n) = b(n)

  do i = 2,n
    w = a(i) / b(i-1)
    b(i) = b(i) - w * c(i-1)
    d(i) = d(i) - w * d(i-1)
  end do

  x(n) = d(n) / b(n)

  do i = n-1, 1, -1
    x(i) = (d(i) - c(i) * x(i+1)) / b(i)
  end do

  print *, "Solution is: ", x

  error = d_original - matmul(m, x)
  print *, "m is: ", m
  print *, "Error is: ", error

end program exercise
