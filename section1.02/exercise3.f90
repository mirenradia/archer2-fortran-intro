program exercise3

  ! Compute an approximation to pi.

  ! An approximation using the Gauss-Legendre algorithm
  ! See, e.g., https://en.wikipedia.org/wiki/Pi
  !
  ! If we initialise:
  !
  !   a_0 = 1
  !   b_0 = 1/sqrt(2)
  !   t_0 = 1/4
  !   p_0 = 1
  !
  ! (in general, a_n etc, with n = 0), then the next terms in the series
  ! can be computed as:
  !
  !   a_{n+1} = (a_n + b_n)/2
  !   b_{n+1} = sqrt(a_n b_n)
  !   t_{n+1} = t_n - p_n (a_n - a_{n+1})^2   with p_{n+1} = 2p_n
  !
  !  then
  !
  !   pi_n \approx (a_n + b_n)^2/4t_n
  !
  !  Compute the first two approximations pi_0 and pi_1.


  use iso_fortran_env, only : real64

  implicit none

  real (real64) :: a_0 = 1d0
  real (real64) :: b_0 = 1d0 / sqrt(2d0)
  real (real64) :: t_0 = 0.25d0
  real (real64) :: p_0 = 1d0

  real (real64) :: a_1, b_1, t_1, p_1, pi_0, pi_1

  a_1 = 0.5d0 * (a_0 + b_0)
  b_1 = sqrt(a_0 * b_0)
  t_1 = t_0 - p_0 * (a_0 - a_1)**2
  p_1 = 2d0 * p_0

  pi_0 = 0.25d0 * (a_0 + b_0)**2 / t_0
  pi_1 = 0.25d0 * (a_1 + b_1)**2 / t_1

  print *, "pi_0 = ", pi_0
  print *, "pi_1 = ", pi_1

end program exercise3
