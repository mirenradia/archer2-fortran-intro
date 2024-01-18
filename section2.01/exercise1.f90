program exercise1

  ! Compute an approximation to pi.

  ! An approximation using the Gauss-Legendre algorithm
  ! See, e.g., https://en.wikipedia.org/wiki/Pi
  !
  ! Add an iteration which computes successive approximations
  ! to pi (say the first 5 or 6)
  !
  ! Try extended precision kind(1.d0)
  ! Can you add a condition that will exit the iteration when
  ! the approximation is close enough to the true value.
  ! Hint: you will need the intrinsic function abs() which returns
  ! the absolute value of the argument.

  implicit none

  integer, parameter :: kp = kind(1.0d0)
  integer, parameter :: nmax = 1000
  real (kp), parameter :: tol = 1.0e-14_kp

  real (kp) :: a = 1.0_kp
  real (kp) :: b = 1.0/sqrt(2.0_kp)
  real (kp) :: t = 0.25_kp
  real (kp) :: p = 1.0_kp
  real (kp) :: pi_n
  real (kp) :: pi

  integer :: n

  real (kp) :: an

  print *, "Approximation pi_0: ", (a + b)**2/(4.0*t)

  pi = 4.0_kp * atan(1.0_kp)

  do n = 1, nmax
    an = a

    a = (an + b)/2.0
    b = sqrt(an*b)
    t = t - p*(an - a)**2
    p = 2.0*p
    
    pi_n = (a + b)**2/(4.0*t)
    if (abs(pi_n - pi) < tol) exit
  end do

  print *, "n: ", n
  print *, "Approximation pi_n: ", (a + b)**2/(4.0*t)

end program exercise1
