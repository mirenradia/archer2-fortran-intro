program exercise1

  ! Compute the two solutions (say x1, and x2) of
  !   a x^2 + b x + c = 0
  ! which may be written
  !   x = (-b +/- sqrt(b^2 - 4ac))/2a.
  !
  ! First, use real variables and compute the discriminant
  !   b^2 - 4ac
  ! and act accordingly.
  !
  ! Then use complex variables to compute, directly, two complex roots
  ! to check your answer for (x1, x2).
  !
  ! Some values might be
  !   a = 1.0, b =  5.0, c = 6.0     (=> two real roots)
  !   a = 2.0, b =  1.0, c = 5.0/8.0 (=> two complex roots)
  !   a = 4.0, b = -8.0, c = 4.0     (=> two equal roots)

  use iso_fortran_env, only : real64

  implicit none

  real (real64), parameter :: a = 4.0d0
  real (real64), parameter :: b = -8.0d0
  real (real64), parameter :: c = 4.0d0

  real (real64) :: discriminant
  real (real64) :: x1, x2
  complex (real64) :: z1, z2

  discriminant = b**2 - 4.0d0*a*c

  if (discriminant >= 0) then
    x1 = 0.5d0 * (-b + sqrt(discriminant)) / a
    x2 = 0.5d0 * (-b - sqrt(discriminant)) / a
    print *, "x1 = ", x1
    print *, "x2 = ", x2
  else
    z1 = 0.5d0 * (-b + sqrt(complex(discriminant, real64))) / a
    z2 = 0.5d0 * (-b - sqrt(complex(discriminant, real64))) / a
    print *, "z1 = ", z1
    print *, "z2 = ", z2
  end if


end program exercise1
