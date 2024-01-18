program exercise4

  ! Compute an approximation to the conductance of a narrow channel.

  ! The steady volume flux (volume flow rate) Q in a rectangular
  ! capillary of cross section width x height 2b x 2c (with 2b > 2c)
  ! we can write:
  !
  !    Q = -C (dp/dx) / eta
  !
  ! with dp/dx the pressure gradient and eta the dynamic viscosity.
  ! One can define a viscosity-independent conductance C
  !
  !    C = (4/3) b c^3 [ 1 - 6(c/b) \sum_k tanh (a_k b/c)/a_k^5 ]
  !
  ! where a_k = (2k - 1) pi/2 and the sum is k = 1, ..., \inf.
  !
  ! E.g. T. Papanastasiou, G. Georiou, and A. Alexandrou,
  ! "Viscous Fluid Flow" CRC Press, Boca Raton, Florida (2000).

  !  Exercise
  !  Compute the first term in the series (i.e., k = 1):
  !
  !  C_1 = (4/3) b c^3 [ 1 - 6(c/b) tanh(a_k b/c) / a_k^5 ]
  !
  !  We will use the sample values w = 62, h = 30.
  !
  !  You will need the intrinsic function tanh() for hyperbolic tangent.
  !
  !  Your result should be in the region of 97277.88 ( the exact figure
  !  depending on precision).

  !  Try for both real32 and real64.
  !  Does your compiler support kind real128?


  ! Some appropriate output might be ...
  ! print *, "Value of w:       ", w
  ! print *, "Value of h:       ", h
  ! print *, "Value of pi:      ", pi
  ! print *, "Approximation is: ", conductance

  use iso_fortran_env, only : real32, real64, real128

  implicit none

  integer, parameter :: real_t = real128

  real (real_t), parameter :: w = 62, h = 30
  real (real_t) :: k = 1
  real (real_t) :: b, c
  real (real_t) :: a_1, C_1
  real (real_t) :: pi

  b = 0.5_real_t * w
  c = 0.5_real_t * h
  pi = 4.0_real_t * atan(1.0_real_t)
  a_1 = 0.5d0 * (2d0 * k - 1) * pi 

  C_1 = (4.0_real_t / 3.0_real_t) * b * (c ** 3) * (1_real_t - 6_real_t * (c / b) * tanh(a_1 * b / c) / (a_1 ** 5)) 

  print *, "Value of w:       ", w
  print *, "Value of h:       ", h
  print *, "Approximation is: ", C_1
end program exercise4
