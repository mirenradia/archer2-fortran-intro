program program_pi

  use pi_legendre
  implicit none

  real (kind = kp) :: pi

  pi = pi_approx()

  print *, "Value of pi:    ", pi

end program program_pi
