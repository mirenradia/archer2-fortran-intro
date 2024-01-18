program exercise_program1

  ! Functions and subroutines

  use exercise_module1
  implicit none

  real (kind = kp) :: pi
  integer, parameter :: nmax = 5
  integer :: fib_32

  call pi_gauss_legendre_sr(nmax, pi)
  
  print *, "pi: ", pi

  fib_32 = fibonacci(32)

  print *, "fib_32: ", fib_32

end program exercise_program1
