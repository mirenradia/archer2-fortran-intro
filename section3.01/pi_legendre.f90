module pi_legendre

  ! Exercise to introduce array storage

  ! Here's a version of the pi program
  ! See, e.g., https://en.wikipedia.org/wiki/Pi
  !
  ! If we initialise:
  !
  !   a_0 = 1, b_0 = 1/sqrt(2), t_0 = 1/4, p_0 = 1
  !
  ! and perform the iteration
  !
  !   a_{n+1} = (a_n + b-n)/2
  !   b_{n+1} = sqrt(a_n b_n)
  !   t_{n+1} = t_n - p_n (a_n - a_{n+1})^2   with p_{n+1} = 2p_n
  !
  !  then
  !
  !   pi_n \approx (a_n + b_n)^2/4t_n
  !
  ! Exercise:
  ! Decide on a fixed number of iterations, and introduce arrays of
  ! appropriate size for the quantities a, b, and t. Use a first
  ! loop to assign values to these three quantities.


  implicit none
  private

  integer, parameter, public :: kp = kind(1.d0)

  public :: pi_approx

contains
  function pi_approx() result(pi)

    integer, parameter :: nmax = 10
    real (kp) :: pi
    real (kp), dimension(0:nmax) :: a
    real (kp), dimension(0:nmax) :: b
    real (kp), dimension(0:nmax) :: t
    real (kp) :: p = 1.0_kp

    integer :: n

    a(0) = 1.0_kp
    b(0) = 1.0/sqrt(2.0_kp)
    t(0) = 0.25_kp

    do n = 0, nmax-1

      a(n+1) = (a(n) + b(n))/2.0
      b(n+1) = sqrt(a(n)*b(n))
      t(n+1) = t(n) - p*(a(n) - a(n+1))**2
      p = 2.0*p
    end do

    do n = 0, nmax
      print *, "Approximation n, pi: ", n, (a(n) + b(n))**2/(4.0*t(n))
    end do

    pi = (a(nmax) + b(nmax))**2/(4.0*t(nmax))
  end function pi_approx

end module pi_legendre
