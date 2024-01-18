program exercise2

  ! A program to identify prime numbers (<= nmax = 120) via th Sieve
  ! of Eratosthenses.

  ! See, e.g., https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
  !
  ! Define a logical array to indicate where relevant integers are
  ! prime.
  !
  ! Implement the sieve to set appropriate values in the logical array
  ! using the algorithm described at the reference above.
  !
  ! Try using loops first; where could you then introduce array
  ! constructs?
  ! Hint 2. You may need an additional integer array to use array constructs.
  !
  ! Count how many prime numbers you have. Check your results.
  !
  ! Is one version any clearer than the other?

  implicit none

  integer, parameter :: nmax = 120
  logical, dimension(2:nmax) :: isprime
  integer :: i, j
  integer :: nprimes

  isprime = .true.
  
  do i = 2, floor(sqrt(real(nmax)))
    if (isprime(i)) then
      isprime(i**2:nmax:i) = .false.
    end if
  end do

  nprimes = count(isprime(:))

  print *, "number of primes up to ", nmax, " is ", nprimes

end program exercise2
