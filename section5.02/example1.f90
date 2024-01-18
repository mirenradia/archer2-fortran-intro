program example1

  ! Use of pointer and target

  implicit none

  integer, target  :: datum = 1
  integer, pointer :: p => null()

  p => datum
  p = 2

  print *, "datum is ", datum

end program example1
