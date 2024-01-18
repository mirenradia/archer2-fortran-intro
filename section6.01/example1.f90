program example1

  ! Calling a external function

  implicit none
  interface
    function array_size(a) result(isize)
      real, dimension(:), intent(in) :: a
      integer                        :: isize
    end function array_size
  end interface

  real, dimension(3,2) :: a

  ! Place an external declaration or interface block (not both) here.

  print *, "The array size is: ", array_size(a), size(a)

end program example1
