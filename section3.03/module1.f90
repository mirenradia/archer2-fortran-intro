module module1

  ! Provides a subroutine to add a to b

  implicit none
  public

contains

  subroutine array_action2(a, b)

    real, dimension(:,:), intent(in)    :: a
    real, dimension(:,:), intent(inout) :: b

    print *, "ubound(a) = ", ubound(a)
    print *, "lbound(a) = ", lbound(a)
    print *, "ubound(b) = ", ubound(b)
    print *, "lbound(b) = ", lbound(b)

    b(:,:) = b(:,:) + a(:,:)

  end subroutine array_action2

end module module1
