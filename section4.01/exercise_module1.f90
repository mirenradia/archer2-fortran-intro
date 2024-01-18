module exercise_module1

  ! String functions exercise.
  !
  ! 1. Write a subroutine string_to_lower_case() to replace the argument
  ! 2. Write a function   to_lower_case() to return a new string
  !
  ! See exercise_program1.f90 for a test.

  implicit none
  public

contains

  subroutine string_to_lower_case(a_string)

    character (len = *), intent(inout) :: a_string
    integer :: n
    integer :: upper_lower_diff

    ! Assume a fixed difference between upper and lower case chars
    upper_lower_diff = iachar('a') - iachar('A')

    do n = 1, len_trim(a_string)
      if (a_string(n:n) >= 'A' .and. a_string(n:n) <= 'Z') then
        a_string(n:n) = achar(iachar(a_string(n:n)) + upper_lower_diff)
      end if
    end do

  end subroutine string_to_lower_case

  function to_lower_case(a_string) result (out_string)

    character (len = *), intent(in) :: a_string
    character (len = len_trim(a_string)) :: out_string

    out_string = a_string
    call string_to_lower_case(out_string)

  end function to_lower_case


end module exercise_module1
