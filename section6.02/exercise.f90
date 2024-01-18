program print_cli_args

  implicit none

  integer :: nargs
  character (len = :), allocatable :: arg
  integer :: arg_length
  integer :: i

  nargs = command_argument_count()

  do i = 1, nargs
    call get_command_argument(i, length = arg_length)
    allocate(character(len = arg_length) :: arg)
    call get_command_argument(i, arg)
    print *, arg
    deallocate(arg)
  end do

end program print_cli_args

