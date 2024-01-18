module solution_module

  implicit none

public

contains
  
  subroutine write_pbm(map, filename, ierr)

    logical, dimension(:,:), intent(in) :: map
    character (len = *), intent(in)    :: filename
    integer, intent(out)               :: ierr

    integer :: fileunit
    integer :: i, j
    integer :: value

    open(newunit = fileunit, file = filename, form = 'formatted', &
         action = 'write', status = 'replace', err = 100, iostat = ierr)

    if (ierr /= 0) then
       return
    end if

    write(fileunit, '(a2)', err = 100, iostat = ierr) "P1"
    write(fileunit, '(i10,x,i10)', err = 100, iostat = ierr) size(map, 2), &
        size(map, 1)

    do i = 1, size(map, 1)
      do j = 1, size(map, 2)
        ! convert to integer
        value = map(i, j)

        write(fileunit, '(i1)', err = 100, iostat = ierr, advance = 'no') value
      end do
      write(fileunit, *, err = 100, iostat = ierr) ''
    end do
      

    100 continue
      return

  end subroutine write_pbm

end module solution_module