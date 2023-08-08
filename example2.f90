program example2
  use loop_mod
  implicit none

  type(loop_iterator_t) :: iterator
  integer, allocatable :: i(:)

  iterator = loop_iterator_t([1,5,1],[3,1,1],[1,-2,1])
  do
    if (.not. iterator%has_next_element()) exit
    i = iterator%get_next_element()
    print *, i
  end do

end program
