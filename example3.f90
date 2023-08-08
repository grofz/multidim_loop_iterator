  program example3
    use loop_mod, only : loop_iterator_t
    implicit none

    integer, parameter, dimension(3) :: NDIMS = [12,4,6]
    integer :: arr(NDIMS(1), NDIMS(2), NDIMS(3))

    integer, allocatable :: ind(:), ngb(:)
    type(loop_iterator_t) :: elements, neighbors

    ! Store the number of neighboring elements to the array
    arr = 0
    elements = loop_iterator_t([1,1,1], NDIMS, [1,1,1])
    do
      if (.not. elements%has_next_element()) exit
      ind = elements%get_next_element()

      neighbors = loop_iterator_t(ind-1, ind+1, [1,1,1])
      do
        if (.not. neighbors%has_next_element()) exit
        ngb = neighbors%get_next_element()

        ! ignore out-of-bounds neighbors
        if (any(ngb<1) .or. any(ngb>NDIMS)) cycle

        ! ignore array element itself, 2nd and 3rd closest ngbs
        if (sum(abs(ngb-ind))/=1) cycle

        ! "ngb" now reference a valid element in array that
        ! shares a wall with "ind" element
        ! just try to access it (check no array out of bounds)
        if (arr(ngb(1),ngb(2),ngb(3)) == -1) print '("Booo")'
        arr(ind(1),ind(2),ind(3)) = arr(ind(1),ind(2),ind(3))+1
      end do
    end do

    ! verify the counts are correct
    associate(n =>count(arr>6 .or. arr<3), &
              n6=>count(arr==6), &
              n5=>count(arr==5), &
              n4=>count(arr==4), &
              n3=>count(arr==3), &
              nd=>NDIMS-2)
      print '("6 neighbors = ",i0," valid?",l2)', n6, &
          n6 == product(nd)
      print '("5 neighbors = ",i0," valid?",l2)', n5, &
          n5 == 2*nd(1)*nd(2) + 2*nd(2)*nd(3) + 2*nd(3)*nd(1)
      print '("4 neighbors = ",i0," valid?",l2)', n4, n4 == 4*sum(nd)
      print '("3 neighbors = ",i0," valid?",l2)', n3, n3 == 8
      print '("Total number of items ",i0," valid?",l2)', &
          n+n3+n4+n5+n6, n+n3+n4+n5+n6 == size(arr)
      print '("Less than 3 or more than 6 neighbors = ",i0," valid?",l2)', &
          n, n == 0
    end associate


  end program example3
