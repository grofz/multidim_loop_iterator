  program count_nearest_neighbours
    use loop_mod, only : loop_iterator_t
    implicit none

    integer, parameter :: MAX_DIM = 10
    integer, allocatable, dimension(:) :: i, closest_ngb_cnt, lb, ub, st
    integer :: nd
    type(loop_iterator_t) :: iterator

    ! Loop over all possible dimensions
    do nd = 1,MAX_DIM
      if (nd/=1) print *
      if (allocated(i)) deallocate(i)
      if (allocated(closest_ngb_cnt)) deallocate(closest_ngb_cnt)
      if (allocated(lb)) deallocate(lb)
      if (allocated(ub)) deallocate(ub)
      if (allocated(st)) deallocate(st)
      allocate(i(nd), closest_ngb_cnt(nd), lb(nd), ub(nd), st(nd))

      ! Count k-th closests neighbours of a "nd"-dimensional hypercube
      closest_ngb_cnt = 0
      lb = -1
      ub = 1
      st = 1
      iterator = loop_iterator_t(lb, ub, st)
      MAIN_LOOP: do
        if (.not. iterator % has_next_element()) exit MAIN_LOOP
        i = iterator % get_next_element()
        associate(k=>sum(abs(i)))
          if (k>0) closest_ngb_cnt(k) = closest_ngb_cnt(k)+1
        end associate
      end do MAIN_LOOP

      print '("No of kth-closest neighbours ",*(i0,1x))', closest_ngb_cnt
      print '("Total neighbours in ",i0,"D space is ",i0,". Validation",l2)', &
          nd, sum(closest_ngb_cnt), sum(closest_ngb_cnt)==3**nd-1
    end do
  end program
