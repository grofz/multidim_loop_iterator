module loop_mod
  implicit none
  private

  type, public :: loop_iterator_t
    private
    integer :: ndims
    integer, allocatable, dimension(:) :: lbounds, ubounds, strides, indices
    logical :: has_next = .false.
  contains
    procedure :: get_next_element
    procedure :: has_next_element
  end type
  interface loop_iterator_t
    module procedure new_iterator
  end interface

contains

  function get_next_element(this) result(indices)
    class(loop_iterator_t), intent(inout) :: this
    integer :: indices(this%ndims)

    integer :: d

    ! Return actual element and update iterator to point to the
    ! next element
    if (.not. this%has_next) &
        error stop 'iterator does not have next element'

    indices = this%indices

    ! Remove indices from the stack "i|d" until found an index
    ! that can be incremented. Increment it and fill the stack
    ! back to its full size by lower bound indices.
    d = 1
    do
      if (this%indices(d)+this%strides(d) <= this%ubounds(d)) then
        this%indices(d) = this%indices(d) + this%strides(d)
        this%indices(:d-1) = this%lbounds(:d-1)
        exit
      else
        d = d + 1
        if (d <= this%ndims) cycle
        this%has_next = .false.
        exit
      end if
    end do
  end function get_next_element


  function has_next_element(this) result(has)
    class(loop_iterator_t), intent(in) :: this
    logical :: has

    has = this%has_next
  end function has_next_element


  function new_iterator(lbounds, ubounds, strides) result(new)
    integer, intent(in), dimension(:) :: lbounds, ubounds, strides
    type(loop_iterator_t) :: new

    if (size(lbounds)/=size(ubounds) .or. size(ubounds)/=size(strides)) &
        error stop 'all input arrays must be of same size'

    new%ndims = size(lbounds)
    ! explicit allocation just to avoid -Wunitialized warning of gfortran
    allocate(new%lbounds(size(lbounds)))
    allocate(new%ubounds(size(lbounds)))
    allocate(new%strides(size(lbounds)))
    allocate(new%indices(size(lbounds)))
    new%lbounds = lbounds
    new%ubounds = ubounds
    new%strides = strides

    new%indices = lbounds
    ! Cf. the formula for the number of iterations for a do loop
    ! e.g. in "Modern Fortran Explained, Sec 4.4."
    new%has_next = all(max(0, (ubounds-lbounds+strides)/strides)>0)
  end function new_iterator

end module loop_mod
