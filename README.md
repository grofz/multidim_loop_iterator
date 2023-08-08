# Multi-Dimensional Loop Iterator for Fortran

The module provides a versatile iterator for multi-dimensional loops with
customizable lower bounds, upper bounds, and strides for each dimension. It
simplifies the process of iterating over multi-dimensional arrays by
abstracting the complexities of nested loops.

## Usage

1. File `loop.f90` provides a module `loop_mod` that can be used in your
   Fortran program:

   ```fortran
   use loop_mod, only : loop_iterator_t
   ```

2. Creating an Iterator: Use constructor to create an iterator instance.
   Provide arrays for lower bounds, upper bounds, and strides for each dimension

   ```fortran
   type(loop_iterator_t) :: my_iterator
   integer, dimension(3) :: lbounds = [1, 1, 1]
   integer, dimension(3) :: ubounds = [5, 10, 3]
   integer, dimension(3) :: strides = [1, 2, 1]

   my_iterator = loop_iterator_t(lbounds, ubounds, strides)
   ```

3. Iterating:

Use `has_next_element()` method to check if there are more elements to iterate.

Use `get_next_element()` method to retrieve the indices of the next element in
the loop.


## Example

Here is a simple example of using the `loop_mod` iterator:

```fortran
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
```

Another example in file `example.f90` showcases the capabilities of the
`loop_mod` module. It iterates through dimensions and calculates the count of
k-th closest neighbours of an n-dimensional hypercube.

A simple script to build these examples is provided. The script assumes the
`gfortran` compiler is available.

```console
./build.sh
./example
```

## License

This Fortran module is provided under the MIT License.
