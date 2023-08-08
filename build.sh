#! /bin/bash

set -xe

FLAGS="-Wall -Wextra -pedantic -std=f2018 -fcheck=all"

gfortran ${FLAGS} -o example loop.f90 example.f90
gfortran ${FLAGS} -o example2 loop.f90 example2.f90
gfortran ${FLAGS} -o example3 loop.f90 example3.f90
