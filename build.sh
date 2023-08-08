#! /bin/bash

set -xe

FLAGS="-Wall -Wextra -pedantic -std=f2018 -fcheck=all"

gfortran ${FLAGS} -o example loop.f90 example.f90
