#!/bin/bash

source /opt/intel/oneapi/setvars.sh > /dev/null 2>&1
ifort -O3 src/*.f90 -o hello-world
./hello-world