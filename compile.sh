#!/bin/bash
set -e
FC=gfortran

rm -rf a.out* *o *mod *genmod*

$FC main.f90 readData.f90 check.f90 move.f90 toUpper.f90 lookFor.f90

