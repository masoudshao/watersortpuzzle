#!/bin/bash
set -e

rm -rf a.out*
rm -f *o
rm -f *mod
rm -f *genmod*

ifort -static main.f90 readData.f90 check.f90 move.f90 toUpper.f90 lookFor.f90

