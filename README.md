# watersortpuzzle
Solves the "water sort puzzle" game for Android.

## Description:
A solution is sought using line search. The solution may not be optimum in terms of number of movements however, any unnecessary move is avoided. 

## Installation:
Get a fortran compiler (gfortran > v9.1.0, ifort, whatever) and run the super simple script `compile.sh`. Change the fortran compilor in the script if needed.

```
./compile.sh
```

## Instructions:
An input file is needed in form of:

color a
color b
color c
color d
<empty line>
color e
color f
color g
color h
<empty line>
...

where the first four lines correspond to first block, etc and the number of blocks is the number of tubes that one sees initially in the game. Hence the number of blocks to be filled is equal to the number of colors. Lines in the input line can be anything but there must be nColor distinct lines (case in-sensitive).

The file `output` has instruction to solve the puzzle.

## Example:
See input file `input` for example input file. Here is what you see.

```
$ ./a.out                                                                                                                      [15:18:24]
Enter number of tubes: 14
 nTube =           14
 nColor =           12
input file: input
input"
```

## License: 
WTFPL :)