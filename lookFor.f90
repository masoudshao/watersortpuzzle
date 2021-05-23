subroutine lookForPlace(NLINE, lSize, nTube, i, move, nmove, matrix, &
     success, illegal, history)
  implicit none
  integer :: NLINE, lSize, nTube, i, nmove, movingIndex
  integer, dimension(nTube, NLINE) :: matrix
  integer, dimension(lsize, 2, (2*nTube)), intent(in) :: illegal
  integer, dimension(ntube, nline, lsize):: history
  integer, dimension(lSize, 2) :: move
  external :: doMove, printall, CheckAndMove
  logical :: fail, success
  integer :: moving, j

  movingIndex = 1 ! matrix(i,movingIndex) is the moving color
  do while(matrix(i,movingIndex) == 0)
     movingIndex = movingIndex + 1
  end do
  moving = matrix(i, movingIndex)

  success = .false.
  do j = 1, nTube
     call CheckAndMove(NLINE, lSize, nTube, i, j, move, nmove, matrix, movingIndex,&
          illegal, history, fail)
     if (fail) cycle
     success = .true.
     exit
  end do
end subroutine lookForPlace

