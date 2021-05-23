subroutine doMove(NLINE, lSize, nTube, i, j, move, nmove, matrix)
  implicit none
  integer :: NLINE, i, j, nmove, nTube, lSize, movingIndex
  integer, dimension(nTube, NLINE) :: matrix
  integer, dimension(lSize, 2) :: move

  integer :: moving, nempty, ntomove
  movingIndex = 1 ! matrix(i,movingIndex) is the moving color
  do while(matrix(i,movingIndex) == 0)
     movingIndex = movingIndex + 1
  end do
  moving = matrix(i,movingIndex)

  ! do the movement
  nmove = nmove + 1
  move(nmove, 1) = i
  move(nmove, 2) = j

  ! calculate new colors
  nempty = count(matrix(j,:) == 0)
  ntomove = 1
  if (movingIndex /= NLINE) then
     do while(moving == matrix(i, movingIndex + ntomove))
        ntomove = ntomove + 1
     end do
  end if

  if (ntomove > nempty) then
     ntomove = nempty
     matrix(i, movingIndex : (movingIndex + ntomove - 1) ) = 0 ! becomes empty
     matrix(j, 1:nempty) = moving ! gets `moving` color
  else
     matrix(i, movingIndex : (movingIndex + ntomove - 1) ) = 0 ! becomes empty
     matrix(j, (nempty - ntomove + 1):nempty) = moving
  end if
end subroutine doMove
