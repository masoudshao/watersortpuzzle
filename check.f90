subroutine CheckAndMove(NLINE, lSize, nTube, i, j, move, nmove, matrix, &
     movingIndex, illegal, history, fail)
  implicit none
  integer :: i,j, nmove, NLINE, lSize, nTube, movingIndex
  integer, dimension(nTube, NLINE) :: matrix
  integer, dimension(lSize, 2) :: move
  integer, dimension(lsize, 2, (2*nTube)), intent(in) :: illegal
  integer, dimension(ntube, nline, lsize):: history
  logical :: fail
  external :: doMove
  integer :: moving, k, temp

  moving = matrix(i, movingIndex)
  fail = .true.
  
  if (j == i) then! cannot pour it itself
     return
  elseif (matrix(j, 1) /= 0) then ! dest tube is full
     return
  elseif ((matrix(j,1) == 0) .and. (matrix(j,2) /= 0) .and. (matrix(j,2) /= moving)) then
     return ! dest tube has 1st empty but second has different color (and is not empty)
  elseif ((matrix(j,2) == 0) .and. (matrix(j,3) /= 0) .and. (matrix(j,3) /= moving)) then
     return ! dest tube has 1st and 2nd empty but third has different color (and is not empty)
  elseif ((matrix(j,3) == 0) .and. (matrix(j,4) /= 0) .and. (matrix(j,4) /= moving)) then
     return ! dest has hast 1st, 2nd and 3rd emtpy but fourth is different color (and is not empty)
  end if
  ! if there is a place but ther has already been 'a last move' and...
  if (nmove /= 0) then
     ! ... this is going to be the mirror of the last move, then 
     if ((move(nmove,1) == j) .and. (move(nmove,2) == i)) then
        return
     end if
  end if

  ! if this move is from one of the illegal movements
  k = 1
  do while(illegal(nmove+1, 1, k) /= 0)
!     write(6, '(3(a,i3),i3)') "k ", k, ", nmove+1", nmove+1, ", illegal(k,:,nmove+1)", illegal(nmove+1,2,k)
     if ((illegal(nmove +1, 1, k) == i) .and. (illegal(nmove +1, 2, k) == j)) then
!        write(6, *) " this move is illegal"
        return
     end if
     k = k +1
  end do

  ! this check ensures half full bottles don't get swapped unnecessariliy
  if (movingIndex > 1) then
     if (all(matrix(j,:) == 0)) then
        if (all(matrix(i, 1 : (movingIndex-1) ) == 0 )) then
           return
        end if
     end if
  end if

  ! apparently moving 1/2,1/3 or 2/3 of colors into a half-full
  ! block is not a good idea. Because this can create different
  ! snapshots of history with additional and unnecessary move-
  ! ments that just increase the total number of moves
  temp = count(matrix(j,:) == 0) ! number of empty
  k = 1 ! number of blocks to move
  if (movingIndex /= NLINE) then
     do while(moving == matrix(i, movingIndex + k))
        k = k + 1
     end do
  end if
  if (temp < k) return
  
  ! the only other check is to make sure a cycle is not created. So I have to use
  ! a history. First, do the move, 
  call domove(NLINE, lSize, nTube, i, j, move, nmove, matrix)
  ! but check if after this move we will end up in a situation that we were before
  temp = 1
  if (nmove > 1) then ! at least two moves should have been made by now
     do temp = 1, nmove - 1 !nmove+1 is new state, nmove was the state before movement
        if (all(history(:,:,temp) == matrix)) then
           ! if such a new state already exist, first move back
           !call domove(NLINE, lSize, nTube, j, i, move, nmove, matrix)
           ! reset nmove
           nmove = nmove - 1
           matrix = history(:,:,nmove)
           ! and quit
           return
        end if
        ! also check for other states (let the loop continue)
     end do
  else
     ! if reached here, all checks have been passed and the first move was made.
     ! no need to do anything esle. (fail will be false after this block)
  end if

  ! if reached here, al checks were passed and a new move has been already made.
  ! Happy moving.
  fail = .false.
end subroutine CheckAndMove
