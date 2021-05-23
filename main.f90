program watersortpuzzlesolver
  implicit none
  character(len=64) :: inputfile
  integer, parameter :: NLINE = 4
  integer, parameter :: L = 30
  integer, parameter :: SEPARATOR = 1
  integer :: ncolor, ntube
  integer, dimension(:,:), allocatable :: matrix
  character(len = L), dimension(:), allocatable :: maps

  integer :: i, j, k, nmove
  integer, dimension(:,:), allocatable :: move
  integer, dimension(:,:,:), allocatable :: illegal, history
  integer :: lSize
  logical, external :: doCheck
  logical :: success, moveSuccess
  logical, external :: isFinished
  external :: doMove, readData, printall, lookForPlace

  write(6, '(A)', advance = 'no') "Enter number of tubes: "
  read(5, *) nTube
  write(6, *) "nTube = ", nTube
  nColor = nTube - 2
  write(6, *) "nColor = ", nColor
  write(6, '(a)', advance = 'no') "input file: "
  read(5, *) inputfile
  write(6, '(a,a1)') trim(inputfile), '"'
  allocate(matrix(nTube, NLINE))
  allocate(maps(1:nColor))
  lSize = 400
  allocate(move(1:lSize, 2))
  allocate(illegal(lSize, 2, (nTube*2)))
  allocate(history(nTube, NLINE, 1:lSize))

  call readData(inputFile, NLINE, L, SEPARATOR, nColor, nTube, Matrix, Maps)
  !  call printall(NLINE, L, matrix, maps, nTube)
  history = 0
  nmove = 0
  illegal(:,:,:) = 0
  success = .false.
  mainloop: do while(.not.success)
     sourceLoop: do i = 1, nTube
        ! if it is already colored
        if (all(matrix(i,1) == matrix(i,1:NLINE))) then
           cycle
        end if

        call lookForPlace(NLINE, lSize, nTube, i, move, nmove, matrix, &
             moveSuccess, illegal, history)

        if (moveSuccess) then
           !write(6, '(i3,2(a,i2))') nmove,":",move(nmove,1),' -> ',move(nmove,2)
           history(:,:,nmove) = matrix(:,:)
           !call printall(NLINE,L,matrix,maps,nTube)
           success = isFinished(NLINE, nTube, matrix)
           exit
        end if

     end do sourceLoop

     if ((i > nTube) .and. (.not.moveSuccess)) then
        !write(0,*) "warning: reached a deadend. Undoing the last move"
        i = move(nmove, 1)
        j = move(nmove, 2)
        k = 1
        do while (illegal(nmove, 1,k) /= 0)
           k = k + 1
        end do
        illegal(nmove,:,k) = move(nmove, :) !previously was from i->j
        ! purge the rest of the illegal movements after this movement
        illegal((nmove+1):lSize,:,:) = 0
        !write(6, '(a, i3)') 'illegal movements for ', nmove
        !do temp = 1, k
        !   write(6, *) temp, ':', illegal(nmove,:,temp)
        !end do
        !call doMove(NLINE, lSize, nTube, j, i, move, nmove, matrix)
        !write(6, '(a,i2,a,i2)') "   :", move(nmove,1), ' -> ', move(nmove, 2)
        nmove = nmove - 1
        matrix = history(:,:,nmove)

     end if
     if (nmove == (lSize - 1))then
        write(0, *) 'reached 99 moves. Obviously some moves should not have been done'
        stop
     end if
  end do mainloop
  open(1, file = "output")
  do i = 1, nmove
     write(1, '(i3,a1,i3,a,i3)') i, ':', move(i,1), '->' , move(i,2)
  end do
  close(1)
end program watersortpuzzlesolver

function isFinished(NLINE, nTube, matrix) result(win)
  implicit none
  integer :: NLINE, nTube
  integer, dimension(nTube,NLINE) :: matrix
  logical :: win
  integer :: i
  win = .true.
  do i = 1, nTube
     win = win .and. all(matrix(i,:) == matrix(i,1))
  end do
end function isFinished
