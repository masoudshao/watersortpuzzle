subroutine readData(inputfile, NLINE, L, separator, ncolor, ntube, matrix, maps)
  implicit none
  character(len=*), intent(in) :: inputfile
  integer, intent(in) :: NLINE
  integer, intent(in) :: L
  integer, intent(in) :: SEPARATOR
  integer, intent(in) :: ncolor, ntube
  integer , dimension(ntube,NLINE), intent(inout) :: matrix
  character(len= L), dimension(1:ncolor), intent(inout) :: maps
  character(len = L) :: c1, c2
  character(len = L), external :: to_upper

  integer :: i, j, k, kk, iunin
  
!  kk = 0
!  write(c1, *) "Empty"
!  c2 = adjustl(to_upper(c1))
!  maps(kk) = c2

  open(newUnit = iunin, file = trim(inputFile), status = 'old')
  matrix = 0
  kk = 0
  do i = 1, nColor
     do j = 1, NLINE
        read(iunin, *) c1
        c2 = adjustl(to_upper(c1))
        k = findloc(maps, c2, 1)
        if (k == 0) then
           kk = kk + 1
           maps(kk) = c2
           k = kk
           if (k > nColor) then
              write(0,*) 'error'
              stop
           end if
        end if
        matrix(i,j) = k

     end do
     do j = 1, SEPARATOR
        read(iunin,*, end = 101)
     end do
  end do

101 continue
end subroutine readData

subroutine printall(NLINE,L,matrix,maps,nTube)
  integer::i,j,nTube,L,NLINE
  integer,dimension(nTube,NLINE)::matrix
  character(len = L),dimension(nTube-2)::maps
  do i = 1, nTube
     do j = 1, NLINE
        if (matrix(i,j) == 0) then
           write(6, '(a,a2,i2,a1)') "EMPTY", ' (', 0 , ')'
        else
           write(6, '(a,a2,i2,a1)') trim(maps(matrix(i,j))), ' (', matrix(i,j) , ')'
        end if
     end do
     write(6, *) ' ----- '
  end do

end subroutine printall
