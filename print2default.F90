subroutine print2default(inarray)
  implicit none
  include 'runhydro.h'

  real, dimension(numr,numz,numphi) :: inarray
  integer :: i,j,k
  character(len=100) :: filename
  character*20 char_np, char_ax, char_by, char_numr  
  
!!Convert numbers to strings
  write (char_np, "(F3.1)") np

  if (ax.lt.100) then
    write (char_ax, "(I2)") ax
  else
    write (char_ax, "(I3)") ax
  endif

  if (by.lt.100) then
    write (char_by, "(I2)") by
  else
    write (char_by, "(I3)") by
  endif

  write (char_numr, "(I3)") numr

!!Make filename	
  filename='n='//trim(char_np)//'_'//trim(char_ax)//"x"//trim(char_by)//"_"//trim(char_numr)//".2"


!!Write array
  open(unit=10,file=filename)
    do j=1,numz
       do i=1,numr
          write(10,*) i,j,inarray(i,j,1)
       enddo
       write(10,*)
    enddo
  close(10)

  print*, trim(filename)

end subroutine print2default



























