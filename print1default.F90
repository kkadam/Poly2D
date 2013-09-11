subroutine print1default(inarray,axis,inum)
  implicit none
  include 'runhydro.h'


  real, dimension(numr,numz,numphi) :: inarray
  integer :: i,j,k,inum
  character(len=*) :: axis  
  character(len=100) :: filename
  character*20 char_np, char_ax, char_by, char_numr  , char_bx
  
!!Convert numbers to strings
  write (char_np, "(F3.1)") np

  if (ax.lt.100) then
    write (char_ax, "(I2)") ax
  else
    write (char_ax, "(I3)") ax
  endif

  if ((by.lt.100).and.(by.gt.9)) then
    write (char_by, "(I2)") by
  elseif (by.lt.10) then  
    write (char_by, "(I1)") by
  else
    write (char_by, "(I3)") by
  endif

  if ((bx.lt.100).and.(bx.gt.10)) then
    write (char_bx, "(I2)") bx
  elseif (bx.lt.10) then  
    write (char_bx, "(I1)") bx
  else
    write (char_bx, "(I3)") bx
  endif 

  write (char_numr, "(I3)") numr

!!Write array  
  if (axis=="x") then
    if (bx==2) then
      filename='n='//trim(char_np)//'_'//trim(char_ax)//"x"//trim(char_by)&
      //"_"//trim(char_numr)//"_x.1"
    else
      filename='n=-'//trim(char_np)//'_'//trim(char_ax)//"x"//trim(char_bx)&
      //"_"//trim(char_numr)//"_x.1"      
    endif
    
    open(unit=10,file=filename)
      do i=1,numz  
        write(10,*) inarray(inum,i,1) 
      enddo
    close(10) 
    
  elseif (axis=="y") then
    if (bx==2) then  
      filename='n='//trim(char_np)//'_'//trim(char_ax)//"x"//trim(char_by)//&
      "_"//trim(char_numr)//"_y.1"
    else
      filename='n=-'//trim(char_np)//'_'//trim(char_ax)//"x"//trim(char_bx)//&
      "_"//trim(char_numr)//"_y.1"
    endif      
      open(unit=10,file=filename)
      do i=1,numr  
        write(10,*) inarray(i,inum,1) 
      enddo
    close(10) 
  
  else
    print*,"Sum Ting Wong in print1d!"
  
  endif
  
  print*,trim(filename)
  
end subroutine print1default
