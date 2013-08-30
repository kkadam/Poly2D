subroutine getinfo(h_0,c_0)
!Prints summary of the run on the screen and in a file 
  implicit none
  include 'runhydro.h'

  real::rav, mom, m, vol, h_0,c_0,am ,w   
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
  filename='n='//trim(char_np)//'_'//trim(char_ax)//"x"//trim(char_by)//"_"//trim(char_numr)//".info" 
  
  call findmass(m)
  call findvol(vol)
  call findmom(mom)  
  
  rav=m/vol
  
  w=(abs(h_0))**(0.5)
  am=mom*w
  
  print*,"==================SUMMARY======================"
  print*,"Polytropic index =", np
  print*,"Resolution =", numr,"x", numz
  print*,"Axis ratio =", ax, "x", by
  print*,"Total mass =",m, "Total volume =", vol
  print*,"Average density =", rav, "MI =", mom
  if (h_0.lt.0) then
    print*,"Angular velocity = -",w, "Angular momentum =", am
  else
    print*,"Angular velocity =",w, "Angular momentum =", am
  endif


  open(unit=10,file=filename)
    write(10,*) "==================SUMMARY======================"
    write(10,*) "Polytropic index =", np
    write(10,*) "Resolution =", numr,"x", numz
    write(10,*) "Axis ratio =", ax, "x", by
    write(10,*) "Total mass =",m, "Total volume =", vol
    write(10,*) "Average density =", rav, "MI =", mom
    if (h_0.lt.0) then
      write(10,*) "Angular velocity = -",w, "Angular momentum =", am
    else
      write(10,*) "Angular velocity =",w, "Angular momentum =", am
    endif
  close(10)

  print*, "File ",trim(filename)," printed"





end subroutine getinfo
