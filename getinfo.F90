subroutine getinfo(h_0,c_0,h_max,count,cput)
!Prints summary of the run on the screen and in a file 
  implicit none
  include 'runhydro.h'

  real::rav, mom, m, vol, h_0,c_0,am ,w, mac_x,mac_y,pi,rb,h_max,p_max,&
  cput
  character(len=100) :: filename
  character*20 char_np, char_ax, char_by, char_numr, char_numz, char_m  
  character*20 char_vol, char_rav, char_mom, char_h_0, char_am, char_rb,&
  char_p_max,char_mac_x,char_mac_y,char_cput,char_count
  integer :: count
  
  Pi=3.14159265359
  rb=(by*1.0)/ax
  
  call findmass(m)
  call findvol(vol)
  call findmom(mom)  
  
  rav=m/vol
  
  w=(abs(h_0))**(0.5)
  am=mom*w
  
  mac_y=h_0/4.0/pi
  mac_x=am**2/(4.0*pi*m**(10.0/3)*rav**(-1/3.0))
  
  p_max=h_max/(1.0+np)
  
  
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
  write (char_numz, "(I3)") numz
  write (char_m, "(F6.4)") m
  write (char_vol, "(F6.4)") vol
  write (char_rav, "(F6.4)") rav
  write (char_mom, "(F6.4)") mom
  write (char_h_0, "(F8.6)") h_0
  write (char_am, "(F8.6)") am
  write (char_rb, "(F6.4)") rb
  write (char_p_max, "(F6.4)") p_max
  write (char_mac_x, "(F6.4)") mac_x
  write (char_mac_y, "(F6.4)") mac_y
  write (char_count, "(I2)") count
  write (char_cput, "(F8.4)") cput
  
  
  
!!Make filename	
  filename='n='//trim(char_np)//'_'//trim(char_ax)//"x"//&
  trim(char_by)//"_"//trim(char_numr)//".info"   
  
  print*,"==================================SUMMARY======================================"
  print*,"Polytropic index = ", char_np
  print*,"Resolution = ", trim(char_numr),"x", trim(char_numz)
  print*,"b/a = ", trim(char_by), "/", trim(char_ax)
  print*,"   rb   ","  Omega_sq  ","    M     ", "    V    ","    J    "&
  ,"    p_max    ","  mac_x   ","   mac_y"
  print*," ",trim(char_rb),"   ",trim(char_h_0),"    ",trim(char_m),"    "&
  , trim(char_vol), "   ",trim(char_am),"   ",trim(char_p_max),"    ",&
  trim(char_mac_x), "     ", trim(char_mac_y)
  
  print*,"cpu time =", cput , "min"
  
  print*,"================================OUTPUT FILES==================================="  
  
  
  
  
  
  open(unit=10,file=filename)
  write(10,*) trim(char_np)," ",trim(char_numr), " ",trim(char_numz)," ", &
  trim(char_by)," ",trim(char_ax)," ",trim(char_rb)," ",trim(char_h_0),&
  " ",trim(char_m)," ", trim(char_vol), " ",trim(char_am)," ",trim(char_p_max),&
  " ",trim(char_mac_x)," ", trim(char_mac_y)," ",trim(char_count)," ",&
  trim(char_cput)
  close(10)

  print*, trim(filename)


end subroutine getinfo
