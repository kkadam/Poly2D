subroutine rhoavg(rav)
  implicit none
  include 'runhydro.h'
 
!*
!*  Global Variables

   real, dimension(numr,numz,numphi) :: pot, rho
   common /poisson/ pot, rho
!*
!*  Local Variables  
   real :: m, dr, rav,pi,vol,r
    integer :: i,j
  Pi=3.14159265359
  
   
   
   dr=1.0/(numr-1)
   vol=0.0
   
   do i=2,ax
     do j=2,by
        r=(i-1.5)*dr
        if (rho(i,j,1).gt.0) then
          vol=vol+2*pi*r*dr**2
        endif
     enddo
   enddo 
   
   vol=vol*2
   
   call findmass(m)
   
   print*,m
   rav=m/vol
   
   print*, "rhoavg vol=",vol
   print*, "rhoavg density=",rav
   
   
end subroutine rhoavg
   
