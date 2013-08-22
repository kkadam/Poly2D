subroutine rhoavg(rav)
  implicit none
  include 'runhydro.h'
 
!*
!*  Global Variables

   real, dimension(numr,numz,numphi) :: pot, rho
   common /poisson/ pot, rho
!*
!*  Local Variables  
   real :: m, dr, rav
   integer :: i,j
   
   
   dr=1.0/(numr-1)
   vol=0.0
   
   do (i=2,ax)
     do (j=2,by)
        r=(i-1.5)*dr
        if (rho(i,j,1).gt.0) then
          vol=2*pi*r*dr**2
        endif
     enddo
   enddo 
   
   call findmass(m)
   
   rav=m/vol
   
   
end subroutine findmass
   
