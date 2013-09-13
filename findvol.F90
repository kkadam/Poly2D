subroutine findvol(vol)
!Returns average value of density (rav) and total volume (vol) 
!from common array rho 
  implicit none
  include 'runhydro.h'
 
!*
!*  Global Variables

   real, dimension(numr,numz,numphi) :: pot, rho
   common /poisson/ pot, rho
!*
!*  Local Variables  
   real :: m, dr, pi,vol,r,Re
   integer :: i,j,count
    
    
!  print*, ">>> rhoavg"
  Pi=3.14159265359
  count=0
   
   
   dr=1.0/(numr-1)
   Re=(ax-1.5)/(numr-1.5)
   vol=0.0
   
   do i=2,ax
     do j=2,numz  !was by
        r=(i-1.5)*dr
        if (rho(i,j,1).gt.0.0) then
          vol=vol+2*pi*r*dr**2
          !count=count+1
        endif
     enddo
   enddo 
   
   vol=vol*2/Re**3
   
!   print*, "rhoavg vol=",vol,"volcount", count
!   print*, "rhoavg density=",rav
   
   
end subroutine findvol
   
