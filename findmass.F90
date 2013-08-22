subroutine findmass
  implicit none
  include 'runhydro.h'
 
!*
!*  Global Variables

   real, dimension(numr,numz,numphi) :: pot, rho
   common /poisson/ pot, rho
  
   real :: m, dr
   integer :: i,j
   
   
   dr=1.0/(numr-1)
   m=0.0
   
   do (i=2,ax)
     do (j=2,by)
        r=(i-1.5)*dr
        m=rho(i,j,1)*2*pi*r*dr**2
        if (j==2) then
          print *, i,j,r
        endif
     enddo
   enddo
   
   print*,m
   m=m*2
   print*,m
end subroutine findmass
   
