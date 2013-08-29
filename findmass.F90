subroutine findmass(m)
!Returns total mass (m) from common array rho
  implicit none
  include 'runhydro.h'
 
!*  Global Variables
   real, dimension(numr,numz,numphi) :: pot, rho
   common /poisson/ pot, rho
!*  
   real :: m, dr,pi,r
   integer :: i,j, count
!*   
   
   Pi=3.14159265359 
   dr=1.0/(numr-1)
   m=0.0
   count=0
   
   
   do i=2,ax
     do j=2,by
        r=(i-1.5)*dr
        m=m+rho(i,j,1)*2*pi*r*dr**2
          
        
        
          if (rho(i,j,1).gt.0) then 
            count=count+1
          endif
        
        
        
!        if (j==2) then
!        endif
     enddo
   enddo

   m=m*2
   print*,"findmass mass=", m, "masscount", count 
end subroutine findmass
   
