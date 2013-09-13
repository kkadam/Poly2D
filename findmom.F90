subroutine findmom(mom)
!Returns moment of inertia (rav) NOT angular momentum 
!from common array rho
  implicit none
  include 'runhydro.h'
 
!*
!*  Global Variables

   real, dimension(numr,numz,numphi) :: pot, rho
   common /poisson/ pot, rho
!*
!*  Local Variables  
   real, dimension(numr,numz,numphi) :: psi
   real :: m, dr, mom, pi, r, Re
   integer :: i,j
   
   
!  print*, ">>> findmom"   
   pi=3.14159
   
   dr=1.0/(numr-1)
   Re=(ax-1.5)/(numr-1.5)
   m=0.0
   mom=0.0
   
   do i=2,ax
     do j=2,numz  !was by
        r=(i-1.5)*dr
        m=rho(i,j,1)*2*pi*r*dr**2
        mom=mom+m*r**2
     enddo
   enddo
   
   mom=mom*2/Re**5
   
!   print*,"findmom mom=",mom
   
end subroutine findmom
