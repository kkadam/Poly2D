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
   real :: m, dr, mom, pi, r, Re, count
   integer :: i,j
   
   
!  print*, ">>> findmom"   
   pi=3.14159
   
   dr=1.0/(ax-1.5)
   Re=1.0!(ax-1.5)/(numr-3.0)
   m=0.0
   mom=0.0
   
   count=2.0
   do i=2,ax
     do j=2,numz  !was by
        r=(count-1.5)*dr
        m=rho(i,j,1)*2*pi*r*dr**2
        mom=mom+m*r**2
     enddo
     count=count+1.0
   enddo
   
   mom=mom*2/Re**5
   
!   print*,"findmom mom=",mom
   
end subroutine findmom
