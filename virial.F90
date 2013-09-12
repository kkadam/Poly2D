subroutine virial(T,W,P,omega)
  implicit none
  include 'runhydro.h'

  real, dimension(numr,numz,numphi) :: pot, rho
  common /poisson/ pot, rho  

  real, dimension(numr,numz,numphi) :: enth
  common /vir/enth  
  
  
  real :: T, W, P, omega, r, m, dr, pi, press
  integer :: i,j
  
  Pi=3.14159265359
  
  dr=1.0/(numr-1)
  print*, dr
  !Find rotational energy T
  T=0.0
  do i=2,ax
     do j=2,numz  !was by
        r=(i-1.5)*dr
        m=rho(i,j,1)*2*pi*r*dr**2
        T=T+0.25*m*r**2*omega**2
     enddo
   enddo  
  
  !Find Potential energy W
  W=0.0
  do i=2,ax
     do j=2,numz  !was by
        r=(i-1.5)*dr
        W=W-0.5*pot(i,j,1)*rho(i,j,1)*2*pi*r*dr**2
     enddo
  enddo   	  
  
  !Find pressure energy P
  P=0.0
  do i=2,ax
     do j=2,numz  !was by
        r=(i-1.5)*dr
        press=rho(i,j,1)*enth(i,j,1)/(1.0+np)
        P=P+press*2*pi*r*dr**2
     enddo
   enddo  
   
  return
end subroutine virial
