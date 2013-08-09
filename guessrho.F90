subroutine guessrho
  implicit none
  include 'runhydro.h'

  real, dimension(numr,numz,numphi) :: pot, rho
  common /poisson/ pot, rho
  
  real, dimension(numr,numz,numphi) :: phi, phi1
!Initialize  
  integer:: i,j,k
  double precision:: radius, Pi, angle, M, G,r, slope, c
  real:: den
  

  

  den=1.0
  G=1
  
  
!Allocate arrays  
  

!ar,az,br,bz are defined in runhydro.h
  slope=-by*1.0/ax	
  c=by*1.0  
  
!  radius=ax
  Pi=3.14159265359
  
  
!  print*, slope, c
  
!Create rho array  
	
  do i=1,numr
    do j=1,numz
      do k=1,numphi
!        angle=k*2*Pi/numphi
!        r=((i*cos(angle)-0.5)**2+(i*sin(angle)-0.5)**2+(j+(radius-c-0.5))**2)**0.5
!        if (r**2.lt.radius**2) then
!          rho(i,j,k)=den   
        if (j .lt. slope * i + c) then
          rho(i,j,k)=den       
!	if ((i.le.ax).and.(j.le.by)) then
!	  rho(i,j,k)=den
        else
          rho(i,j,k)=0.0
        endif
      enddo
    enddo
  enddo
 
!Find mass
!  radius=radius/ax	
!  c=c/ax	
	
!  M=den*4/3.0*Pi*radius**3	
	
!  print*, M	
	
!Create phi array which is theoretical solution to the Poisson solver
!  do i=1,numr
!    do j=1,numz
!      do k=1,numphi
!        angle=k*2*Pi/numphi
!        r=(((i*cos(angle)-0.5)**2+(i*sin(angle)-0.5)**2+(j+(radius-c-0.5))**2)**0.5)/ax
!        if (r**2.lt.radius**2) then
!          phi(i,j,k)= -G*M*(3*radius**2-r**2)/2/radius**3          	  
!        else
!          phi(i,j,k)= -G*M/r
!        endif
       
!	if (j==numz/2) then
!	  print*, angle
!	endif
!      enddo
!    enddo
!  enddo	
	
	
!Latest calculations
	
!      do i=1, numr
 !       do j=1, numz
  !        r=(i**2+j**2)**(0.5)/ax
   !       if (r.lt.1) then
    !        phi1(i,j,1)= -2.0/3 *pi *(3-r**2)	
     !     else
      !      phi1(i,j,1)=-4.0*pi/3/r
       !   endif
        !enddo
      !enddo
  
  
  
  
  
  
      open(unit=10,file='rho.txt')
      do j=1,numz
        do i=1,numr  
          write(10,*) i,j,rho(i,j,1) 
        enddo
        write(10,*)
      enddo
      close(10)
      print*,"First guess of density rho.txt printed" 
      
!      open(unit=10,file='rho.txt')
 !       do i=1,numr  
  !        
   !       write(10,*) rho(i,1,1) 
    !    enddo
     ! close(10)
      !print*,"Cross section file rho.txt printed"
      
      
      
!      open(unit=10,file='ana.txt')
 !     do j=1,numz
  !      do i=1,numr  
   !       write(10,*) i,j,phi(i,j,1) 
    !    enddo
     !   write(10,*)
      !enddo
!      close(10)
 !     print*,"Analytically solved potential ana.txt printed" 

 	 
!      open(unit=10,file='ana.txt')
 !       do i=1,numr  
  !        write(10,*) phi(i,1,1) 
   !     enddo
    !  close(10)
     ! print*,"Cross section file ana.txt printed" 	 
 	 
 	 
!      open(unit=10,file='ana1.txt')
 !       do i=1,numr  
  !        write(10,*) phi1(i,1,1) 
   !     enddo
    !  close(10)
     ! print*,"Cross section file ana1.txt printed"
      
      
  
  return
end subroutine guessrho
