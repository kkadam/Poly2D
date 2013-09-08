subroutine guessrho
  implicit none
  include 'runhydro.h'

  real, dimension(numr,numz,numphi) :: pot, rho
  common /poisson/ pot, rho
  
  real, dimension(numr,numz,numphi) :: phi, phi1
!Initialize  
  integer:: i,j,k
  double precision:: radius, Pi, angle, M, G,r, slope, c, rb,imx,imy,m1,m2
  double precision:: c1,c2
  real:: den


  den=1.0
  G=1.0
  rb=by*1.0/ax
  
!Allocate arrays  
  

!ar,az,br,bz are defined in runhydro.h
  slope=-by*1.0/ax	
  c=by*1.0  
  
  radius=(ax-1.5)
  Pi=3.14159265359
  
  
!  print*, slope, c
  
!Create rho array  
if ((rb.lt.0.4).and.(rb.ge.0.15)) then
  print*, "small axis ratio = ",rb

do i=2,numr
    do j=2,numz
      if ((i.le.ax).and.(j.le.by)) then
        rho(i,j,1)=den

      else
        rho(i,j,1)=0.0
      endif
    enddo
  enddo

elseif (rb.lt.0.15) then
imx=(ax-bx)/2.0
imy=ax*0.15

print*,"imx",imx,"imy",imy

m1=(imy-by)/(imx-bx)
m2=(imy-ay)/(imx-ax)
c1=by*1.0-m1*bx
c2=ay*1.0-m1*ax
print*,m1,m2,c1,c2
 do i=1,numr
    do j=1,numz
        if ((j.le.m1*i+c1).and.(i.le.ax).and.(j.le.0.15*ax)) then

          rho(i,j,1)=den
        else
          rho(i,j,1)=0.0
        endif
      enddo
    enddo


 
else

 do i=1,numr
    do j=1,numz
      do k=1,numphi

        if (j .lt. slope * i + c) then
          rho(i,j,k)=den       
        else
          rho(i,j,k)=0.0
        endif
      enddo
    enddo
  enddo
 endif
!Find mass
  radius=radius/(ax-1.5)	
  c=c/(ax-1.5)	
	
  M=den*4/3.0*Pi*radius**3	
	
!  print*, M	
	
!Create phi array which is theoretical solution to the Poisson solver
  do i=1,numr
    do j=1,numz
      do k=1,numphi
        angle=k*2*Pi/numphi
        r=(((i*cos(angle)-1.5)**2+(i*sin(angle)-0.5)**2+(j+(radius-c-1.5))**2)**0.5)/(ax-1.5)
        if (r**2.lt.radius**2) then
          phi(i,j,k)= -G*M*(3*radius**2-r**2)/2/radius**3          	  
        else
          phi(i,j,k)= -G*M/r
        endif
        
      enddo
    enddo
  enddo	
	
!  call print1d(phi,"y",2,"anay")
!  call print1d(phi,"x",2,"anax")
  
  
  
!Latest calculations but origin at 0,0,0
	
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

  return
end subroutine guessrho
