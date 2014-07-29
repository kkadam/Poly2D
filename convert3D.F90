program convert3D
  implicit none
  double precision, allocatable :: density(:,:,:)
  double precision, allocatable :: ang_mom(:,:,:)
  double precision, allocatable :: rad_mom(:,:,:)
  
  double precision, allocatable :: rho2d(:,:,:)

  integer :: temp_int1, temp_int2, temp_int3, temp_int4, qint
  double precision :: temp_dbl1, temp_dbl2, temp_dbl3, temp_dbl4
  integer :: numr, numz, numphi, i,j,k,r2d,z2d, temp, count,ax
  double precision ::r,dr,omega, pi, vol, dphi, Re
  
  integer :: numr_procs 
  integer :: numz_procs 
  integer :: numr_dd 
  integer :: numz_dd 
  
  integer :: n, jlwb, jupb, klwb, kupb, record_length
  

  numr=130      !Change!2D SCF resolution chosen to match these
  numz=130      !Change  
  numphi=256    !Change
  
  numr_procs = 16            !Change
  numz_procs = 8             !Change 
  numr_dd = ( (numr - 2)/numr_procs ) + 2
  numz_dd = ( (numz - 2)/numz_procs ) + 2
  

  omega=sqrt(0.1642)  !Change !omega is normalized the same way in Wess's thesis
  pi=3.14159265359    !rho is normalized wrt rho_c of secondary, only one star here

  
  
  !!!!NOTE: SCF values for numr and numz are supposed to be 
  !!!!        r2d=numr+1        
  !!!!        z2d=numz/2+1	
	
	
  r2d=numr+1        !2D SCF resolution, accounts for one ghost zone
  z2d=numz/2+1
  ax=65               !Change 
!  dr=(1.0/(r2d0-1))/((ax-1.5)/(r2d-1.5)) -old dr?!  !Same dr from SCF code, normalized wrt 
  !                                      equatorial radius
  	  
  	  
  dr=1/(numr-1)	  
  Re=(ax-1.5)/(r2d-1.5)	  
  dphi=2.0*pi/numphi
  	  
  	  
  allocate (rho2d(r2d,z2d,1))
  allocate (density(numr,numz,numphi))
  allocate (ang_mom(numr,numz,numphi))
  allocate (rad_mom(numr,numz,numphi))
  
!Read from input file  
  open(unit = 10, file='2d_density.bin',form='unformatted')  
    read(10) rho2d
  close(10)  
  
!Test input  
  open(unit=10,file='test_input')
    do j=1,z2d
       do i=1,r2d
          write(10,*) i,j,rho2d(i,j,1)
       enddo
       write(10,*)
    enddo
  close(10)
 
  print*, 'File test_input printed'
  
!Get 3D axisymmetric density
  do i=1,numr/2
    do j=1,numz/2
      do k=1,numphi        
          density(i,j,k)=rho2d(i+1,z2d-j,1)             
          density(i,j+numz/2,k)=rho2d(i+1,j+1,1)
      enddo
    enddo
  enddo

  do i=1,numr
    do j=1,numz
      do k=1,numphi      
          if (density(i,j,k).lt. 1d-10) then 
            density (i,j,k)  = 1d-10
          endif
      enddo
    enddo
  enddo

  do i=1,numr
    do j=1,numz
      do k=1,numphi      
          if (density(i,j,k).lt. 1d-10) then 
            print*, density (i,j,k) ,i,j,k
          endif
      enddo
    enddo
  enddo      
      
      
      
!Test 3D density
  open(unit=10,status='replace',file='test_3d')
    do j=1,numz
       do i=1,numr
          write(10,*) i,j,density(i,j,111)
       enddo
       write(10,*)
    enddo
  close(10)

  print*, 'File test_3d printed'  
  
!Find angular momentum density
  do i=1,numr
    do j=1,numz
      do k=1,numphi  
          r=(i-0.5)*dr 
          vol=2*pi*r*dr**2
          ang_mom(i,j,k)=density(i,j,k)*vol*r**2*omega/(r**2*dr*dphi)*Re**2	
      enddo
    enddo
  enddo    
     
!Test angular momentum
  open(unit=10,status='replace',file='test_amd')
    do j=1,numz
       do i=1,numr
          write(10,*) i,j,ang_mom(i,j,101)
       enddo
       write(10,*)
    enddo
  close(10)  
  
  print*, 'File test_amd printed' 
  
!Find radial momentum
  do i=1,numr
    do j=1,numz
      do k=1,numphi  	
        rad_mom(i,j,k)=0.0
      enddo
    enddo
  enddo  
    
  inquire(iolength=record_length) density(1:numr_dd,1:numz_dd,:)

     open(unit=10,file='density',form='unformatted',status='new',&
     access='direct', recl=record_length)

       write(*,*) 'record_length ',record_length
       n = 1
       jlwb = 1
       jupb = numr_dd
       klwb = 1
       kupb = numz_dd
       do i = 1, numz_procs
          do j = 1, numr_procs
             write(10,rec=n) density(jlwb:jupb,klwb:kupb,:)
             jlwb = jupb - 1
             jupb = jlwb + numr_dd - 1
             n = n + 1
          enddo
          jlwb = 1
          jupb = numr_dd
          klwb = kupb - 1
          kupb = klwb + numz_dd - 1
       enddo

       open(unit=11,file='ang_mom',form='unformatted',status='unknown',&
       access='direct',recl=record_length)
 
       n = 1
       jlwb = 1
       jupb = numr_dd
       klwb = 1
       kupb = numz_dd
       do i = 1, numz_procs
          do j = 1, numr_procs
             write(11,rec=n) ang_mom(jlwb:jupb,klwb:kupb,:)
             jlwb = jupb - 1
             jupb = jlwb + numr_dd - 1
             n = n + 1
          enddo
          jlwb = 1
          jupb = numr_dd
          klwb = kupb - 1
          kupb = klwb + numz_dd - 1
       enddo

       open(unit=12,file='rad_mom',form='unformatted',status='unknown',&
       access='direct', recl=record_length)

       n = 1
       jlwb = 1
       jupb = numr_dd
       klwb = 1
       kupb = numz_dd
       do i = 1, numz_procs
          do j = 1, numr_procs
             write(12,rec=n) rad_mom(jlwb:jupb,klwb:kupb,:)
             jlwb = jupb - 1
             jupb = jlwb + numr_dd - 1
             n = n + 1
          enddo
          jlwb = 1
          jupb = numr_dd
          klwb = kupb - 1
          kupb = klwb + numz_dd - 1
       enddo
      
       close(10)
       close(11)
       close(12) 
   print*,"Output files density, ang_mom, rad_mom printed"
   
   
!Write fort.7 file!!
	
	
  OPEN(UNIT=10,FILE="fort.7")
  temp_int1 = 1
  temp_int2 = 0
  WRITE(10,*) temp_int1,temp_int2                          !1
  temp_int1 = 100001
  temp_int2 = 101001
  temp_int3 = 100
  WRITE(10,*) temp_int1, temp_int2, temp_int3     	   !2
  temp_int1 = 3 
  temp_int2 = 1
  temp_int3 = 0
  WRITE(10,*) temp_int1, temp_int2, temp_int3              !3 
  temp_dbl1 = 1.5
  temp_dbl2 = 1.6666666666666667
  temp_dbl3 = 0.0                                          !No kappas
  WRITE(10,*) temp_dbl1, temp_dbl2, temp_dbl3, temp_dbl3   !4
  temp_dbl1 = 120.0
  WRITE(10,*) omega, temp_dbl1, omega                      !5
  temp_dbl1 = 5.0
  temp_dbl2 = 0.0
  WRITE(10,*) temp_dbl1, temp_dbl2                         !6
  temp_dbl1 = 1.0e-10
  temp_dbl2 = 1.0e-11
  WRITE(10,*) temp_dbl1, temp_dbl2                         !7 
  temp_int1 = 3
  WRITE(10,*) temp_int1, temp_int1, temp_int1              !8
  temp_dbl1 = 1.0e-5 
  temp_dbl2 = 0.19000000000
  temp_dbl3 = 2.0
  WRITE(10,*) temp_dbl1, temp_dbl2, temp_dbl3              !9 

  CLOSE(10)
  write(*,*) "File fort.7 printed"	
	
  
end program convert3D
