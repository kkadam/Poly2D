       integer, parameter :: numr = 100
       integer, parameter :: numz = 100
       integer, parameter :: numphi = 1


       integer, parameter :: rlwb = 2, rupb = numr - 1

       integer, parameter :: zlwb = 2, zupb = numz - 1

       integer, parameter :: philwb = 1, phiupb = numphi

       integer, parameter :: numphi_by_two = numphi / 2

       real, parameter :: numphiinv = 1.0 / numphi
       
       integer, parameter :: isym=3

!Polytropic index       
       real, parameter :: np=1.5
       
!Specify Boundary points A and B for a 2d configuration     
      integer, parameter :: ax=60  
      integer, parameter :: ay=2
      
      integer, parameter :: bx=2
      integer, parameter :: by=50       


