!Gridsize
       integer, parameter :: numr = 220
       integer, parameter :: numz = 220
       integer, parameter :: numphi = 1

!Not sure what these are
       integer, parameter :: rlwb = 2, rupb = numr - 1

       integer, parameter :: zlwb = 2, zupb = numz - 1

       integer, parameter :: philwb = 1, phiupb = numphi

       integer, parameter :: numphi_by_two = numphi / 2

       real, parameter :: numphiinv = 1.0 / numphi

!Symmetry type       
       integer, parameter :: isym=2

!Polytropic index       
       real, parameter :: np=0.0
       
!Specify Boundary points A and B for a 2d configuration     
      integer, parameter :: ax=200
      integer, parameter :: ay=2
      
      integer, parameter :: bx=2
      integer, parameter :: by=200

!Convergence critarion for SCF
      integer, parameter :: conv=2
