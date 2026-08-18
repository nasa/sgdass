!@This is the start of file &PLTFL
!
! note: if this is updated, the used_pltfl  parameter
!       in solve.i must be updated.
!       Changes should also be made in ../sdbh/pltblk.f.
!
!  changes
!
!   kdb  951207  Integer*4 number of observations.
!   BA   980826  Added note above about pltblk.f.
!
!
!                                                            size in 2*byte words
      INTEGER*2 NUMBLK                                      !                  = 1
!
      INTEGER*4 NPLST (MAX_DBS)                             !  2*max_dbs
      INTEGER*2 NPLBLK(MAX_DBS)                             !  max_dbs
      INTEGER*2 NPLEX (MAX_DBS)                             !  max_dbs         = 8*max*dbs
      REAL*8 TPLBLK   (MAX_DBS)                             !  4*max_dbs
!
      INTEGER*2 NBLPL(4,2,max_plots)                        !  8*max_plots
      INTEGER*2 NPTPL(    max_plots)                        !  max_plots       = 9*max_plots
!
      INTEGER*2 IFREE_PLTFL(free_len_pltfl)                 !                  =  free_len_pltfl
!
      INTEGER*2 IPLTFIL(JPLTFIL_WORDS)
      EQUIVALENCE (IPLTFIL,TPLBLK)
!
      character*8 nblpl_c(             2,max_plots)
      equivalence (nblpl,nblpl_c)
!
      COMMON/PLTFL/TPLBLK,NUMBLK,NPLST,NBLPL,NPLBLK,NPLEX,NPTPL, &
     &             IFREE_PLTFL
!
! NUMBLK   :  Number of data bases (nomenclature: blocks)
! NPLST ( ):  Number of obs in OBSFIL that starts database
! NPLBLK( ):  Number of first plot in databases
! NPLEX(  ):  Number of plots in database
! TPLBLK( ):  Start time of plots for database
! NPTPL(  ):  Number of points in plot
! NBLPL(  ):  Name of baseline in plot
!
! maximum plot # = max_arc_bls*2  Set in solve.i
