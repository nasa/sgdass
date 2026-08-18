!This is the start of file &COREMA
!
!     modifications
!
!     kdb 960514 Change local max_par to max_summed_par to distinguish it from
!                the solve.i max_par.
!
      INTEGER*4 ARCPOS(MAX_ARC)
      REAL*8 PARSIG(MAX_SUMMED_PAR),MAT(MAX_PER,MAX_PER)
      COMMON/COREMA/ARCPOS,PARSIG,MAT
