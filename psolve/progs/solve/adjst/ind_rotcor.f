      SUBROUTINE IND_ROTCOR ( EOP_INDICIES, NUT_INDICIES, MAT )
      IMPLICIT NONE
!
!     When eop daily estimates are turned on this routine will create the
!     eop/nutation correlation matrix and write it to the spoolfile
!
      INTEGER*2 EOP_INDICIES(3,3), NUT_INDICIES(2), I
      REAL*8    COV_MAT(8,8), COR_MAT(8,8)
      INTEGER*4 PARM_LIST(8)
      REAL*8 MAT(*)
      INTEGER*4 IERR4, NUM_PARMS
      PARAMETER  ( NUM_PARMS=8 )
!
!     1997.11.14  jwr  Created from whole cloth.
!     2001.04.18  pet  Improved comments
!
!
! --- Create a clean, linear list of parameter numbers of the 8 eop/nut parms
!
      PARM_LIST(1) = EOP_INDICIES(1,1)
      PARM_LIST(2) = EOP_INDICIES(2,1)
      PARM_LIST(3) = EOP_INDICIES(1,2)
      PARM_LIST(4) = EOP_INDICIES(2,2)
      PARM_LIST(5) = EOP_INDICIES(1,3)
      PARM_LIST(6) = EOP_INDICIES(2,3)
      PARM_LIST(7) = NUT_INDICIES(1)
      PARM_LIST(8) = NUT_INDICIES(2)
!
! --- Create the covariance matrix and then convert it to a correlation matrix.
!
      CALL CREATE_COVARIANCE_MATRIX ( NUM_PARMS, PARM_LIST, MAT, COV_MAT, IERR4)
      CALL COVAR2COREL ( NUM_PARMS, COV_MAT, COR_MAT )
!
      write(23,'(1X)')
      write(23,*) 'EOP Correlations:'
      write(23,*) ' X Wob  Off  1.0000'
      write(23,'("  X Wob Rate",1X,F7.4,   "  1.0000")')  cor_mat(2,1)
      write(23,'("  Y Wob  Off",2(1X,F7.4),"  1.0000")') (cor_mat(3,i),i=1,2)
      write(23,'("  Y Wob Rate",3(1X,F7.4),"  1.0000")') (cor_mat(4,i),i=1,3)
      write(23,'("UT1-TAI  Off",4(1x,f7.4),"  1.0000")') (cor_mat(5,i),i=1,4)
      write(23,'("UT1-TAI Rate",5(1x,f7.4),"  1.0000")') (cor_mat(6,i),i=1,5)
      write(23,'("         Psi",6(1x,f7.4),"  1.0000")') (cor_mat(7,i),i=1,6)
      write(23,'("         Eps",7(1x,f7.4),"  1.0000")') (cor_mat(8,i),i=1,7)
      write(23, &
     & '(14x," X Wob   X Wob   Y Wob   Y Wob     UT1     UT1 ",4x,"Psi",5x,"Eps")')
      write(23,'(14x,"   Off    Rate     Off    Rate     Off    Rate")')
      write(23,'(1X)')
!
      RETURN
      END  !#!  IND_ROTCOR  #!#
