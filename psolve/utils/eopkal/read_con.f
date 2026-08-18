        SUBROUTINE READ_CON ( FJD_CON, Z_CON, COV_CON, FJD_START, NUM_CON )
!
! read in EOP file concrete.dat
! start at fjd_start_con.
! read in num_con measurements (if you can).
! Modified 99JAN11.  Checks to see if concrete is still there
!
        IMPLICIT   NONE
        INCLUDE   'solve.i'
        INCLUDE   'precm.i'
        REAL*8     FJD_CON(*), Z_CON(3,*), COV_CON(6,*)
        REAL*8     FJD_START, JDAY, JDAY_PRE
        INTEGER*4  I ,NUM_CON
        REAL*8     XY_COR, XU_COR, YU_COR, X_SIG, Y_SIG, UT_SIG, UT, X, Y
        LOGICAL*4  KEXIST
        CHARACTER  CON_FINAM*128
        INTEGER*4  I_LEN
!
        CALL CLRCH ( CON_FINAM )
        CON_FINAM = PRE_SAV_DIR(1:PRE_SV_LEN)//'concrete.dat'
        INQUIRE ( FILE=CON_FINAM, EXIST=KEXIST )
        IF ( .NOT. KEXIST ) THEN
             WRITE ( 6, '(A)' ) 'EOPKAL(read_con): Concretee EOP file '// &
     &                           CON_FINAM(1:I_LEN(CON_FINAM))//' was not '// &
     &                          'found'
             WRITE ( 6, '(A)' ) 'If you don''t have it take it from '// &
     &                  'http://gemini.gsfc.nasa.gov/solve_save/concrete.dat'
             WRITE ( 6, * ) 'EOPKAL (read_con) Abnormal termination'
             CALL EXIT ( 1 ) 
        ENDIF
        OPEN ( 1, FILE=CON_FINAM, STATUS='OLD' )
!
        JDAY = -1
        JDAY_PRE = FJD_START-1
        DO WHILE ( ABS(JDAY-JDAY_PRE) .GT. 0.1 )
           READ ( 1, *, END=900 ) JDAY
        END DO
!
! ----- now start reading in the stuff.
!
        DO I=1,NUM_CON
           READ ( 1, *, END=900 ) JDAY, X, Y, UT, X_SIG, Y_SIG, UT_SIG, &
     &                            XY_COR, XU_COR, YU_COR
           FJD_CON(I) = JDAY
!
! -------- remove tides from UT1
!
! -------- convert wobble units to mas
!
           Z_CON(1,I)=X*100.D0
           Z_CON(2,I)=Y*100.D0
           Z_CON(3,I)=UT
!
! ------- convert pole sigmas to mas. (UT1 sigma ok.)
!
          X_SIG=X_SIG*100.
          Y_SIG=Y_SIG*100.
!
          COV_CON(1,I)=X_SIG *X_SIG
          COV_CON(2,I)=Y_SIG *X_SIG* XY_COR
          COV_CON(3,I)=Y_SIG*Y_SIG
          COV_CON(4,I)=X_SIG *UT_SIG*XU_COR
          COV_CON(5,I)=Y_SIG *UT_SIG*YU_COR
          COV_CON(6,I)=UT_SIG*UT_SIG
      END DO
!
      CLOSE(1)
      RETURN
900   CONTINUE
      NUM_CON = 0
      CLOSE(1)
!
      RETURN
      END  !#!  READ_CON  #!#
