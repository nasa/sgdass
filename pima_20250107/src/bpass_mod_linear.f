      SUBROUTINE BPASS_MOD_LINEAR ( MODE, CMPL, AC_REF, AC_REM, &
     &                 L_IN,  FRQ_IN,  PHS_IN,  AMP_IN, PHS_MOD,  AMP_MOD, &
     &                 L_OUT, FRQ_OUT, PHS_OUT, AMP_OUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Axilliary routine  BPASS_MOD_LINEAR
! *                                                                      *
! * ### 02-DEC-2017 BPASS_MOD_LINEAR v1.2 (c) L. Petrov 01-FEB-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      LOGICAL*4  BPASS_MOD_SPLINE
      INTEGER*4  MODE, L_IN, L_OUT, IUER
      COMPLEX*8  CMPL(L_OUT)
      REAL*4     AC_REF(L_OUT),  AC_REM(L_OUT), &
     &           FRQ_IN(L_IN),   PHS_IN(L_IN),   AMP_IN(L_IN),  &
     &                           PHS_MOD(L_IN),  AMP_MOD(L_IN), &
     &           FRQ_OUT(L_OUT), PHS_OUT(L_OUT), AMP_OUT(L_OUT)
      CHARACTER  STR*128
      LOGICAL*1  FL_TEST
      INTEGER*4  M_AMB_TRIES
      PARAMETER  ( M_AMB_TRIES = 5 )
      COMPLEX*8  CMPL_ACC
      REAL*4     RAT, AMPL_MAX, PHS_ADD
      INTEGER*4  J1, J2, J3, J4, J5, IND_MAX, IER 
      INTEGER*4  I_LEN, ILEN
!
      FL_TEST = .FALSE.
!
! --- We can run several iterations for phase ambiguity resolution
!
      IF ( L_IN .NE. 2 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( L_IN, STR )
           CALL ERR_LOG ( 6451, IUER, 'BPASS_MOD_LINEAR', 'Trap of '// &
     &         'internal control: L_IN='//TRIM(STR)//' while 2 was '// &
     &         'expected' )
           RETURN 
      END IF
      RAT = (PHS_IN(2) - PHS_IN(1))/(PI2*FRQ_IN(2) - PI2*FRQ_IN(1))
      IF ( MODE == 2 ) THEN
           AMPL_MAX = -1.0
           DO 410 J1=-M_AMB_TRIES,M_AMB_TRIES
              CMPL_ACC = 0.0
              DO 420 J2=1,L_OUT
                 PHS_ADD = (RAT + J1/(FRQ_IN(2) - FRQ_IN(1)))*PI2*(FRQ_OUT(J2) - FRQ_IN(1))
                 CMPL_ACC = CMPL_ACC + CMPL(J2)*CMPLX(COS(PHS_ADD),SIN(PHS_ADD))
 420          CONTINUE 
              CMPL_ACC = CMPL_ACC/L_OUT
              IF ( ABS(CMPL_ACC) > AMPL_MAX ) THEN
                   AMPL_MAX = ABS(CMPL_ACC)
                   IND_MAX = J1
              END IF
              if ( fl_test ) write ( 6, * ) 'bpas_mod_linear-60 j1= ', int2(j1), ' ampl= ', abs(cmpl_acc), ' ampl_max = ', ampl_max ! %%%w
 410       CONTINUE 
           RAT = RAT + IND_MAX/(FRQ_IN(2) - FRQ_IN(1))
           if ( fl_test ) write ( 6, * ) 'bpas_mod_linear-62 ind_max = ', ind_max
      END IF
!
      DO 430 J3=1,L_IN
         PHS_MOD(J3) = PHS_IN(1) + RAT*PI2*(FRQ_IN(J3) - FRQ_IN(1))
         AMP_MOD(J3) = AC_REM(J3)
 430  CONTINUE 
!
      DO 440 J4=1,L_OUT
         PHS_OUT(J4) = PHS_IN(1) + RAT*PI2*(FRQ_OUT(J4) - FRQ_IN(1))
         AMP_OUT(J4) = AC_REM(J4)
 440  CONTINUE 
      if ( fl_test ) write ( 6, * ) 'bpas_mod_linear-64 frq= ', frq_in(1:2) ! %%%%
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  BPASS_MOD_LINEAR  !#!#
