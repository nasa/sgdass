      FUNCTION   COMPUTE_MECHI ( NOBS, NPAR, COV, CNSTROBJ, WEI_CNS )
! ************************************************************************
! *                                                                      *
! *   Routine  COMPUTE_MECHI  computes mathematical expectation of       *
! *   the weighted sum of suqares of postfit residuals.                  *
! *                                                                      *
! * ### 11-JAN-2001  COMPUTE_MECHI  v1.2 (c) L. Petrov  15-MAY-2003 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'cnstr.i'
      INTEGER*4  NOBS, NPAR
      REAL*8     COMPUTE_MECHI, COV(*), WEI_CNS(*)
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      REAL*8     CNS_NRM
      INTEGER*4  J1, J2, J3, J4
      INTEGER*4  I, J
      INTEGER*8  LOCC
      LOCC(I,J) = INT8(MIN(I,J)) + ( INT8(MAX(I,J))*INT8(MAX(I,J)-1))/2
!
      COMPUTE_MECHI = NOBS - NPAR
      IF ( CNSTROBJ%N_ECNST .EQ. 0 ) THEN
           RETURN
      END IF
!
! --- Clear weight matrix of constraints
!
      CALL NOUT_R8 ( (CNSTROBJ%N_EQUAT*(CNSTROBJ%N_EQUAT+1))/2, WEI_CNS )
!
! --- Set diagonal elements of the weight matrix of constraints
!
      DO 410 J1=1,CNSTROBJ%N_EQUAT
         WEI_CNS( LOCC(J1,J1) ) = 1.D0/CNSTROBJ%SIG_CNS(J1)**2
 410  CONTINUE
!
! --- Set off-diagonal elements of matrix of constraints
!
      DO 420 J2=1,CNSTROBJ%N_OFD
         WEI_CNS ( LOCC(CNSTROBJ%INE1_OFD(J2),CNSTROBJ%INE2_OFD(J2)) ) = &
     &             CNSTROBJ%WEI_OFD(J2)
 420  CONTINUE
!
!%  write ( 6, * ) 'mechi-45  cnstrobj%n_ecnst = ', cnstrobj%n_ecnst ! %%%%
!%  call tim_init () ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      DO 430 J3=1,CNSTROBJ%N_ECNST
         IF ( CNSTROBJ%EQU_INP(J3) .LE. 0 ) THEN
              WRITE ( 6, * ) ' J3=',J3,' CNSTROBJ%EQU_INP(J3) ', &
     &        CNSTROBJ%EQU_INP(J3)
              CALL ERR_LOG ( 4511, -1, 'COMPUTE_MECHI', 'Trap of internal '// &
     &            'control: bad constraint equation index' )
              CALL EXIT ( 1 )
         END IF
!
         IF ( CNSTROBJ%EQU_INP(J3) .GT. NPAR ) THEN
              WRITE ( 6, * ) ' J3=',J3, &
     &                       ' CNSTROBJ%EQU_INP(J3) ',CNSTROBJ%EQU_INP(J3), &
     &                       ' NPAR = ',NPAR
              CALL ERR_LOG ( 4512, -1, 'COMPUTE_MECHI', 'Trap of internal '// &
     &            'control: bad constraint equation index' )
              CALL EXIT ( 1 )
         END IF
!
         DO 440 J4=1,J3
            CNS_NRM = CNSTROBJ%EQU_CNS(J3)*CNSTROBJ%EQU_CNS(J4)* &
     &                WEI_CNS( LOCC(CNSTROBJ%EQU_INE(J3),CNSTROBJ%EQU_INE(J4)) )
!
            IF ( CNSTROBJ%EQU_INP(J3) .EQ. CNSTROBJ%EQU_INP(J4) ) THEN
                 COMPUTE_MECHI = COMPUTE_MECHI + &
     &              COV(LOCC(CNSTROBJ%EQU_INP(J3),CNSTROBJ%EQU_INP(J4)))*CNS_NRM
               ELSE
!
! -------------- Coefficient 2.0 since we have to count off-diagonal elements
! -------------- twice because we keep only up-triangular terms
!
                 COMPUTE_MECHI = COMPUTE_MECHI + 2.0D0* &
     &              COV(LOCC(CNSTROBJ%EQU_INP(J3),CNSTROBJ%EQU_INP(J4)))*CNS_NRM
            END IF
 440     CONTINUE
 430  CONTINUE
!%  call tim_tp ( %val(0), %val(0), %val(0), %val(0) ) ! %%%%%%%%%%%%%%%%%
!%  write ( 6, * ) 'mechi-81  ' ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      RETURN
      END  !#!  COMPUTE_MECHI  #!#
