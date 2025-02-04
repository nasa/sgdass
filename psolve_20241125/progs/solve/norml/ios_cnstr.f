      SUBROUTINE IOS_CNSTR ( L_PAR, C_PAR, IOS_SIG, CNSTROBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine IOS_CNSTR
! *                                                                      *
! *  ### 22-AUG-2022   IOS_CNSTR   v1.0 (c)  L. Petrov  22-AUG-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'cnstr.i'
      INTEGER*4  L_PAR, IUER
      CHARACTER  C_PAR(L_PAR)*(*), CNS_ABR*8
      PARAMETER  ( CNS_ABR = 'IOS_CNS ' )
      REAL*8     IOS_SIG
      TYPE ( CNSTR__STRU ) :: CNSTROBJ
      LOGICAL*4   FALSE_L4, TRUE_L4
      PARAMETER ( FALSE_L4 = .TRUE. )
      PARAMETER ( TRUE_L4  = .TRUE. )
      INTEGER*4  J1, K_IOS, IER
!
      K_IOS = 0
      DO 410 J1=1,L_PAR
         IF ( C_PAR(J1)(1:8) == 'IOS_SES ' .OR. &
     &        C_PAR(J1)(1:8) == 'IOS_STA ' .OR. &
     &        C_PAR(J1)(1:4) == 'IOB_'          ) THEN
!
! ----------- Add information about the type of the constraint applied
!
              K_IOS = K_IOS + 1
              CALL ERR_PASS ( IUER, IER )
              CALL ADDCNS_NAM ( CNS_ABR, K_IOS, 'Ionosphere path delay scale', &
     &                         'dimensionless', 0.0D0, IOS_SIG, FALSE_L4, &
     &                         CNSTROBJ, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8591, IUER, 'IOS_CNSTR', 'Error in '// &
     &                 'an attempt to put information about '// &
     &                 'constraint on ionosphere scale' )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL ADDCNS_EQU ( CNS_ABR, K_IOS, J1, 1.0D0, TRUE_L4, CNSTROBJ, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8592, IUER, 'IOS_CNSTR', &
     &                 'Error in an attempt to put coefficients of '// &
     &                 'constraint on ionosphere scale' )
                   RETURN
              END IF
         END IF
 410  CONTINUE 
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  IOS_CNSTR  !#!#
