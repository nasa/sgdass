      SUBROUTINE SOUADM_CNS ( N_PAR, C_PAR, FGLOBAL_L4, CNSTROBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SOUADM_CNS imposes constraints on structure source         *
! *   admittance.                                                        *
! *                                                                      *
! *  ### 09-AUG-2007   SOUADM_CNS  v1.0 (c)  L. Petrov  09-AUG-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'glbc3.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'prfil.i'
      INCLUDE   'socom.i'
      INCLUDE   'cnstr.i'
!
      INTEGER*4  N_PAR, IUER
      CHARACTER  C_PAR(N_PAR)*(*)
      LOGICAL*4  FGLOBAL_L4
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
!
      REAL*8     CNS_MIN
      PARAMETER  ( CNS_MIN = 1.D-15 ) 
      INTEGER*4  J1, NC, IER
!
      NC = 0
      IF ( SOU_ADM_FLAG .NE. SOUADM__NO  .AND.  SOU_ADM_CNS > CNS_MIN ) THEN
           DO 410 J1=1,N_PAR
              IF ( (       FGLOBAL_L4 .AND. C_PAR(J1)(1:11) == 'GLB_SOU_ADM' ) .OR. &
     &             ( .NOT. FGLOBAL_L4 .AND. C_PAR(J1)(1:11) == 'LCL_SOU_ADM' )    ) THEN
!
                   NC = NC + 1
                   CALL ERR_PASS ( IUER, IER )
                   CALL ADDCNS_NAM ( 'SADM_CNS', NC, 'Structure source '// &
     &                 'admittance', 'd/l', 0.D0, SOU_ADM_CNS, FGLOBAL_L4, &
     &                  CNSTROBJ, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8691, IUER, 'SOUADM_CNS', &
     &                      'Failure to add description of constraint '// &
     &                      'to an internal data structure' )
                        RETURN
                   END IF
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL ADDCNS_EQU ( 'SADM_CNS', NC, J1, 1.0D0, &
     &                                FGLOBAL_L4, CNSTROBJ, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8692, IUER, 'SOUADM_CNS', &
     &                      'Failure to to write a structure source '// &
     &                      'admittance equation of constraints' )
                        RETURN
                   END IF
              END IF                        
 410       CONTINUE 
      END IF
!
      CALL ERR_LOG ( 0, IUER ) 
      RETURN
      END  SUBROUTINE  SOUADM_CNS  !#!#
