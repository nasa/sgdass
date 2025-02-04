      SUBROUTINE FLYBY_MERM ( MERM, TIM_ARG, DER_EROT, DER_EROT_RATE, DT, RT )
! ************************************************************************
! *                                                                      *
! *   Routine FLYBY_MERM 
! *                                                                      *
! *  ### 29-JAN-2006   FLYBY_MERM  v1.0 (c)  L. Petrov  29-JAN-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'erm.i'
      REAL*8     TIM_ARG, DER_EROT(3), DER_EROT_RATE(3), DT, RT
      TYPE      ( ERM__TYPE ) MERM
      INTEGER*4  KNOT, J1, J2
      REAL*8     ERM_PAR(3), ERM_PAR_RATE(3)
      INTEGER*4, EXTERNAL :: IXMN8 
      REAL*8     BSPL_VAL, BSPL_DER
!
      DO 410 J1=1,3
         KNOT = IXMN8 ( MERM%NKNOTS(J1), MERM%TIM(1,J1), TIM_ARG )
         ERM_PAR(J1) = 0.0D0
         ERM_PAR_RATE(J1) = 0.0D0
         IF ( KNOT > 0  ) THEN
              DO 420 J2=-MERM%DEGREE(J1),0
                 ERM_PAR(J1) = ERM_PAR(J1) + MERM%VAL(KNOT+J2,J1)* &
     &                                 BSPL_VAL ( MERM%NKNOTS(J1), &
     &                                            MERM%TIM(1,J1),  &
     &                                            MERM%DEGREE(J1), &
     &                                            KNOT + J2, TIM_ARG )
                 ERM_PAR_RATE(J1) = ERM_PAR_RATE(J1) + MERM%VAL(KNOT+J2,J1)* &
     &                                 BSPL_DER ( MERM%NKNOTS(J1), &
     &                                            MERM%TIM(1,J1),  &
     &                                            MERM%DEGREE(J1), &
     &                                            KNOT + J2, TIM_ARG )
420          CONTINUE 
         END IF
         DT = DT + DER_EROT(J1)*ERM_PAR(J1)*1.D6
         RT = RT + DER_EROT_RATE(J1)*ERM_PAR(J1) + DER_EROT(J1)*ERM_PAR_RATE(J1)
 410  CONTINUE 
!
      RETURN
      END  SUBROUTINE  FLYBY_MERM  !#!#
