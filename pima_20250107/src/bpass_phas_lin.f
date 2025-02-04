      SUBROUTINE BPASS_PHAS_LIN ( L_FRQ, L_CHN, FRQ, PHS, AMP, MODE, IUER )
! ************************************************************************
! *                                                                      *
! *   Axilliary routine  BPASS_PHAS_LIN
! *                                                                      *
! * ### 22-MAY-2006  BPASS_PHAS_LIN  v1.0 (c) L. Petrov  22-MAY-2006 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INTEGER*4  L_FRQ, L_CHN, MODE, IUER
      REAL*4     FRQ(PIM__MCHN,PIM__MFRQ), PHS(PIM__MCHN,PIM__MFRQ), &
     &           AMP(PIM__MCHN,PIM__MFRQ)
      REAL*8     ARG(PIM__MCHN), VAL(PIM__MCHN), WEI(PIM__MCHN), &
     &           FRQ_0(PIM__MFRQ), SH(PIM__MFRQ), DR(PIM__MFRQ), VAL_REG
      INTEGER*4  M_ITER
      PARAMETER  ( M_ITER = 8 )
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, IV(PIM__MCHN), N_ITER, &
     &           ITURN, IER
!
      IF ( MODE == 1 ) THEN
           N_ITER = 1
         ELSE IF ( MODE == 2 ) THEN
           N_ITER = M_ITER
      END IF
!
      DO 410 J1=1,N_ITER
         DO 420 J2=1,L_FRQ
            FRQ_0(J2) = FRQ(1,J2)
            DO 430 J3=1,L_CHN
               ARG(J3) = FRQ(J3,J2) - FRQ_0(J2)
               VAL(J3) = PHS(J3,J2)
               WEI(J3) = AMP(J3,J2)
               IF ( MODE == 2 ) THEN
                    IF ( J3 > 1 ) THEN
                         IF ( VAL(J3) - VAL(J3-1) > PI__NUM ) THEN
                              VAL(J3) = VAL(J3) - PI2
                            ELSE IF ( VAL(J3) - VAL(J3-1) < -PI__NUM ) THEN
                              VAL(J3) = VAL(J3) + PI2
                         END IF
                    END IF
               END IF
               IV(J3) = 1
 430        CONTINUE
!
            CALL ERR_PASS ( IUER, IER )
            CALL REGRW8   ( L_CHN, ARG, VAL, WEI, IV, DR(J2), SH(J2), IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 5881, IUER, 'BPASS_PHAS_LIN', 'Failure in '// &
     &               'REGRW8' )
                 RETURN
            END IF
!
            DO 440 J4=1,L_CHN
               VAL_REG = SH(J2) + DR(J2)*(ARG(J4)-ARG(1))
               IF ( MODE == 2 ) THEN
                    ITURN = IDNINT( (VAL(J4) - VAL_REG)/PI2 )
                    PHS(J4,J2) = VAL(J4) - ITURN*PI2
               END IF
 440        CONTINUE
 420     CONTINUE
 410  CONTINUE
!
      DO 450 J5=1,L_FRQ
         DO 460 J6=1,L_CHN
            ARG(J6) = FRQ(J6,J5) - FRQ_0(J5)
            PHS(J6,J5) = SH(J5) + DR(J5)*(ARG(J6)-ARG(1))
 460     CONTINUE
 450  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  BPASS_PHAS_LIN !#!
