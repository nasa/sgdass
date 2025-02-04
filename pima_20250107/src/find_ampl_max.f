      SUBROUTINE FIND_AMPL_MAX ( RES_U, RES_V, ARR_C8, IND_U_MAX, IND_V_MAX, &
     &                           AMP_MAX, AMP_SCN_RAT, RMS )
! ************************************************************************
! *                                                                      *
! *   Auxilliar routine  FIND_AMPL_MAX
! *                                                                      *
! *  ### 01-JAN-2012  FIND_AMPL_MAX  v1.0 (c) L. Petrov 01-JAN-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  RES_U, RES_V, IND_U_MAX, IND_V_MAX
      REAL*4     AMP_MAX, AMP_SCN_RAT, AMP_SCN
      COMPLEX*8  ARR_C8(RES_U,RES_V)
      INTEGER*4  J1, J2, J3, J4, J5, J6, NP, ISCN, &
     &           IND_U, IND_V, IND_U_SCN, IND_V_SCN, IND_U_CNT, IND_V_CNT
      PARAMETER  ( ISCN = 20 )
      REAL*4     RMT_SHR, RMS
      PARAMETER  ( RMT_SHR = 0.8 )
!
      AMP_MAX = -1.0
      DO 410 J1=1,RES_V
         DO 420 J2=1,RES_U
            IF ( ABS(ARR_C8(J2,J1)) > AMP_MAX ) THEN
                 AMP_MAX = ABS(ARR_C8(J2,J1))
                 IND_V_MAX = J1
                 IND_U_MAX = J2
            END IF
 420     CONTINUE
 410  CONTINUE
      IND_U_CNT = IND_U_MAX - 1
      IF ( IND_U_CNT > RES_U/2 ) IND_U_CNT = IND_U_CNT  - (RES_U + 1)
      IND_V_CNT = IND_V_MAX - 1
      IF ( IND_V_CNT > RES_V/2 ) IND_V_CNT = IND_V_CNT  - (RES_V + 1)
!
      AMP_SCN = -1.0
      DO 430 J3=1,RES_V
         IND_V = J3 - 1
         IF ( J3 > RES_V/2 ) IND_V = IND_V - (RES_V + 1)
         IF ( ABS(IND_V - IND_V_CNT) < ISCN ) GOTO 430
         DO 440 J4=1,RES_U
            IND_U = J4 -1
            IF ( J4 > RES_U/2 ) IND_U = IND_U - (RES_U + 1)
            IF ( ABS(J4 - IND_U_CNT) < ISCN ) GOTO 440
            IF ( ABS(ARR_C8(J4,J3)) > AMP_SCN ) THEN
                 AMP_SCN = ABS(ARR_C8(J4,J3))
                 IND_V_SCN = J3
                 IND_U_SCN = J4
            END IF
 440     CONTINUE
 430  CONTINUE
!
      RMS = 0.0
      NP  = 0
      DO 450 J5=1,RES_V
         IND_V = J5 -1
         IF ( J5 > RES_V/2 ) IND_V = IND_V - (RES_V + 1)
         IF ( ABS(IND_V - IND_V_CNT) < RMT_SHR*RES_V/2 ) GOTO 450
         DO 460 J6=1,RES_U
            IND_U = J6 -1
            IF ( J6 > RES_U/2 ) IND_U = IND_U - (RES_U + 1)
            IF ( ABS(IND_U - IND_U_CNT) < RMT_SHR*RES_U/2 ) GOTO 460
            NP = NP + 1
            RMS = RMS + ABS(ARR_C8(J6,J5))**2
 460     CONTINUE
 450  CONTINUE
      RMS = SQRT ( RMS/NP )
      AMP_SCN_RAT = AMP_SCN / MAX ( AMP_MAX, 1.E-10 )
!
      RETURN
      END  SUBROUTINE  FIND_AMPL_MAX  !#!
