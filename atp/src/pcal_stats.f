      SUBROUTINE PCAL_AVER ( NP, TIM, PCAL, TIM_AVR, PCAL_AVR,          &
     &                       PCAL_AMP_RMS, PCAL_PHA_RMS, KP, IUER )
!     
! ***************************************************************************
! *                                                                         *
! *   This routine computes the average and rms of a PCAL array             *
! *                                                                         *
! *   INPUT:                                                                *
! *         NP        =  No. of input points      { INT }                   *
! *                                                                         *
! *         TIM       =  Time array               { REAL }   [NPx1]         *
! *                                                                         *
! *         PCAL      =  Pcal array               { CMPLX }  [NPx1]         *
! *                                                                         *
! *         IUER      =  Error flag               { INT }                   *
! *                                                                         *
! *   OUTPUT:                                                               *
! *         TIM_AVR   =  Average Time             { REAL }                  *
! *                                                                         *
! *         PCAL_AVR  =  Pcal average             { CMPLX }                 *
! *                                                                         *
! *         PCAL_RMS  =  Pcal RMS                 { CMPLX }                 *
! *                                                                         *
! *         KP        =  No. of used points       { INT }                   *
! *                                                                         *
! * ### 27-SEP-2022   PCAL_AVER   v2.0  (c)    N. Habana   30-SEP-2022 ###  *
! *                                                                         *
! ***************************************************************************
!
      IMPLICIT   NONE
      INTEGER*4  NP, KP, IUER
      REAL*8     TIM(NP), TIM_AVR
      COMPLEX*8  PCAL(NP), PCAL_AVR
      REAL*8     PCAL_AMP_RMS, RMS_OLD, PCAL_PHA_RMS
      REAL*8     NSIG_VAL, NSIG_MAX
      REAL*8     NSIG_LIM
      PARAMETER  ( NSIG_LIM = CMPLX(3.D0) )
      INTEGER*4  IV(NP)
      REAL*8     SHR
      PARAMETER  ( SHR = 0.50 )
      INTEGER*4  J1, J2, J3, J4, J5, LP, IND_NSIG, J7
      REAL*4,    EXTERNAL ::  PHAS_CMPL_R4
      
!
! --- Initial values
!
      IV = 1
      TIM_AVR  = 0.0D0
      PCAL_AVR = CMPLX(0.D0)
      PCAL_AMP_RMS = 0.D0
      PCAL_PHA_RMS = 0.D0
      J7 = 0
!
! ---
!
      DO 410 J1= 1, INT(NP*SHR)
         TIM_AVR  = 0.0D0
         PCAL_AVR = CMPLX(0.D0)
         PCAL_AMP_RMS = 0.D0
         PCAL_PHA_RMS = 0.D0
         KP = 0
!
! ------ Sum up 
!
         DO 420 J2 = 1, NP
            IF ( IV(J2) < 1 ) GOTO 420
            KP = KP + 1
            TIM_AVR  = TIM_AVR  + TIM(J2)
            PCAL_AVR = PCAL_AVR + PCAL(J2)
 420     CONTINUE 
!
! ------
!
         IF ( KP > 1 ) THEN
            TIM_AVR  = TIM_AVR/KP
            PCAL_AVR = PCAL_AVR/KP
         END IF
! ------
         IF ( KP < 1 ) GOTO 810
! ------
         DO 432 J3 = 1, NP
            IF ( IV(J3) .NE. 0 ) THEN
                PCAL_AMP_RMS = PCAL_AMP_RMS +                           &
     &                         ( ABS(PCAL(J3)) - ABS(PCAL_AVR) )**2
                PCAL_PHA_RMS = PCAL_PHA_RMS +                           &
     &                         ( PHAS_CMPL_R4(PCAL(J3)) -               &
     &                           PHAS_CMPL_R4(PCAL_AVR) )**2
             END IF
 432     CONTINUE
         PCAL_AMP_RMS = SQRT(PCAL_AMP_RMS/KP)
         PCAL_PHA_RMS = SQRT(PCAL_PHA_RMS/KP)
!
! ------
!
         NSIG_MAX = -1.D0
         DO 430 J3 = 1, NP
            IF ( IV(J3) > 0 ) THEN
               NSIG_VAL = ABS ( (PCAL(J3) - PCAL_AVR)/PCAL_AMP_RMS )
               IF ( NSIG_VAL > NSIG_MAX ) THEN
                  NSIG_MAX = NSIG_VAL
                  IND_NSIG = J3
                  J7 = J7 + 1
               END IF
            END IF
 430     CONTINUE
!
! ------
!
         IF ( NSIG_MAX > NSIG_LIM ) THEN
            IV(IND_NSIG) = 0
         ELSE
            GOTO 810 
         END IF
 410  CONTINUE 
! ---
 810  CONTINUE 
! ---
      LP = 0
      DO 440 J4=1,NP
         IF ( IV(J4) > 0 ) THEN
            LP = LP + 1
            TIM(LP)  = TIM(J4)
            PCAL(LP) = PCAL(J4)
         END IF
 440  CONTINUE 
! ---
      IF ( KP > 2 ) THEN
         PCAL_AMP_RMS = PCAL_AMP_RMS/DSQRT(KP-1.0D0)
         PCAL_PHA_RMS = PCAL_PHA_RMS/DSQRT(KP-1.0D0)
      END IF
! ---
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PCAL_AVER   !#!
!
