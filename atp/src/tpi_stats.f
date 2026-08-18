      SUBROUTINE TPI_AVER ( NP, TIM, TPION, TPIOFF, TIM_AVR, TPION_AVR, &
     &                      TPIOFF_AVR, TPION_RMS, TPIOFF_RMS, KP, IUER)
!
! ***************************************************************************
! *                                                                         *
! *   This routine computes the average and rms of a TPI arrays.            *
! *   For now we assume that TPIZERO is always empty, i.e., -999            *
! *                                                                         *
! *   INPUT:                                                                *
! *         NP        =  No. of input points      { INT }                   *
! *                                                                         *
! *         TIM       =  Time array               { REAL }   [NPx1]         *
! *                                                                         *
! *         TPION     =  TpiOn array              { INT }    [NPx1]         *
! *                                                                         *
! *         TPIOFF    =  TpiOff array             { INT }    [NPx1]         *
! *                                                                         *
! *         IUER      =  Error flag               { INT }                   *
! *                                                                         *
! *   OUTPUT:                                                               *
! *         TIM_AVR    =  Average Time             { REAL }                 *
! *                                                                         *
! *         TPION_AVR  =  TpiOn average            { INT }                  *
! *                                                                         *
! *         TPION_RMS  =  TpiOn RMS                { INT }                  *
! *                                                                         *
! *         TPIOFF_AVR  =  TpiOff average          { INT }                  *
! *                                                                         *
! *         TPIOFF_RMS  =  TpiOff RMS              { INT }                  *
! *                                                                         *
! *         KP        =  No. of used points        { INT }                  *
! *                                                                         *
! *   Copyright (c) 1975-2025 United States Government as represented by    *
! *   the Administrator of the National Aeronautics and Space               *
! *   Administration. All Rights Reserved.                                  *
! *   License: NASA Open Source Software Agreement (NOSA).                  *
! *                                                                         *
! * ### 10-SEP-2025   TPI_AVER   v1.0  (d)    L. Petrov   10-SEP-2025 ###  *
! *                                                                         *
! ***************************************************************************
!
      IMPLICIT   NONE 
      INTEGER*4  NP, KP, IUER
      REAL*8     TIM(NP),  TIM_AVR
      INTEGER*4  TPION(NP), TPIOFF(NP)
      REAL*8     RTPION(NP), RTPIOFF(NP)
      INTEGER*4  TPION_AVR, TPION_RMS, TPIOFF_AVR, TPIOFF_RMS
      INTEGER*4  NSIG_VAL, NSIG_MAX, TPION_RMS_OLD, TPIOFF_RMS_OLD
      REAL*8     RTPION_AVR, RTPION_RMS, RTPIOFF_AVR, RTPIOFF_RMS
      REAL*8     RSIG_VAL, RSIG_MAX, RTPION_RMS_OLD, RTPIOFF_RMS_OLD
      INTEGER*4  NSIG_LIM
      PARAMETER  ( NSIG_LIM = 3 )
      REAL*8     RSIG_LIM
      PARAMETER  ( RSIG_LIM = 3 )
      INTEGER*4  IV(NP)
      REAL*8     SHR
      PARAMETER  ( SHR = 0.50 )
      LOGICAL*1  FL_OTL
      INTEGER*4  J1, J2, J3, J4, J5, LP, IND_NSIG
!
! --- Initial values
!
      IV = 1
      TIM_AVR     = 0.0D0
      TPION_AVR   = 0
      TPION_RMS   = 0
      TPIOFF_AVR  = 0
      TPIOFF_RMS  = 0
! ---
      RTPION  = TPION
      RTPIOFF = TPIOFF      
!     
! ---
!
      KP = 0
      DO 410 J1= 1, INT(NP*SHR)
         TIM_AVR      = 0.D0
         RTPION_AVR   = 0.D0
         RTPION_RMS   = 0.D0
         RTPIOFF_AVR  = 0.D0
         RTPIOFF_RMS  = 0.D0
         KP = 0
!
! ------ Sum up 
!
         DO 420 J2 = 1, NP
            IF ( IV(J2) < 1 ) GOTO 420
            KP = KP + 1
            TIM_AVR      =  TIM_AVR     + TIM(J2)
            RTPION_AVR   =  RTPION_AVR  + RTPION(J2)
            RTPION_RMS   =  RTPION_RMS  + RTPION(J2)**2
            RTPIOFF_AVR  =  RTPIOFF_AVR + RTPIOFF(J2)
            RTPIOFF_RMS  =  RTPIOFF_RMS + RTPIOFF(J2)**2
 420     CONTINUE 
!
! ------
!
         IF ( KP > 1 ) THEN
              TIM_AVR     = TIM_AVR/KP
              RTPION_AVR  = RTPION_AVR/KP
              RTPIOFF_AVR = RTPIOFF_AVR/KP
         END IF
!         IF ( KP < 1 ) GOTO 810
!
         RTPION_RMS  = DSQRT( RTPION_RMS/KP  - RTPION_AVR**2  )
         RTPIOFF_RMS = DSQRT( RTPIOFF_RMS/KP - RTPIOFF_AVR**2 )
         IF ( RTPION_RMS  < 1.0 ) RTPION_RMS  = 1.0
         IF ( RTPIOFF_RMS < 1.0 ) RTPIOFF_RMS = 1.0
!
         RSIG_MAX = -1.D0
         DO 430 J3 = 1, NP
            IF ( IV(J3) > 0 ) THEN
               RSIG_VAL = DABS ( (RTPION(J3) - RTPION_AVR)/RTPION_RMS )
               IF ( RSIG_VAL > RSIG_MAX ) THEN
                  RSIG_MAX = RSIG_VAL
                  IND_NSIG = J3
               END IF
            END IF
 430     CONTINUE 
! ------
         IF ( RSIG_MAX > RSIG_LIM ) THEN
            IV(IND_NSIG) = 0
            RTPION_RMS_OLD   = (RTPION_RMS**2  + RTPION_AVR**2)*KP
            RTPIOFF_RMS_OLD  = (RTPIOFF_RMS**2 + RTPIOFF_AVR**2)*KP
            TIM_AVR  = (TIM_AVR*KP  - TIM(IND_NSIG) )/(KP-1)
            RTPION_AVR = (RTPION_AVR*KP - RTPION(IND_NSIG))/(KP-1)
            RTPION_RMS = DSQRT((RTPION_RMS_OLD - RTPION(IND_NSIG)**2 )    &
     &                         /(KP-1) - RTPION_AVR**2 )
            RTPIOFF_AVR = (RTPIOFF_AVR*KP - RTPIOFF(IND_NSIG))/(KP-1)
            RTPIOFF_RMS = DSQRT((RTPIOFF_RMS_OLD - RTPIOFF(IND_NSIG)**2) &
     &                          /(KP-1) - RTPIOFF_AVR**2)
         ELSE
            GOTO 810
         END IF
 410  CONTINUE
 810  CONTINUE
!
      IF ( KP .GT. 1 ) THEN
           RTPION_RMS   = RTPION_RMS/DSQRT(REAL(KP - 1, 8))
           RTPIOFF_RMS  = RTPIOFF_RMS/DSQRT(REAL(KP - 1, 8))
         ELSE
           RTPION_RMS   = 0.0D0
           RTPIOFF_RMS  = 0.0D0
      END IF
!
      IF ( KP .GE. 1 ) THEN
           TPION_AVR   =  RTPION_AVR
           TPION_RMS   =  RTPION_RMS
           TPIOFF_AVR  =  RTPIOFF_AVR
           TPIOFF_RMS  =  RTPIOFF_RMS
         ELSE
           TPION_AVR   =  RTPION(1)
           TPION_RMS   =  0.0D0
           TPIOFF_AVR  =  RTPIOFF(1)
           TPIOFF_RMS  =  0.0D0
      END IF
!    
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  TPI_AVER   !#!1
