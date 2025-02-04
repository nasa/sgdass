      SUBROUTINE IONO_ERR_DSTR_PAR ( NOBS, IONO_MOD_ERR, IONO_ERR_NRML, &
     &                               DSTR_AVR, DSTR_SIG, IVRB )
! ************************************************************************
! *                                                                      *
! *   Routine  IONO_ERR_DSTR_PAR 
! *                                                                      *
! * ## 14-MAR-2022  IONO_ERR_DSTR_PAR v1.0 (c)  L. Petrov 14-MAR-2022 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INTEGER*4  NOBS
      INTEGER*4  MG
      PARAMETER  ( MG = 513 )
      REAL*8     IONO_MOD_ERR(NOBS), IONO_ERR_NRML(NOBS), DSTR_AVR, DSTR_SIG
      REAL*8     HST_MIN, HST_MAX, HST_ARG(MG), HST_VAL(MG)
      REAL*8     GAU_ARG(MG), GAU_VAL(MG), MEA, SIG, RMS, RMS_MIN, &
     &           SIG_MIN, SIG_MAX, SIG_STP, SIG_OPT, &
     &           MEA_MIN, MEA_MAX, MEA_STP, MEA_OPT 
      INTEGER*4  J1, J2, J3, J4, J5, J6, NS, NM, NBUF, LH, IVRB, IER 
!
!      LH      =  201
      HST_MIN = -6.0D0
      HST_MAX =  6.0D0
      LH      =  501
      SIG_MIN =  0.200D0
      SIG_MAX =  8.000D0
      SIG_STP =  0.001D0
      MEA_MIN = -1.000D0
      MEA_MAX =  1.000D0
      MEA_STP =  0.001D0
!
      CALL HIST ( NOBS, IONO_ERR_NRML, LH, HST_MIN, HST_MAX, HST_ARG, HST_VAL, 1 )
      NS = IDNINT( (SIG_MAX - SIG_MIN)/SIG_STP ) + 1
!
      RMS_MIN = 1.D9
      SIG_OPT = 1.D9
      DO 420 J2=1,NS
         SIG = SIG_MIN + (J2-1)*SIG_STP
         RMS = 0.0D0
         DO 430 J3=1,LH
            GAU_ARG(J3) = HST_MIN + (J3-1)*(HST_MAX - HST_MIN)/(LH-1)
            GAU_VAL(J3) = 1.D0/DSQRT(PI2)/SIG * DEXP ( -GAU_ARG(J3)**2/(2.0D0*SIG**2) )
            RMS = RMS + (HST_VAL(J3) - GAU_VAL(J3))**2
 430     CONTINUE 
         IF ( RMS < RMS_MIN ) THEN
              RMS_MIN = RMS
              SIG_OPT = SIG
         END IF
 420  CONTINUE 
      SIG = SIG_OPT
!
      RMS_MIN = 1.D9
      MEA_OPT = 1.D9
      NM = IDNINT( (MEA_MAX - MEA_MIN)/MEA_STP ) + 1
      DO 440 J4=1,NM
         MEA = MEA_MIN + (J4-1)*MEA_STP
         RMS = 0.0D0
         DO 450 J5=1,LH
            GAU_ARG(J5) = HST_MIN + (J5-1)*(HST_MAX - HST_MIN)/(LH-1)
            GAU_VAL(J5) = 1.D0/DSQRT(PI2)/SIG * DEXP ( -(GAU_ARG(J5)-MEA)**2/(2.0D0*SIG**2) )
            RMS = RMS + (HST_VAL(J5) - GAU_VAL(J5))**2
 450     CONTINUE 
         IF ( RMS < RMS_MIN ) THEN
              RMS_MIN = RMS
              MEA_OPT = MEA
         END IF
 440  CONTINUE 
!
      DSTR_AVR = MEA_OPT
      DSTR_SIG = SIG_OPT
      IF ( IVRB .EQ. 4 ) THEN
           MEA_OPT = 0.0
           SIG_OPT = 1.0
           DO 460 J6=1,LH
              GAU_VAL(J6) = 1.D0/DSQRT(PI2)/SIG_OPT * DEXP ( -(GAU_ARG(J6)-MEA_OPT)**2/(2.0D0*SIG_OPT**2) )
 460       CONTINUE 
           CALL DIAGI_2 ( LH, HST_ARG, HST_VAL, LH, GAU_ARG, GAU_VAL, IER )
      END IF
!
      RETURN
      END  SUBROUTINE IONO_ERR_DSTR_PAR  !#!#
