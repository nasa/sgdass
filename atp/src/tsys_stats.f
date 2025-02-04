      SUBROUTINE TSYS_AVER ( NP, TIM, TSYS, TIM_AVR, TSYS_AVR,          &
     &                       TSYS_RMS, KP, IUER )
!
! ***************************************************************************
! *                                                                         *
! *   This routine computes the average and rms of a TSYS array             *
! *                                                                         *
! *   INPUT:                                                                *
! *         NP        =  No. of input points      { INT }                   *
! *                                                                         *
! *         TIM       =  Time array               { REAL }   [NPx1]         *
! *                                                                         *
! *         TSYS      =  Tsys array               { REAL }   [NPx1]         *
! *                                                                         *
! *         IUER      =  Error flag               { INT }                   *
! *                                                                         *
! *   OUTPUT:                                                               *
! *         TIM_AVR   =  Average Time             { REAL }                  *
! *                                                                         *
! *         TSYS_AVR  =  Tsys average             { REAL }                  *
! *                                                                         *
! *         TSYS_RMS  =  Tsys RMS                 { REAL }                  *
! *                                                                         *
! *         KP        =  No. of used points       { INT }                   *
! *                                                                         *
! * ### 28-JUL-2022   TSYS_AVER   v1.0  (c)    L. Petrov   28-JUL-2022 ###  *
! *                                                                         *
! ***************************************************************************
!
      IMPLICIT   NONE 
      INTEGER*4  NP, KP, IUER
      REAL*8     TIM(NP), TSYS(NP), TIM_AVR, TSYS_AVR, TSYS_RMS
      REAL*8     NSIG_VAL, NSIG_MAX, RMS_OLD
      REAL*8     NSIG_LIM
      PARAMETER  ( NSIG_LIM = 3.0D0 )
      INTEGER*4  IV(NP)
      REAL*8     SHR
      PARAMETER  ( SHR = 0.50 )
      LOGICAL*1  FL_OTL
      INTEGER*4  J1, J2, J3, J4, J5, LP, IND_NSIG
!
! --- Initial values
!
      IV = 1
      TIM_AVR  = 0.0D0
      TSYS_AVR = 0.0D0
      TSYS_RMS = 0.0D0
!
! ---
!
      DO 410 J1= 1, INT(NP*SHR)
         TIM_AVR  = 0.0D0
         TSYS_AVR = 0.0D0
         TSYS_RMS = 0.0D0
         KP = 0
!
! ------ Sum up 
!
         DO 420 J2 = 1, NP
            IF ( IV(J2) < 1 ) GOTO 420
            KP = KP + 1
            TIM_AVR  = TIM_AVR  + TIM(J2)
            TSYS_AVR = TSYS_AVR + TSYS(J2)
            TSYS_RMS = TSYS_RMS + TSYS(J2)**2
 420     CONTINUE 
!
! ------
!
         IF ( KP > 1 ) THEN
            TIM_AVR  = TIM_AVR/KP
            TSYS_AVR = TSYS_AVR/KP
         END IF
! ------
         IF ( KP < 1 ) GOTO 810
! ------
         TSYS_RMS = DSQRT(TSYS_RMS/KP - TSYS_AVR**2)
! ------
         NSIG_MAX = -1.0D0
         DO 430 J3 = 1, NP
            IF ( IV(J3) > 0 ) THEN
               NSIG_VAL = DABS ( (TSYS(J3) - TSYS_AVR)/TSYS_RMS )
               IF ( NSIG_VAL > NSIG_MAX ) THEN
                  NSIG_MAX = NSIG_VAL
                  IND_NSIG = J3
               END IF
            END IF
 430     CONTINUE 
! ------
         IF ( NSIG_MAX > NSIG_LIM ) THEN
            IV(IND_NSIG) = 0
            RMS_OLD  = (TSYS_RMS**2 + TSYS_AVR**2)*KP
            TIM_AVR  = (TIM_AVR*KP  - TIM(IND_NSIG) )/(KP-1)
            TSYS_AVR = (TSYS_AVR*KP - TSYS(IND_NSIG))/(KP-1)
            TSYS_RMS = DSQRT( (RMS_OLD - TSYS(IND_NSIG)**2)/(KP-1) -    &
     &                        TSYS_AVR**2 )
         ELSE
            GOTO 810 
         END IF
 410  CONTINUE 
! ---
 810  CONTINUE
! ---
      IF ( KP .GT. 1 ) THEN
         TSYS_RMS = TSYS_RMS/DSQRT(REAL(KP - 1, 8))
      END IF
! ---
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  TSYS_AVER   !#!
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ELEV_NRML ( KI, TIM, EL, TSYS, KP, EL_NRML, TSYS_NRML )
!     
! ***************************************************************************
! *                                                                         *
! *   This routine normalizes the elevation and tsys                        *
! *                                                                         *
! *   INPUT:                                                                *
! *         KI        =                           { INT }                   *
! *                                                                         *
! *         TIM       =  Time array               { REAL }   [KIx1]         *
! *                                                                         *
! *         EL        =  Elevation array          { REAL }   [KIx1]         *
! *                                                                         *
! *         TSYS      =  Tsys array               { REAL }   [KIx1]         *
! *                                                                         *
! *         IUER      =  Error flag               { INT }                   *
! *                                                                         *
! *   OUTPUT:                                                               *
! *         KP        =  No. of used points       { INT }                   *
! *                                                                         *
! *         EL_NRML   =  Elevation array          { REAL }                  *
! *                                                                         *
! *         TSYS_NRML =  Tsys array               { REAL }                  *
! *                                                                         *
! * ### 28-JUL-2022   ELEV_NRML   v1.0  (c)    L. Petrov   28-JUL-2022 ###  *
! *                                                                         *
! ***************************************************************************
!
      IMPLICIT   NONE 
      INTEGER*4  KI, KP
      REAL*8     TIM(KI), EL(KI), TSYS(KI), EL_NRML(KI), TSYS_NRML(KI)
      INTEGER*4  MP, MZ
      PARAMETER  ( MP = 8192 )
      PARAMETER  ( MZ = 128  )
      REAL*8     ARR_TMP(MP), TIM_ZEN(MP), TSYS_ZEN(MP), TIM_Z_DIF_MIN
      REAL*8     SCL
      INTEGER*4  IARR(MP), IARR_ZEN(MP), KARR(MP)
      INTEGER*4  J1, J2, J3, J4, J5, IS, IARR_MAX, LZ, IND_Z, IUER
      INTEGER*4  ADD_LIS
!
      KP = 0
      SCL = 7
      IARR_MAX = -1
      DO 410 J1=1,KI
         IS = ADD_LIS ( MP, KP, IARR, IDNINT( SCL*EL(J1) ), IUER )
         IARR_MAX = MAX( IARR_MAX, IARR(IS) )
 410  CONTINUE 
      CALL SORT_I ( KP, IARR )
!
      LZ = 0
      DO 420 J2=1,KI
         IF ( IDNINT( SCL*EL(J2) ) == IARR_MAX ) THEN
            LZ = LZ + 1
            TIM_ZEN(LZ)  = TIM(J2)
            TSYS_ZEN(LZ) = TSYS(J2)
         END IF
 420  CONTINUE 
!
      TSYS_NRML = 0.0D0
      KARR = 0
      DO 430 J3=1,KI
         IS = ADD_LIS ( MP, KP, IARR, IDNINT( SCL*EL(J3) ), IUER )
         TIM_Z_DIF_MIN = 1.D10
         DO 440 J4=1,LZ
            IF ( DABS(TIM_ZEN(J4) - TIM(J3)) < TIM_Z_DIF_MIN ) THEN
               TIM_Z_DIF_MIN = DABS(TIM_ZEN(J4) - TIM(J3))
               IND_Z = J4
            END IF
 440     CONTINUE 
!!         write ( 6, * ) ' j3= ',j3, ' ind_z= ', ind_z, ' is= ', is ! %%%
         TSYS_NRML(IS) = TSYS_NRML(IS) + TSYS(J3)/TSYS_ZEN(IND_Z)
         KARR(IS) = KARR(IS) + 1
 430  CONTINUE 
!
      DO 450 J5=1,KP
         EL_NRML(J5) = IARR(J5)/SCL
         TSYS_NRML(J5) = TSYS_NRML(J5)/KARR(J5)
 450  CONTINUE 
!
      RETURN
      END  SUBROUTINE ELEV_NRML  !#!#!
!
! ------------------------------------------------------------------------
!
      SUBROUTINE AZIM_NRML ( KI, TIM, AZ, TSYS, KP, AZ_NRML, TSYS_NRML )
      IMPLICIT   NONE 
      INTEGER*4  KI, KP
      REAL*8     TIM(KI), AZ(KI), TSYS(KI), AZ_NRML(KI), TSYS_NRML(KI)
      INTEGER*4  MP, MZ
      PARAMETER  ( MP = 8192 )
      PARAMETER  ( MZ = 128  )
      REAL*8     ARR_TMP(MP), TIM_ZEN(MP), TSYS_ZEN(MP), TIM_Z_DIF_MIN
      REAL*8     SCL
      INTEGER*4  IARR(MP), IARR_ZEN(MP), KARR(MP)
      INTEGER*4  J1, J2, J3, J4, J5, IS, IARR_MAX, LZ, IND_Z, IUER
      INTEGER*4  ADD_LIS
!
      KP = 0
      SCL = 7
      IARR_MAX = -1
      DO 410 J1=1,KI
         IS = ADD_LIS ( MP, KP, IARR, IDNINT( SCL*AZ(J1) ), IUER )
         IARR_MAX = MAX( IARR_MAX, IARR(IS) )
!!         WRITE ( 6, * ) 'J1= ',J1, ' AZ= ', AZ(J1), ' TIM= ', TIM(J1) ! %%q
 410  CONTINUE 
      CALL SORT_I ( KP, IARR )
!
      LZ = 0
      DO 420 J2=1,KI
         IF ( IDNINT( SCL*AZ(J2) ) == IARR_MAX ) THEN
            LZ = LZ + 1
            TIM_ZEN(LZ)  = TIM(J2)
            TSYS_ZEN(LZ) = TSYS(J2)
         END IF
 420  CONTINUE 
!!      WRITE ( 6, * ) 'KP= ', KP, ' IARR_MAX= ', IARR_MAX, ' LZ= ', LZ
!
      TSYS_NRML = 0.0D0
      KARR = 0
      DO 430 J3=1,KI
         IS = ADD_LIS ( MP, KP, IARR, IDNINT( SCL*AZ(J3) ), IUER )
         TIM_Z_DIF_MIN = 1.D10
         DO 440 J4=1,LZ
            IF ( DABS(TIM_ZEN(J4) - TIM(J3)) < TIM_Z_DIF_MIN ) THEN
               TIM_Z_DIF_MIN = DABS(TIM_ZEN(J4) - TIM(J3))
               IND_Z = J4
            END IF
 440     CONTINUE 
!!         write ( 6, * ) ' j3= ',j3, ' ind_z= ', ind_z, ' is= ', is ! %%%
         TSYS_NRML(IS) = TSYS_NRML(IS) + TSYS(J3)/TSYS_ZEN(IND_Z)
         KARR(IS) = KARR(IS) + 1
 430  CONTINUE 
!
      DO 450 J5=1,KP
         AZ_NRML(J5) = IARR(J5)/SCL
         TSYS_NRML(J5) = TSYS_NRML(J5)/KARR(J5)
 450  CONTINUE 
!
      RETURN
      END  SUBROUTINE AZIM_NRML  !#!#!#!
