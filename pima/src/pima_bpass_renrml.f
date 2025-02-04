      FUNCTION   PIMA_BPASS_RENRML ( PIM, IND_STA, IFRQ, POLAR, MASK_TYPE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_BPASS_RENRML computes the renormalization factor     *
! *   for a given intermediate frequency with index IFRQ using only      *
! *   a part of the bandwidth. Initially, the bandpass is normalized     *
! *   to unity over the *entire* bandwidth of the IF. However, often     *
! *   decorrelation occurs at the edges of the band. PIMA allows         *
! *   to specify the portion of the band that is considered              *
! *   "representative". It is expressed in parameters Bl, Bh. The        *
! *   representative bandwidth is [F_low + Bl*Fw, F_low*Bh*fw].          *
! *   Renormalization factor R = (sum B_r/N_r ) / Sum B_t/N_t, where     *
! *   B_r -- bandpass in the bandwidth is [F_low + Bl*Fw, F_low*Bh*fw],  *
! *   N_r -- the number of points in that bandwidth; B_t bandpass in the *
! *   total bandwidth, N_t the total number of points in the entire IF.  *
! *                                                                      *
! *   Factor R returned by PIMA_BPASS_RENRML is multiplied by every      *
! *   point of the bandpass makes its normalized over the representative *
! *   bandwidth [F_low + Bl*Fw, F_low*Bh*fw]. Usually, R > 1.0           *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       PIM ( PIMA__TYP ) -- Object with information related to        *
! *                            program PIMA.                             *
! *   IND_STA ( INTEGER*4 ) -- Station index.                            *
! *      IFRQ ( INTEGER*4 ) -- Index of the intermediate frequency.      *
! *     POLAR ( CHARACTER ) -- Polarization PIMA__SP1 or  PIMA__SP2.     *
! *                            If PIMA__SP1 then main bandpass is        *
! *                               renormalized.                          *
! *                            If PIMA__SP1 then polarization bandpass   *
! *                               is renormalized.                       *
! * MASK_TYPE ( INTEGER*4 ) -- Type of the band mask. List of supported  *
! *                            mask types:                               *
! *                            PIMA__MASK_AUTC = 1,                      *
! *                            PIMA__MASK_BPAS = 2,                      *
! *                            PIMA__MASK_FRNG = 3,                      *
! *                            PIMA__MASK_SPLT = 4.                      *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * RENRML_CROS_IF ( REAL*8    ) -- Renormalization factor for           *
! *                                 cross-correlation amplitude for      *
! *                                 a given IF, given polarization.      *
! * RENRML_CROS_CH ( REAL*8    ) -- Array of renormalization factors for *
! *                                 cross-correlation amplitude for      *
! *                                 all channels of the given IF, given  *
! *                                 polarization. Dimension: NCHN.       *
! * RENRML_AUTO_IF ( REAL*8    ) -- Renormalization factors for          *
! *                                 auto-correlation amplitude for       *
! *                                 a given IF, given polarization.      *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 18-AUG-2013 PIMA_BPASS_RENRML v3.2 (c) L. Petrov 03-OCT-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      REAL*8     PIMA_BPASS_RENRML
      REAL*8     BPS_MIN(2), BPS_MAX(2), PLR_MIN(2), PLR_MAX(2), &
     &           AMP_REM, AMP_REF, PLR_REF, PLR_REM
      INTEGER*4  IND_STA, IFRQ, MASK_TYPE, IUER
      CHARACTER  POLAR*(*)
      CHARACTER  STR*128
      REAL*4     AMPL, AMP_SUM, WEI_SUM
      INTEGER*1  MASK_CHN
      INTEGER*4  J1, J2, IND_STA_REF
!
! PIM%CONF%BPS_AMP_MIN
!
      AMP_SUM = 0.0D0
      WEI_SUM = 0.0D0
      IF ( IND_STA < 1  .OR.  IND_STA > PIM%NSTA ) THEN
           CALL CLRCH   ( STR )
           CALL INCH    ( IND_STA, STR )
           CALL ERR_LOG ( 6721, IUER, 'PIMA_BPAS_RENRML', 'Trap of internal '// &
     &         ' control: wrong parameter IND_STA: '//STR )
           RETURN 
      END IF
!
      IND_STA_REF = PIM%BPASS(IND_STA)%IND_STA_REF
      IF ( IND_STA_REF == 0 ) THEN
           PIMA_BPASS_RENRML = 1.0D0
           CALL ERR_LOG ( 0, IUER )
           RETURN       
      END IF
      IF ( IND_STA_REF < 1  .OR.  IND_STA_REF > PIM%NSTA ) THEN
           CALL CLRCH   ( STR )
           CALL INCH    ( IND_STA_REF, STR )
           CALL ERR_LOG ( 6722, IUER, 'PIMA_BPAS_RENRML', 'Trap of internal '// &
     &         ' control: wrong reference station was found in the bandpass '// &
     &         ' file: '//STR )
           RETURN 
      END IF
!      
      BPS_MIN =  1.D8
      BPS_MAX = -1.D8
      PLR_MIN =  1.D8
      PLR_MAX = -1.D8
      DO 410 J1=1,PIM%NCHN
         BPS_MIN(1) = MIN ( BPS_MIN(1), ABS( PIM%BPASS(IND_STA)%BPS(J1,IFRQ)     ) )
         BPS_MIN(2) = MIN ( BPS_MIN(2), ABS( PIM%BPASS(IND_STA_REF)%BPS(J1,IFRQ) ) )
         BPS_MAX(1) = MAX ( BPS_MAX(1), ABS( PIM%BPASS(IND_STA)%BPS(J1,IFRQ)     ) )
         BPS_MAX(2) = MAX ( BPS_MAX(2), ABS( PIM%BPASS(IND_STA_REF)%BPS(J1,IFRQ) ) )
         IF ( POLAR == PIMA__POLAR_2ND ) THEN
              PLR_MIN(1) = MIN ( PLR_MIN(1), ABS( PIM%PBP(IND_STA)%CMPL(J1,IFRQ)     ) )
              PLR_MIN(2) = MIN ( PLR_MIN(2), ABS( PIM%PBP(IND_STA_REF)%CMPL(J1,IFRQ) ) )
              PLR_MAX(1) = MAX ( PLR_MAX(1), ABS( PIM%PBP(IND_STA)%CMPL(J1,IFRQ)     ) )
              PLR_MAX(2) = MAX ( PLR_MAX(2), ABS( PIM%PBP(IND_STA_REF)%CMPL(J1,IFRQ) ) )
         END IF
 410  CONTINUE 
      DO 420 J2=1,PIM%NCHN
         IF ( J2 .GE. IDNINT(PIM%NCHN*PIM%CONF%SPLT_BPASS_NRML_RANGE(1)) .AND. &
     &        J2 .LE. IDNINT(PIM%NCHN*PIM%CONF%SPLT_BPASS_NRML_RANGE(2))       ) THEN
!     
              IF ( PIM%BANDPASS_MASK_STYLE .NE. PIMA__NO ) THEN
                   MASK_CHN = PIM%BANDPASS_MASK(J2,IFRQ,IND_STA,MASK_TYPE)* &
     &                        PIM%BANDPASS_MASK(J2,IFRQ,IND_STA_REF,MASK_TYPE)
                 ELSE
                   MASK_CHN = 1.0
              END IF
              IF ( POLAR == PIMA__POLAR_1ST ) THEN
                   IF ( BPS_MAX(1) > PIMA__BPS_AMP_SPLN_MIN*1.001D0 ) THEN
                        AMP_REM = ABS ( PIM%BPASS(IND_STA)%BPS(J2,IFRQ)     ) 
                      ELSE 
                        AMP_REM = 1.0D0
                   END IF
                   IF ( BPS_MAX(2) > PIMA__BPS_AMP_SPLN_MIN*1.001D0 ) THEN
                        AMP_REF = ABS ( PIM%BPASS(IND_STA_REF)%BPS(J2,IFRQ) ) 
                      ELSE
                        AMP_REF = 1.0D0
                   END IF
                   AMP_SUM = AMP_SUM + MASK_CHN * SQRT ( AMP_REM * AMP_REF )
                 ELSE IF ( POLAR == PIMA__POLAR_2ND ) THEN
                   IF ( PLR_MAX(1) > PIMA__BPS_AMP_SPLN_MIN*1.001D0 ) THEN
                        PLR_REM = ABS ( PIM%PBP(IND_STA)%CMPL(J2,IFRQ)      ) 
                     ELSE
                        PLR_REM = 1.0D0
                   END IF
                   IF ( BPS_MAX(2) > PIMA__BPS_AMP_SPLN_MIN*1.001D0 ) THEN
                        PLR_REF = ABS ( PIM%PBP(IND_STA_REF)%CMPL(J2,IFRQ)      ) 
                      ELSE
                        PLR_REF = 1.0D0
                   END IF
                   AMP_SUM = AMP_SUM + MASK_CHN * SQRT ( PLR_REM * PLR_REF )
              END IF
              WEI_SUM = WEI_SUM + MASK_CHN
         END IF
 420  CONTINUE 
      IF ( WEI_SUM > 0.999 ) THEN
           PIMA_BPASS_RENRML = AMP_SUM/WEI_SUM
         ELSE
           PIMA_BPASS_RENRML = 1.0D0
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  FUNCTION  PIMA_BPASS_RENRML  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   PIMA_BPASS_RENRML_PRE20170303 ( PIM, IFRQ, STA_IND, MASK_TYPE )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_BPASS_RENRML_PRE20170303
! *                                                                      *
! * # 10-MAR-2017 PIMA_BPASS_RENRML_PRE20170303 v1.1 (c) L. Petrov 14-JUN-2018 #  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*8     PIMA_BPASS_RENRML_PRE20170303 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  IFRQ, MASK_TYPE
      INTEGER*2  STA_IND(2)
      CHARACTER  POLAR*8
      REAL*4     AMPL, AMP_SUM, WEI_SUM
      REAL*4     WEI_MIN
      PARAMETER  ( WEI_MIN = 0.3 )
      INTEGER*4  J1
!
! --- OLD pre-2017.03.03 case. For compatibility only
!
      WEI_SUM = 0.0D0
      DO 410 J1=1,PIM%NCHN
         IF ( J1 .GE. IDNINT(PIM%NCHN*PIM%CONF%SPLT_BPASS_NRML_RANGE(1)) .AND. &
     &        J1 .LE. IDNINT(PIM%NCHN*PIM%CONF%SPLT_BPASS_NRML_RANGE(2))       ) THEN
!
! ----------- Keep in mind: the bandpass for a reference station was
! ----------- set to (1.0, 0.0). Parameter LOBS for the bandpass
! ----------- reference station is 0
!
              IF ( STA_IND(2) > 0 ) THEN
                   IF ( PIM%BPASS(STA_IND(1))%L_OBS > 0 .AND. &
     &                  PIM%BPASS(STA_IND(2))%L_OBS > 0       ) THEN
!
! --------------------- Neither station was the bandpass reference station
!
                        AMPL = SQRT( ABS( PIM%BPASS(STA_IND(1))%BPS(J1,IFRQ) )* &
     &                               ABS( PIM%BPASS(STA_IND(2))%BPS(J1,IFRQ) )  )
                     ELSE IF ( PIM%BPASS(STA_IND(1))%L_OBS == 0 .AND. &
     &                         PIM%BPASS(STA_IND(2))%L_OBS >  0       ) THEN
!
! --------------------- The first station was the bandpass reference station
!
                        AMPL = ABS( PIM%BPASS(STA_IND(2))%BPS(J1,IFRQ) )
                     ELSE IF ( PIM%BPASS(STA_IND(2))%L_OBS == 0 .AND. &
     &                         PIM%BPASS(STA_IND(1))%L_OBS >  0       ) THEN
!
! --------------------- The second station was the bandpass reference station
!
                        AMPL = ABS( PIM%BPASS(STA_IND(1))%BPS(J1,IFRQ) )
                     ELSE
!
! --------------------- This is a pathological case
!
                        AMPL = 1.0
                   END IF
                 ELSE IF ( STA_IND(2) == 0 ) THEN
!
! ---------------- Station-based case
!
                   AMPL = ABS( PIM%BPASS(STA_IND(1))%BPS(J1,IFRQ) )
              END IF
!
              IF ( STA_IND(2) > 0 ) THEN
!
! ---------------- Baseline case
!
                   IF ( PIM%CONF%BANDPASS_MASK_FILE .NE. PIMA__BPASS_NO ) THEN
                        AMP_SUM = AMP_SUM + AMPL* &
     &                            PIM%BANDPASS_MASK(J1,IFRQ,STA_IND(1),MASK_TYPE)* &
     &                            PIM%BANDPASS_MASK(J1,IFRQ,STA_IND(2),MASK_TYPE)
                        WEI_SUM = WEI_SUM + &
     &                            PIM%BANDPASS_MASK(J1,IFRQ,STA_IND(1),MASK_TYPE)* &
     &                            PIM%BANDPASS_MASK(J1,IFRQ,STA_IND(2),MASK_TYPE)
                     ELSE
                        AMP_SUM = AMP_SUM + AMPL
                        WEI_SUM = WEI_SUM + 1.0
                   END IF
                 ELSE 
!
! ---------------- Station case
!
                   IF ( PIM%CONF%BANDPASS_MASK_FILE .NE. PIMA__BPASS_NO ) THEN
                        AMP_SUM = AMP_SUM + AMPL*PIM%BANDPASS_MASK(J1,IFRQ,STA_IND(1),MASK_TYPE)
                        WEI_SUM = WEI_SUM +      PIM%BANDPASS_MASK(J1,IFRQ,STA_IND(1),MASK_TYPE)
                      ELSE
                        AMP_SUM = AMP_SUM + AMPL
                        WEI_SUM = WEI_SUM + 1.0
                   ENDIF
              END IF
         END IF
 410  CONTINUE 
      IF ( WEI_SUM > WEI_MIN ) THEN
           PIMA_BPASS_RENRML_PRE20170303 = AMP_SUM/WEI_SUM
         ELSE 
           PIMA_BPASS_RENRML_PRE20170303 = 1.0D0
      END IF
!
      RETURN
      END  FUNCTION  PIMA_BPASS_RENRML_PRE20170303 !#!  
