#include <mk5_preprocessor_directives.inc>
      SUBROUTINE PIMA_FRIP ( PIM, VTD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_FRIP
! *                                                                      *
! *  ### 20-DEC-2011   PIMA_FRIP   v1.0 (c)  L. Petrov  20-DEC-2011 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'fftw3.f'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      TYPE     ( VTD__TYPE   ) :: VTD
      TYPE     ( VTD__OBS_TYPE  ) :: OBS_TYP
      TYPE     ( SOUMAP__TYPE   ) :: CAL_MOD_MAP, TAG_MOD_MAP
!
      INTEGER*4  MB
      PARAMETER  ( MB = 128 )
      INTEGER*4  IUER
      CHARACTER  STR*128, STR1*128, BUF_INC(MB)*128, C_STA(PIM__MSTA)*8, &
     &           CI_STA(PIM__MSTA)*8, CE_STA(PIM__MSTA), &
     &           BUF_EXC(MB)*128, FIL_FRIP*128, FRIP_CAL_PLOT*8, &
     &           FRIP_TAG_PLOT*8, FRIPDIR*128, PLOT_TITLE*128, &
     &           CAL_PLOT_FINAM*128, TAG_PLOT_FINAM*128, FINAM_MOD*128, &
     &           FRIP_MAP_DIR*128, DEC_OLD_STR*15, RA_OLD_STR*15, &
     &           DEC_NEW_STR*15, RA_NEW_STR*15, PIMA_FRI_USED_VERS*24
      INTEGER*4  IS, J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           J14, J15, J16, J17, J18, J19, J20, &
     &           LT_OBS, LC_OBS, LIST_OBS(PIM__MOBS), LISC_OBS(PIM__MOBS), &
     &           L_STA, I_STA, LI_STA, LE_STA, &
     &           NB_INC, NB_EXC, IND_INC, IND_EXC, IND_OBS, IND_SOU_PIMA, &
     &           IND_U_MAX, IND_V_MAX, FRIP_CAL_RES, FRIP_TAG_RES, IND_BND, &
     &           MODE, IPAR, ISCL, IPAL, IDEV, IP, IND_STA(2), IND_RA_MAX, &
     &           IND_DEC_MAX, IND_SOU_VTD, ND, NA, GRID_ALG, IER
      ADDRESS__TYPE :: DIR_DESC, STREAM
      REAL*4     CAL_AMP_MAX, CAL_AMP_SCN_RAT, CAL_RMS
      REAL*4     TAG_AMP_MAX, TAG_AMP_SCN_RAT, TAG_RMS
      REAL*8     RA_NEW, DEC_NEW, FREQ_REF
      REAL*8     D_ALPHA, D_DELTA, ALPHA_NEW, DELTA_NEW, &
     &           ALPHA_OLD, DELTA_OLD, ALPHA_REF, DELTA_REF, &
     &           TAG_ALPHA, TAG_DELTA, AMP_BEST, SNR_BEST, &
     &           ALPHA_BEST, DELTA_BEST, FRIP_FRQ_MSEG, FRIP_TIM_MSEG 
      REAL*8     RA_STEP, DEC_STEP, RA_RANGE, DEC_RANGE, MAP_OVS
      REAL*8     PIMA__MAP_OVS
      PARAMETER  ( PIMA__MAP_OVS = 3.0D0 )
      LOGICAL*1  LEX, FL_TIMING, FL_FINAL_REFINE
      REAL*8     BSL_MAX, UV_MAX, VIS_SCL, MAP_SCL, MAP_RES
      INTEGER*4, EXTERNAL :: ADD_CLIST, ILEN, I_LEN, LTM_DIF
      INTEGER*4, EXTERNAL :: FFTWF_IMPORT_WISDOM_FROM_FILE
      REAL*8,    EXTERNAL :: GET_MAX_BASELEN
      ADDRESS__TYPE, EXTERNAL :: FUNC_OPENDIR, CLOSEDIR, FOPEN, FCLOSE
!
      GRID_ALG = PIMA__GRID_EXS
      FL_TIMING = .TRUE.
      FL_FINAL_REFINE = .FALSE.
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'PIMAVAR_MAP_DIR', STR )
      IF ( ILEN(STR) > 0 ) THEN
           PIM%CONF%FRIP_MAP_DIR = STR
      END IF
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'PIMAVAR_MAP_OVS', STR )
      IF ( ILEN(STR) > 0 ) THEN
           READ ( UNIT=STR, FMT='(F10.4)' ) MAP_OVS
         ELSE 
           MAP_OVS = PIMA__MAP_OVS 
      END IF
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'PIMAVAR_ATM_ZEN_FILE', STR )
      IF ( ILEN(STR) > 0 ) THEN
           PIM%CONF%FRIP_ATM_ZEN_FILE = STR
      END IF
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'PIMAVAR_FRIP_CAL_PLOT', STR )
      IF ( ILEN(STR) > 0 ) THEN
           FRIP_CAL_PLOT = STR
         ELSE
           FRIP_CAL_PLOT = 'NO'
      END IF
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'PIMAVAR_FRIP_TAG_PLOT', STR )
      IF ( ILEN(STR) > 0 ) THEN
           FRIP_TAG_PLOT = STR
         ELSE
           FRIP_TAG_PLOT = 'NO'
      END IF
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'PIMAVAR_FRIP_CAL_RES', STR )
      IF ( ILEN(STR) > 0 ) THEN
           CALL CHIN ( STR, FRIP_CAL_RES )
         ELSE
           FRIP_CAL_RES  = 384
      END IF
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'PIMAVAR_FRIP_TAG_RES', STR )
      IF ( ILEN(STR) > 0 ) THEN
           CALL CHIN ( STR, FRIP_TAG_RES )
         ELSE
           FRIP_TAG_RES  = 384
      END IF
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'PIMAVAR_FRIP_FRQ_MSEG', STR )
      IF ( ILEN(STR) > 0 ) THEN
           CALL CHIN ( STR, FRIP_FRQ_MSEG )
         ELSE
           FRIP_FRQ_MSEG = 128
      END IF
      IF ( FRIP_FRQ_MSEG > PIM%NCHN ) FRIP_FRQ_MSEG = PIM%NCHN 
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'PIMAVAR_FRIP_TIM_MSEG', STR )
      IF ( ILEN(STR) > 0 ) THEN
           CALL CHIN ( STR, FRIP_TIM_MSEG )
         ELSE
           FRIP_TIM_MSEG = 8192
      END IF
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'PIMAVAR_FRIP_MAP_DIR', FRIP_MAP_DIR )
      IF ( FRIP_MAP_DIR == 'SAME' ) THEN
           FRIP_MAP_DIR = PIM%CONF%EXPER_DIR(1:I_LEN(PIM%CONF%EXPER_DIR))//'/'// &
     &                    PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))// &
     &                    '_uvs'
      END IF
!
      RA_STEP   = 0.0D0
      RA_RANGE  = 0.0D0
      DEC_STEP  = 0.0D0
      DEC_RANGE = 0.0D0
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'PIMAVAR_FRIP_RA_STEP', STR )
      IF ( ILEN(STR) > 0 ) THEN
           READ ( UNIT=STR, FMT='(F10.5)' ) RA_STEP
           RA_STEP = RA_STEP/RAD__TO__ARCSEC
      END IF
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'PIMAVAR_FRIP_DEC_STEP', STR )
      IF ( ILEN(STR) > 0 ) THEN
           READ ( UNIT=STR, FMT='(F10.5)' ) DEC_STEP
           DEC_STEP = DEC_STEP/RAD__TO__ARCSEC
      END IF
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'PIMAVAR_FRIP_RA_RANGE', STR )
      IF ( ILEN(STR) > 0 ) THEN
           READ ( UNIT=STR, FMT='(F10.5)' ) RA_RANGE
           RA_RANGE = RA_RANGE/RAD__TO__ARCSEC
      END IF
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'PIMAVAR_FRIP_DEC_RANGE', STR )
      IF ( ILEN(STR) > 0 ) THEN
           READ ( UNIT=STR, FMT='(F10.5)' ) DEC_RANGE
           DEC_RANGE = DEC_RANGE/RAD__TO__ARCSEC
      END IF
      IF ( RA_STEP == 0.0D0 .OR. RA_RANGE == 0.0D0 ) THEN
           NA = 0
         ELSE
           NA = IDNINT ( RA_RANGE/RA_STEP )
      END IF
      IF ( DEC_STEP == 0.0D0 .OR. DEC_RANGE == 0.0D0 ) THEN
           ND = 0
         ELSE
           ND = IDNINT ( DEC_RANGE/DEC_STEP )
      END IF
!
      IF ( PIM%CONF%FRIB_OBS_STATUS  == PIMA__OBS_NO ) THEN
           CALL ERR_LOG ( 9101, IUER )
           RETURN
         ELSE IF ( PIM%CONF%FRIB_OBS_STATUS  == PIMA__OBS_ALL .AND. &
     &             PIM%CONF%FRIB_NOBS        == -1                  ) THEN
!
! -------- Create and populate an array of used observations
!
           PIM%CONF%FRIB_NOBS = PIM%NOBS
           ALLOCATE ( PIM%CONF%FRIB_OBS(PIM%CONF%FRIB_NOBS), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 4*PIM%CONF%FRIB_NOBS, STR )
                CALL ERR_LOG ( 9102, IUER, 'PIMA_FRIP', &
     &              'Failure to allocate '//STR(1:I_LEN(STR))//' bytes '//&
     &              'of dynamic memory for the list of observations' )
                RETURN
           END IF
!
           DO 410 J1=1,PIM%CONF%FRIB_NOBS
              PIM%CONF%FRIB_OBS(J1) = J1
 410       CONTINUE
      END IF
!
      IF ( PIM%CONF%FRIP_STA_INC_FILE == PIMA__FRIP_NO ) THEN
           LI_STA = PIM%NSTA
           CI_STA = PIM%C_STA
         ELSE
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT  ( PIM%CONF%FRIP_STA_INC_FILE, MB, &
     &                     BUF_INC, NB_INC, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9103, IUER, 'PIMA_FRIP', 'Error in '// &
     &              'an attempt to read station include file '// &
     &               PIM%CONF%FRIP_STA_INC_FILE )
                RETURN
           END IF
           LI_STA = 0
           DO 420 J2=1,NB_INC
              CALL CHASHL ( BUF_INC(J2) )
              IF ( BUF_INC(J2)(1:1) == ' ' ) GOTO 420
              IF ( BUF_INC(J2)(1:1) == '#' ) GOTO 420
              LI_STA = LI_STA + 1
              CI_STA(LI_STA) = BUF_INC(J2)
 420       CONTINUE
      END IF
!
      IF ( PIM%CONF%FRIP_STA_EXC_FILE == PIMA__FRIP_NO ) THEN
           LE_STA = 0
         ELSE
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT  ( PIM%CONF%FRIP_STA_EXC_FILE, MB, &
     &                     BUF_EXC, NB_EXC, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9104, IUER, 'PIMA_FRIP', 'Error in '// &
     &              'an attempt to read station exclude file '// &
     &               PIM%CONF%FRIP_STA_EXC_FILE )
                RETURN
           END IF
           LE_STA = 0
           DO 430 J3=1,NB_EXC
              CALL CHASHL ( BUF_EXC(J3) )
              IF ( BUF_EXC(J3)(1:1) == ' ' ) GOTO 430
              IF ( BUF_EXC(J3)(1:1) == '#' ) GOTO 430
              LE_STA = LE_STA + 1
              CE_STA(LE_STA) = BUF_EXC(J3)
 430       CONTINUE
      END IF
!
      DIR_DESC = FUNC_OPENDIR ( FRIP_MAP_DIR(1:I_LEN(FRIP_MAP_DIR))//CHAR(0) )
      IF ( DIR_DESC .EQ. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 9105, IUER, 'PIMA_FRIP', 'Failure in '// &
     &         'an attempt to open directory with source maps '// &
     &          FRIP_MAP_DIR(1:I_LEN(FRIP_MAP_DIR))//' -- '//STR )
           RETURN
      END IF
      IP = CLOSEDIR ( %VAL(DIR_DESC) )
!
! --- Set up for VTD
!
      OBS_TYP%PLRZ       = 'RR'
      OBS_TYP%FRQ_REF(1) = PIM%REF_FREQ
      OBS_TYP%N_BND      = 1
      OBS_TYP%DELAY_TYPE = VTD__ML__DTP
      OBS_TYP%FRQ_ION_EFF(1) = PIM%REF_FREQ
      OBS_TYP%STATUS     = VTD__BND
      IND_BND = 1 ! So far, only the first band
!
! --- Get fringe results
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_FRI_READ ( PIM, IND_BND, PIMA_FRI_USED_VERS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9106, IUER, 'PIMA_FRIP', 'Error in an '// &
     &         'attempt to read results of fringing from the fringe file '// &
     &          PIM%CONF%FRINGE_FILE )
           RETURN
      END IF
!
      IF ( PIM%CONF%FRIP_LAST_SCA == 0 ) PIM%CONF%FRIP_LAST_SCA = PIM%NSCA
      IF ( PIM%CONF%FFT_METHOD .NE. FFT_MKL ) THEN
!
! -------- Read FFT wisdom file
!
           STREAM = FOPEN ( PIM%CONF%FFT_CONFIG_FILE(1:I_LEN(PIM%CONF%FFT_CONFIG_FILE))//CHAR(0), 'r'//CHAR(0) )
           IF ( STREAM < 0 ) THEN
                CALL CLRCH   ( STR )
                CALL GERROR  ( STR )
                CALL ERR_LOG ( 9107, IUER, 'PIMA_FRIP', 'Error during '// &
     &              'opening FFTW wisdom file '//PIM%CONF%FFT_CONFIG_FILE )
                RETURN
           END IF
           IS = FFTWF_IMPORT_WISDOM_FROM_FILE ( %VAL(STREAM) )
           IF ( IS < 0 ) THEN
                CALL ERR_LOG ( 9108, IUER, 'PIMA_FRINGE', 'Error during '// &
     &              'reading FFTW wisdom file '//PIM%CONF%FFT_CONFIG_FILE )
                RETURN
           END IF
           IS = FCLOSE ( %VAL(STREAM) )
      END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!  write ( 6, * ) 'PIM%CONF%FRIP_MAP_DIR = ',PIM%CONF%FRIP_MAP_DIR
!  write ( 6, * ) 'PIM%CONF%FRIP_ATM_ZEN_FILE = ',PIM%CONF%FRIP_ATM_ZEN_FILE
!  write ( 6, * ) 'PIM%CONF%FRIP_STA_REFS(1) = ', PIM%CONF%FRIP_STA_REFS(1)
!  write ( 6, * ) 'PIM%CONF%FRIP_STA_REFS(2) = ', PIM%CONF%FRIP_STA_REFS(2)
!  write ( 6, * ) 'PIM%CONF%FRIP_NSCA = ', PIM%CONF%FRIP_NSCA ! %%%%
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      IF ( PIM%CONF%FRIP_FIRST_SCA == -1 .AND. &
     &     PIM%CONF%FRIP_FIRST_SCA == -1       ) THEN
           DO 530 J3=1,PIM%NSCA
              IF ( PIM%C_SOU(PIM%SCA(J3)%SOU_IND) == PIM%CONF%FRIP_SOU ) THEN
                   PIM%CONF%FRIP_FIRST_SCA = J3
                   PIM%CONF%FRIP_LAST_SCA  = J3
              END IF
 530       CONTINUE
           IF ( PIM%CONF%FRIP_FIRST_SCA == -1 ) THEN
                CALL ERR_LOG ( 9109, IUER, 'PIMA_FRINGE', 'Cannot find '// &
     &              'source '//PIM%CONF%FRIP_SOU//' specified in keyword '// &
     &              'FRIP.SCA' )
                RETURN
           END IF
      END IF
      DO 440 J4=1,PIM%CONF%FRIP_NSCA
         IF ( PIM%CONF%FRIP_SCA(0,J4) < PIM%CONF%FRIP_FIRST_SCA ) GOTO 440
         IF ( PIM%CONF%FRIP_SCA(0,J4) > PIM%CONF%FRIP_LAST_SCA  ) GOTO 440
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
              WRITE ( 6, 2120 ) PIM%CONF%FRIP_SCA(0,J4), &
     &                          PIM%CONF%FRIP_SCA(1,J4), &
     &                          PIM%C_SOU(PIM%SCA(PIM%CONF%FRIP_SCA(0,J4))%SOU_IND), &
     &                          PIM%C_SOU(PIM%SCA(PIM%CONF%FRIP_SCA(1,J4))%SOU_IND)
 2120         FORMAT ( 'PIMA_FRIP:  scans ',I3, 2X, I3, &
     &                 '  Sources  target: ', A, '  calibrator: ', A )
         END IF
         D_ALPHA = 0.0D0
         D_DELTA = 0.0D0
         CALL CLRCH ( STR )
         CALL GETENVAR ( 'PIMAVAR_FRIP_TARGET_ALPHA', STR )
         IF ( ILEN(STR) > 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL HR_TAT ( STR, TAG_ALPHA, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 9110, IUER, 'PIMA_FRIP', 'Failure in '// &
     &                 'attempt to decode keyword PIMAVAR_FRIP_TARGET_ALPHA: '// &
     &                  STR )
                   RETURN
              END IF
              D_ALPHA = TAG_ALPHA - PIM%SOU(PIM%SCA(PIM%CONF%FRIP_SCA(0,J4))%SOU_IND)%ALPHA
         END IF
!
         CALL CLRCH ( STR )
         CALL GETENVAR ( 'PIMAVAR_FRIP_TARGET_DELTA', STR )
         IF ( ILEN(STR) > 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL GR_TAT ( STR, TAG_DELTA, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 9111, IUER, 'PIMA_FRIP', 'Failure in '// &
     &                 'attempt to decode keyword PIMAVAR_FRIP_TARGET_DELTA: '// &
     &                  STR )
                   RETURN
              END IF
              D_DELTA = TAG_DELTA - PIM%SOU(PIM%SCA(PIM%CONF%FRIP_SCA(0,J4))%SOU_IND)%DELTA
         END IF
!
         LC_OBS = 0
         LT_OBS = 0
         L_STA = 0
         DO 450 J5=1,PIM%SCA(PIM%CONF%FRIP_SCA(1,J4))%NBAS
            IND_OBS = PIM%SCA(PIM%CONF%FRIP_SCA(1,J4))%OBS_IND(J5)
            IF ( .NOT. PIM%USE_OBS(IND_OBS) ) GOTO 450
            DO 460 J6=1,2
               IND_INC = LTM_DIF ( 0, LI_STA, CI_STA, &
     &                             PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(J6)) )
               IF ( IND_INC .LE. 0 ) GOTO 450
               IF ( LE_STA > 0 ) THEN
                    IND_EXC = LTM_DIF ( 0, LE_STA, CE_STA, &
     &                                   PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(J6)) )
                    IF ( IND_EXC > 0 ) GOTO 450
               END IF
 460        CONTINUE
            I_STA = ADD_CLIST ( PIM__MSTA, L_STA, C_STA, &
     &                          PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)), IER )
            I_STA = ADD_CLIST ( PIM__MSTA, L_STA, C_STA, &
     &                          PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)), IER )
            LC_OBS = LC_OBS + 1
            LISC_OBS(LC_OBS) = IND_OBS
 450     CONTINUE
         IF ( L_STA == 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J4, STR )
              CALL ERR_LOG ( 9112, IUER, 'PIMA_FRIP', 'Cannot find any '// &
     &            'stations for scan '//STR )
              RETURN
         END IF
         IF ( L_STA < 3 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J4, STR )
              CALL CLRCH ( STR1 )
              CALL INCH  ( L_STA, STR1 )
              CALL ERR_LOG ( 9113, IUER, 'PIMA_FRIP', 'Too few stations '// &
     &            'for scan '//STR(1:I_LEN(STR))//' -- only '//STR1 )
              RETURN
         END IF
!
         CALL SORT_CH ( L_STA, C_STA )
         PIM%FRIP(PIMA__CAL)%L_STA = L_STA
         PIM%FRIP(PIMA__TAG)%L_STA = L_STA
         PIM%FRIP(PIMA__CAL)%C_STA = C_STA
         PIM%FRIP(PIMA__TAG)%C_STA = C_STA
!
         DO 470 J7=1,PIM%CONF%FRIP_N_STA_REF
            PIM%FRIP(PIMA__CAL)%IND_STA_REF = LTM_DIF ( 0, L_STA, C_STA, PIM%CONF%FRIP_STA_REFS(J7) )
            IF ( PIM%FRIP(PIMA__CAL)%IND_STA_REF > 0 ) GOTO 870
 470     CONTINUE
 870     CONTINUE
         IF ( PIM%FRIP(PIMA__CAL)%IND_STA_REF .LE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J4, STR )
              CALL ERR_LOG ( 9114, IUER, 'PIMA_FRIP', 'Cannot find any '// &
     &            'reference station for scan '//STR(1:I_LEN(STR))// &
     &            '. The first reference station: '// &
     &            PIM%CONF%FRIP_STA_REFS(1) )
              RETURN
         END IF
         PIM%FRIP(PIMA__TAG)%IND_STA_REF = PIM%FRIP(PIMA__CAL)%IND_STA_REF
!
         DO 480 J8=1,PIM%SCA(PIM%CONF%FRIP_SCA(0,J4))%NBAS
            IND_OBS = PIM%SCA(PIM%CONF%FRIP_SCA(0,J4))%OBS_IND(J8)
            DO 490 J9=1,2
               IND_INC = LTM_DIF ( 0, LI_STA, CI_STA, &
     &                             PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(J9)) )
               IF ( IND_INC .LE. 0 ) GOTO 480
               IF ( LE_STA > 0 ) THEN
                    IND_EXC = LTM_DIF ( 0, LE_STA, CE_STA, &
     &                                   PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(J9)) )
                    IF ( IND_EXC > 0 ) GOTO 480
               END IF
 490        CONTINUE
            LT_OBS = LT_OBS + 1
            LIST_OBS(LT_OBS) = IND_OBS
 480     CONTINUE
!
! ------ Build the frip file name FIL
!
         FRIPDIR = PIM%CONF%EXPER_DIR(1:I_LEN(PIM%CONF%EXPER_DIR))// &
     &             '/'//PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))// &
     &             '_frip'
         CALL CLRCH  ( STR )
         CALL INCH   ( PIM%CONF%FRIP_SCA(0,J4), STR(1:4) )
         CALL CHASHR ( STR(1:4) )
         CALL BLANK_TO_ZERO ( STR(1:4) )
         FIL_FRIP = FRIPDIR(1:I_LEN(FRIPDIR))//'/sca_'//STR(1:4)//'.frip'
         INQUIRE ( FILE=FIL_FRIP, EXIST=LEX )
!
         IF ( LEX ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_FRIP_READ ( PIM, FIL_FRIP, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( PIM%CONF%FRIP_SCA(1,J4), STR )
                   CALL ERR_LOG ( 9115, IUER, 'PIMA_FRIP', 'Failure in '// &
     &                 'an attempt to load calibrator observations for '// &
     &                 'scan '//STR )
                   RETURN
              END IF
!
              PIM%FRIP(PIMA__CAL)%L_STA = L_STA
              PIM%FRIP(PIMA__TAG)%L_STA = L_STA
              PIM%FRIP(PIMA__CAL)%C_STA = C_STA
              PIM%FRIP(PIMA__TAG)%C_STA = C_STA
!
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                   WRITE ( 6, 210 ) PIM%FRIP(PIMA__TAG)%IND_SCA, &
     &                              PIM%FRIP(PIMA__CAL)%IND_SCA, &
     &                              FIL_FRIP(1:I_LEN(FIL_FRIP))
 210               FORMAT ( 'PIMA_FRIP: loaded visibilities for tag/cal scans ', &
     &                       I4, 1X, I4/' from file ',A )
              END IF
            ELSE
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_FRIP_LOAD ( PIM, VTD, PIMA__CAL, PIM%CONF%FRIP_SCA(1,J4), &
     &                              LC_OBS, LISC_OBS, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( PIM%CONF%FRIP_SCA(1,J4), STR )
                   CALL ERR_LOG ( 9116, IUER, 'PIMA_FRIP', 'Failure in '// &
     &                 'an attempt to load calibrator observations for '// &
     &                 'scan '//STR )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_FRIP_LOAD ( PIM, VTD, PIMA__TAG, PIM%CONF%FRIP_SCA(0,J4), &
     &                              LT_OBS, LIST_OBS, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( PIM%CONF%FRIP_SCA(0,J4), STR )
                   CALL ERR_LOG ( 9117, IUER, 'PIMA_FRIP', 'Failure in '// &
     &                 'an attempt to load calibrator observations for '// &
     &                 'scan '//STR )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_FRIP_WRITE ( PIM, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( PIM%CONF%FRIP_SCA(0,J4), STR )
                   CALL ERR_LOG ( 9118, IUER, 'PIMA_FRIP', 'Failure in '// &
     &                 'an attempt to write calibrator observations for '// &
     &                 'scan '//STR )
                   RETURN
              END IF
         END IF
!
         CALL PIMA_FRIP_SET_EPOCH ( PIM, IER )
!
         CALL ERR_PASS ( IUER, IER )
         ALLOCATE ( PIM%FRIP(PIMA__CAL)%USED(PIM%FRIP(PIMA__CAL)%NOBS), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( PIM%CONF%FRIP_SCA(0,J4), STR )
              CALL ERR_LOG ( 9119, IUER, 'PIMA_FRIP', 'Failure in '// &
     &             'an attempt to allocate array (cal)USED for scan '//STR )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         ALLOCATE ( PIM%FRIP(PIMA__TAG)%USED(PIM%FRIP(PIMA__TAG)%NOBS), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( PIM%CONF%FRIP_SCA(0,J4), STR )
              CALL ERR_LOG ( 9120, IUER, 'PIMA_FRIP', 'Failure in '// &
     &             'an attempt to allocate array (tag)USED for scan '//STR )
              RETURN
         END IF
!
         DO 4100 J10=1,PIM%FRIP(PIMA__CAL)%NOBS
            IND_OBS = PIM%FRIP(PIMA__CAL)%OBS(J10)%IND_OBS
            IND_STA(1) = LTM_DIF ( 0, PIM%FRIP(PIMA__CAL)%L_STA, PIM%FRIP(PIMA__CAL)%C_STA, &
     &                             PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)) )
            IND_STA(2) = LTM_DIF ( 0, PIM%FRIP(PIMA__CAL)%L_STA, PIM%FRIP(PIMA__CAL)%C_STA, &
     &                             PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)) )
            IF ( IND_STA(1) > 0 .AND. IND_STA(2) > 0 ) THEN
                 PIM%FRIP(PIMA__CAL)%USED(J10) = .TRUE.
               ELSE
                 PIM%FRIP(PIMA__CAL)%USED(J10) = .FALSE.
            END IF
 4100    CONTINUE
!
         DO 4110 J11=1,PIM%FRIP(PIMA__TAG)%NOBS
            IND_OBS = PIM%FRIP(PIMA__TAG)%OBS(J11)%IND_OBS
            IND_STA(1) = LTM_DIF ( 0, PIM%FRIP(PIMA__TAG)%L_STA, PIM%FRIP(PIMA__TAG)%C_STA, &
     &                             PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)) )
            IND_STA(2) = LTM_DIF ( 0, PIM%FRIP(PIMA__TAG)%L_STA, PIM%FRIP(PIMA__TAG)%C_STA, &
     &                             PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)) )
            IF ( IND_STA(1) > 0 .AND. IND_STA(2) > 0 ) THEN
                 PIM%FRIP(PIMA__TAG)%USED(J11) = .TRUE.
               ELSE
                 PIM%FRIP(PIMA__TAG)%USED(J11) = .FALSE.
            END IF
 4110    CONTINUE
!
         CALL ERR_PASS ( IUER, IER )
         CALL PIMA_SPLT_FITSTA ( PIM, PIM%SCA(PIM%CONF%FRIP_SCA(1,J4))%SOU_IND, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 9121, IER, 'PIMA_FRIP', 'Failure in '// &
     &            'attempt to fix fringe fitting to make it station '// &
     &            'based for source '// &
     &            PIM%SOU(PIM%SCA(PIM%CONF%FRIP_SCA(1,J4))%SOU_IND)%J2000_NAME )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL PIMA_FRIP_APR_ADJ ( PIM, VTD, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( PIM%FRIP(PIMA__CAL)%IND_SCA, STR )
              CALL ERR_LOG ( 9122, IER, 'PIMA_FRIP', 'Failure in '// &
     &            'attempt to adjust residual phases and residual group '// &
     &            'delay and phase delay rates for better a priori model '// &
     &            'when calibrator source '// &
     &            PIM%SOU(PIM%SCA(PIM%CONF%FRIP_SCA(1,J4))%SOU_IND)%J2000_NAME// &
     &            ' scan id '//STR(1:I_LEN(STR))//' has been processed' )
              RETURN
         END IF
!
         BSL_MAX = GET_MAX_BASELEN ( PIM, L_STA, C_STA )
         UV_MAX = BSL_MAX*PIM%REF_FREQ/VTD__C
         VIS_SCL = UV_MAX*MAP_OVS
         MAP_RES = 0.5D0/VIS_SCL
!
         ALLOCATE ( PIM%FRIP(PIMA__CAL)%MAP(PIM%CONF%FRIP_RESOLUTION, &
     &                                      PIM%CONF%FRIP_RESOLUTION), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH8 ( INT8(8)*INT8(PIM%CONF%FRIP_RESOLUTION)* &
     &                      INT8(PIM%CONF%FRIP_RESOLUTION), STR )
              CALL CLRCH  ( STR1 )
              CALL INCH   ( PIM%CONF%FRIP_SCA(1,J4), STR1 )
              CALL ERR_LOG ( 9123, IUER, 'PIMA_FRIP', 'Failure in '// &
     &            'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &            ' bytes of dynamic memory for the map of the '// &
     &            'calibrator source scan '//STR1 )
              RETURN
         END IF
!
         ALLOCATE ( PIM%FRIP(PIMA__TAG)%MAP(PIM%CONF%FRIP_RESOLUTION, &
     &                                      PIM%CONF%FRIP_RESOLUTION), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH8 ( INT8(8)*INT8(PIM%CONF%FRIP_RESOLUTION)* &
     &                      INT8(PIM%CONF%FRIP_RESOLUTION), STR )
              CALL CLRCH  ( STR1 )
              CALL INCH   ( PIM%CONF%FRIP_SCA(1,J4), STR1 )
              CALL ERR_LOG ( 9124, IUER, 'PIMA_FRIP', 'Failure in '// &
     &            'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &            ' bytes of dynamic memory for the map of the '// &
     &            'target source scan '//STR1 )
              RETURN
         END IF
!
         FINAM_MOD = FRIP_MAP_DIR(1:I_LEN(FRIP_MAP_DIR))//'/'// &
     &               PIM%SOU(PIM%SCA(PIM%CONF%FRIP_SCA(1,J4))%SOU_IND)%J2000_NAME// &
     &               '_'//PIM%CONF%BAND//'_map.fits'
!
         CALL ERR_PASS ( IUER, IER )
         CALL GET_FITS_MAP ( FINAM_MOD, .TRUE., .FALSE., CAL_MOD_MAP, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( PIM%CONF%FRIP_SCA(1,J4), STR )
              CALL ERR_LOG ( 9125, IUER, 'PIMA_FRIP', 'Failure in '// &
     &            'an attempt to read the model map for calibrator '// &
     &            'source '//PIM%SOU(PIM%SCA(PIM%CONF%FRIP_SCA(1,J4))%SOU_IND)%J2000_NAME// &
     &            ' for scan '//STR )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL PIMA_FRIP_AVR ( PIM, PIMA__CAL, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( PIM%CONF%FRIP_SCA(1,J4), STR )
              CALL ERR_LOG ( 9126, IUER, 'PIMA_FRIP', 'Failure in '// &
     &            'an attempt to write calibrator observations for '// &
     &            'scan '//STR )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL PIMA_FRIP_AMCLO ( PIM, PIMA__CAL, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( PIM%CONF%FRIP_SCA(1,J4), STR )
              CALL ERR_LOG ( 9127, IUER, 'PIMA_FRIP', 'Failure in '// &
     &            'an attempt to solve for phase ambiguities within '// &
     &            'scan '//STR )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL PIMA_FRIP_SC ( PIM, PIMA__CAL, PIMA__SC_PHS, CAL_MOD_MAP, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 9128, IUER, 'PIMA_FRIP', 'Error in '// &
     &            'an attempt to perform first self-calibration' )
              RETURN
         END IF
!
! ------ Check misclosure
!
         CALL ERR_PASS ( IUER, IER )
         CALL PIMA_FRIP_CHECK_MISCLS ( PIM, PIMA__CAL, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 9129, IUER, 'PIMA_FRIP', 'The misclosure '// &
     &            'test for the calibrator has failed' )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL PIMA_FRIP_SC ( PIM, PIMA__CAL, PIMA__SC_AMP, CAL_MOD_MAP, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 9129, IUER, 'PIMA_FRIP', 'Error in '// &
     &            'an attempt to perform first self-calibration' )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL PIMA_FRIP_GRID ( PIM, PIMA__CAL, GRID_ALG, VIS_SCL, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( PIM%CONF%FRIP_SCA(1,J4), STR )
              CALL ERR_LOG ( 9130, IUER, 'PIMA_FRIP', 'Failure in '// &
     &            'an attempt to write calibrator observations for '// &
     &            'scan '//STR )
              RETURN
         END IF
!
! ------ Perform inverse FFT of the gridded visibilities for the calibrator
!
         CALL ERR_PASS  ( IUER, IER )
         CALL FFT_2D_C8 ( PIM%CONF%FRIP_RESOLUTION, PIM%CONF%FRIP_RESOLUTION, &
     &                    -1, PIM%FRIP(PIMA__CAL)%MAP, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 9131, IUER, 'PIMA_FRIP', 'Error in '// &
     &            'an attempt to run 2D FFT transform' )
              RETURN
         END IF
!
         CALL ERR_PASS  ( IUER, IER )
         CALL PIMA_GRID_CORR ( PIM%CONF%FRIP_RESOLUTION, &
     &                         PIM%FRIP(PIMA__CAL)%MAP, GRID_ALG, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 9132, IUER, 'PIMA_FRIP', 'Error in '// &
     &            'an attempt to perform grid correction' )
              RETURN
         END IF
!
         CALL FIND_AMPL_MAX ( PIM%CONF%FRIP_RESOLUTION, &
     &                        PIM%CONF%FRIP_RESOLUTION, &
     &                        PIM%FRIP(PIMA__CAL)%MAP, &
     &                        IND_U_MAX, IND_V_MAX, &
     &                        CAL_AMP_MAX, CAL_AMP_SCN_RAT, CAL_RMS )
         WRITE ( 6, 110 ) PIM%CONF%FRIP_SCA(1,J4), PIMA__FRIP_SCATYP(PIMA__CAL), &
     &                    PIM%SOU(PIM%SCA(PIM%CONF%FRIP_SCA(1,J4))%SOU_IND)%J2000_NAME, &
     &                    CAL_AMP_MAX*1.D3, CAL_AMP_SCN_RAT, CAL_RMS*1.D3, &
     &                    CAL_AMP_MAX/CAL_RMS, MAP_OVS, MAP_RES*RAD__TO__MAS
 110     FORMAT ( 'PIMA_FRIP SCAN: ', I4, 1X, A, 2X, A, ' Cal_peak: ', F8.2, &
     &            ' mJy  Sec_max: ', F5.3/ &
     &            ' Dirty_map_rms: ', F8.2, ' mJy ', &
     &            ' SNR: ', F6.2, ' Map_ovs: ', F6.2, ' Map_scale: ', &
     &             F9.3, ' mas ' )
!
         IF ( FRIP_CAL_PLOT .NE. PIMA__PLOT_NO ) THEN
              PLOT_TITLE = 'Dirty map of calibrator '// &
     &                      PIM%SOU(PIM%SCA(PIM%CONF%FRIP_SCA(1,J4))%SOU_IND)%J2000_NAME
              IF ( FRIP_CAL_PLOT == PIMA__PLOT_XW ) THEN
                   IDEV = 1
                ELSE IF ( FRIP_CAL_PLOT == PIMA__PLOT_GIF ) THEN
                   IDEV = 3
                ELSE IF ( FRIP_CAL_PLOT == PIMA__PLOT_PS ) THEN
                   IDEV = 4
              END IF
              MODE = 2
              IPAR = 1
              ISCL = 2
              IPAL = 7
              MAP_SCL = MAP_RES*RAD__TO__MAS
              CAL_PLOT_FINAM = FRIPDIR(1:I_LEN(FRIPDIR))//'/sca_'// &
     &                         STR(1:4)//'_cal_'// &
     &                         PIM%SOU(PIM%SCA(PIM%CONF%FRIP_SCA(1,J4))%SOU_IND)%J2000_NAME
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!  write ( 6, * ) ' frip_cal_plot = ', frip_cal_plot, &     ! %%%%%%%%%
!     &            ' frip_cal_res = ', frip_cal_res, &      ! %%%%%%%%%
!     &            ' cpf= ', CAL_PLOT_FINAM ! %%%%%%%%%%%%%%%%%%%%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_2D_MAP_PLOT ( CAL_PLOT_FINAM, PIM, &
     &                           PIM%FRIP(PIMA__CAL)%MAP, PLOT_TITLE, &
     &                           IND_U_MAX, IND_V_MAX, &
     &                           PIM%CONF%FRIP_RESOLUTION, &
     &                           FRIP_CAL_RES, MAP_SCL, &
     &                           MODE, IPAR, ISCL, IPAL, IDEV, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 9133, IUER, 'PIMA_FRIP', 'Error in '// &
     &                 'an attempt to make a plot' )
                  RETURN
            END IF
         END IF
!
         PIM%FRIP(PIMA__TAG)%GAIN = PIM%FRIP(PIMA__CAL)%GAIN
!
         FREQ_REF = PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%FREQ
         IND_SOU_PIMA = PIM%SCA(PIM%FRIP(PIMA__TAG)%IND_SCA)%SOU_IND
!
         ALPHA_REF = PIM%SOU(IND_SOU_PIMA)%ALPHA
         DELTA_REF = PIM%SOU(IND_SOU_PIMA)%DELTA
         IF ( D_ALPHA .NE. 0.0D0 .OR. D_DELTA .NE. 0.0D0 ) THEN
!
! ----------- UV shift to new source positions
!
              ALPHA_NEW = ALPHA_REF + D_ALPHA
              DELTA_NEW = DELTA_REF + D_DELTA
              DO 4120 J12=1,PIM%FRIP(PIMA__TAG)%NOBS
                  IF ( .NOT. PIM%FRIP(PIMA__TAG)%USED(J12) ) GOTO 4120
                  IER = -1
                  CALL PIMA_UV_SHIFT ( PIM, VTD, PIMA__TAG, J12, &
     &                         ALPHA_REF, DELTA_REF, ALPHA_NEW, DELTA_NEW, &
     &                         FREQ_REF, PIM%FRIP(PIMA__TAG)%TIM_EPC, &
     &                         PIM%FRIP(PIMA__TAG)%OBS(J12)%UVW_SRT, IER )
 4120         CONTINUE
              ALPHA_REF = ALPHA_NEW
              DELTA_REF = DELTA_NEW
         END IF
!
         ALPHA_OLD = ALPHA_REF
         DELTA_OLD = DELTA_REF
         ALPHA_BEST = ALPHA_REF
         DELTA_BEST = DELTA_REF
         AMP_BEST = -1.0D0
         SNR_BEST = -1.0D0
         DO 4130 J13=-ND,ND
            DO 4140 J14=-NA,NA
               IF ( FL_TIMING ) CALL TIM_INIT ( )
               D_DELTA = J13*DEC_STEP
               D_ALPHA = J14*RA_STEP
               ALPHA_NEW = ALPHA_REF + D_ALPHA
               DELTA_NEW = DELTA_REF + D_DELTA
               DO 4150 J15=1,PIM%FRIP(PIMA__TAG)%NOBS
                  IF ( .NOT. PIM%FRIP(PIMA__TAG)%USED(J15) ) GOTO 4150
                  IER = -1
                  CALL PIMA_UV_SHIFT ( PIM, VTD, PIMA__TAG, J15, &
     &                         ALPHA_OLD, DELTA_OLD, ALPHA_NEW, DELTA_NEW, &
     &                         FREQ_REF, PIM%FRIP(PIMA__TAG)%TIM_EPC, &
     &                         PIM%FRIP(PIMA__TAG)%OBS(J15)%UVW_SRT, IER )
 4150          CONTINUE
               ALPHA_OLD = ALPHA_NEW
               DELTA_OLD = DELTA_NEW
!!
!              IF ( FL_TIMING ) THEN
!                   CALL TIM_TP ( %VAL(0), %VAL(0), %VAL(0), %VAL(0) )
!                   WRITE ( 6, * ) '-- this was time for uv-split' 
!                   CALL TIM_INIT ( )
!              END IF
!!
               CALL ERR_PASS ( IUER, IER )
               CALL PIMA_FRIP_AVR ( PIM, PIMA__TAG, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( PIM%CONF%FRIP_SCA(0,J4), STR )
                    CALL ERR_LOG ( 9134, IUER, 'PIMA_FRIP', 'Failure in '// &
     &                  'an attempt to write calibrator observations for '// &
     &                  'scan '//STR )
                    RETURN
               END IF
!!
!              IF ( FL_TIMING ) THEN
!                   CALL TIM_TP ( %VAL(0), %VAL(0), %VAL(0), %VAL(0) )
!                   WRITE ( 6, * ) '-- this was time for avr' 
!                   CALL TIM_INIT ( )
!              END IF
!!
               CALL ERR_PASS ( IUER, IER )
               CALL PIMA_FRIP_AMCLO ( PIM, PIMA__TAG, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( PIM%CONF%FRIP_SCA(0,J4), STR )
                    CALL ERR_LOG ( 9135, IUER, 'PIMA_FRIP', 'Failure in '// &
     &                  'an attempt to solve for phase ambiguities within '// &
     &                  'scan '//STR )
                    RETURN
               END IF
!!
!              IF ( FL_TIMING ) THEN
!                   CALL TIM_TP ( %VAL(0), %VAL(0), %VAL(0), %VAL(0) )
!                   WRITE ( 6, * ) '-- this was time for amclo' 
!                   CALL TIM_INIT ( )
!              END IF
!!
               CALL ERR_PASS ( IUER, IER )
               CALL PIMA_FRIP_GRID ( PIM, PIMA__TAG, GRID_ALG, VIS_SCL, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( PIM%CONF%FRIP_SCA(1,J4), STR )
                    CALL ERR_LOG ( 9136, IUER, 'PIMA_FRIP', 'Failure in '// &
     &                  'an attempt to write calibrator observations for '// &
     &                  'scan '//STR )
                    RETURN
               END IF
!!
!               IF ( FL_TIMING ) THEN
!                    CALL TIM_TP ( %VAL(0), %VAL(0), %VAL(0), %VAL(0) )
!                    WRITE ( 6, * ) '-- this was time for grid' 
!                    CALL TIM_INIT ( )
!               END IF
!!
!
! ------------ Perform inverse FFT of the gridded visibilities of the target
!
               CALL ERR_PASS  ( IUER, IER )
               CALL FFT_2D_C8 ( PIM%CONF%FRIP_RESOLUTION, PIM%CONF%FRIP_RESOLUTION, &
                                -1, PIM%FRIP(PIMA__TAG)%MAP, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 9137, IUER, 'PIMA_FRIP', 'Error in '// &
     &                  'an attempt to run 2D FFT transform' )
                    RETURN
               END IF
!!
!              IF ( FL_TIMING ) THEN
!                   CALL TIM_TP ( %VAL(0), %VAL(0), %VAL(0), %VAL(0) )
!                   WRITE ( 6, * ) '-- this was time for FFT' 
!                   CALL TIM_INIT ( )
!              END IF
!!
               CALL ERR_PASS  ( IUER, IER )
               CALL PIMA_GRID_CORR ( PIM%CONF%FRIP_RESOLUTION, &
     &                               PIM%FRIP(PIMA__TAG)%MAP, &
     &                               PIMA__GRID_EXS, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 9138, IUER, 'PIMA_FRIP', 'Error in '// &
     &                  'an attempt to perform grid correction' )
                    RETURN
               END IF
!!
!               IF ( FL_TIMING ) THEN
!                    CALL TIM_TP ( %VAL(0), %VAL(0), %VAL(0), %VAL(0) )
!                    WRITE ( 6, * ) '-- this was time for grid-correction' 
!                    CALL TIM_INIT ( )
!               END IF
!!
               CALL FIND_AMPL_MAX ( PIM%CONF%FRIP_RESOLUTION, &
     &                              PIM%CONF%FRIP_RESOLUTION, &
     &                              PIM%FRIP(PIMA__TAG)%MAP, &
     &                              IND_U_MAX, IND_V_MAX, &
     &                              TAG_AMP_MAX, TAG_AMP_SCN_RAT, TAG_RMS )
               IF ( FL_TIMING ) THEN
                    CALL TIM_TP ( %VAL(0), %VAL(0), %VAL(0), %VAL(0) )
                    WRITE ( 6, * ) '-- this was time for all' 
                    CALL TIM_INIT ( )
               END IF
               WRITE ( 6, 130 ) PIM%CONF%FRIP_SCA(0,J4), PIMA__FRIP_SCATYP(PIMA__TAG), &
     &                          PIM%SOU(PIM%SCA(PIM%CONF%FRIP_SCA(0,J4))%SOU_IND)%J2000_NAME, &
     &                          TAG_AMP_MAX*1.D3, TAG_AMP_SCN_RAT, TAG_RMS*1.D3, &
     &                          J14, J13, IND_U_MAX, IND_V_MAX
 130           FORMAT ( 'PIMA_FRIP SCAN: ',I4, 1X, A, 2X, A, ' Peak: ', F8.2, &
     &                  ' mJy  Sec_max: ', F5.3/   &
     &                  'Dirty_map_rms: ', F8.2, ' mJy ', 2X, &
     &                  ' Shift ra: ', I4,' dec: ', I4, &
     &                  ' Ind_max u: ', I5, ' v: ', I5 )
               IND_SOU_VTD = 0
               DO 4160 J16=1,VTD%L_SOU
                  IF ( VTD%SOU(J16)%IVS_NAME == PIM%SOU(PIM%SCA(PIM%CONF%FRIP_SCA(0,J4))%SOU_IND)%IVS_NAME ) THEN
                       IND_SOU_VTD = J16
                  END IF
 4160          CONTINUE
               IF ( IND_U_MAX .LE. PIM%CONF%FRIP_RESOLUTION/2 ) THEN
                    IND_RA_MAX = IND_U_MAX - 1
                  ELSE
                    IND_RA_MAX = IND_U_MAX - (PIM%CONF%FRIP_RESOLUTION + 1)
               END IF
               IF ( IND_V_MAX .LE. PIM%CONF%FRIP_RESOLUTION/2 ) THEN
                    IND_DEC_MAX = IND_V_MAX - 1
                  ELSE
                    IND_DEC_MAX = IND_V_MAX - (PIM%CONF%FRIP_RESOLUTION + 1)
               END IF
               DEC_NEW = VTD%SOU(IND_SOU_VTD)%DELTA - IND_DEC_MAX*MAP_RES
               RA_NEW  = VTD%SOU(IND_SOU_VTD)%ALPHA - IND_RA_MAX*MAP_RES/DCOS(DEC_NEW)
               CALL RG_TAT ( VTD%SOU(IND_SOU_VTD)%DELTA, 5, DEC_OLD_STR, -2 )
               CALL RH_TAT ( VTD%SOU(IND_SOU_VTD)%ALPHA, 6, RA_OLD_STR,  -2 )
               CALL RG_TAT ( DEC_NEW, 5, DEC_NEW_STR, -2 )
               CALL RH_TAT ( RA_NEW,  6, RA_NEW_STR,  -2 )
               IF ( DEC_OLD_STR(1:1) == ' ' ) DEC_OLD_STR(1:1) = '+'
               CALL CHASHL ( RA_OLD_STR )
               IF ( DEC_NEW_STR(1:1) == ' ' ) DEC_NEW_STR(1:1) = '+'
               CALL CHASHL ( RA_NEW_STR )
!
               WRITE  ( 6, 140 ) PIM%SOU(PIM%SCA(PIM%CONF%FRIP_SCA(0,J4))%SOU_IND)%IVS_NAME, &
     &                           PIM%SOU(PIM%SCA(PIM%CONF%FRIP_SCA(0,J4))%SOU_IND)%J2000_NAME, &
     &                           TAG_AMP_MAX/TAG_RMS, TAG_RMS*1.D3, &
     &                           RA_OLD_STR, DEC_OLD_STR, IND_U_MAX, IND_V_MAX, &
     &                           RA_NEW_STR, DEC_NEW_STR, &
     &                           MAP_RES*RAD__TO__MAS
 140           FORMAT ( 'PIMA_FRIP  Target ', A, 2X, A, &
     &                  '  SNR: ', F6.2, ' Map_rms: ', F6.2, ' mJy'/ &
     &                   2X, 'APRIORI coordinates: ', A, 2X, A, &
     &                   '  UV_max: ', I5, 1X, I5/ &
     &                   2X, 'NEW     coordinates: ', A, 2X, A, &
     &                   '  Pixel_step: ', F5.3, ' mas'/  )
!!               IF ( TAG_AMP_MAX > AMP_BEST ) THEN
               IF ( TAG_AMP_MAX/TAG_RMS > SNR_BEST ) THEN
                    AMP_BEST = TAG_AMP_MAX
                    SNR_BEST = TAG_AMP_MAX/TAG_RMS
                    ALPHA_BEST = RA_NEW
                    DELTA_BEST = DEC_NEW
               END IF
 4140       CONTINUE
 4130    CONTINUE
!
! ------ UV shift to new source positions
!
         IF ( FL_FINAL_REFINE ) THEN
              DO 4170 J17=1,PIM%FRIP(PIMA__TAG)%NOBS
                 IF ( .NOT. PIM%FRIP(PIMA__TAG)%USED(J17) ) GOTO 4170
                 IER = -1
                 CALL PIMA_UV_SHIFT ( PIM, VTD, PIMA__TAG, J17, &
     &                              ALPHA_OLD, DELTA_OLD, ALPHA_BEST, DELTA_BEST, &
     &                              FREQ_REF, PIM%FRIP(PIMA__TAG)%TIM_EPC, &
     &                              PIM%FRIP(PIMA__TAG)%OBS(J17)%UVW_SRT, IER )
 4170         CONTINUE
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL PIMA_FRIP_AVR ( PIM, PIMA__TAG, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( PIM%CONF%FRIP_SCA(0,J4), STR )
              CALL ERR_LOG ( 9139, IUER, 'PIMA_FRIP', 'Failure in '// &
     &            'an attempt to write calibrator observations for '// &
     &            'scan '//STR )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL PIMA_FRIP_AMCLO ( PIM, PIMA__TAG, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( PIM%CONF%FRIP_SCA(0,J4), STR )
              CALL ERR_LOG ( 9140, IUER, 'PIMA_FRIP', 'Failure in '// &
     &            'an attempt to solve for phase ambiguities within '// &
     &            'scan '//STR )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL PIMA_FRIP_GRID ( PIM, PIMA__TAG, GRID_ALG, VIS_SCL, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( PIM%CONF%FRIP_SCA(1,J4), STR )
              CALL ERR_LOG ( 9141, IUER, 'PIMA_FRIP', 'Failure in '// &
     &            'an attempt to write calibrator observations for '// &
     &            'scan '//STR )
              RETURN
         END IF
!
! ------ Perform inverse FFT of the gridded visibilities ofthe target
!
         CALL ERR_PASS  ( IUER, IER )
         CALL FFT_2D_C8 ( PIM%CONF%FRIP_RESOLUTION, PIM%CONF%FRIP_RESOLUTION, &
                          -1, PIM%FRIP(PIMA__TAG)%MAP, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 9142, IUER, 'PIMA_FRIP', 'Error in '// &
     &            'an attempt to run 2D FFT transform' )
              RETURN
         END IF
!
         CALL ERR_PASS  ( IUER, IER )
         CALL PIMA_GRID_CORR ( PIM%CONF%FRIP_RESOLUTION, &
     &                          PIM%FRIP(PIMA__TAG)%MAP, GRID_ALG, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 9143, IUER, 'PIMA_FRIP', 'Error in '// &
     &            'an attempt to perform grid correction' )
              RETURN
         END IF
!
         CALL FIND_AMPL_MAX ( PIM%CONF%FRIP_RESOLUTION, &
     &                        PIM%CONF%FRIP_RESOLUTION, &
     &                        PIM%FRIP(PIMA__TAG)%MAP, &
     &                        IND_U_MAX, IND_V_MAX, &
     &                        TAG_AMP_MAX, TAG_AMP_SCN_RAT, TAG_RMS )
!
         IF ( IND_U_MAX .LE. PIM%CONF%FRIP_RESOLUTION/2 ) THEN
               IND_RA_MAX = IND_U_MAX - 1
             ELSE
               IND_RA_MAX = IND_U_MAX - (PIM%CONF%FRIP_RESOLUTION + 1)
         END IF
         IF ( IND_V_MAX .LE. PIM%CONF%FRIP_RESOLUTION/2 ) THEN
              IND_DEC_MAX = IND_V_MAX - 1
            ELSE
              IND_DEC_MAX = IND_V_MAX - (PIM%CONF%FRIP_RESOLUTION + 1)
         END IF
         DEC_NEW = VTD%SOU(IND_SOU_VTD)%DELTA - IND_DEC_MAX*MAP_RES
         RA_NEW  = VTD%SOU(IND_SOU_VTD)%ALPHA - IND_RA_MAX*MAP_RES/DCOS(DEC_NEW)
         CALL RG_TAT ( VTD%SOU(IND_SOU_VTD)%DELTA, 5, DEC_OLD_STR, -2 )
         CALL RH_TAT ( VTD%SOU(IND_SOU_VTD)%ALPHA, 6, RA_OLD_STR,  -2 )
         CALL RG_TAT ( DEC_NEW, 5, DEC_NEW_STR, -2 )
         CALL RH_TAT ( RA_NEW,  6, RA_NEW_STR,  -2 )
         IF ( DEC_OLD_STR(1:1) == ' ' ) DEC_OLD_STR(1:1) = '+'
         CALL CHASHL ( RA_OLD_STR )
         IF ( DEC_NEW_STR(1:1) == ' ' ) DEC_NEW_STR(1:1) = '+'
         CALL CHASHL ( RA_NEW_STR )
!
         WRITE  ( 6, 150 ) PIM%SOU(PIM%SCA(PIM%CONF%FRIP_SCA(0,J4))%SOU_IND)%IVS_NAME, &
     &                     PIM%SOU(PIM%SCA(PIM%CONF%FRIP_SCA(0,J4))%SOU_IND)%J2000_NAME, &
     &                     TAG_AMP_MAX*1.D3, TAG_RMS*1.D3, TAG_AMP_MAX/TAG_RMS, &
     &                     IND_U_MAX, IND_V_MAX, &
     &                     RA_NEW_STR, DEC_NEW_STR, &
     &                     MAP_RES*RAD__TO__MAS
 150           FORMAT ( 'PIMA_FRIP  Target ', A, 2X, A, &
     &                  ' Amp_best: ', F6.2, &
     &                  ' Map_rms: ', F6.2, ' mJy  SNR: ', F6.2, &
     &                  ' UV_max: ', I5, 1X, I5/ &
     &                  'BEST coordinates: ', A, 2X, A, &
     &                  '  Pixel_step: ', F5.3, ' mas'  )
!
         IF ( FRIP_TAG_PLOT .NE. PIMA__PLOT_NO ) THEN
              PLOT_TITLE = 'Dirty map of target '// &
     &                      PIM%SOU(PIM%SCA(PIM%CONF%FRIP_SCA(0,J4))%SOU_IND)%J2000_NAME
              IF ( FRIP_TAG_PLOT == PIMA__PLOT_XW ) THEN
                   IDEV = 1
                ELSE IF ( FRIP_TAG_PLOT == PIMA__PLOT_GIF ) THEN
                   IDEV = 3
                ELSE IF ( FRIP_TAG_PLOT == PIMA__PLOT_PS ) THEN
                   IDEV = 4
              END IF
              MODE = 2
              IPAR = 1
              ISCL = 2
              IPAL = 7
              MAP_SCL = MAP_RES*RAD__TO__MAS
              TAG_PLOT_FINAM = FRIPDIR(1:I_LEN(FRIPDIR))//'/sca_'// &
     &                         STR(1:4)//'_tag_'// &
     &                         PIM%SOU(PIM%SCA(PIM%CONF%FRIP_SCA(0,J4))%SOU_IND)%J2000_NAME
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_2D_MAP_PLOT ( TAG_PLOT_FINAM, PIM, &
     &                           PIM%FRIP(PIMA__TAG)%MAP, PLOT_TITLE, &
     &                           IND_U_MAX, IND_V_MAX, &
     &                           PIM%CONF%FRIP_RESOLUTION, &
     &                           FRIP_TAG_RES, MAP_SCL, &
     &                           MODE, IPAR, ISCL, IPAL, IDEV, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 9144, IUER, 'PIMA_FRIP', 'Error in '// &
     &                 'an attempt to make a plot' )
                  RETURN
            END IF
         END IF
!
! ------ We finished with this source. Time came to deallocate memory
!
         DO 4180 J18=1,2
            DO 4190 J19=1,PIM%FRIP(J18)%NOBS
               DEALLOCATE ( PIM%FRIP(J18)%OBS(J19)%TIM_AP  )
               DEALLOCATE ( PIM%FRIP(J18)%OBS(J19)%WEI  )
               DEALLOCATE ( PIM%FRIP(J18)%OBS(J19)%VIS  )
               DEALLOCATE ( PIM%FRIP(J18)%OBS(J19)%UVW  )
 4190       CONTINUE
            DEALLOCATE ( PIM%FRIP(J18)%OBS  )
            DEALLOCATE ( PIM%FRIP(J18)%FRQ  )
            DEALLOCATE ( PIM%FRIP(J18)%FREQ_AF )
            DEALLOCATE ( PIM%FRIP(J18)%WEI_AF  )
            DEALLOCATE ( PIM%FRIP(J18)%UVW_AF  )
            DEALLOCATE ( PIM%FRIP(J18)%IND_STA )
            DEALLOCATE ( PIM%FRIP(J18)%IAMB_AF )
            DEALLOCATE ( PIM%FRIP(J18)%VIS_AF  )
            DEALLOCATE ( PIM%FRIP(J18)%USED    )
            DEALLOCATE ( PIM%FRIP(J18)%MAP     )
 4180    CONTINUE
 440  CONTINUE
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PIMA_FRIP   !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   GET_MAX_BASELEN ( PIM, L_STA, C_STA )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_MAX_BASELEN
! *                                                                      *
! * ### 28-DEC-2011 GET_MAX_BASELEN v1.0 (c)  L. Petrov  28-DEC-2011 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  L_STA
      CHARACTER  C_STA(L_STA)*(*)
      REAL*8     GET_MAX_BASELEN
      REAL*8     DIST
      INTEGER*4  J1, J2
      REAL*8,    EXTERNAL :: DP_VV_V
!
      GET_MAX_BASELEN = 0.0D0
      IF ( L_STA < 2 ) THEN
           RETURN
      END IF
      DO 410 J1=1,L_STA
         DO 420 J2=J1+1,L_STA
            DIST = DSQRT ( DP_VV_V ( 3, PIM%STA(J1)%COO, PIM%STA(J2)%COO ) )
            GET_MAX_BASELEN = MAX ( GET_MAX_BASELEN, DIST )
 420     CONTINUE
 410  CONTINUE
!
      RETURN
      END  FUNCTION   GET_MAX_BASELEN  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_FRIP_SET_EPOCH ( PIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_FRIP_SET_EPOCH
! *                                                                      *
! *  ### 01-JAN-2012               v1.0 (c)  L. Petrov  01-JAN-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  IUER
      REAL*8     WEI, TIM, TIS, WES
      REAL*8     MOD_SCA_TAG_BEG, MOD_SCA_TAG_END, &
     &           MOD_SCA_CAL_BEG, MOD_SCA_CAL_END, &
     &           MOD_OBS_BEG, MOD_OBS_END, MOD_BEG, MOD_END
      CHARACTER  STR*128
      REAL*8       EPS, TIS_ADJ
      PARAMETER  ( EPS     = 1.D-4 )
      PARAMETER  ( TIS_ADJ = 2.D0  )
      INTEGER*4  J1, J2, J3, J4, IND_BND, IND_OBS
!
      TIS = 0.0D0
      WES = 0.0D0
      IND_BND = 1
      MOD_SCA_TAG_BEG = -1.D9
      MOD_SCA_TAG_END =  1.D9
      DO 410 J1=1,PIM%FRIP(PIMA__TAG)%NOBS
         IND_OBS = PIM%FRIP(PIMA__TAG)%OBS(J1)%IND_OBS
         TIM = PIM%OBS(IND_OBS)%TIM_BEG + PIM%OBS(IND_OBS)%FRT_OFFSET(IND_BND)
         MOD_OBS_BEG = MAX ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%MOD(PIM%OBS(IND_OBS)%MOD_IND_BEG(1))%TIM_BEG, &
     &                       PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%MOD(PIM%OBS(IND_OBS)%MOD_IND_BEG(2))%TIM_BEG  )
         MOD_OBS_END = MIN ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%MOD(PIM%OBS(IND_OBS)%MOD_IND_END(1))%TIM_END, &
     &                       PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%MOD(PIM%OBS(IND_OBS)%MOD_IND_END(2))%TIM_END  )
         DO 420 J2=1,PIM%FRIP(PIMA__TAG)%OBS(J1)%NAP
            WEI = 0.0
            DO 430 J3=1,PIM%FRIP(PIMA__TAG)%NFRQ
               WEI = WEI + PIM%FRIP(PIMA__TAG)%OBS(J1)%WEI(J3,J2)
 430        CONTINUE
            TIS = TIS + TIM*WEI
            WES = WES + WEI
 420     CONTINUE
         MOD_SCA_TAG_BEG = MAX ( MOD_SCA_TAG_BEG, MOD_OBS_BEG )
         MOD_SCA_TAG_END = MIN ( MOD_SCA_TAG_END, MOD_OBS_END )
 410  CONTINUE
!
      IF ( WES > EPS ) THEN
           TIS = TIS/WES
         ELSE
           TIS = PIM%OBS(PIM%FRIP(PIMA__TAG)%OBS(1)%IND_OBS)%TIM_BEG + &
     &           PIM%OBS(PIM%FRIP(PIMA__TAG)%OBS(1)%IND_OBS)%FRT_OFFSET(IND_BND)
      END IF
!
      MOD_SCA_CAL_BEG = -1.D9
      MOD_SCA_CAL_END =  1.D9
      DO 440 J4=1,PIM%FRIP(PIMA__CAL)%NOBS
         IND_OBS = PIM%FRIP(PIMA__CAL)%OBS(J4)%IND_OBS
         TIM = PIM%OBS(IND_OBS)%TIM_BEG + PIM%OBS(IND_OBS)%FRT_OFFSET(IND_BND)
         MOD_OBS_BEG = MAX ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%MOD(PIM%OBS(IND_OBS)%MOD_IND_BEG(1))%TIM_BEG, &
     &                       PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%MOD(PIM%OBS(IND_OBS)%MOD_IND_BEG(2))%TIM_BEG  )
         MOD_OBS_END = MIN ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%MOD(PIM%OBS(IND_OBS)%MOD_IND_END(1))%TIM_END, &
     &                       PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%MOD(PIM%OBS(IND_OBS)%MOD_IND_END(2))%TIM_END  )
         MOD_SCA_CAL_BEG = MAX ( MOD_SCA_CAL_BEG, MOD_OBS_BEG )
         MOD_SCA_CAL_END = MIN ( MOD_SCA_CAL_END, MOD_OBS_END )
 440  CONTINUE 
!
      MOD_BEG = MAX ( MOD_SCA_TAG_BEG, MOD_SCA_CAL_BEG )
      MOD_END = MIN ( MOD_SCA_TAG_END, MOD_SCA_CAL_END )
      IF ( TIS < MOD_BEG + TIS_ADJ ) TIS = MOD_BEG + TIS_ADJ 
      IF ( TIS > MOD_END - TIS_ADJ ) TIS = MOD_END - TIS_ADJ 
      TIS = IDNINT ( TIS + PIM%TAI_0 ) - PIM%TAI_0
      IF ( TIS > MOD_BEG .AND. TIS < MOD_END ) THEN
           PIM%FRIP(PIMA__TAG)%TIM_EPC = TIS
           PIM%FRIP(PIMA__CAL)%TIM_EPC = TIS
         ELSE 
           WRITE ( 6, * ) ' TIS = ', TIS
           WRITE ( 6, * ) ' Scans: (tag/cal): ', PIM%FRIP(PIMA__TAG)%IND_SCA, PIM%FRIP(PIMA__CAL)%IND_SCA
           WRITE ( 6, * ) ' MOD_SCA_CAL: ', MOD_SCA_CAL_BEG, MOD_SCA_CAL_END
           WRITE ( 6, * ) ' MOD_SCA_TAG: ', MOD_SCA_TAG_BEG, MOD_SCA_TAG_END
!
           CALL CLRCH ( STR )
           CALL INCH  ( PIM%FRIP(PIMA__TAG)%IND_SCA, STR )
           CALL ERR_LOG ( 7491, IUER, 'PIMA_FRIP', 'Cannot find the '// &
     &         'reference epoch for target scan '//STR )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_FRIP_SET_EPOCH  !#!
