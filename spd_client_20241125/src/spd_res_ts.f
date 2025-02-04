      PROGRAM    SPD_RES_TS
! ************************************************************************
! *                                                                      *
! *   Program SPD_RES_TS 
! *                                                                      *
! * ### 08-JAN-2024    SPD_RES_TS    v1.1 (c)  L. Petrov 15-FEB-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      TYPE       ( SPD_DEL__TYPE ) :: SPD_DEL
      CHARACTER  SPD_DIR*128, STA_NAM*8, DATE_BEG_STR*21, DATE_END_STR*21, &
     &           DATE_OBS_STR*30, ELEV_STR*12, &
     &           AZIM_STR*12, FREQ_STR*128, TS_STR*128, PAR_STR*12, PROC_STR*12, &
     &           OUT_DIR*128, ERRSTR*128, FILOUT*128, MODE_STR*128, &
     &           STR*128, STR1*128
      INTEGER*4    M_TIM
      PARAMETER  ( M_TIM = 1024*1024 )
      REAL*8,    ALLOCATABLE :: TIM_ARR(:), VAL_ARR(:)
      REAL*8     F_RANGE(2), FREQ, EL
      REAL*8     TAI_BEG, TAI_END, TAI_OBS, AZ, TS
      INTEGER*4  MJD_BEG, MJD_END, MJD_OBS, J1, IDAY, N_TIM, IL, &
     &           LUN, IUER
      REAL*8,    EXTERNAL :: GET_SPD_RES
      LOGICAL*1, EXTERNAL :: IS_DIR_EXIST
      INTEGER*4, EXTERNAL :: GET_UNIT, ILEN, I_LEN
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
!
      F_RANGE(1) = 1.D9
      F_RANGE(2) = 1000.D9
!
      IF ( IARGC() < 10 ) THEN
           WRITE ( 6, * ) 'Usage: spd_res_ts spd_dir sta_nam freq el az date_beg '// &
     &                    'date_end ts par plot|table [output_dir]'
           CALL EXIT ( 0 ) 
        ELSE
           CALL GETARG (  1, SPD_DIR  )
           CALL GETARG (  2, STA_NAM  )
           CALL GETARG (  3, FREQ_STR )
           CALL GETARG (  4, ELEV_STR )
           CALL GETARG (  5, AZIM_STR )
           CALL GETARG (  6, DATE_BEG_STR )
           CALL GETARG (  7, DATE_END_STR )
           CALL GETARG (  8, TS_STR       )
           CALL GETARG (  9, PAR_STR  )
           CALL GETARG ( 10, PROC_STR )
           IF ( IARGC() .GE. 11 ) THEN
                CALL GETARG ( 11, OUT_DIR )
              ELSE
                CALL CLRCH ( OUT_DIR )
           END IF
           CALL TRAN ( 11, STA_NAM, STA_NAM )
      END IF
 !
! --- Check arguments
!
      IF ( .NOT. IS_DIR_EXIST ( SPD_DIR, ERRSTR ) ) THEN
           IUER = -1
           CALL ERR_LOG ( 4701, IUER, 'SPD_RES_TS', 'Error in '// &
     &         'attempt to open diectory with computed slant path delay, '// &
     &         'atmospheric opacity and brightness temperature '//TRIM(SPD_DIR)// &
     &         ' -- '//ERRSTR )
           CALL EXIT ( 1 )
      END IF
      IF ( ILEN(OUT_DIR) .NE. 0 .AND. OUT_DIR .NE. 'XW' ) THEN
           IF ( .NOT. IS_DIR_EXIST ( OUT_DIR, ERRSTR ) ) THEN
                 IUER = -1
                 CALL ERR_LOG ( 4702, IUER, 'SPD_RES_TS', 'Directory '// &
     &                TRIM(OUT_DIR)//' does not exist: '//ERRSTR )
                 CALL EXIT ( 1 )
           END IF
      END IF
!
      READ ( UNIT=FREQ_STR, FMT='(F12.6)', IOSTAT=IUER ) FREQ
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4703, IUER, 'SPD_RES_TS', 'Error in parsing '// &
     &         'frequency '//FREQ_STR )
           CALL EXIT ( 1 )
      END IF
      IF ( FREQ < F_RANGE(1) .OR. FREQ > F_RANGE(2) )  THEN
           IUER = -1
           CALL ERR_LOG ( 4704, IUER, 'SPD_RES_TS', 'Frequency '//TRIM(FREQ_STR)// &
     &         ' is out of range [1.0D9, 1.D12] Hz' )
           CALL EXIT ( 1 )
      END IF
!
      IF ( INDEX ( ELEV_STR, '.' ) < 1 ) ELEV_STR = TRIM(ELEV_STR)//'.0'
      READ ( UNIT=ELEV_STR, FMT='(F12.6)', IOSTAT=IUER ) EL
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4705, IUER, 'SPD_RES_TS', 'Error in parsing '// &
     &         'elevation '//ELEV_STR )
           CALL EXIT ( 1 )
      END IF
      IF ( EL > 90.000001D0 .OR. EL < 2.999999D0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4706, IUER, 'SPD_RES_TS', 'Wrong elevation angle '// &
     &          TRIM(ELEV_STR)//' -- it should be in a range of 3 to 90 deg' )
           CALL EXIT ( 1 )
      END IF
      IF ( EL < 3.0000001D0 ) EL = 3.0000001D0
      IF ( EL > 89.999999D0 ) EL = 89.999999D0 
      EL = EL*DEG__TO__RAD
!
      IF ( INDEX ( AZIM_STR, '.' ) < 1 ) AZIM_STR = TRIM(AZIM_STR)//'.0'
      READ ( UNIT=AZIM_STR, FMT='(F12.6)', IOSTAT=IUER ) AZ
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4707, IUER, 'SPD_RES_TS', 'Error in parsing '// &
     &         'azimuth '//AZIM_STR )
           CALL EXIT ( 1 )
      END IF
      IF ( AZ > 360.000001D0 .OR. AZ < 0.0D0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4708, IUER, 'SPD_RES_TS', 'Wrong azimuth angle '// &
     &          TRIM(AZIM_STR)//' -- it should be in a range of 0 to 360 deg' )
           CALL EXIT ( 1 )
      END IF
      IF ( AZ < 0.0000001D0  ) AZ = 0.0000001D0
      IF ( AZ > 359.999999D0 ) AZ = 359.999999D0 
      AZ = AZ*DEG__TO__RAD
!
      IUER = -1
      IF ( ILEN(DATE_BEG_STR) < 21 ) THEN
           IL = ILEN(DATE_BEG_STR)
           STR = '2000.01.01_00:00:00.0'
           DATE_BEG_STR(IL+1:21) = STR(IL+1:21)
      END IF
      CALL DATE_TO_TIME ( DATE_BEG_STR, MJD_BEG, TAI_BEG, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4709, IUER, 'SPD_RES_TS', 'Error in parsing '// &
     &         'start date '//DATE_BEG_STR )
           CALL EXIT ( 1 )
      END IF
!
      IF ( ILEN(DATE_END_STR) < 21 ) THEN
           IL = ILEN(DATE_END_STR)
           STR = '2000.01.01_00:00:00.0'
           DATE_END_STR(IL+1:21) = STR(IL+1:21)
      END IF
      IUER = -1
      CALL DATE_TO_TIME ( DATE_END_STR, MJD_END, TAI_END, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4710, IUER, 'SPD_RES_TS', 'Error in parsing '// &
     &         'end date '//DATE_END_STR )
           CALL EXIT ( 1 )
      END IF
!
      IF ( INDEX ( TS_STR, '.' ) < 1 ) TS_STR = TRIM(TS_STR)//'.0'
      READ ( UNIT=TS_STR, FMT='(F12.6)', IOSTAT=IUER ) TS
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4711, IUER, 'SPD_RES_TS', 'Error in parsing '// &
     &              'time step argument '//TS_STR )
           CALL EXIT ( 1 )
      END IF
!
      CALL TRAN ( 12, PAR_STR, PAR_STR )
      IF ( PAR_STR == 'opa'  .OR. &
     &     PAR_STR == 'tatm' .OR. &
     &     PAR_STR == 'delt' .OR. &
     &     PAR_STR == 'del'  .OR. &
     &     PAR_STR == 'delw' .OR. &
     &     PAR_STR == 'deld' .OR. &
     &     PAR_STR == 'pres' .OR. &
     &     PAR_STR == 'pwp'  .OR. &
     &     PAR_STR == 'temp'      ) THEN
         ELSE
           IUER = -1
           CALL ERR_LOG ( 4712, IUER, 'SPD_RES_TS', 'Wrong parameter '// &
     &          TRIM(PAR_STR)//'. Only opa, tatm, del, delt, delw, '// &
     &         ' deld, pres, pwp, or temp are supported' )
           CALL EXIT ( 1 )
      END IF
!
      CALL TRAN ( 12, PROC_STR, PROC_STR )
      IF ( PROC_STR(1:4) == 'plot'  .OR. &
     &     PROC_STR(1:5) == 'table'      ) THEN
         ELSE
           IUER = -1
           CALL ERR_LOG ( 4713, IUER, 'SPD_RES_TS', 'Wrong processing '// &
     &         'code '//TRIM(PROC_STR)//'. Only plot and table are supported' )
           CALL EXIT ( 1 )
      END IF
!
      SPD_DEL%STATUS       = SPD__UNDF
      SPD_DEL%MODE_OPA_TAT = SPD__UNDF
!
      IF ( DABS(EL - P2I) < 1.0D-4 ) THEN
           MODE_STR = 'zen_'//PAR_STR
         ELSE 
           MODE_STR = 'azel'
      END IF
!
      IUER = -1
      CALL SPD_RES_INTRP ( SPD_DIR, 1, STA_NAM, MJD_BEG, TAI_BEG, &
     &                     MJD_END, TAI_END, SPD_DEL, MODE_STR, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4715, IUER, 'SPD_RES_TS', 'Error in '// &
     &        'interpolation of spd results' )
           CALL EXIT ( 1 )
      END IF
!
      IF ( TS > 0.0D0 ) THEN
           N_TIM = ((MJD_END - MJD_BEG)*86400.0D0 + (TAI_END - TAI_BEG))/TS + 1
           IF ( N_TIM < 1 ) N_TIM = 1
           ALLOCATE ( TIM_ARR(N_TIM), VAL_ARR(N_TIM), STAT=IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL CLRCH ( STR  )
                CALL INCH8 ( INT8(N_TIM)*INT(16), STR  )
                CALL CLRCH ( STR1  )
                CALL INCH  ( N_TIM, STR )
!
                IUER = -1
                CALL ERR_LOG ( 4714, IUER, 'SPD_RES_TS', 'Failure to allocate '// &
     &               TRIM(STR)//' bytes memorr for processing '//TRIM(STR1)//' epochs. '// &
     &              'Please fix date_beg, date_end and time step arguments' )
                CALL EXIT ( 1 )
           END IF
         ELSE
           N_TIM = 1
           ALLOCATE ( TIM_ARR(N_TIM), VAL_ARR(N_TIM), STAT=IUER )
      END IF
!
      IF ( PROC_STR == 'table' ) THEN
           CALL CLRCH ( STR )
           STR = STA_NAM
           CALL TRAN ( 12, STR, STR )
           CALL CLRCH ( STR1 )
           WRITE ( UNIT=STR1(1:10), FMT='(F7.3,"GHz")' ) FREQ*1.D-9
           CALL CHASHL ( STR1 )
           IF ( ILEN(OUT_DIR) > 0 ) THEN
                FILOUT = TRIM(OUT_DIR)//'/ts_'//TRIM(STR)//'_'//TRIM(PAR_STR)//'_'// &
     &                   DATE_BEG_STR(1:4)//DATE_BEG_STR(6:7)//     &
     &                   DATE_BEG_STR(9:10)//DATE_BEG_STR(12:13)//  &
     &                   DATE_BEG_STR(15:16)//'_'//                 &
     &                   DATE_END_STR(1:4)//DATE_END_STR(6:7)//     &
     &                   DATE_END_STR(9:10)//DATE_END_STR(12:13)//  &
     &                   DATE_END_STR(15:16)
                IF ( INDEX ( STR1, '*' ) < 1 ) THEN
                     FILOUT = TRIM(FILOUT)//'_'//TRIM(STR1)//'.txt'
                   ELSE
                     FILOUT = TRIM(FILOUT)//'.txt'
                END IF
                LUN = GET_UNIT()
                OPEN ( UNIT=LUN, FILE=FILOUT, STATUS='UNKNOWN', IOSTAT=IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 4716, IUER, 'SPD_RES_TS', 'Error in '// &
  &                      'an attempt to open output file '//FILOUT )
                     CALL EXIT ( 1 )
                END IF
              ELSE
                LUN = 6
           END IF
!
           WRITE ( UNIT=LUN, FMT=110 ) SPD_RES_TS__LABEL, &
     &                                 TRIM(PAR_STR), TRIM(STA_NAM), DATE_BEG_STR(1:16), &
     &                                 DATE_END_STR(1:16), TRIM(FREQ_STR), &
     &                                 TRIM(ELEV_STR), TRIM(AZIM_STR), TRIM(SPD_DIR), &
     &                                 GET_CDATE()
 110       FORMAT ( A / &
     &              '# '/ &
     &              '# ', A, ' at ', A, ' for a date range [ ', A, ' , ', A, ' ]'&
     &              ' for frequency ', A, ' Hz, elevation ', A, ' and azimuth ', A, ' deg'/ &
     &              '# Used expansion: ', A/ &
     &              '# Created on ', A/ &
     &              '#' )
      END IF
!
      DO 410 J1=1,N_TIM
         TAI_OBS = TAI_BEG + (J1-1)*TS
         IF ( TAI_OBS .GE. 86400.0D0 ) THEN
              IDAY = INT(TAI_OBS/86400.0D0)
              TAI_OBS = TAI_OBS - IDAY*86400.0D0
              MJD_OBS = MJD_BEG + IDAY
            ELSE
              MJD_OBS = MJD_BEG
         END IF
!
! ------ Get the value of the requested parameter
!
         IUER = -1
         TIM_ARR(J1) = (MJD_OBS - J2000__MJD)*86400.0D0 + TAI_OBS
         VAL_ARR(J1) = GET_SPD_RES ( PAR_STR, SPD_DEL, MJD_OBS, TAI_OBS, &
     &                               FREQ, AZ, EL, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 4717, IUER, 'SPD_RES_TS', 'Error in '// &
     &            'computation of the interpolated parameter computed by spd' )
              CALL EXIT ( 1 )
         END IF
!
         IF ( PROC_STR == 'table' ) THEN
              DATE_OBS_STR = MJDSEC_TO_DATE ( MJD_OBS, TAI_OBS, IUER )
              IF ( PAR_STR == 'opa' ) THEN
                   WRITE ( UNIT=STR, FMT='("opacity: ", F6.4)' ) VAL_ARR(J1) 
                ELSE IF  ( PAR_STR == 'tatm' ) THEN
                   WRITE ( UNIT=STR, FMT='("Tatm: ", F5.1, " K")' ) VAL_ARR(J1) 
                ELSE IF  ( PAR_STR == 'del' .OR. PAR_STR == 'delt' ) THEN
                   WRITE ( UNIT=STR, FMT='("Total delay: ", 1PD12.5, " s")' ) VAL_ARR(J1) 
                ELSE IF  ( PAR_STR == 'deld' ) THEN
                   WRITE ( UNIT=STR, FMT='("Dry delay: ", 1PD12.5, " s")' ) VAL_ARR(J1) 
                ELSE IF  ( PAR_STR == 'delw' ) THEN
                   WRITE ( UNIT=STR, FMT='("Wet delay: ", 1PD12.5, " s")' ) VAL_ARR(J1) 
                ELSE IF  ( PAR_STR == 'pres' ) THEN
                   WRITE ( UNIT=STR, FMT='("Surface atmospheric pressure: ", F8.1, " Pa")' ) VAL_ARR(J1)  
                ELSE IF  ( PAR_STR == 'pwp' ) THEN
                   WRITE ( UNIT=STR, FMT='("Surface partial pressure of water vapour: ", F8.1, " Pa")' ) VAL_ARR(J1)  
                ELSE IF  ( PAR_STR == 'temp' ) THEN
                   WRITE ( UNIT=STR, FMT='("Surface air temperature: ", F5.1, " K")' ) VAL_ARR(J1)  
             END IF
             WRITE  ( UNIT=LUN, FMT=120 ) J1, MJD_OBS, TAI_OBS, DATE_OBS_STR(1:21), &
     &                                    EL/DEG__TO__RAD, AZ/DEG__TO__RAD, TRIM(STR)
 120         FORMAT ( 'I= ', I6,' MJD: ', I5, ' Tai: ', F7.1, ' Date: ', A, &
     &                ' Elev: ', F4.1, ' Az: ', F6.1, ' deg  ', A )
         END IF
 410  CONTINUE 
      IF ( PROC_STR == 'table' ) THEN
           IF ( ILEN(OUT_DIR) .NE. 0 ) THEN
                CLOSE ( UNIT=LUN )
                WRITE ( 6, '(A)' ) 'Wrote output file '//TRIM(FILOUT)
           END IF
        ELSE IF ( PROC_STR(1:4) == 'plot' ) THEN
           IUER = -1
           IF ( ILEN(OUT_DIR) == 0 ) THEN
                CALL CLRCH ( FILOUT )
              ELSE
                CALL CLRCH ( STR )
                STR = STA_NAM
                CALL TRAN ( 12, STR, STR )
                FILOUT = TRIM(OUT_DIR)//'/ts_'//TRIM(STR)//'_'//TRIM(PAR_STR)//'_'// &
     &                   DATE_BEG_STR(1:4)//DATE_BEG_STR(6:7)//     &
     &                   DATE_BEG_STR(9:10)//DATE_BEG_STR(12:13)//  &
     &                   DATE_BEG_STR(15:16)//'_'//                 &
     &                   DATE_END_STR(1:4)//DATE_END_STR(6:7)//     &
     &                   DATE_END_STR(9:10)//DATE_END_STR(12:13)//  &
     &                   DATE_END_STR(15:16)//'.ps'
           END IF
!
           IUER = -1
           CALL SPD_RES_PLOT ( PAR_STR, PROC_STR, STA_NAM, &
     &                         FREQ, AZ, EL, N_TIM, TIM_ARR, VAL_ARR, &
     &                         FILOUT, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 4718, IUER, 'SPD_RES_TS', 'Failure in an '// &
     &              'attempt to make a plot of '//TRIM(PAR_STR)//' time series' )
                CALL EXIT ( 1 )
           END IF
           IF ( ILEN(OUT_DIR) .NE. 0 ) THEN 
                WRITE ( 6, '(A)' ) 'Wrote output file '//TRIM(FILOUT)
           END IF
      END IF
!
! --- Release memory
!
      CALL SPD_DEL_QUIT ( SPD_DEL, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4719, IUER, 'SPD_RES_TS', 'Failure in an '// &
     &         'attempt to release memory used by SPD_DEL' )
           CALL EXIT ( 1 )
      END IF
      DEALLOCATE ( TIM_ARR, VAL_ARR )
!
      END PROGRAM  SPD_RES_TS  !#!#
!
! ------------------------------------------------------------------------
!
#ifdef USE_PETOOLS
      SUBROUTINE SPD_RES_PLOT ( PAR_STR, PLOT_STR, STA_NAM, &
     &                          FREQ, AZ, EL, N_TIM, TIM_ARR, VAL_ARR, &
     &                          FILOUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SPD_RES_PLOT 
! *                                                                      *
! * ## 09-JAN-2024     SPD_RES_PLOT   v1.0 (c) L. Petrov 08-JAN-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'diagi.i'
      INCLUDE   'astro_constants.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      CHARACTER  PAR_STR*(*), PLOT_STR*(*), STA_NAM*(*), FILOUT*(*)
      INTEGER*4  N_TIM, IUER
      REAL*8     FREQ, AZ, EL, TIM_ARR(N_TIM), VAL_ARR(N_TIM)
      REAL*8,    ALLOCATABLE :: T8(:)
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, ICL1, ICL2, ICL3, J1, IER
      REAL*8     T8_MIN, T8_MAX, T8_RN, VAL_MIN, VAL_MAX, VAL_RN
      INTEGER*4  DIAGI_LEN
      CHARACTER  ZAG*128, UNIT*128, DATE_STR*32, UNIT_TYPE*5, STR*128, &
     &           STR1*128, STR2*128
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, TIM_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      ALLOCATE ( T8(N_TIM) ) 
      IF ( TIM_ARR(N_TIM) - TIM_ARR(1) < 31.D0*86400.0D0 ) THEN
           UNIT_TYPE = 'days'
         ELSE
           UNIT_TYPE = 'years'
      END IF
      DO 410 J1=1,N_TIM
         IF ( UNIT_TYPE == 'days' ) THEN
              T8(J1) = (TIM_ARR(J1) - TIM_ARR(1))/86400.0D0
            ELSE
              T8(J1) = TIM_ARR(J1)/86400.0D0/365.25D0 + 2000.0D0
         END IF
         IF ( J1 == 1 ) THEN
              T8_MIN  = T8(1)
              T8_MAX  = T8(1)
              VAL_MIN = VAL_ARR(1)
              VAL_MAX = VAL_ARR(1)
            ELSE
              T8_MIN  = MIN ( T8_MIN,  T8(J1) )
              T8_MAX  = MAX ( T8_MAX,  T8(J1) )
              VAL_MIN = MIN ( VAL_MIN, VAL_ARR(J1) )
              VAL_MAX = MAX ( VAL_MAX, VAL_ARR(J1) )
         END IF
 410  CONTINUE 
      T8_RN = (T8_MAX - T8_MIN)
      T8_MIN = T8_MIN - 0.01*T8_RN
      T8_MAX = T8_MAX + 0.01*T8_RN
!
      IF ( PLOT_STR == 'plot' ) THEN
           VAL_RN = (VAL_MAX - VAL_MIN)
           VAL_MIN = VAL_MIN - 0.02*VAL_RN
           VAL_MAX = VAL_MAX + 0.02*VAL_RN
        ELSE IF ( PLOT_STR == 'plot1' ) THEN
           IF ( PAR_STR == 'opa' ) THEN
                VAL_MIN = 0.0D0
                VAL_MAX = 0.5D0
              ELSE IF ( PAR_STR == 'tatm' ) THEN
                VAL_MIN = 0.0D0
                VAL_MAX = 100.0D0
           END IF
         ELSE IF ( PLOT_STR == 'plot2' ) THEN
           IF ( PAR_STR == 'opa' ) THEN
                VAL_MIN = 0.0D0
                VAL_MAX = 1.0D0
              ELSE IF ( PAR_STR == 'tatm' ) THEN
                VAL_MIN = 0.0D0
                VAL_MAX = 150.0D0
           END IF
         ELSE IF ( PLOT_STR == 'plot3' ) THEN
           IF ( PAR_STR == 'opa' ) THEN
                VAL_MIN = 0.0D0
                VAL_MAX = 2.0D0
              ELSE IF ( PAR_STR == 'tatm' ) THEN
                VAL_MIN = 0.0D0
                VAL_MAX = 250.0D0
           END IF
         ELSE IF ( PLOT_STR == 'plot4' ) THEN
           IF ( PAR_STR == 'opa' ) THEN
                VAL_MIN = 0.0D0
                VAL_MAX = 5.0D0
              ELSE IF ( PAR_STR == 'tatm' ) THEN
                VAL_MIN = 0.0D0
                VAL_MAX = 300.0D0
           END IF
      END IF
!
! --- Clear DIAGI_S object
!
      DIAGI_LEN = LOC(DIAGI_S%STATUS) - LOC(DIAGI_S%IFIRST_FIELD) + 4
      CALL NOUT ( DIAGI_LEN, DIAGI_S )
!
! --- Setting defaults values of the plotting parameters
!
      CALL ERR_PASS   ( IUER, IER )
      CALL DIAGI_DEF  ( IBST, ILST, IOST, IPST, IWST, IDEV, ZAG, UNIT, &
     &                  ICL1, ICL2, ICL3, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4779, IUER, 'SPD_RES_PLOT', 'Error in setting '// &
     &         'default values for the plot' )
           RETURN
      END IF
      IF ( PAR_STR == 'opa' ) THEN
           WRITE ( UNIT=ZAG, FMT=110 ) STA_NAM, FREQ/1.D9, &
     &                                 AZ/DEG__TO__RAD, EL/DEG__TO__RAD
 110       FORMAT ( 'Atmospheric opacity at ', A, ' at ', F5.1, &
     &              ' GHz az ', F5.1, ' el ', F5.1, ' deg' )
        ELSE IF ( PAR_STR == 'tatm' ) THEN
           WRITE ( UNIT=ZAG, FMT=120 ) STA_NAM, FREQ/1.D9, &
     &                                 AZ/DEG__TO__RAD, EL/DEG__TO__RAD
 120       FORMAT ( 'Atmosphere brightness temp. in K at ', A, &
     &              ' at ', F5.1, ' GHz az ', F5.1, ' el ', F4.1, ' deg' )
        ELSE IF ( PAR_STR == 'del' .OR. PAR_STR == 'delt' ) THEN
           WRITE ( UNIT=ZAG, FMT=130 ) STA_NAM, AZ/DEG__TO__RAD, EL/DEG__TO__RAD
 130       FORMAT ( 'Total atmosphere path delay in sec at ', A, &
     &              ' az ', F5.1, ' el ', F4.1, ' deg' )
        ELSE IF ( PAR_STR == 'deld' ) THEN
           WRITE ( UNIT=ZAG, FMT=140 ) STA_NAM, AZ/DEG__TO__RAD, EL/DEG__TO__RAD
 140       FORMAT ( 'Dry atmosphere path delay in sec at ', A, &
     &              ' az ', F5.1, ' el ', F4.1, ' deg' )
        ELSE IF ( PAR_STR == 'delw' ) THEN
           WRITE ( UNIT=ZAG, FMT=150 ) STA_NAM, AZ/DEG__TO__RAD, EL/DEG__TO__RAD
 150       FORMAT ( 'Wet atmosphere path delay in sec at ', A, &
     &              ' az ', F5.1, ' el ', F4.1, ' deg' )
      END IF
!
! --- Setting up the values of the DIAGI internal data structure for the further
! --- plotting
!
      DIAGI_S%NCLR      = 1
      DIAGI_S%NPOI(1)   = N_TIM
      DIAGI_S%ADR_X8(1) = LOC(T8)
      DIAGI_S%ADR_Y8(1) = LOC(VAL_ARR)
      DIAGI_S%LER(1)    = .FALSE.
      DIAGI_S%ICOL(1)   = ICL1
      DIAGI_S%IBST(1)   = 0
      DIAGI_S%ILST(1)   = 3
      DIAGI_S%IOST(1)   = 1
      DIAGI_S%IPST(1)   = 1
      DIAGI_S%IWST(1)   = 2
      DIAGI_S%ICLR      = 1
      DIAGI_S%XMIN      = SNGL(T8_MIN)
      DIAGI_S%XMAX      = SNGL(T8_MAX)
      DIAGI_S%YMIN      = SNGL(VAL_MIN)
      DIAGI_S%YMAX      = SNGL(VAL_MAX)
      DIAGI_S%ZAG       = ZAG
      IF ( UNIT_TYPE == 'days' ) THEN
           CALL CLRCH ( STR )
           STR = TIM_TO_DATE ( TIM_ARR(1), IER )
           DIAGI_S%ARG_UNITS = 'Days since '//STR(1:10)
        ELSE
           DIAGI_S%ARG_UNITS = 'Years'
      END IF
      DIAGI_S%ITRM      = 0
      DIAGI_S%STATUS    = DIA__DEF
      IF ( ILEN(FILOUT) == 0 ) THEN
           DIAGI_S%IDEV      = IDEV
           DIAGI_S%NAME      = NAME__DEF
           DIAGI_S%IBATCH    = 0
         ELSE
           CALL CLRCH ( STR )
           STR = STA_NAM
           CALL TRAN ( 12, STR, STR )
           STR1 = TIM_TO_DATE ( TIM_ARR(1),     IER )
           STR2 = TIM_TO_DATE ( TIM_ARR(N_TIM), IER )
!
           DIAGI_S%IDEV   = 8
           DIAGI_S%NAME   = FILOUT
           DIAGI_S%IBATCH = 1
      END IF
!
! --- Calling the main routine of DiaGI
!
      CALL ERR_PASS ( IUER, IER )
      CALL DIAGI    ( DIAGI_S, IER )
      DEALLOCATE ( T8 )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4792, IUER, 'GEN_TIPPING_PLOT', 'Failure to '// &
     &         'make a plot' )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_RES_PLOT  !#!#
!
! ------------------------------------------------------------------------
!
#else
      SUBROUTINE SPD_RES_PLOT ( PAR_STR, PLOT_STR, STA_NAM, &
     &                          FREQ, AZ, EL, N_TIM, TIM_ARR, VAL_ARR, &
     &                          FILOUT, IUER )
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      CHARACTER  PAR_STR*(*), PLOT_STR*(*), STA_NAM*(*), FILOUT*(*)
      INTEGER*4  N_TIM, IUER
      REAL*8     FREQ, AZ, EL, TIM_ARR(N_TIM), VAL_ARR(N_TIM)
      REAL*8,    ALLOCATABLE :: T8(:)
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, ICL1, ICL2, ICL3, J1, IER
      REAL*8     T8_MIN, T8_MAX, T8_RN, VAL_MIN, VAL_MAX, VAL_RN
      INTEGER*4  DIAGI_LEN
      CHARACTER  ZAG*128, UNIT*128, DATE_STR*32, UNIT_TYPE*5, STR*128, &
     &           STR1*128, STR2*128
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, TIM_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      WRITE ( 6, '(A)' ) 'spd_client was build without pgpplot. Quitting' 
      CALL EXIT ( 0 )
      RETURN
      END  SUBROUTINE  SPD_RES_PLOT !#!#
#endif
