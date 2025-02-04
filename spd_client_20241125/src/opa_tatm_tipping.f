      PROGRAM    OPA_TATM_TIPPING
! ************************************************************************
! *                                                                      *
! *   Program OPA_TATM_TIPPING 
! *                                                                      *
! * ### 08-JAN-2024  OPA_TATM_TIPPING v1.0 (c) L. Petrov 08-JAN-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      TYPE       ( SPD_DEL__TYPE ) :: SPD_DEL
      CHARACTER  SPD_DIR*128, STA_NAM*8, DATE_BEG_STR*21, DATE_END_STR*21, &
     &           DATE_OBS_STR*30, ELMIN_STR*12, ELMAX_STR*12, ELSTP_STR*12, &
     &           AZIM_STR*12, FREQ_STR*128, TS_STR*128, PAR_STR*12, PROC_STR*12, &
     &           OUT_DIR*128, ERRSTR*128, FILOUT*128, MODE_STR*128, &
     &           STR*128, STR1*128
      INTEGER*4    M_ELV, MS
      PARAMETER  ( M_ELV = 1024 )
      PARAMETER  ( MS    = 32*1024 )
      REAL*8     F_RANGE(2), FREQ, EL_MIN, EL_MAX, EL_STP, EL_ARR(M_ELV), VAL_ARR(M_ELV)
      REAL*8     TAI_BEG, TAI_END, TAI_OBS, AZ, TS
      INTEGER*4  MJD_BEG, MJD_END, MJD_OBS, J1, J2, J3, IDAY, NS, &
     &           N_EL, LUN, IUER
      LOGICAL*1, EXTERNAL :: IS_DIR_EXIST
      INTEGER*4, EXTERNAL :: GET_UNIT, ILEN, I_LEN
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
!
      F_RANGE(1) = 1.D9
      F_RANGE(2) = 1000.D9
!
      IF ( IARGC() < 10 ) THEN
           WRITE ( 6, * ) 'Usage: opa_tatm_tipping spd_dir sta_nam date '// &
     &                    'freq el_min el_max el_step az opa|tatm plot|table '// &
     &                    '[output_dir] [mjd_end ts]]'
           CALL EXIT ( 0 ) 
        ELSE
           CALL GETARG (  1, SPD_DIR  )
           CALL GETARG (  2, STA_NAM  )
           CALL GETARG (  3, DATE_BEG_STR )
           CALL GETARG (  4, FREQ_STR )
           CALL GETARG (  5, ELMIN_STR )
           CALL GETARG (  6, ELMAX_STR )
           CALL GETARG (  7, ELSTP_STR )
           CALL GETARG (  8, AZIM_STR )
           CALL GETARG (  9, PAR_STR  )
           CALL GETARG ( 10, PROC_STR )
           IF ( IARGC() .GE. 11 ) THEN
                CALL GETARG ( 11, OUT_DIR )
              ELSE
                CALL CLRCH ( OUT_DIR )
           END IF
           IF ( IARGC() .GE. 13 ) THEN
                CALL GETARG ( 12, DATE_END_STR )
                CALL GETARG ( 13, TS_STR       )
              ELSE 
                DATE_END_STR = DATE_BEG_STR 
                TS_STR = '0'
           ENDIF
      END IF
!
! --- Check arguments
!
      IF ( .NOT. IS_DIR_EXIST ( SPD_DIR, ERRSTR ) ) THEN
           IUER = -1
           CALL ERR_LOG ( 4601, IUER, 'OPA_TATM_TIPPING', 'Error in '// &
     &         'attempt to open diectory with computed slant path delay, '// &
     &         'atmospheric opacity and brightness temperature '//TRIM(SPD_DIR)// &
     &         ' -- '//ERRSTR )
           CALL EXIT ( 1 )
      END IF
      IF ( ILEN(OUT_DIR) .NE. 0 .AND. OUT_DIR .NE. 'XW' ) THEN
           IF ( .NOT. IS_DIR_EXIST ( OUT_DIR, ERRSTR ) ) THEN
                 IUER = -1
                 CALL ERR_LOG ( 4602, IUER, 'OPA_TATM_TIPPING', 'Directory '// &
     &                TRIM(OUT_DIR)//' does not exist: '//ERRSTR )
                 CALL EXIT ( 1 )
           END IF
      END IF
!
      IUER = -1
      CALL DATE_TO_TIME ( DATE_BEG_STR, MJD_BEG, TAI_BEG, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4603, IUER, 'OPA_TATM_TIPPING', 'Error in parsing '// &
     &         'date' )
           CALL EXIT ( 1 )
      END IF
!
      READ ( UNIT=FREQ_STR, FMT='(F12.6)', IOSTAT=IUER ) FREQ
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4604, IUER, 'OPA_TATM_TIPPING', 'Error in parsing '// &
     &         'frequency '//FREQ_STR )
           CALL EXIT ( 1 )
      END IF
      IF ( FREQ < F_RANGE(1) .OR. FREQ > F_RANGE(2) )  THEN
           IUER = -1
           CALL ERR_LOG ( 4605, IUER, 'OPA_TATM_TIPPING', 'Frequency '//TRIM(FREQ_STR)// &
     &         ' is out of range [1.0D9, 3.6D11] Hz' )
           CALL EXIT ( 1 )
      END IF
!
      IF ( INDEX ( ELMIN_STR, '.' ) < 1 ) ELMIN_STR = TRIM(ELMIN_STR)//'.0'
      READ ( UNIT=ELMIN_STR, FMT='(F12.6)', IOSTAT=IUER ) EL_MIN
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4606, IUER, 'OPA_TATM_TIPPING', 'Error in parsing '// &
     &         'minimum elevation '//ELMIN_STR )
           CALL EXIT ( 1 )
      END IF
      IF ( EL_MIN > 90.000001D0 .OR. EL_MIN < 2.999999D0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4607, IUER, 'OPA_TATM_SPECTRUM', 'Wrong minimum '// &
     &         'elevation angle '//TRIM(ELMIN_STR)//' -- it should be in '// &
     &         'a range of 3 to 90 deg' )
           CALL EXIT ( 1 )
      END IF
      IF ( EL_MIN < 3.0000001D0 ) EL_MIN = 3.0000001D0
      IF ( EL_MIN > 89.999999D0 ) EL_MIN = 89.999999D0 
      EL_MIN = EL_MIN*DEG__TO__RAD
!
      IF ( INDEX ( ELMAX_STR, '.' ) < 1 ) ELMAX_STR = TRIM(ELMAX_STR)//'.0'
      READ ( UNIT=ELMAX_STR, FMT='(F12.6)', IOSTAT=IUER ) EL_MAX
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4608, IUER, 'OPA_TATM_TIPPING', 'Error in parsing '// &
     &         'maximum elevation '//ELMAX_STR )
           CALL EXIT ( 1 )
      END IF
      IF ( EL_MAX > 90.000001D0 .OR. EL_MAX < 2.999999D0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4609, IUER, 'OPA_TATM_SPECTRUM', 'Wrong maximum '// &
     &         'elevation angle '//TRIM(ELMAX_STR)//' -- it should be in '// &
     &         'a range of 3 to 90 deg' )
           CALL EXIT ( 1 )
      END IF
      EL_MAX = EL_MAX*DEG__TO__RAD
!
      IF ( INDEX ( ELSTP_STR, '.' ) < 1 ) ELSTP_STR = TRIM(ELSTP_STR)//'.0'
      READ ( UNIT=ELSTP_STR, FMT='(F12.6)', IOSTAT=IUER ) EL_STP
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4610, IUER, 'OPA_TATM_TIPPING', 'Error in parsing '// &
     &         'elevation step '//ELSTP_STR )
           CALL EXIT ( 1 )
      END IF
      EL_STP = EL_STP*DEG__TO__RAD
!
      IF ( (EL_MAX - EL_MIN)/EL_STP < 2 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4611, IUER, 'OPA_TATM_TIPPING', 'Bad combination of '// &
     &         'the mimimum elevation, maximum elevation, and elevation step: '// &
     &         'too few points for the tipping curve' )
           CALL EXIT ( 1 )
      END IF
      N_EL = IDNINT ( (EL_MAX - EL_MIN)/EL_STP ) + 1
      IF ( N_EL > M_ELV ) THEN
           IUER = -1
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( N_EL, STR  )
           CALL INCH  ( M_ELV, STR1 )
           CALL ERR_LOG ( 4612, IUER, 'OPA_TATM_TIPPING', 'Too many elevation steps: '// &
     &          TRIM(STR)//', which exceeds the limit '//TRIM(STR1)//' -- please '// &
     &          'reduce elevation resolution' )
           CALL EXIT ( 1 )
      END IF
      IF ( N_EL < 1 ) N_EL = 1
!
      IF ( INDEX ( AZIM_STR, '.' ) < 1 ) AZIM_STR = TRIM(AZIM_STR)//'.0'
      READ ( UNIT=AZIM_STR, FMT='(F12.6)', IOSTAT=IUER ) AZ
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4613, IUER, 'OPA_TATM_TIPPING', 'Error in parsing '// &
     &         'azimuth' )
           CALL EXIT ( 1 )
      END IF
      IF ( AZ > 360.000001D0 .OR. AZ < 0.0D0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4614, IUER, 'OPA_TATM_TIPPING', 'Wrong azimuth angle '// &
     &          TRIM(AZIM_STR)//' -- it should be in a range of 0 to 360 deg' )
           CALL EXIT ( 1 )
      END IF
      IF ( AZ < 0.0000001D0  ) AZ = 0.0000001D0
      IF ( AZ > 359.999999D0 ) AZ = 359.999999D0 
      AZ = AZ*DEG__TO__RAD
!
      CALL TRAN ( 12, PAR_STR, PAR_STR )
      IF ( PAR_STR == 'opa'  .OR. &
     &     PAR_STR == 'tatm'      ) THEN
         ELSE
           IUER = -1
           CALL ERR_LOG ( 4615, IUER, 'OPA_TATM_TIPPING', 'Wrong parameter '//TRIM(PAR_STR)// &
     &         '. Only opa and tatm are supported' )
           CALL EXIT ( 1 )
      END IF
!
      CALL TRAN ( 12, PROC_STR, PROC_STR )
      IF ( PROC_STR(1:4) == 'plot'  .OR. &
     &     PROC_STR(1:5) == 'table'      ) THEN
         ELSE
           IUER = -1
           CALL ERR_LOG ( 4616, IUER, 'OPA_TATM_TIPPING', 'Wrong processing '// &
     &         'code '//TRIM(PROC_STR)//'. Only plot and table are supported' )
           CALL EXIT ( 1 )
      END IF
!
      DO 410 J1=1,N_EL
         EL_ARR(J1) = EL_MIN + (J1-1)*EL_STP
 410  CONTINUE 
!
      IUER = -1
      CALL DATE_TO_TIME ( DATE_END_STR, MJD_END, TAI_END, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4617, IUER, 'OPA_TATM_TIPPING', 'Error in parsing '// &
     &         'end date '//DATE_END_STR )
           CALL EXIT ( 1 )
      END IF
!
      IF ( TS_STR .EQ. '0' ) THEN
           TS = 0.0D0
         ELSE
           IF ( INDEX ( TS_STR, '.' ) < 1 ) TS_STR = TRIM(TS_STR)//'.0'
           READ ( UNIT=TS_STR, FMT='(F12.6)', IOSTAT=IUER ) TS
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 4618, IUER, 'OPA_TATM_TIPPING', 'Error in parsing '// &
     &              'time step argument '//TS_STR )
                CALL EXIT ( 1 )
           END IF
      END IF
      IF ( TS > 0.0D0 ) THEN
           NS = ((MJD_END - MJD_BEG)*86400.0D0 + (TAI_END - TAI_BEG))/TS + 1
           IF ( NS < 1 ) NS = 1
           IF ( NS > MS ) THEN
                CALL CLRCH ( STR  )
                CALL CLRCH ( STR1 )
                CALL INCH8  ( INT8(NS), STR  )
                CALL INCH   ( MS,       STR1 )
!
                IUER = -1
                CALL ERR_LOG ( 4619, IUER, 'OPA_TATM_TIPPING', 'To many '// &
     &              'time epochs: '//TRIM(STR)//' -- more than the maximum '// &
     &               TRIM(STR1)//'. Please fix date_beg and date_end '// &
     &              'arguments' )
                CALL EXIT ( 1 )
           END IF
         ELSE
           NS = 1
      END IF
!
      SPD_DEL%STATUS       = SPD__UNDF
      SPD_DEL%MODE_OPA_TAT = SPD__UNDF
!
      MODE_STR = 'azel'
      IUER = -1
      CALL SPD_RES_INTRP ( SPD_DIR, 1, STA_NAM, MJD_BEG, TAI_BEG, &
     &                     MJD_END, TAI_END, SPD_DEL, MODE_STR, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4620, IUER, 'OPA_TATM_TIPPING', 'Error in '// &
     &        'interpolation of atmospheric opacity and brightness '// &
     &        'temperature' )
           CALL EXIT ( 1 )
      END IF
!
      DO 420 J2=1,NS
         TAI_OBS = TAI_BEG + (J2-1)*TS
         IF ( TAI_OBS .GE. 86400.0D0 ) THEN
              IDAY = INT(TAI_OBS/86400.0D0)
              TAI_OBS = TAI_OBS - IDAY*86400.0D0
              MJD_OBS = MJD_BEG + IDAY
            ELSE
              MJD_OBS = MJD_BEG
         END IF
!
         IUER = -1
         CALL GET_OPA_TATM_TIPPING ( PAR_STR, SPD_DEL, MJD_OBS, TAI_OBS, &
     &                               FREQ, AZ, N_EL, EL_ARR, VAL_ARR, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 4621, IUER, 'OPA_TATM_TIPPING', 'Error in '// &
     &            'computation of the interpolated spectrum' )
              CALL EXIT ( 1 )
         END IF
!
         DATE_OBS_STR = MJDSEC_TO_DATE ( MJD_OBS, TAI_OBS, IUER )
         IF ( PROC_STR(1:4) == 'plot' ) THEN
              IUER = -1
              CALL GEN_TIPPING_PLOT ( PAR_STR, PROC_STR, MJD_OBS, TAI_OBS, &
     &                                STA_NAM, FREQ, AZ, N_EL, EL_ARR, VAL_ARR, &
     &                                OUT_DIR, IUER )
            ELSE IF ( PROC_STR == 'table' ) THEN
              IF ( ILEN(OUT_DIR) == 0 ) THEN
                   LUN = 6
                 ELSE
                   CALL CLRCH ( STR )
                   STR = STA_NAM
                   CALL TRAN ( 12, STR, STR )
                   FILOUT = TRIM(OUT_DIR)//'/'//TRIM(STR)//'_'//TRIM(PAR_STR)//'_'// &
     &                      DATE_OBS_STR(1:4)//DATE_OBS_STR(6:7)// &
     &                      DATE_OBS_STR(9:13)//DATE_OBS_STR(15:16)// &
     &                      '.txt'
                   LUN = GET_UNIT()
                   OPEN ( UNIT=LUN, FILE=FILOUT, STATUS='UNKNOWN', IOSTAT=IUER )
                   IF ( IUER .NE. 0 ) THEN
                        IUER = -1
                        CALL ERR_LOG ( 4622, IUER, 'OPA_TATM_TIPPING', 'Error in '// &
     &                      'an attempt to open output file '//FILOUT )
                        CALL EXIT ( 1 )
                   END IF
              END IF
              WRITE ( UNIT=LUN, FMT=110 ) SPD_OPA_TATM_TIPPING__LABEL, &
     &                                    TRIM(PAR_STR), TRIM(STA_NAM), DATE_OBS_STR(1:16), &
     &                                    TRIM(FREQ_STR), TRIM(AZIM_STR), TRIM(SPD_DIR), &
     &                                    GET_CDATE()
 110          FORMAT ( A / &
     &                 '# ', A, ' at ', A, ' on ', A, &
     &                 ' for frequency ', A, ' Hz and azimuth ', A, ' deg'/ &
     &                 '# Used expansion: ', A/ &
     &                 '# Created on ', A/ &
     &                 '#' )
              DO 430 J3=1,N_EL
                 IF ( PAR_STR == 'opa' ) THEN
                      WRITE  ( UNIT=LUN, FMT=120 ) J3, EL_ARR(J3)/DEG__TO__RAD, VAL_ARR(J3)
 120                  FORMAT ( 'I= ', I5,' Elev: ', F4.1, ' deg  opacity: ', F8.4 )
                    ELSE IF  ( PAR_STR == 'tatm' ) THEN
                      WRITE  ( UNIT=LUN, FMT=130 ) J3, EL_ARR(J3)/DEG__TO__RAD, VAL_ARR(J3)
 130                  FORMAT ( 'I= ', I5,' Elev: ', F4.1, ' deg  Tatm: ', F5.1 )
                 END IF
 430          CONTINUE 
              IF ( ILEN(OUT_DIR) .NE. 0 ) THEN
                   CLOSE ( UNIT=LUN )
              END IF
         END IF
 420  CONTINUE 
!
      CALL SPD_DEL_QUIT ( SPD_DEL, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4623, IUER, 'OPA_TATM_TIPPING', 'Failure in an '// &
     &         'attempt to release memory used by SPD_DEL' )
           RETURN 
      END IF
!
      END  PROGRAM  OPA_TATM_TIPPING  !#!#
!
! ------------------------------------------------------------------------
!
#ifdef USE_PETOOLS
      SUBROUTINE GEN_TIPPING_PLOT ( PAR_STR, PLOT_STR, MJD, TAI, STA_NAM, &
     &                              FREQ, AZ, N_EL, EL_ARR, VAL_ARR, &
     &                              OUT_DIR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GEN_TIPPING_PLOT
! *                                                                      *
! * ## 08-JAN-2024  GEN_TIPPING_PLOT  v1.0 (c) L. Petrov 08-JAN-2024 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'diagi.i'
      INCLUDE   'astro_constants.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      CHARACTER  PAR_STR*(*), PLOT_STR*(*), STA_NAM*(*), OUT_DIR*(*)
      INTEGER*4  N_EL, MJD, IUER
      REAL*8     TAI, FREQ, AZ, EL_ARR(N_EL), VAL_ARR(N_EL)
      REAL*8,    ALLOCATABLE :: EL_R8(:)
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, ICL1, ICL2, ICL3, J1, IER
      REAL*8     EL_MIN, EL_MAX, EL_RN, VAL_MIN, VAL_MAX, VAL_RN
      INTEGER*4  DIAGI_LEN
      CHARACTER  ZAG*128, UNIT*128, DATE_STR*32, STR*128
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      ALLOCATE ( EL_R8(N_EL) )
      DO 410 J1=1,N_EL
         EL_R8(J1) = EL_ARR(J1)/DEG__TO__RAD
         IF ( J1 == 1 ) THEN
              EL_MIN = EL_R8(1)
              EL_MAX = EL_R8(1)
              VAL_MIN = VAL_ARR(1)
              VAL_MAX = VAL_ARR(1)
            ELSE
              EL_MIN = MIN ( EL_MIN, EL_R8(J1) )
              EL_MAX = MAX ( EL_MAX, EL_R8(J1) )
              VAL_MIN = MIN ( VAL_MIN, VAL_ARR(J1) )
              VAL_MAX = MAX ( VAL_MAX, VAL_ARR(J1) )
         END IF
 410  CONTINUE 
!
      IF ( EL_MIN < 3.0D0  ) EL_MIN = 3.0D0
      IF ( EL_MAX > 90.0D0 ) EL_MAX = 90.0D0
      EL_RN = (EL_MAX - EL_MIN)
      EL_MIN = EL_MIN - 0.01*EL_RN
      EL_MAX = EL_MAX + 0.01*EL_RN
      IF ( PLOT_STR == 'plot' ) THEN
           VAL_RN = (VAL_MAX - VAL_MIN)
           VAL_MIN = VAL_MIN - 0.02*VAL_RN
           VAL_MAX = VAL_MAX + 0.02*VAL_RN
        ELSE IF ( PLOT_STR == 'plot0' ) THEN
           IF ( PAR_STR == 'opa' ) THEN
                VAL_MIN = 0.0D0
                VAL_MAX = 0.2D0
              ELSE IF ( PAR_STR == 'tatm' ) THEN
                VAL_MIN = 0.0D0
                VAL_MAX = 50.0D0
           END IF
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
           CALL ERR_LOG ( 4151, IUER, 'DIAGI_1', 'Error in setting '// &
     &         'default values for the plot' )
           RETURN
      END IF
      IF ( PAR_STR == 'opa' ) THEN
           DATE_STR = MJDSEC_TO_DATE ( MJD, TAI, IER )
           WRITE ( UNIT=ZAG, FMT=110 ) STA_NAM, DATE_STR(1:16), FREQ/1.D9, AZ/DEG__TO__RAD
 110       FORMAT ( 'Atmospheric opacity at ', A, ' on ', A, ' at ', F5.1, ' GHz az ', F5.1, ' deg' )
        ELSE IF ( PAR_STR == 'tatm' ) THEN
           DATE_STR = MJDSEC_TO_DATE ( MJD, TAI, IER )
           WRITE ( UNIT=ZAG, FMT=120 ) STA_NAM, DATE_STR(1:16), FREQ/1.D9, AZ/DEG__TO__RAD
 120       FORMAT ( 'Atm. brightness temp. in K at ', A, &
     &              ' on ', A, ' at ', F5.1, ' GHz az ', F5.1, ' deg' )
      END IF
!
! --- Setting up the values of the DIAGI internal data structure for the further
! --- plotting
!
      DIAGI_S%NCLR      = 1
      DIAGI_S%NPOI(1)   = N_EL
      DIAGI_S%ADR_X8(1) = LOC(EL_R8)
      DIAGI_S%ADR_Y8(1) = LOC(VAL_ARR)
      DIAGI_S%LER(1)    = .FALSE.
      DIAGI_S%ICOL(1)   = ICL1
      DIAGI_S%IBST(1)   = 0
      DIAGI_S%ILST(1)   = 3
      DIAGI_S%IOST(1)   = 1
      DIAGI_S%IPST(1)   = 1
      DIAGI_S%IWST(1)   = 2
      DIAGI_S%ICLR      = 1
      DIAGI_S%XMIN      = SNGL(EL_MIN)
      DIAGI_S%XMAX      = SNGL(EL_MAX)
      DIAGI_S%YMIN      = SNGL(VAL_MIN)
      DIAGI_S%YMAX      = SNGL(VAL_MAX)
      DIAGI_S%ZAG       = ZAG
      DIAGI_S%ARG_UNITS = 'Elevation (deg)'
      DIAGI_S%ITRM      = 0
      DIAGI_S%STATUS    = DIA__DEF
      IF ( ILEN(OUT_DIR) == 0 ) THEN
           DIAGI_S%IDEV      = IDEV
           DIAGI_S%NAME      = NAME__DEF
           DIAGI_S%IBATCH    = 0
         ELSE
           CALL CLRCH ( STR )
           STR = STA_NAM
           CALL TRAN ( 12, STR, STR )
           DIAGI_S%IDEV   = 8
           DIAGI_S%NAME   = TRIM(OUT_DIR)//'/tipping_'//TRIM(STR)//'_'// &
     &                      TRIM(PAR_STR)//'_'//DATE_STR(1:4)//DATE_STR(6:7)// &
     &                      DATE_STR(9:13)//DATE_STR(15:16)//'.ps'
           DIAGI_S%IBATCH = 1
      END IF
!
! --- Calling the main routine of DiaGI
!
      CALL ERR_PASS ( IUER, IER )
      CALL DIAGI    ( DIAGI_S, IER )
      DEALLOCATE ( EL_R8 )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4591, IUER, 'GEN_TIPPING_PLOT', 'Failure to '// &
     &         'make a plot' )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GEN_TIPPING_PLOT  !#!#
#else
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GEN_TIPPING_PLOT ( PAR_STR, PLOT_STR, MJD, TAI, STA_NAM, &
     &                              FREQ, AZ, N_EL, EL_ARR, VAL_ARR, &
     &                              OUT_DIR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GEN_TIPPING_PLOT
! *                                                                      *
! * ## 08-JAN-2024  GEN_TIPPING_PLOT  v1.0 (c) L. Petrov 08-JAN-2024 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      CHARACTER  PAR_STR*(*), PLOT_STR*(*), STA_NAM*(*), OUT_DIR*(*)
      INTEGER*4  N_EL, MJD, IUER
      REAL*8     TAI, FREQ, AZ, EL_ARR(N_EL), VAL_ARR(N_EL)
      REAL*8,    ALLOCATABLE :: EL_R8(:)
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, ICL1, ICL2, ICL3, J1, IER
      REAL*8     EL_MIN, EL_MAX, EL_RN, VAL_MIN, VAL_MAX, VAL_RN
      INTEGER*4  DIAGI_LEN
      CHARACTER  ZAG*128, UNIT*128, DATE_STR*32, STR*128
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      WRITE ( 6, '(A)' ) 'spd_client was build without pgpplot. Quitting' 
      CALL EXIT ( 0 )
      RETURN
      END  SUBROUTINE  GEN_TIPPING_PLOT  !#!#
#endif
