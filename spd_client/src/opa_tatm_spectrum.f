       PROGRAM OPA_TATM_SPECTRUM
! ************************************************************************
! *                                                                      *
! *   Program OPA_TATM_SPECTRUM 
! *                                                                      *
! * ### 05-JAN-2024 OPA_TATM_SPECTRUM v1.0 (c) L. Petrov 08-JAN-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      TYPE       ( SPD_DEL__TYPE ) :: SPD_DEL
      CHARACTER  SPD_DIR*128, STA_NAM*8, DATE_BEG_STR*21, DATE_END_STR*21, &
     &           DATE_OBS_STR*30, ELEV_STR*12, AZIM_STR*12, FL_STR*12, &
     &           FH_STR*12, FS_STR*12, TS_STR*128, PAR_STR*12, PROC_STR*12, &
     &           OUT_DIR*128, ERRSTR*128, FILOUT*128, MODE_STR*128, &
     &           STR*128, STR1*128
      INTEGER*4    M_FRQ, MS
      PARAMETER  ( M_FRQ = 32*1024 )
      PARAMETER  ( MS    = 32*1024 )
      REAL*8     F_RANGE(2), FS_MIN, FRQ_ARR(M_FRQ), VAL_ARR(M_FRQ)
      REAL*8     TAI_BEG, TAI_END, TAI_OBS, EL, AZ, FL, FH, FS, TS
      INTEGER*4  MJD_BEG, MJD_END, MJD_OBS, J1, J2, J3, IDAY, NS, &
     &           N_FRQ, LUN, IUER
      LOGICAL*1, EXTERNAL :: IS_DIR_EXIST
      INTEGER*4, EXTERNAL :: GET_UNIT, ILEN, I_LEN
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
!
      F_RANGE(1) = 1.D9
      F_RANGE(2) = 1000.D9
      FS_MIN     = 1.D5
!
      IF ( IARGC() < 10 ) THEN
           WRITE ( 6, * ) 'Usage: opa_tatm_spectrum spd_dir sta_nam date '// &
     &                    'elev azim fl fh fs opa|tatm plot|table [output_dir [date_end ts]]'
           CALL EXIT ( 0 ) 
        ELSE
           CALL GETARG (  1, SPD_DIR  )
           CALL GETARG (  2, STA_NAM  )
           CALL GETARG (  3, DATE_BEG_STR )
           CALL GETARG (  4, ELEV_STR )
           CALL GETARG (  5, AZIM_STR )
           CALL GETARG (  6, FL_STR   )
           CALL GETARG (  7, FH_STR   )
           CALL GETARG (  8, FS_STR   )
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
           CALL ERR_LOG ( 4501, IUER, 'OPA_TATM_SPECTRUM', 'Error in '// &
     &         'attempt to open diectory with computed slant path delay, '// &
     &         'atmospheric opacity and brightness temperature '//TRIM(SPD_DIR)// &
     &         ' -- '//ERRSTR )
           CALL EXIT ( 1 )
      END IF
      IF ( ILEN(OUT_DIR) .NE. 0 .AND. OUT_DIR .NE. 'XW' ) THEN
           IF ( .NOT. IS_DIR_EXIST ( OUT_DIR, ERRSTR ) ) THEN
                 IUER = -1
                 CALL ERR_LOG ( 4502, IUER, 'OPA_TATM_SPECTRUM', 'Directory '// &
     &                TRIM(OUT_DIR)//' does not exist: '//ERRSTR )
                 CALL EXIT ( 1 )
           END IF
      END IF
!
      IUER = -1
      CALL DATE_TO_TIME ( DATE_BEG_STR, MJD_BEG, TAI_BEG, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4503, IUER, 'OPA_TATM_SPECTRUM', 'Error in parsing '// &
     &         'date' )
           CALL EXIT ( 1 )
      END IF
!
      IF ( INDEX ( ELEV_STR, '.' ) < 1 ) ELEV_STR = TRIM(ELEV_STR)//'.0'
      READ ( UNIT=ELEV_STR, FMT='(F12.6)', IOSTAT=IUER ) EL
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4504, IUER, 'OPA_TATM_SPECTRUM', 'Error in parsing '// &
     &         'elevation' )
           CALL EXIT ( 1 )
      END IF
      IF ( EL > 90.000001D0 .OR. EL < 2.999999D0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4505, IUER, 'OPA_TATM_SPECTRUM', 'Wrong elevation angle '// &
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
           CALL ERR_LOG ( 4506, IUER, 'OPA_TATM_SPECTRUM', 'Error in parsing '// &
     &         'azimuth' )
           CALL EXIT ( 1 )
      END IF
      IF ( AZ > 360.000001D0 .OR. AZ < 0.0D0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4507, IUER, 'OPA_TATM_SPECTRUM', 'Wrong azimuth angle '// &
     &          TRIM(AZIM_STR)//' -- it should be in a range of 0 to 360 deg' )
           CALL EXIT ( 1 )
      END IF
      IF ( AZ < 0.0000001D0  ) AZ = 0.0000001D0
      IF ( AZ > 359.999999D0 ) AZ = 359.999999D0 
      AZ = AZ*DEG__TO__RAD
!
      READ ( UNIT=FL_STR, FMT='(F12.6)', IOSTAT=IUER ) FL
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4508, IUER, 'OPA_TATM_SPECTRUM', 'Error in parsing '// &
     &         'low frequency' )
           CALL EXIT ( 1 )
      END IF
      IF ( FL < F_RANGE(1) .OR. FL > F_RANGE(2)  ) THEN
           IUER = -1
           CALL ERR_LOG ( 4509, IUER, 'OPA_TATM_SPECTRUM', 'Wrong low frequency '// &
     &          TRIM(FL_STR)//' -- it should be in a range of 1.D9 to 1.D12 Hz' )
           CALL EXIT ( 1 )
      END IF
!
      READ ( UNIT=FH_STR, FMT='(F12.6)', IOSTAT=IUER ) FH
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4510, IUER, 'OPA_TATM_SPECTRUM', 'Error in parsing '// &
     &         'high frequency' )
           CALL EXIT ( 1 )
      END IF
      IF ( FH < F_RANGE(1) .OR. FH > F_RANGE(2)  ) THEN
           IUER = -1
           CALL ERR_LOG ( 4511, IUER, 'OPA_TATM_SPECTRUM', 'Wrong upper frequencty '// &
     &          TRIM(FH_STR)//' -- it should be in a range of 1.D9 0 to 1.D11 Hz' )
           CALL EXIT ( 1 )
      END IF
!      
      READ ( UNIT=FS_STR, FMT='(F12.6)', IOSTAT=IUER ) FS
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4512, IUER, 'OPA_TATM_SPECTRUM', 'Error in parsing '// &
     &         'frequency step' )
           CALL EXIT ( 1 )
      END IF
      IF ( FS < FS_MIN ) THEN
           IUER = -1
           CALL ERR_LOG ( 4513, IUER, 'OPA_TATM_SPECTRUM', 'Two small frequency step '// &
     &          FS_STR//' -- less than 100KHz' )
           CALL EXIT ( 1 )
      END IF
      IF ( (FH - FL)/FS < 2 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4514, IUER, 'OPA_TATM_SPECTRUM', 'Bad combination of '// &
     &         'lower, upper frequencies and the frequency step: too few points '// &
     &         ' for the spectrum' )
           CALL EXIT ( 1 )
      END IF
      N_FRQ = IDNINT ( (FH - FL)/FS ) + 1
      IF ( N_FRQ > M_FRQ ) THEN
           IUER = -1
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( N_FRQ, STR  )
           CALL INCH  ( M_FRQ, STR1 )
           CALL ERR_LOG ( 4515, IUER, 'OPA_TATM_SPECTRUM', 'Too many frequenies: '// &
     &          TRIM(STR)//', which exceeds the limit '//TRIM(STR1)//' -- please '// &
     &          'reduce frequency resolution' )
           CALL EXIT ( 1 )
      END IF
      IF ( N_FRQ < 1 ) N_FRQ = 1
!
      CALL TRAN ( 12, PAR_STR, PAR_STR )
      IF ( PAR_STR == 'opa'  .OR. &
     &     PAR_STR == 'tatm'      ) THEN
         ELSE
           IUER = -1
           CALL ERR_LOG ( 4516, IUER, 'OPA_TATM_SPECTRUM', 'Wrong parameter '//TRIM(PAR_STR)// &
     &         '. Only opa and tatm are supported' )
           CALL EXIT ( 1 )
      END IF
!
      CALL TRAN ( 12, PROC_STR, PROC_STR )
      IF ( PROC_STR(1:4) == 'plot'  .OR. &
     &     PROC_STR(1:5) == 'table'      ) THEN
         ELSE
           IUER = -1
           CALL ERR_LOG ( 4517, IUER, 'OPA_TATM_SPECTRUM', 'Wrong processing '// &
     &         'code '//TRIM(PROC_STR)//'. Only plot and table are supported' )
           CALL EXIT ( 1 )
      END IF
!
      DO 410 J1=1,N_FRQ
         FRQ_ARR(J1) = FL + (J1-1)*FS
 410  CONTINUE 
!
      IUER = -1
      CALL DATE_TO_TIME ( DATE_END_STR, MJD_END, TAI_END, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4518, IUER, 'OPA_TATM_SPECTRUM', 'Error in parsing '// &
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
                CALL ERR_LOG ( 4519, IUER, 'OPA_TATM_SPECTRUM', 'Error in parsing '// &
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
                CALL ERR_LOG ( 4521, IUER, 'OPA_TATM_SPECTRUM', 'To many '// &
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
           CALL ERR_LOG ( 4520, IUER, 'OPA_TATM_SPECTRUM', 'Error in '// &
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
         CALL GET_OPA_TATM_SPECTRUM ( PAR_STR, SPD_DEL, MJD_OBS, TAI_OBS, &
     &                                EL, AZ, N_FRQ, FRQ_ARR, VAL_ARR, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 4521, IUER, 'OPA_TATM_SPECTRUM', 'Error in '// &
     &            'computation of the interpolated spectrum' )
              CALL EXIT ( 1 )
         END IF
!
         DATE_OBS_STR = MJDSEC_TO_DATE ( MJD_OBS, TAI_OBS, IUER )
         IF ( PROC_STR(1:4) == 'plot' ) THEN
              IUER = -1
              CALL GEN_OPA_TATM_PLOT ( PAR_STR, PROC_STR, MJD_OBS, TAI_OBS, &
     &                                 STA_NAM, EL, AZ, N_FRQ, FRQ_ARR, VAL_ARR, &
     &                                 OUT_DIR, IUER )
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
                        CALL ERR_LOG ( 4522, IUER, 'OPA_TATM_SPECTRUM', 'Error in '// &
     &                      'an attempt to open output file '//FILOUT )
                        CALL EXIT ( 1 )
                   END IF
              END IF
              WRITE ( UNIT=LUN, FMT=110 ) SPD_OPA_TATM_SPECTRUM__LABEL, &
     &                                    TRIM(PAR_STR), TRIM(STA_NAM), DATE_OBS_STR(1:16), &
     &                                    TRIM(ELEV_STR), TRIM(AZIM_STR), TRIM(SPD_DIR), &
     &                                    GET_CDATE()
 110          FORMAT ( A / &
     &                 '# ', A, ' at ', A, ' on ', A, &
     &                 ' for elevation ', A, ' and azimuth ', A, ' deg'/ &
     &                 '# Used expansion: ', A/ &
     &                 '# Created on ', A/ &
     &                 '#' )
              DO 430 J3=1,N_FRQ
                 IF ( PAR_STR == 'opa' ) THEN
                      WRITE  ( UNIT=LUN, FMT=120 ) J3, FRQ_ARR(J3)*1.D-9, VAL_ARR(J3)
 120                  FORMAT ( 'I= ', I5,' Freq: ', F8.4, ' GHz  opacity: ', F8.4 )
                    ELSE IF  ( PAR_STR == 'tatm' ) THEN
                      WRITE  ( UNIT=LUN, FMT=130 ) J3, FRQ_ARR(J3)*1.D-9, VAL_ARR(J3)
 130                  FORMAT ( 'I= ', I5,' Freq: ', F8.4, ' GHz  Tatm: ', F5.1 )
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
           CALL ERR_LOG ( 4523, IUER, 'OPA_TATM_SPECTRUM', 'Failure in an '// &
     &         'attempt to release memory used by SPD_DEL' )
           RETURN 
      END IF
!
      END  PROGRAM  OPA_TATM_SPECTRUM  !#!#
!
! ------------------------------------------------------------------------
!
#ifdef USE_PETOOLS
      SUBROUTINE GEN_OPA_TATM_PLOT ( PAR_STR, PLOT_STR, MJD, TAI, STA_NAM, &
     &                               EL, AZ, N_FRQ, FRQ_ARR, VAL_ARR, &
     &                               OUT_DIR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GEN_OPA_TATM_PLOT
! *                                                                      *
! * ## 06-JAN-2024  GEN_OPA_TATM_PLOT  v1.0 (c) L. Petrov 06-JAN-2024 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'diagi.i'
      INCLUDE   'astro_constants.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      CHARACTER  PAR_STR*(*), PLOT_STR*(*), STA_NAM*(*), OUT_DIR*(*)
      INTEGER*4  N_FRQ, MJD, IUER
      REAL*8     TAI, EL, AZ, FRQ_ARR(N_FRQ), VAL_ARR(N_FRQ)
      REAL*8,    ALLOCATABLE :: FRQ_R8(:)
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, ICL1, ICL2, ICL3, J1, IER
      REAL*8     FRQ_MIN, FRQ_MAX, VAL_MIN, VAL_MAX, FRQ_RN, VAL_RN
      INTEGER*4  DIAGI_LEN
      CHARACTER  ZAG*128, UNIT*128, DATE_STR*32, STR*128
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      ALLOCATE ( FRQ_R8(N_FRQ) )
      DO 410 J1=1,N_FRQ
         FRQ_R8(J1) = 1.D-9*FRQ_ARR(J1)
         IF ( J1 == 1 ) THEN
              FRQ_MIN = FRQ_R8(1)
              FRQ_MAX = FRQ_R8(1)
              VAL_MIN = VAL_ARR(1)
              VAL_MAX = VAL_ARR(1)
            ELSE
              FRQ_MIN = MIN ( FRQ_MIN, FRQ_R8(J1) )
              FRQ_MAX = MAX ( FRQ_MAX, FRQ_R8(J1) )
              VAL_MIN = MIN ( VAL_MIN, VAL_ARR(J1) )
              VAL_MAX = MAX ( VAL_MAX, VAL_ARR(J1) )
         END IF
 410  CONTINUE 
!
      FRQ_RN = (FRQ_MAX - FRQ_MIN)
      FRQ_MIN = FRQ_MIN - 0.01*FRQ_RN
      FRQ_MAX = FRQ_MAX + 0.01*FRQ_RN
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
           CALL ERR_LOG ( 4151, IUER, 'DIAGI_1', 'Error in setting '// &
     &         'default values for the plot' )
           RETURN
      END IF
      IF ( PAR_STR == 'opa' ) THEN
           DATE_STR = MJDSEC_TO_DATE ( MJD, TAI, IER )
           WRITE ( UNIT=ZAG, FMT=110 ) STA_NAM, DATE_STR(1:16), EL/DEG__TO__RAD, AZ/DEG__TO__RAD
 110       FORMAT ( 'Atmospheric opacity at ', A, ' on ', A, ' at el ', F4.1, ' az ', F5.1, ' deg' )
        ELSE IF ( PAR_STR == 'tatm' ) THEN
           DATE_STR = MJDSEC_TO_DATE ( MJD, TAI, IER )
           WRITE ( UNIT=ZAG, FMT=120 ) STA_NAM, DATE_STR(1:16), EL/DEG__TO__RAD, AZ/DEG__TO__RAD
 120       FORMAT ( 'Atm. brightness temper. in K at ', A, &
     &              ' on ', A, ' at el ', F4.1, ' az ', F5.1, ' deg' )
      END IF
!
! --- Setting up the values of the DIAGI internal data structure for the further
! --- plotting
!
      DIAGI_S%NCLR      = 1
      DIAGI_S%NPOI(1)   = N_FRQ
      DIAGI_S%ADR_X8(1) = LOC(FRQ_R8)
      DIAGI_S%ADR_Y8(1) = LOC(VAL_ARR)
      DIAGI_S%LER(1)    = .FALSE.
      DIAGI_S%ICOL(1)   = ICL1
      DIAGI_S%IBST(1)   = 0
      DIAGI_S%ILST(1)   = 3
      DIAGI_S%IOST(1)   = 1
      DIAGI_S%IPST(1)   = 1
      DIAGI_S%IWST(1)   = 2
      DIAGI_S%ICLR      = 1
      DIAGI_S%XMIN      = SNGL(FRQ_MIN)
      DIAGI_S%XMAX      = SNGL(FRQ_MAX)
      DIAGI_S%YMIN      = SNGL(VAL_MIN)
      DIAGI_S%YMAX      = SNGL(VAL_MAX)
      DIAGI_S%ZAG       = ZAG
      DIAGI_S%ARG_UNITS = 'Frequency (GHz)'
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
           DIAGI_S%NAME   = TRIM(OUT_DIR)//'/spectrum_'//TRIM(STR)//'_'//TRIM(PAR_STR)//'_'// &
     &                      DATE_STR(1:4)//DATE_STR(6:7)// &
     &                      DATE_STR(9:13)//DATE_STR(15:16)//'.ps'
           DIAGI_S%IBATCH = 1
      END IF
!
! --- Calling the main routine of DiaGI
!
      CALL ERR_PASS ( IUER, IER )
      CALL DIAGI    ( DIAGI_S, IER )
      DEALLOCATE ( FRQ_R8 )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4591, IUER, 'GEN_OPA_TATM_PLOT', 'Failure to '// &
     &         'make a plot' )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GEN_OPA_TATM_PLOT  !#!#
#else
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GEN_OPA_TATM_PLOT ( PAR_STR, PLOT_STR, MJD, TAI, STA_NAM, &
     &                               EL, AZ, N_FRQ, FRQ_ARR, VAL_ARR, &
     &                               OUT_DIR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GEN_OPA_TATM_PLOT
! *                                                                      *
! * ## 06-JAN-2024  GEN_OPA_TATM_PLOT  v1.0 (c) L. Petrov 06-JAN-2024 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      CHARACTER  PAR_STR*(*), PLOT_STR*(*), STA_NAM*(*), OUT_DIR*(*)
      INTEGER*4  N_FRQ, MJD, IUER
      REAL*8     TAI, EL, AZ, FRQ_ARR(N_FRQ), VAL_ARR(N_FRQ)
      REAL*8,    ALLOCATABLE :: FRQ_R8(:)
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, ICL1, ICL2, ICL3, J1, IER
      REAL*8     FRQ_MIN, FRQ_MAX, VAL_MIN, VAL_MAX, FRQ_RN, VAL_RN
      INTEGER*4  DIAGI_LEN
      CHARACTER  ZAG*128, UNIT*128, DATE_STR*32, STR*128
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      WRITE ( 6, '(A)' ) 'spd_client was build without pgpplot. Quitting' 
      CALL EXIT ( 0 )
      RETURN
      END  SUBROUTINE  GEN_OPA_TATM_PLOT  !#!#
#endif
