      SUBROUTINE SUR_SOURCE ( SUR, VTD, SOURCE_FILE, L_SOU, SOU, LOBS_SOU, &
     &                        OBS_CAT, DUR_SEC, SCAN_LEN, TYP, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SUR_SOURCE
! *                                                                      *
! *  ### 23-NOV-2005   SUR_SOURCE  v3.4 (c)  L. Petrov  09-JAN-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      INCLUDE   'getpar.i'
      INCLUDE   'vtd.i'
      INTEGER*4   M_BUF
      PARAMETER ( M_BUF = SUR__M_SOU )
      TYPE      ( SUR__TYPE        ) :: SUR
      TYPE      ( SUR_SOU__TYPE    ) :: SOU(SUR__M_SOU)
      TYPE      ( SOURCE_CAT__TYPE ) :: OBS_CAT(SUR__M_SOU)
      TYPE      ( VTD__TYPE        ) :: VTD
      CHARACTER  SOURCE_FILE*128, BUF(M_BUF)*256
      INTEGER*4  L_SOU, LOBS_SOU, TYP, IVRB, IUER
      LOGICAL*4  FL_VIS, FL_ERROR, FL_FINISH
      CHARACTER  J2000_NAME*10, B1950_NAME*8, ALPHA_STR*12, DELTA_STR*12, &
     &           DATE_STR*128, STR*128, STR1*128
      REAL*8     S_VEC(3), ALPHA, DELTA, TAI_OBS, DIST_COS, DUR_SEC, &
     &           SCAN_LEN, AZ, ELEV, HA, FLUX_VAL, DUR, PRI, EL_MIN, &
     &           TAI_MID, COO_EAR(3), VEL_EAR(3), ACC_EAR(3), COO_SUN(3), &
     &           VEL_SUN(3), ACC_SUN(3), RD_SUN, SUN_DIST, GAP_MIN, GAP_NOR, &
     &           TAI_EPOCH, RANGE, DST
      LOGICAL*1  FL_DUR, FL_NOB, FL_GAP
      INTEGER*4  IDAY, MJD_OBS, N_BUF, L_CHK, J1, J2, J3, J4, J5, J6, IP, &
     &           MIN_STA, K_STA, NOBS, MJD_MID, NSCA_MIN, NSCA_MAX, &
     &           IVAL, NTHR, MJD_EPOCH, IER
      REAL*8,    EXTERNAL :: DP_VV_V
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      LOGICAL*4, EXTERNAL :: SUR_CHECK_VIS 
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
! --- Read the source file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( SOURCE_FILE, M_BUF, BUF, N_BUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1411, IUER, 'SUR_SOURCE', 'Error in reading '// &
     &         'source file '//SOURCE_FILE )
           RETURN
      END IF
      IF ( BUF(1)(1:68) .NE. &
     & '# CATRES Flux and Spectral index file. Format version of 2004.12.18 ' ) THEN
           CALL ERR_LOG ( 1412, IUER, 'SUR_SOURCE', 'Input source '// &
     &         'file '//SOURCE_FILE(1:I_LEN(SOURCE_FILE))//' is not in '// &
     &         'CATRES format' )
           WRITE ( 6, * ) 'BUF(1) = ', BUF(1)
           RETURN
      END IF
      IF ( BUF(2)(1:23) == '# DURATION AND PRIORITY' ) THEN
           FL_DUR = .TRUE.
           FL_NOB = .FALSE.
           FL_GAP = .FALSE.
           IF ( IVRB .GE. 4 .AND. IVRB .NE. 21 ) THEN
                WRITE ( 6, * ) 'Source file: '//SOURCE_FILE(1:I_LEN(SOURCE_FILE))
                WRITE ( 6, * ) 'Durations and priorities ARE defined'
           END IF
         ELSE IF ( BUF(2)(1:29) == '# DURATION, PRIORITY AND NOBS' ) THEN
           FL_DUR = .TRUE.
           FL_NOB = .TRUE.
           FL_GAP = .FALSE.
           IF ( IVRB .GE. 4 .AND. IVRB .NE. 21 ) THEN
                WRITE ( 6, * ) 'Source file: '//SOURCE_FILE(1:I_LEN(SOURCE_FILE))
                WRITE ( 6, * ) 'Durations, priorities, and nobs ARE defined'
           END IF
         ELSE IF ( BUF(2)(1:41) == '# DURATION, PRIORITY, NOBS, AND INTERVALS' ) THEN
           FL_DUR = .TRUE.
           FL_NOB = .TRUE.
           FL_GAP = .TRUE.
           IF ( IVRB .GE. 4 .AND. IVRB .NE. 21 ) THEN
                WRITE ( 6, * ) 'Source file: '//SOURCE_FILE(1:I_LEN(SOURCE_FILE))
                WRITE ( 6, * ) 'Durations, priorities, nobs, and intervlas ARE defined'
           END IF
         ELSE 
           FL_DUR = .FALSE.
           FL_NOB = .FALSE.
           IF ( IVRB .GE. 4 .AND. IVRB .NE. 21 ) THEN
                WRITE ( 6, * ) 'Source file: '//SOURCE_FILE(1:I_LEN(SOURCE_FILE))
                WRITE ( 6, * ) 'Durations and priorities are NOT defined'
           END IF
      END IF
!
      MJD_MID = SUR%MJD_START
      TAI_MID = SUR%TAI_START + ( SUR%MJD_STOP - SUR%MJD_START )*86400.0D0 + &
     &                          ( SUR%TAI_STOP - SUR%TAI_START )
      IF ( TAI_MID > 86400.0D0 ) THEN
           TAI_MID = TAI_MID - 86400.0D0
           MJD_MID = MJD_MID + 1
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL PLANETA_DE_EPH ( VTD%DE_EPH, MJD_MID, TAI_MID, 'EARTH', &
     &                      COO_EAR, VEL_EAR, ACC_EAR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1413, IUER, 'SUR_SOURCE', 'Failure in attempt '// &
     &         'to get baricentri coordinats of the Earth' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL PLANETA_DE_EPH ( VTD%DE_EPH, MJD_MID, TAI_MID, 'SUN', &
     &                      COO_SUN, VEL_SUN, ACC_SUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1414, IUER, 'SUR_SOURCE', 'Failure in attempt '// &
     &         'to get baricentri coordinats of the Earth' )
           RETURN
      END IF
      COO_SUN = COO_SUN - COO_EAR
      CALL NORM_VEC ( 3, COO_SUN, RD_SUN )
!
      L_CHK = DUR_SEC/SUR__INTV_CHECK + 1
      IF ( IVRB .GE. 5 .AND. IVRB .NE. 21 ) THEN
           WRITE ( 6, * ) 'The number of checks for determining source up time: ', L_CHK
      END IF
!
! --- Parse the source file
!
      L_SOU = 0
      DO 420 J2=1,N_BUF
         IF ( BUF(J2)(1:1)    == '#' ) GOTO 420
         IF ( BUF(J2)(78:78)  == '@' ) GOTO 420
         IF ( BUF(J2)(78:78)  == '%' ) GOTO 420
         IF ( ILEN(BUF(J2)) <   1    ) GOTO 420
         J2000_NAME = BUF(J2)(1:10)
         B1950_NAME = BUF(J2)(81:88)
         ALPHA_STR  = BUF(J2)(13:23)
         DELTA_STR  = BUF(J2)(26:36)
         CALL BLANK_TO_ZERO ( ALPHA_STR )
         CALL BLANK_TO_ZERO ( DELTA_STR )
!
! ------ Parse right ascension and declination
!
         CALL ERR_PASS ( IUER, IER )
         CALL HR_TAT   ( ALPHA_STR, ALPHA, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1415, IUER, 'SUR_SOURCE', 'Error in '// &
     &            'decording right ascension '//ALPHA_STR//' in input '// &
     &            ' SOURCE_FILE '//SOURCE_FILE )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL GR_TAT   ( DELTA_STR, DELTA, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1416, IUER, 'SUR_SOURCE', 'Error in '// &
     &            'decording declination '//DELTA_STR//' in input '// &
     &            ' SOURCE_FILE '//SOURCE_FILE )
              RETURN
         END IF
!
         S_VEC(1) = DCOS(DELTA)*DCOS(ALPHA)
         S_VEC(2) = DCOS(DELTA)*DSIN(ALPHA)
         S_VEC(3) = DSIN(DELTA)
!
         IF ( INDEX ( BUF(J2), 'Dst: ' ) > 0 ) THEN
              IP = INDEX ( BUF(J2), 'Dst:' ) + LEN('Dst:')
              STR = BUF(J2)(IP:)
              CALL CHASHL ( STR )
              IP = INDEX( STR, ' ' )
              IF ( IP > 0 ) CALL CLRCH ( STR(IP:) )
              READ ( UNIT=STR, FMT='(F25.12)', IOSTAT=IER  ) DST
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR1 )
                   CALL INCH  ( J2, STR1 )
                   CALL ERR_LOG ( 1417, IUER, 'SUR_SOURCE', 'Error in '// &
     &                 'parsing Dst: word at the '//TRIM(STR)// &
     &                 ' th line of the source file '//SOURCE_FILE )
                   RETURN 
              END IF
           ELSE
              DST = 0.0D0
         END IF
         IF ( INDEX ( BUF(J2), 'Date: ' ) > 0 ) THEN
              IP = INDEX ( BUF(J2), 'Date:' ) + LEN('Date:')
              STR = BUF(J2)(IP:)
              CALL CHASHL ( STR )
              IP = INDEX( STR, ' ' )
              IF ( IP > 0 ) CALL CLRCH ( STR(IP:) )
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( STR, MJD_EPOCH, TAI_EPOCH, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR1 )
                   CALL INCH  ( J2, STR1 )
                   CALL ERR_LOG ( 1418, IUER, 'SUR_SOURCE', 'Error in '// &
     &                 'parsing Date: word at the '//TRIM(STR)// &
     &                 ' th line of the source file '//SOURCE_FILE )
                   RETURN 
              END IF
!
              IP = INDEX ( BUF(J2), 'Range:' ) + LEN('Range:')
              IF ( IP < 1 ) THEN
                   CALL CLRCH ( STR1 )
                   CALL INCH  ( J2, STR1 )
                   CALL ERR_LOG ( 1419, IUER, 'SUR_SOURCE', 'Did not '// &
     &                 'find Range word at the '//TRIM(STR)// &
     &                 ' th line of the source file '//SOURCE_FILE )
                   RETURN 
              END IF
              STR = BUF(J2)(IP:)
              CALL CHASHL ( STR )
              IP = INDEX( STR, ' ' )
              IF ( IP > 0 ) CALL CLRCH ( STR(IP:) )
              READ ( UNIT=STR(1:8), FMT='(F8.1)', IOSTAT=IER  ) RANGE
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR1 )
                   CALL INCH  ( J2, STR1 )
                   CALL ERR_LOG ( 1420, IUER, 'SUR_SOURCE', 'Error in '// &
     &                 'parsing Range: word at the '//TRIM(STR)// &
     &                 ' th line of the source file '//SOURCE_FILE )
                   RETURN 
              END IF
            ELSE              
              MJD_EPOCH = 0.0
              TAI_EPOCH = 0.0
              RANGE = 1.001D20
         END IF
!
         SUN_DIST = DACOS ( DP_VV_V ( 3, S_VEC, COO_SUN ) ) 
         IF ( SUN_DIST < SUR%SUN_DIST_MIN ) THEN
              IF ( IVRB .GE. 2 .AND. IVRB .NE. 21 ) THEN
                   DATE_STR = MJDSEC_TO_DATE ( MJD_MID, TAI_MID, IER )
                   WRITE ( 6, 110 ) B1950_NAME, SUN_DIST/DEG__TO__RAD, DATE_STR(1:16)
 110               FORMAT ( 'Source ', A, ' is too close to the Sun: ', &
     &                      F6.2, ' deg on ', A, ' so it is discarded' )
              END IF
              GOTO 420
         END IF
!
         READ ( UNIT=BUF(J2)(39:48), FMT='(F10.1)', IOSTAT=IER ) FLUX_VAL
         IF ( IER .NE. 0 ) THEN
              FLUX_VAL = 0.0
!@              CALL ERR_LOG ( 1421, IUER, 'SUR_SOURCE', 'Error in '// &
!@     &            'decoding flux density '//BUF(J2)(39:48)//' in the '// &
!@     &            'input SOURCE_FILE '//SOURCE_FILE )
!@              RETURN
         END IF
!
! ------ Defaults
!
         DUR     = SCAN_LEN
         PRI     = 1.0
         MIN_STA = SUR%L_STA
         EL_MIN  = EL__SOU_MIN
         NSCA_MIN = SUR%SCAN_PER_SOURCE_MIN
         NSCA_MAX = SUR%SCAN_PER_SOURCE_MAX
!
         IF ( FL_DUR ) THEN
              IF ( ILEN(BUF(J2)(91:96)) == 0 ) THEN
                   DUR = SCAN_LEN
                 ELSE 
                   READ ( UNIT=BUF(J2)(91:96), FMT='(F6.1)', IOSTAT=IER ) DUR
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 1422, IUER, 'SUR_SOURCE', 'Error in '// &
     &                      'decoding duration length '//BUF(J2)(91:96)// &
     &                      ' in the input SOURCE_FILE '//SOURCE_FILE )
                        RETURN
                  END IF
              END IF
!
              IF ( ILEN(BUF(J2)(88:104)) == 0 ) THEN
                   PRI = 1.0D0
                 ELSE
                   READ ( UNIT=BUF(J2)(98:104), FMT='(F7.1)', IOSTAT=IER ) PRI
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 1423, IUER, 'SUR_SOURCE', 'Error in '// &
     &                      'decoding source priority '//BUF(J2)(98:104)// &
     &                      ' in the input SOURCE_FILE '//SOURCE_FILE )
                        RETURN
                   END IF
              END IF
!
              IF ( ILEN(BUF(J2)(106:107)) == 0 ) THEN
                   MIN_STA = SUR%L_STA
                ELSE 
                   READ ( UNIT=BUF(J2)(106:107), FMT='(I2)', IOSTAT=IER ) MIN_STA
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 1424, IUER, 'SUR_SOURCE', 'Error in '// &
     &                      'decoding min number of stations for the source '// &
     &                       BUF(J2)(1:10)//' -- field: '//BUF(J2)(106:107)// &
     &                      '  in the input SOURCE_FILE '//SOURCE_FILE )
                        RETURN
                   END IF
              END IF
!
              IF ( ILEN(BUF(J2)(109:112)) == 0 ) THEN
                   EL_MIN = EL__SOU_MIN
                ELSE 
                   READ ( UNIT=BUF(J2)(109:112), FMT='(F4.1)', IOSTAT=IER ) EL_MIN
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 1425, IUER, 'SUR_SOURCE', 'Error in '// &
     &                      'decoding min elevation angle for the source '// &
     &                       BUF(J2)(1:10)//' -- field: '//BUF(J2)(109:112)// &
     &                      '  in the input SOURCE_FILE '//SOURCE_FILE )
                        RETURN
                   END IF
                   EL_MIN = EL_MIN*DEG__TO__RAD
              END IF
         END IF
         IF ( FL_NOB ) THEN
              READ ( UNIT=BUF(J2)(113:115), FMT='(I3)', IOSTAT=IER ) NSCA_MIN 
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1426, IUER, 'SUR_SOURCE', 'Error in '// &
     &                 'decoding field min scheduled observations '// &
     &                  BUF(J2)(114:115)//' in the input SOURCE_FILE '// &
     &                  SOURCE_FILE )
                   RETURN
              END IF
!
              READ ( UNIT=BUF(J2)(116:118), FMT='(I3)', IOSTAT=IER ) NSCA_MAX
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1427, IUER, 'SUR_SOURCE', 'Error in '// &
     &                 'decoding field max scheduled observations '// &
     &                  BUF(J2)(117:118)//' in the input SOURCE_FILE '// &
     &                  SOURCE_FILE )
                   RETURN
              END IF
         END IF
!
         IF ( FL_GAP ) THEN
              READ ( UNIT=BUF(J2)(121:123), FMT='(I3)', IOSTAT=IER ) IVAL
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1428, IUER, 'SUR_SOURCE', 'Error in '// &
     &                 'decoding field minimum intervla between scans '// &
     &                  BUF(J2)(121:123)//' in the input SOURCE_FILE '// &
     &                  TRIM(SOURCE_FILE)//' while procerssing line '// &
     &                  TRIM(BUF(J2)) )
                   RETURN
              END IF
              GAP_MIN = IVAL*60.0
!
              READ ( UNIT=BUF(J2)(125:127), FMT='(I3)', IOSTAT=IER ) IVAL
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1429, IUER, 'SUR_SOURCE', 'Error in '// &
     &                 'decoding field maximum interval between scans '// &
     &                  BUF(J2)(125:127)//' in the input SOURCE_FILE '// &
     &                  TRIM(SOURCE_FILE)//' while processing line '// &
     &                  TRIM(BUF(J2)) )
                   RETURN
              END IF
              GAP_NOR = IVAL*60.0
            ELSE
              GAP_MIN = SUR%SCAN_GAP_SOURCE_MIN
              GAP_NOR = SUR%SCAN_GAP_SOURCE_NORM
         END IF
         NOBS = 0
!
! ------ If yes, bypass it
!
         DO 430 J3=1,LOBS_SOU
            DIST_COS = DP_VV_V ( 3, S_VEC, OBS_CAT(J3)%S_VEC )
            IF ( DIST_COS > DCOS(SUR__DIST_LIM) ) THEN
                 IF ( SUR%ALGORITHM == 'ASTROMET_12' ) THEN
                      IF (.NOT.  FL_NOB ) NSCA_MAX = 1
                      IF ( OBS_CAT(J3)%NOBS_TOTAL .GE. NSCA_MAX ) THEN
                           IF ( IVRB .GE. 5 .AND. IVRB .NE. 21 ) THEN
                                WRITE ( 6, '(A,I2,A)' ) 'The source '//B1950_NAME// &
     &                                '   '//J2000_NAME// &
     &                                ' was already observed ', INT2(OBS_CAT(J3)%NOBS_TOTAL), ' times. Skipping'
                           END IF
                           GOTO 420
                      END IF
                    ELSE IF ( SUR%ALGORITHM == 'ASTROMET_05' ) THEN
                      IF ( OBS_CAT(J3)%NOBS_TOTAL == 0 ) THEN
                           GOTO 420
                        ELSE IF ( OBS_CAT(J3)%NOBS_TOTAL > SUR%SCAN_PER_SOURCE_MAX ) THEN
                           GOTO 420
                      END IF
                    ELSE IF ( SUR%ALGORITHM == 'GEODETIC_01' ) THEN
                      CONTINUE 
                    ELSE 
                      IF ( OBS_CAT(J3)%NOBS_TOTAL .GE. SUR%SCAN_PER_SOURCE_MAX ) THEN
                           GOTO 420
                      END IF
                 END IF
            END IF
 430     CONTINUE
!
         L_SOU = L_SOU + 1
         SOU(L_SOU)%J2000_NAME = J2000_NAME
         SOU(L_SOU)%B1950_NAME = B1950_NAME
         SOU(L_SOU)%ALPHA_STR  = ALPHA_STR
         SOU(L_SOU)%DELTA_STR  = DELTA_STR
         SOU(L_SOU)%ALPHA      = ALPHA
         SOU(L_SOU)%DELTA      = DELTA
         SOU(L_SOU)%FLUX       = FLUX_VAL
         SOU(L_SOU)%DUR        = DUR
         SOU(L_SOU)%PRI        = PRI
         SOU(L_SOU)%MIN_STA    = MIN_STA
         SOU(L_SOU)%EL_MIN     = EL_MIN
         SOU(L_SOU)%NSCA_MIN   = NSCA_MIN
         SOU(L_SOU)%NSCA_MAX   = NSCA_MAX
         SOU(L_SOU)%GAP_MIN    = GAP_MIN
         SOU(L_SOU)%GAP_NOR    = GAP_NOR
         SOU(L_SOU)%NOBS       = NOBS
         SOU(L_SOU)%MJD_EPOCH  = MJD_EPOCH
         SOU(L_SOU)%TAI_EPOCH  = TAI_EPOCH
         SOU(L_SOU)%RANGE = RANGE
         SOU(L_SOU)%DST   = DST
         CALL COPY_R8 ( 3, S_VEC, SOU(L_SOU)%S_VEC )
         SOU(L_SOU)%FL_USE = .FALSE.
!
! ------ Now check whether the source will be up during experiment
! ------ at least at MIN_STA antennas at least once
!
         FL_FINISH = .FALSE.
!$OMP    PARALLEL DO IF ( NTHR > 1 ), DEFAULT ( NONE), &
!$OMP&   PRIVATE ( J4, J5, MJD_OBS, TAI_OBS, IDAY, K_STA, AZ, ELEV, HA, IER ), &
!$OMP&   SHARED  ( SUR, VTD, SOU, FL_ERROR, FL_FINISH, IVRB, &
!$OMP&             L_SOU, TYP, MIN_STA, SUR__TYP_STR, L_CHK, IUER ), &
!$OMP&   SCHEDULE ( GUIDED )
         DO 440 J4=1,L_CHK
            IF ( FL_FINISH ) GOTO 440
            IF ( FL_ERROR  ) GOTO 440
            TAI_OBS = SUR%TAI_START + SUR__INTV_CHECK*(J4-1)
            IDAY = TAI_OBS/86400.0D0
            MJD_OBS = SUR%MJD_START + IDAY
            TAI_OBS = TAI_OBS - IDAY*86400.0D0
            K_STA = 0
            DO 450 J5=1,SUR%L_STA
               IF ( SUR%STA(J5)%TAGALONE ) GOTO 450
!
! ------------ Compute elevation and azimuth for this moment of time
!
               CALL ERR_PASS ( IUER, IER )
               CALL SUR_AZEL ( SUR, VTD, TYP, MJD_OBS, TAI_OBS, &
     &                         J5, L_SOU, AZ, ELEV, HA, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 1430, IUER, 'SUR_SOURCE', 'Error in '// &
     &                   'computing azimuth and elevation' )
!$OMP               CRITICAL (ERROR)
                    FL_ERROR = .TRUE.
!$OMP               END CRITICAL (ERROR)
                    GOTO 440
               END IF
               IF ( ELEV > SOU(L_SOU)%EL_MIN ) THEN
                    IF ( SUR_CHECK_VIS ( SUR, J5, TYP, L_SOU, AZ, ELEV, &
     &                                   HA, IER ) ) THEN
                         K_STA = K_STA + 1
                   END IF
               END IF
 450        CONTINUE
            IF ( K_STA .GE. MIN_STA ) THEN
                 IF ( IVRB .GE. 5 .AND. IVRB .NE. 21 ) THEN
!$OMP                 CRITICAL (UPDATE)
                      WRITE ( 6, '(A)' ) 'The source '//SOU(L_SOU)%B1950_NAME// &
     &                           '   '//SOU(L_SOU)%J2000_NAME// &
     &                               ' IS INCLUDED IN THE LIST '//SUR__TYP_STR(TYP)
!$OMP                 END CRITICAL (UPDATE)
                 END IF
                 FL_FINISH = .TRUE.
            END IF
 440     CONTINUE
!$OMP END PARALLEL DO
         IF ( FL_FINISH ) GOTO 420
!
! ------ The source is not visible
!
         IF ( IVRB .GE. 5 .AND. IVRB .NE. 21 ) THEN
              WRITE ( 6, '(A)' ) 'The source '//SOU(L_SOU)%B1950_NAME// &
     &                           '   '//SOU(L_SOU)%J2000_NAME// &
     &                           ' is not visible '//SUR__TYP_STR(TYP)
         END IF
         L_SOU = L_SOU - 1
 420  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SUR_SOURCE  !#!#
