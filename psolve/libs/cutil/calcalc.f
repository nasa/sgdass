      SUBROUTINE CALCALC ( SITE, ISITN, STAR, VSTARC, S_AZ, S_ELEV, PI, &
     &                     S_ATMPR, S_RELHU, S_TEMPC, VLIGHT, CFADRY, &
     &                     CFAWET, ID_JJ, FLYBY_WARNING, LATS, HEIGHTS, &
     &                     AXISOFF, AXISTYP, BARO_CALS, BARO_HEIGHTS, APP, &
     &                     MTT_SEAS_DRY, MTT_SEAS_WET, IFA_SEAS_DRY, &
     &                     IFA_SEAS_WET, AXDIF )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!
! 1.  CFACALC PROGRAM SPECIFICATION
!
! 1.1 Calculate the CFA WET and DRY calibrations for an observation.
!     Calculate 8 CFA values for the observation: A CFA WET and DRY
!     value for DT and RT, for each of the two stations in the
!     observation.  These values must be calculated for databases which
!     do not have them.
!
! 1.2 REFERENCES:
!
! 2.  CFACALC INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
      INCLUDE 'oborg.i'
      INCLUDE 'bindisp.i'
      INCLUDE 'flyby.i'
!
! 2.2 INPUT Variables:
!
      REAL*8 APP(2,2)
      INTEGER*2 STAR,SITE(2),ISITN(4,*)
      INTEGER*2 AXISTYP(MAX_ARC_STA),ID_JJ(2)
      REAL*8 S_ELEV(2),S_AZ(2),S_ATMPR(2),S_RELHU(2),S_TEMPC(2)
      REAL*8 AXISOFF(MAX_ARC_STA)
      REAL*8 PI,VLIGHT
      REAL*8 LATS(MAX_ARC_STA),HEIGHTS(MAX_ARC_STA)
      REAL*8 BARO_CALS(MAX_ARC_STA),BARO_HEIGHTS(MAX_ARC_STA)
      REAL*8 VSTARC(2,*), AXDIF(*)
      LOGICAL*2 FLYBY_WARNING, MTT_SEAS_DRY(2), MTT_SEAS_WET(2)
      LOGICAL*2 IFA_SEAS_DRY(2), IFA_SEAS_WET(2)
!
! S_ATMPR - Site Atmospheric pressure, millibars
! AXISOFF - Axis offset for each station
! AXISTYP - Axis type for each station
! S_AZ -  Site Azimuth
! BARO_CALS - Barometer calibration for each station
! BARO_HEIGHTS - Height of barometer for each station
! FLYBY_WARNING - If .TRUE., then if some sites or sources are not
!                 in flyby files warning is issued.
! S_ELEV - Site Elevation
! HEIGHTS - Height of each station
! ID_JJ - Set to 1 if we are to correct the dry value for antenna geometry
!         for the
!         given station (ie, the dry value output will be cfajjdry)
!         Set to 0 if we are to output a non-corrected dry value (cfakbdry)
!         Set to 2 to use MTTDRY
!         Set to 3 to use IFADRY
! SITE - Site numbers of the two stations in this observation
! ISITN - Array of site names
! STAR - Source number for this observation
! LATS - Latitude of each station
! PI - 3.14159.....
! S_RELHU - Site Relative humidity (0 to 1)
! S_TEMPC - Site Temperature (Celsius)
! VLIGHT - Velocity of light, m/s
! VSTARC - Array of source coordinates (RA and DEC, in radians)
! MTT_SEAS_DRY - if .TRUE. use seasonal temperature for MTTDRY
!            if .FALSE. use observed surface temperature for MTTDRY
! MTT_SEAS_WET - if .TRUE. use seasonal temperature for MTTWET
!            if .FALSE. use observed surface temperature for MTTWET
!
! 2.3 OUTPUT
!
      REAL*8 CFADRY(2,2),CFAWET(2,2)
!
! CFADRY - CFA dry calibrations
! CFAWET - CFA wet calibrations
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INCLUDE 'hold_atm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES:
!       CALLED SUBROUTINES: get_time,get_prog_name,elrat,use_spool,
!                           sastd,sastw,cfa22,antcor
!
! 3.  LOCAL VARIABLES
!
!     integer*2 iobs_ct
      INTEGER*2 INAME(4),PRONAM(3), TRIMLEN
      CHARACTER  PRONAM_CH*6
      EQUIVALENCE ( PRONAM, PRONAM_CH )
      INTEGER*2 AXTP
      REAL*8 AXOF, HSCALE, BCALERR, BHEIGHT
      REAL*8 BETA,CFAMAP,CFARAT,DEC,ELDOT
      REAL*8 LAT,SITHIT,TROPHT,ZD,ZDDOT,ZW,ZWDOT
      REAL*8 AZ6,ELEV6,ATMPR6,RELHU6,TEMPC6,TIME
      LOGICAL*2 MET_CHANGE,kbit
      INTEGER*2 I,J,II,IS
      REAL*8 ATMPR_STAND,RELHU_STAND,TEMPC_STAND
      character*79 bufstr
      REAL*8  jlday                   ! julian day
      REAL*8  HMF(2)                  ! Niell mapping function and rate
      REAL*8       EPS_SCAN 
      PARAMETER  ( EPS_SCAN = 0.099 ) ! min difference between scans

! === The tropospheric height and temperature lapse rate are set to
! === constants.  (Tropospheric height is in km.)
!
      DATA TROPHT  /10.0D0/
      DATA BETA    /-5.6D-3/
!
      DATA ATMPR_STAND /1000.0D0/
      DATA RELHU_STAND /.5D0/
      DATA TEMPC_STAND /20.0D0/
!     data iobs_ct /0/
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   KDB  870706  Created
!   KDB  880610  Check for reasonable met sensor data
!   KDB  880615  Replace READLS (which looked up geodetic latitudes
!                and elevations above the geoid in a table) with
!                code in NCORT to calculate these values.
!   JWR  890202  Logic added to remember the last new values for up
!                to MAX_ARC_STA sites and reuse them if needed.
!   JWR  890206  Improved logic for handling error conditions
!   JWR  890207  Improved logic for handling pathological conditions
!                (more information and pausing only in PROC)
!   JRR  890213  Added corrections to dry zenith path delay to
!                account for geometrical effects within antennas`
!                due to axis offset
!   JRR  890221  Added corrections to atmosphere pressure to account
!                for calibration errors in barometers and physical
!                offset of heights (this is done for CORFIL setting
!                CFAJJDRY but BOT for CFAKBDRY)
!   AEE  911127  Added code for MTT and IFADIS mapping functions.
!   AEE  920114  Added LAT and SITHIT to call to SASTW (see sastw.f)
!   AEE  920212  Added CHAO DRY stuff.
!   :94.06.20: jwr Added small common block to pass info up to routine
!                'secnd' in mdlpl.
!   :97.08.14: pet Added test: are some "old" or "new" meteoparametr has
!              default values and therefore has not been measured. Making
!              correction for such a case.
!   :98.12.24: pet Added setting flag SASSTOM_USED
!   pet  06-APR-99 Added printing warnings in batch mode also
!   pet  19-APR-99 Suppressed printing meteo warning more than once for the
!                  specific station
!   pet  21-APR-99 Changed logic: comparison with the previous values is done
!                  only if CAL_INIT(is) is TRUE for this station. CAL_INIT
!                  is set in 0 by FLYBY_MAP_INIT. Improved comments
!   pet  2001.04.16 Fixed the bug: the logic of calcal assumes that variable
!                   ap_old is global and saved, but it was declared as local
!                   variable.
!   pet  2005.05.26 Relaxed the mininal time difference between two consequtive
!                   scans. Set it to 0.1 sec.
!
! 5.  CFACALC PROGRAM STRUCTURE
!
!     Calculate various values needed to calculate the Saastamoinen dry and
!     wet delays and rates, and the CFA22 mapping function.  These values
!     in turn will be used to calculate the CFA DRY and WET values,
!     which is what we're after.
!
!     Note:  Elevation  (from elevation/azimuth coordinate system),
!            atmospheric pressure value and rate, relative humidity
!            value and rate and temperature valueand rate come straight
!            from OBSFIL.
!
!     Get the time of this observation.  This subroutine used to avoid
!     in inelegance of having to put OBORG in this routine.  In a perfect
!     world the time would have been passed in from 3 routine up.
!
!
! --- Default values of meteoparameters
!
      REAL*8       ATMPR_DEF, RELHU_DEF, TEMPC_DEF
      PARAMETER  ( ATMPR_DEF = 1000.0D0  )
      PARAMETER  ( RELHU_DEF =    0.0D0  )
      PARAMETER  ( TEMPC_DEF =    0.0D0  )
!
      IF ( MAX_FLYBY_EOP_VALUES .NE. MAX_F_SAVED ) THEN
           WRITE ( 6, * ) 'CALCALC: MAX_FLYBY_EOP_VALUES = ', &
     &     MAX_FLYBY_EOP_VALUES
           WRITE ( 6, * ) 'CALCALC: MAX_F_SAVED = ',MAX_F_SAVED
           CALL FERR ( INT2(552), &
     &         'CALCALC: Trap of internal control: constant '// &
     &         'definition of MAX_FLYBY_EOP_VALUES in ../cutil/caclalc.f '// &
     &         'does not coincide with definition in ../include/glnc4.i', INT2(0), &
     &          INT2(0) )
           STOP 'Fatal error'
      END IF
      CALL GET_TIME(TIME )
      SASSTOM_USED = .FALSE.
!
      DO I = 1,2   !Loop over stations in the observation
         IS = SITE(I)
         IF ( IS .GT. MAX_ARC_STA ) THEN
              CALL FERR ( INT2(148), 'CALCALC station list is exceded.', &
     &             INT2(0), INT2(0) )
              STOP
         ENDIF
!
! ------ See if it's the old value for this site.
!
         IF ( CAL_INIT(IS)                                       .AND. &
     &        ABS(TIME-TIME_CAL_OLD(IS))*86400.0D0 .LT. EPS_SCAN .AND. &
     &        STAR .EQ. ISTAR_CAL_OLD(IS)                        ) THEN ! Found
!
! ---------- Test: do we have case of default atmosphere pressure?
!
             IF ( ABS( S_ATMPR(I)    - ATMPR_DEF ) .LT. 0.01 .AND. &
     &            ABS( ATMPR_OLD(IS) - ATMPR_DEF ) .GT. 0.01       ) THEN
                  S_ATMPR(I) = ATMPR_OLD(IS)
               ELSE IF ( ABS( S_ATMPR(I)    - ATMPR_DEF ) .GT. 0.01 .AND. &
     &                   ABS( ATMPR_OLD(IS) - ATMPR_DEF ) .LT. 0.01       ) THEN
                  ATMPR_OLD(IS) = S_ATMPR(I)
             END IF
!
! ---------- Test: do we have case of default relative humidity?
!
             IF ( ABS( S_RELHU(I)    - RELHU_DEF ) .LT. 0.01 .AND. &
     &            ABS( RELHU_OLD(IS) - RELHU_DEF ) .GT. 0.01       ) THEN
                  S_RELHU(I) = RELHU_OLD(IS)
               ELSE IF ( ABS( S_RELHU(I)    - RELHU_DEF ) .GT. 0.01 .AND. &
     &                   ABS( RELHU_OLD(IS) - RELHU_DEF ) .LT. 0.01       ) THEN
                  RELHU_OLD(IS) = S_RELHU(I)
             END IF
!
! ---------- Test: do we have case of default temperature?
!
             IF ( ABS( S_TEMPC(I)    - TEMPC_DEF ) .LT. 0.01 .AND. &
     &            ABS( TEMPC_OLD(IS) - TEMPC_DEF ) .GT. 0.01       ) THEN
                  S_TEMPC(I) = TEMPC_OLD(IS)
               ELSE IF ( ABS( S_TEMPC(I)    - TEMPC_DEF ) .GT. 0.01 .AND. &
     &                   ABS( TEMPC_OLD(IS) - TEMPC_DEF ) .LT. 0.01       ) THEN
                  TEMPC_OLD(IS) = S_TEMPC(I)
             END IF
!
! ---------- Double check that the met values, source, az, and el have
! ---------- not changed.
!
             IF ( STAR .EQ. ISTAR_CAL_OLD(IS)                   .AND. &
     &            ( ABS( S_ELEV(I) -  ELEV_OLD(IS)) .GT. 0.001  .OR.  &
     &              ABS(   S_AZ(I) -    AZ_OLD(IS)) .GT. 0.001  .OR.  &
     &              ABS(S_ATMPR(I) - ATMPR_OLD(IS)) .GT. 0.01   .OR.  &  
     &              ABS(S_RELHU(I) - RELHU_OLD(IS)) .GT. 0.01   .OR.  &
     &              ABS(S_TEMPC(I) - TEMPC_OLD(IS)) .GT. 0.01         &
     &            )                                                   &
                ) THEN
!
! --------------- Something wrong with here.
!
                  IF ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6)) ) THEN
                       CALL NL_MN()
                       CALL ADDSTR_F ( "Trying to reuse old CFA values "// &
     &                                 "but something does not" )
                       CALL NL_MN()
                       CALL ADDSTR_F ( "match that should.  See subroutine "// &
     &                                 "CALCALC. " )
                       CALL NL_MN()
                       WRITE ( BUFSTR, '("Problem site is ", 4A2, &
     &                         " problem source is ",I4)') &
     &                         (ISITN(J,IS),J=1,4), STAR
                       CALL ADDSTR_F ( BUFSTR )
                       CALL NL_MN()
                       WRITE ( BUFSTR, '("The Julian date of the obs is ", &
     &                         F20.8)' ) TIME
                       CALL ADDSTR_F ( BUFSTR )
                       CALL NL_MN()
                       CALL ADDSTR_F ( "Value:           New              "// &
     &                                 "Old     " )
                       CALL NL_MN()
                       WRITE ( BUFSTR, '("Elev. ",2F15.5," radians  ")') &
     &                         S_ELEV(I), ELEV_OLD(IS)
                       CALL ADDSTR_F ( BUFSTR )
                       CALL NL_MN()
                       WRITE ( BUFSTR, '("Az.   ",2F15.5," radians  ")') &
     &                         S_AZ(I), AZ_OLD(IS)
                       CALL ADDSTR_F ( BUFSTR )
                       CALL NL_MN()
                       WRITE ( BUFSTR, '("Pres. ",2F15.5," millibars")') &
     &                         S_ATMPR(I), ATMPR_OLD(IS)
!
                       CALL ADDSTR_F ( BUFSTR )
                       CALL NL_MN()
                       WRITE ( BUFSTR, '("Hum.  ",2F15.5,"          ")') &
     &                         S_RELHU(I), RELHU_OLD(IS)
                       CALL ADDSTR_F ( BUFSTR )
                       CALL NL_MN()
                       WRITE ( BUFSTR, &
     &                         '("Temp. ",2F15.5," Centigrade")')S_TEMPC(I),TEMPC_OLD(IS)
                       CALL ADDSTR_F ( BUFSTR )
                       CALL NL_MN()
                  ENDIF
!
                  IF ( KSPOOL ) THEN
                       WRITE(23,'(/, &
     &                  " Trying to reuse old CFA values but something does ",/, &
     &                  " not match that should.  See subroutine CALCALC.")')
                       WRITE(23,'(" Problem site is "4A2, &
     &                            " problem star is ",I4)') &
     &                           (ISITN(J,IS),J=1,4), STAR
                       WRITE(23, '(" The old Julian date of the obs is ",F24.12)' ) TIME_CAL_OLD(IS)
                       WRITE(23, '(" The new Julian date of the obs is ",F24.12)' ) TIME 
                       WRITE(23,'(" Value:           New              Old ",/, &
     &                  " Elev. ",2F15.5," radians  ",/, &
     &                  " Az.   ",2F15.5," radians  ",/, &
     &                  " Pres. ",2F15.5," millibars",/, &
     &                  " Hum.  ",2F15.5,"          ",/, &
     &                  " Temp. ",2F15.5," Centigrade")') &
     &                  S_ELEV(I),  ELEV_OLD(IS), &
     &                  S_AZ(I),    AZ_OLD(IS), &
     &                  S_ATMPR(I), ATMPR_OLD(IS), &
     &                  S_RELHU(I), RELHU_OLD(IS), &
     &                  S_TEMPC(I), TEMPC_OLD(IS)
                  ENDIF
!
! --------------- Get program name
!
                  CALL GET_PROG_NAM ( PRONAM )
                  IF ( PRONAM_CH(1:2) .EQ. 'PR'  ) THEN
                       CALL FERR ( INT2(2983), 'CALCALC:  Pathological '// &
     &                     'error detected in PROC', INT2(0), INT2(0) )
                       STOP 'CALCCALC: Abnormal termination'
                  ENDIF
                ELSE
!
! --------------- Everything checks out to reuse old data.
!
                  DO II=1,2
                     CFADRY(I,II) = CFADRY_OLD(IS,II)
                     CFAWET(I,II) = CFAWET_OLD(IS,II)
                     APP(I,II) = AP_OLD(IS,II)
                  ENDDO
             ENDIF ! second check
          ELSE ! old value not the right value.
!
! ---------- Pull out the station name, its geodetic latitude, height
! ---------- above the ellipsoid, axis offset (meter), and axis mount
! ---------- type.  But why? I don't know!
!
            DO J = 1,4
               INAME(J) = ISITN(J,IS)  ! get name of station i
            END DO
            LAT = LATS(IS)
            SITHIT = HEIGHTS(IS)
            AXOF = AXISOFF(IS)
            AXTP = AXISTYP(IS)
            BCALERR = BARO_CALS(IS)
            BHEIGHT = BARO_HEIGHTS(IS)
!
! --------- Calculate the elevation rate, but first compute
! --------- the declination of this observation's source.
!
            DEC = VSTARC(2,STAR)
            CALL ELRAT ( S_AZ(I), S_ELEV(I), LAT, PI, ELDOT )
!
! --------- Make sure the met sensor data is reasonable.
!
            ATMPR6 = S_ATMPR(I)
            RELHU6 = S_RELHU(I)
            TEMPC6 = S_TEMPC(I)
            MET_CHANGE = .FALSE.
!
! --------- Check meteo data and fix them if necessary
!
            CALL METFIX ( ATMPR6, RELHU6, TEMPC6, MET_CHANGE, SITHIT )
!
            IF ( MET_CHANGE            .AND.PRE_IBATCH .EQ. 0     .AND. &
     &           FLYBY_WARNING         .AND..NOT. METEO_WARN(IS)  .AND. &
     &           KBIT(PRE_IP(2), INT2(6))           ) THEN
!
                 CALL NL_MN()
                 CALL NL_MN()
                 CALL NL_MN()
                 WRITE ( BUFSTR, 1000 ) INAME
 1000            FORMAT(" Warning (CALCALC): Met sensor data has been changed", &
     &                  " for ",4A2)
                 WRITE ( 23, '(A)' ) BUFSTR(1:TRIMLEN(BUFSTR))
                 CALL ADDSTR_F ( BUFSTR )
                 CALL NL_MN()
!
                 WRITE ( BUFSTR, '(20X,"Temperature",11X,"Pressure",7X, &
     &                                 "Relative humidity")' )
                 WRITE ( 23, '(A)' ) BUFSTR(1:TRIMLEN(BUFSTR))
                 CALL ADDSTR_F ( BUFSTR )
                 CALL NL_MN()
                 CALL NL_MN()
!
                 WRITE ( BUFSTR, &
     &                   '("Original values",3(2X,F18.4)  ) ')S_TEMPC(I),S_ATMPR(I),S_RELHU(I)
                 WRITE ( 23, '(A)' ) BUFSTR(1:TRIMLEN(BUFSTR))
                 CALL ADDSTR_F(BUFSTR )
                 CALL NL_MN()
!
                 WRITE ( BUFSTR, &
     &                   '("     New values",3(2X,F18.4))')TEMPC6,ATMPR6,RELHU6
                 WRITE ( 23, '(A)' ) BUFSTR(1:TRIMLEN(BUFSTR))
                 CALL ADDSTR_F ( BUFSTR )
                 CALL NL_MN()
                 CALL NL_MN()
!
                 METEO_WARN(IS) = .TRUE. ! set flag: warning for station IS is
!                                        ! already issued
               ELSE IF ( MET_CHANGE           .AND. &
     &                   FLYBY_WARNING        .AND. &
     &                   .NOT. METEO_WARN(IS)       ) THEN
!
! -------------- Print warnings in batch mode
!
                 WRITE ( BUFSTR, 1000 ) INAME
                 WRITE ( 23, '(A)' ) BUFSTR(1:TRIMLEN(BUFSTR))
!
                 WRITE ( BUFSTR, '(20X,"Temperature",11X,"Pressure",7X, &
     &                                 "Relative humidity")')
                 WRITE ( 23, '(A)' ) BUFSTR(1:TRIMLEN(BUFSTR))
!
                 WRITE ( BUFSTR, &
     &                   '("Original values",3(2X,F18.4)  ) ')S_TEMPC(I),S_ATMPR(I),S_RELHU(I)
                 WRITE ( 23, '(A)' ) BUFSTR(1:TRIMLEN(BUFSTR))
!
                 WRITE ( BUFSTR, &
     &                   '("     New values",3(2X,F18.4))')TEMPC6,ATMPR6,RELHU6
                 WRITE ( 23, '(A)' ) BUFSTR(1:TRIMLEN(BUFSTR))
!
                 METEO_WARN(IS) = .TRUE. ! set flag: warning for station IS is
!                                        ! already issued
            END IF
!
! --------- We should have reasonable met data by this point.
!
            HSCALE = 8.567D03 * (TEMPC6 + 273.15D0) / 292.D0
!
            IF ( ID_JJ(I) .EQ. 1) THEN
                 ATMPR6 = (ATMPR6 - BCALERR) * DEXP(-BHEIGHT/HSCALE)
            END IF
!
! --------- Calculate Sasstomione dry and wet zenith delays and rates.
!
            CALL SASTD ( ATMPR6, 0.D0, LAT, SITHIT, ZD, ZDDOT )
            CALL SASTW ( RELHU6, TEMPC6, 0.D0, 0.D0, ZW, ZWDOT )
!
! --------- Capture the Sass. zenith delays and met data for high level
! --------- routines in mdlpl.
! --------- Convert the zenith correction from meters to ps.
!
            PRESSURE           (I) = ATMPR6
            TEMPERATURE        (I) = TEMPC6
            HUMIDITY           (I) = RELHU6*100.D0
            SASSTOM_DRY_ZENITH (I) = (ZD/VLIGHT)*1.D12
            SASSTOM_WET_ZENITH (I) = (ZW/VLIGHT)*1.D12
            SASSTOM_USED       (I) = .TRUE.
!
! --------- Calculate CFA2.2 mapping function in the direction of the source.
!
            AZ6 = S_AZ(I)
            ELEV6 = S_ELEV(I)
            IF ( ID_JJ(I) .EQ. 2 ) THEN ! MTTDRY
                 CALL MTTDRY ( ELEV6, TEMPC6, ELDOT, LAT, SITHIT, CFAMAP, &
     &                         CFARAT, MTT_SEAS_DRY(I) )
               ELSE IF ( ID_JJ(I) .EQ. 3 ) THEN ! IFADRY
                 CALL IFAD_DRY ( ATMPR6, TEMPC6, RELHU6, ELEV6, ELDOT, CFAMAP, &
     &                           CFARAT, IFA_SEAS_DRY(I) )
               ELSE IF (ID_JJ(I) .EQ. 4) THEN
!
! -------------- Call Niell mapping function:
!
                 JLDAY = FJD + FRACT
                 CALL NHMF2 ( JLDAY, LAT, SITHIT, ELEV6, HMF )
                 CFAMAP = HMF(1)
                 CFARAT = HMF(2)*ELDOT
               ELSE IF ( ID_JJ(I) .EQ. 0 ) THEN
                 CALL CFA22 ( ELEV6, ATMPR6, RELHU6, TEMPC6, ELDOT, 0.D0, &
     &                        0.D0, 0.D0, LAT, SITHIT, TROPHT, BETA, CFAMAP, &
     &                        CFARAT )
            END IF
!
            CFADRY(I,1) = (ZD*CFAMAP)/VLIGHT
            CFADRY(I,2) = (ZD*CFARAT + ZDDOT*CFAMAP) / VLIGHT
            CFAWET(I,1) = (ZW*CFAMAP)/VLIGHT
            CFAWET(I,2) = (ZW*CFARAT + ZWDOT*CFAMAP) / VLIGHT
            APP(I,1) = CFAMAP
            APP(I,2) = CFARAT
!
! --------- Save what we have just computed
!
            CAL_INIT(IS)      = .TRUE.
            TIME_CAL_OLD(IS)  = TIME
            ISTAR_CAL_OLD(IS) = STAR
!
            ELEV_OLD(IS)      = S_ELEV(I)
            AZ_OLD(IS)        = S_AZ(I)
            ATMPR_OLD(IS)     = S_ATMPR(I)
            RELHU_OLD(IS)     = S_RELHU(I)
            TEMPC_OLD(IS)     = S_TEMPC(I)
!
            DO II = 1,2
               CFAWET_OLD(IS,II) = CFAWET(I,II)
               CFADRY_OLD(IS,II) = CFADRY(I,II)
               AP_OLD(IS,II)     = APP(I,II)
            ENDDO
        ENDIF
      ENDDO ! loop over stations in the observation
!
! --- Change sign for CORFIL compatibility - CORFIL has -1 1 set up.
!
      DO J = 1,2
         CFADRY(1,J) = -CFADRY(1,J)
         CFAWET(1,J) = -CFAWET(1,J)
         APP(1,J)    = -APP(1,J)
      END DO
!
      RETURN
      END  !#!  CALCALC  #!#
