      SUBROUTINE SUR_ASTROMET ( SUR, VTD, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SUR_ASTROMET
! *                                                                      *
! *  ### 12-JAN-2007  SUR_ASTROMET  v1.4 (c) L. Petrov  06-JUL-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      INCLUDE   'vtd.i'
      INCLUDE   'getpar.i'
      TYPE     ( SUR__TYPE ) :: SUR
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  IVRB, IUER
      CHARACTER  STR*128, STA_LST*8, STR_RIS*30, STR_SET*30
      REAL*8     AZ_ARR(SUR__M_SPL), EL_ARR(SUR__M_SPL), HA_ARR(SUR__M_SPL), &
     &           AZ, EL, HA, DUR_SES, DUR_OBS, TIM, TAI_OBS, TIM_LAST
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, J14, J15, &
     &           ITURN, NUM_EPC(SUR__M_SOU,SUR__NTYP), I_TYP, MJD_OBS, &
     &           IDAY, NOBS_MIN, NOBS_NORM, NOBS, L_SOU, K_STA, &
     &           IND_PT, IND_PA, IND_EF, NSOU_UP, NTHR, IND_THR, IER
      REAL*8     ARR_TMP(SUR__M_SPL), D1, DN, TIM_RIS, TIM_SET, TIM_UP, &
     &           LST_PT_RIS, LST_PT_SET, LST_PST_RIS_REL, LST_PST_SET_REL, S_ANG, TW
      LOGICAL*1  FL_STA(SUR__M_STA), FL_STI_FAIL, FL_UP, FL_VIS_ONLY, FL_ERROR, FL_GAP
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
      LOGICAL*4, EXTERNAL :: SUR_CHECK_VIS
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      REAL*8,    EXTERNAL :: GET_LST 
      REAL*8,    EXTERNAL :: WALL_TIMER, CPU_TIMER
      INTEGER*4, EXTERNAL :: OMP_GET_MAX_THREADS, OMP_GET_NUM_THREADS, OMP_GET_THREAD_NUM
#ifdef GNU
      LOGICAL*4, INTRINSIC :: ISATTY
      INTRINSIC  FLUSH
#else
#define ISATTY        ISATTY_
      LOGICAL*4, EXTERNAL :: ISATTY
#endif
!
      CALL GETENVAR ( 'SUR_SKED_VIS_ONLY', STR )
      IF ( STR(1:3) == 'YES' .OR. STR(1:3) == 'yes' ) THEN
           FL_VIS_ONLY = .TRUE.
         ELSE
           FL_VIS_ONLY = .FALSE.
      END IF
!
      CALL NOUT_I4 ( SUR__M_SOU*SUR__NTYP, NUM_EPC )
      CALL NOUT_I4 ( SUR__M_SOU,           SUR%NOBS_SRC )
      CALL NOUT_R8 ( SUR__M_SOU,           SUR%GAPS_SRC )
      CALL NOUT_I4 ( SUR__M_SOU*SUR__NTYP, SUR%N_VIS   )
      CALL NOUT_I4 ( SUR__M_VIS*2*SUR__M_SOU*SUR__NTYP, SUR%MJD_VIS )
      CALL NOUT_R8 ( SUR__M_VIS*2*SUR__M_SOU*SUR__NTYP, SUR%TAI_VIS )
!
      DUR_SES = (SUR%MJD_STOP - SUR%MJD_START)*86400.0D0 + &
     &          (SUR%TAI_STOP - SUR%TAI_START)
!
      SUR%STATUS_SPL(SUR__TYP_TAG) = SUR__UND
      SUR%STATUS_SPL(SUR__TYP_SEC) = SUR__UND
      SUR%STATUS_SPL(SUR__TYP_CAL) = SUR__UND
      SUR%STATUS_SPL(SUR__TYP_POC) = SUR__UND
      SUR%STATUS_SPL(SUR__TYP_PLA) = SUR__UND
      SUR%SLEW_DUR = 0.0D0
!
      ALLOCATE ( SUR%VAL_SOU(SUR__M_SPL,SUR%L_STA,SUR%L_SOU,3), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*SUR__M_SPL*SUR%L_SOU*SUR%L_STA*3)
           CALL ERR_LOG ( 1671, IUER, 'SUR_ASTROMET', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN
      END IF
!
      ALLOCATE ( SUR%SPL_SOU(SUR__M_SPL,SUR%L_STA,SUR%L_SOU,3), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*SUR__M_SPL*SUR%L_SOU*SUR%L_STA*3)
           CALL ERR_LOG ( 1672, IUER, 'SUR_ASTROMET', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN
      END IF
      SUR%STATUS_SPL(SUR__TYP_TAG) = SUR__ALC
!
      IF ( SUR%L_SO2 > 0 ) THEN
           ALLOCATE ( SUR%VAL_SO2(SUR__M_SPL,SUR%L_STA,SUR%L_SO2,3), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*SUR__M_SPL*SUR%L_SO2*SUR%L_STA*3)
                CALL ERR_LOG ( 1673, IUER, 'SUR_ASTROMET', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
                RETURN
           END IF
           ALLOCATE ( SUR%SPL_SO2(SUR__M_SPL,SUR%L_STA,SUR%L_SO2,3), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*SUR__M_SPL*SUR%L_SO2*SUR%L_STA*3)
                CALL ERR_LOG ( 1674, IUER, 'SUR_ASTROMET', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
                RETURN
           END IF
           SUR%STATUS_SPL(SUR__TYP_SEC) = SUR__ALC
      END IF
!
      IF ( SUR%L_CAL > 0 ) THEN
           ALLOCATE ( SUR%VAL_CAL(SUR__M_SPL,SUR%L_STA,SUR%L_CAL,3), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*SUR__M_SPL*SUR%L_CAL*SUR%L_STA*3 )
                CALL ERR_LOG ( 1675, IUER, 'SUR_ASTROMET', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
                RETURN
           END IF
           ALLOCATE ( SUR%SPL_CAL(SUR__M_SPL,SUR%L_STA,SUR%L_CAL,3), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*SUR__M_SPL*SUR%L_CAL*SUR%L_STA*3 )
                CALL ERR_LOG ( 1676, IUER, 'SUR_ASTROMET', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
                RETURN
           END IF
           SUR%STATUS_SPL(SUR__TYP_CAL) = SUR__ALC
      END IF
!
      IF ( SUR%L_SOP > 0 ) THEN
           ALLOCATE ( SUR%VAL_SOP(SUR__M_SPL,SUR%L_STA,SUR%L_SOP,3), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*SUR__M_SPL*SUR%L_SOP*SUR%L_STA*3)
                CALL ERR_LOG ( 1677, IUER, 'SUR_ASTROMET', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
                RETURN
           END IF
           ALLOCATE ( SUR%SPL_SOP(SUR__M_SPL,SUR%L_STA,SUR%L_SOP,3), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*SUR__M_SPL*SUR%L_SOP*SUR%L_STA*3)
                CALL ERR_LOG ( 1678, IUER, 'SUR_ASTROMET', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
                RETURN
           END IF
           SUR%STATUS_SPL(SUR__TYP_POC) = SUR__ALC
      END IF
!
      IF ( SUR%L_PLA > 0 ) THEN
           ALLOCATE ( SUR%VAL_PLA(SUR__M_SPL,SUR%L_STA,SUR%L_SOP,3), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*SUR__M_SPL*SUR%L_SOP*SUR%L_STA*3)
                CALL ERR_LOG ( 1679, IUER, 'SUR_ASTROMET', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
                RETURN
           END IF
           ALLOCATE ( SUR%SPL_PLA(SUR__M_SPL,SUR%L_STA,SUR%L_SOP,3), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*SUR__M_SPL*SUR%L_SOP*SUR%L_STA*3)
                CALL ERR_LOG ( 1680, IUER, 'SUR_ASTROMET', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
                RETURN
           END IF
           SUR%STATUS_SPL(SUR__TYP_PLA) = SUR__ALC
      END IF
      NTHR = OMP_GET_MAX_THREADS()
      IF ( IVRB .GE. 3 ) THEN
           WRITE ( 6, * ) ' Compute interpolating spline for az, el, ha  NTHR= ', INT2(NTHR)
           CALL FLUSH ( 6 )
      END IF
      TW = WALL_TIMER ( %VAL(0) ) ! -- resets to zero used WALL time.   
      DO 410 J1=1,SUR__NTYP
         IF ( J1 == SUR__TYP_TAG ) THEN
              I_TYP = SUR__TYP_TAG
              L_SOU = SUR%L_SOU
            ELSE IF ( J1 == SUR__TYP_SEC ) THEN
              I_TYP = SUR__TYP_SEC
              L_SOU = SUR%L_SO2
            ELSE IF ( J1 == SUR__TYP_CAL ) THEN
              I_TYP = SUR__TYP_CAL
              L_SOU = SUR%L_CAL
            ELSE IF ( J1 == SUR__TYP_POC ) THEN
              I_TYP = SUR__TYP_POC
              L_SOU = SUR%L_SOP
            ELSE IF ( J1 == SUR__TYP_PLA ) THEN
              I_TYP = SUR__TYP_PLA
              L_SOU = SUR%L_PLA
         END IF
         IF ( L_SOU == 0 ) GOTO 410
         DO 540 J4=1,SUR__M_SPL
             SUR%TIM_SPL(J4) = (J4-1)*DUR_SES/(SUR__M_SPL-1)
 540     CONTINUE 
         FL_ERROR = .FALSE.
!
!$OMP    PARALLEL DO IF ( NTHR > 1 ), DEFAULT ( NONE), &
!$OMP&   PRIVATE ( J2, J3, J4, IER, MJD_OBS, TAI_OBS, IDAY, &
!$OMP&             AZ_ARR, EL_ARR, HA_ARR, D1, DN, ARR_TMP, &
!$OMP&             ITURN ), &
!$OMP&   SHARED  ( SUR, VTD, L_SOU, FL_ERROR, SUR__TYP_STR, I_TYP, IUER )
         DO 420 J2=1,L_SOU
            IF ( FL_ERROR ) GOTO 420
            DO 430 J3=1,SUR%L_STA
               DO 440 J4=1,SUR__M_SPL
                  IF ( FL_ERROR ) GOTO 420
                  MJD_OBS = SUR%MJD_START
                  TAI_OBS = SUR%TAI_START + SUR%TIM_SPL(J4)
                  IF ( TAI_OBS > 86400.0D0 ) THEN
                       IDAY = TAI_OBS/86400.0D0
                       TAI_OBS = TAI_OBS - IDAY*86400.D0
                       MJD_OBS = MJD_OBS + IDAY
                  END IF
!
                  CALL ERR_PASS ( IUER, IER )
                  CALL SUR_AZEL ( SUR, VTD, I_TYP, MJD_OBS, TAI_OBS, J3, J2, &
     &                            AZ_ARR(J4), EL_ARR(J4), HA_ARR(J4), IER )
                  IF ( IER .NE. 0 ) THEN
                       CALL ERR_LOG ( 1681, IUER, 'SUR_ASTROMET', &
     &                     'Error in computing azimuth and elevation' )
!$OMP                  CRITICAL (ERROR)
                       FL_ERROR = .TRUE.
!$OMP                  END CRITICAL (ERROR)
                       GOTO 420
                  END IF
                  IF ( J4 > 1 ) THEN
                       ITURN = IDNINT ( (AZ_ARR(J4) - AZ_ARR(J4-1))/PI2 )
                       AZ_ARR(J4) = AZ_ARR(J4) - PI2*ITURN
                       ITURN = IDNINT ( (HA_ARR(J4) - HA_ARR(J4-1))/PI2 )
                       HA_ARR(J4) = HA_ARR(J4) - PI2*ITURN
                  END IF
 440          CONTINUE
!
! ----------- Copy arrays to the final destiation and compute spline
! ----------- coefficients for azimuth
!
              CALL ERR_PASS ( IUER, IER )
              IF ( I_TYP == SUR__TYP_TAG ) THEN
                   CALL LIB$MOVC3 ( SUR__M_SPL*8, AZ_ARR, &
     &                              SUR%VAL_SOU(1,J3,J2,SUR__AZ) )
                   CALL LIB$MOVC3 ( SUR__M_SPL*8, EL_ARR, &
     &                              SUR%VAL_SOU(1,J3,J2,SUR__EL) )
                   CALL LIB$MOVC3 ( SUR__M_SPL*8, HA_ARR, &
     &                              SUR%VAL_SOU(1,J3,J2,SUR__HA) )
!
                   CALL MAKE_SPLINE ( 3, SUR__M_SPL, SUR%TIM_SPL, &
     &                       SUR%VAL_SOU(1,J3,J2,SUR__AZ), D1, DN, &
     &                       SUR%SPL_SOU(1,J3,J2,SUR__AZ), ARR_TMP, IER )
                 ELSE IF ( I_TYP == SUR__TYP_SEC ) THEN
                   CALL LIB$MOVC3 ( SUR__M_SPL*8, AZ_ARR, &
     &                              SUR%VAL_SO2(1,J3,J2,SUR__AZ) )
                   CALL LIB$MOVC3 ( SUR__M_SPL*8, EL_ARR, &
     &                              SUR%VAL_SO2(1,J3,J2,SUR__EL) )
                   CALL LIB$MOVC3 ( SUR__M_SPL*8, HA_ARR, &
     &                              SUR%VAL_SO2(1,J3,J2,SUR__HA) )
!
                   CALL MAKE_SPLINE ( 3, SUR__M_SPL, SUR%TIM_SPL, &
     &                       SUR%VAL_SO2(1,J3,J2,SUR__AZ), D1, DN, &
     &                       SUR%SPL_SO2(1,J3,J2,SUR__AZ), ARR_TMP, IER )
                 ELSE IF ( I_TYP == SUR__TYP_CAL ) THEN
                   CALL LIB$MOVC3 ( SUR__M_SPL*8, AZ_ARR, &
     &                              SUR%VAL_CAL(1,J3,J2,SUR__AZ) )
                   CALL LIB$MOVC3 ( SUR__M_SPL*8, EL_ARR, &
     &                              SUR%VAL_CAL(1,J3,J2,SUR__EL) )
                   CALL LIB$MOVC3 ( SUR__M_SPL*8, HA_ARR, &
     &                              SUR%VAL_CAL(1,J3,J2,SUR__HA) )
!
                   CALL MAKE_SPLINE ( 3, SUR__M_SPL, SUR%TIM_SPL, &
     &                       SUR%VAL_CAL(1,J3,J2,SUR__AZ), D1, DN, &
     &                       SUR%SPL_CAL(1,J3,J2,SUR__AZ), ARR_TMP, IER )
                 ELSE IF ( I_TYP == SUR__TYP_POC ) THEN
                   CALL LIB$MOVC3 ( SUR__M_SPL*8, AZ_ARR, &
     &                              SUR%VAL_SOP(1,J3,J2,SUR__AZ) )
                   CALL LIB$MOVC3 ( SUR__M_SPL*8, EL_ARR, &
     &                              SUR%VAL_SOP(1,J3,J2,SUR__EL) )
                   CALL LIB$MOVC3 ( SUR__M_SPL*8, HA_ARR, &
     &                              SUR%VAL_SOP(1,J3,J2,SUR__HA) )
!
                   CALL MAKE_SPLINE ( 3, SUR__M_SPL, SUR%TIM_SPL, &
     &                       SUR%VAL_SOP(1,J3,J2,SUR__AZ), D1, DN, &
     &                       SUR%SPL_SOP(1,J3,J2,SUR__AZ), ARR_TMP, IER )
                 ELSE IF ( I_TYP == SUR__TYP_PLA ) THEN
                   CALL LIB$MOVC3 ( SUR__M_SPL*8, AZ_ARR, &
     &                              SUR%VAL_PLA(1,J3,J2,SUR__AZ) )
                   CALL LIB$MOVC3 ( SUR__M_SPL*8, EL_ARR, &
     &                              SUR%VAL_PLA(1,J3,J2,SUR__EL) )
                   CALL LIB$MOVC3 ( SUR__M_SPL*8, HA_ARR, &
     &                              SUR%VAL_PLA(1,J3,J2,SUR__HA) )
!
                   CALL MAKE_SPLINE ( 3, SUR__M_SPL, SUR%TIM_SPL, &
     &                       SUR%VAL_PLA(1,J3,J2,SUR__AZ), D1, DN, &
     &                       SUR%SPL_PLA(1,J3,J2,SUR__AZ), ARR_TMP, IER )
              END IF
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1682, IUER, 'SUR_ASTROMET', 'Error in '// &
     &                 'computing spline coefficients for azimuth' )
!$OMP              CRITICAL (ERROR)
                   FL_ERROR = .TRUE.
!$OMP              END CRITICAL (ERROR)
                   GOTO 420
              END IF
!
! ----------- Compute spline coefficients for elevation
!
              CALL ERR_PASS ( IUER, IER )
              IF ( I_TYP == SUR__TYP_TAG ) THEN
                   CALL MAKE_SPLINE ( 3, SUR__M_SPL, SUR%TIM_SPL, &
     &                       SUR%VAL_SOU(1,J3,J2,SUR__EL), D1, DN, &
     &                       SUR%SPL_SOU(1,J3,J2,SUR__EL), ARR_TMP, IER )
                 ELSE IF ( I_TYP == SUR__TYP_SEC ) THEN
                   CALL MAKE_SPLINE ( 3, SUR__M_SPL, SUR%TIM_SPL, &
     &                       SUR%VAL_SO2(1,J3,J2,SUR__EL), D1, DN, &
     &                       SUR%SPL_SO2(1,J3,J2,SUR__EL), ARR_TMP, IER )
                 ELSE IF ( I_TYP == SUR__TYP_CAL ) THEN
                   CALL MAKE_SPLINE ( 3, SUR__M_SPL, SUR%TIM_SPL, &
     &                       SUR%VAL_CAL(1,J3,J2,SUR__EL), D1, DN, &
     &                       SUR%SPL_CAL(1,J3,J2,SUR__EL), ARR_TMP, IER )
                 ELSE IF ( I_TYP == SUR__TYP_POC ) THEN
                   CALL MAKE_SPLINE ( 3, SUR__M_SPL, SUR%TIM_SPL, &
     &                       SUR%VAL_SOP(1,J3,J2,SUR__EL), D1, DN, &
     &                       SUR%SPL_SOP(1,J3,J2,SUR__EL), ARR_TMP, IER )
                 ELSE IF ( I_TYP == SUR__TYP_PLA ) THEN
                   CALL MAKE_SPLINE ( 3, SUR__M_SPL, SUR%TIM_SPL, &
     &                       SUR%VAL_PLA(1,J3,J2,SUR__EL), D1, DN, &
     &                       SUR%SPL_PLA(1,J3,J2,SUR__EL), ARR_TMP, IER )
              END IF
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1683, IUER, 'SUR_ASTROMET', 'Error in '// &
     &                 'computing spline coefficients for elevation for '// &
     &                  SUR__TYP_STR(I_TYP)//' source type' )
!$OMP              CRITICAL (ERROR)
                   FL_ERROR = .TRUE.
!$OMP              END CRITICAL (ERROR)
                   GOTO 420
              END IF
!
! ----------- Compute spline coefficients for hour angle
!
              CALL ERR_PASS ( IUER, IER )
              IF ( I_TYP == SUR__TYP_TAG ) THEN
                   CALL MAKE_SPLINE ( 3, SUR__M_SPL, SUR%TIM_SPL, &
     &                       SUR%VAL_SOU(1,J3,J2,SUR__HA), D1, DN, &
     &                       SUR%SPL_SOU(1,J3,J2,SUR__HA), ARR_TMP, IER )
                 ELSE IF ( I_TYP == SUR__TYP_SEC ) THEN
                   CALL MAKE_SPLINE ( 3, SUR__M_SPL, SUR%TIM_SPL, &
     &                       SUR%VAL_SO2(1,J3,J2,SUR__HA), D1, DN, &
     &                       SUR%SPL_SO2(1,J3,J2,SUR__HA), ARR_TMP, IER )
                 ELSE IF ( I_TYP == SUR__TYP_CAL ) THEN
                   CALL MAKE_SPLINE ( 3, SUR__M_SPL, SUR%TIM_SPL, &
     &                       SUR%VAL_CAL(1,J3,J2,SUR__HA), D1, DN, &
     &                       SUR%SPL_CAL(1,J3,J2,SUR__HA), ARR_TMP, IER )
                 ELSE IF ( I_TYP == SUR__TYP_POC ) THEN
                   CALL MAKE_SPLINE ( 3, SUR__M_SPL, SUR%TIM_SPL, &
     &                       SUR%VAL_SOP(1,J3,J2,SUR__HA), D1, DN, &
     &                       SUR%SPL_SOP(1,J3,J2,SUR__HA), ARR_TMP, IER )
                 ELSE IF ( I_TYP == SUR__TYP_PLA ) THEN
                   CALL MAKE_SPLINE ( 3, SUR__M_SPL, SUR%TIM_SPL, &
     &                       SUR%VAL_PLA(1,J3,J2,SUR__HA), D1, DN, &
     &                       SUR%SPL_PLA(1,J3,J2,SUR__HA), ARR_TMP, IER )
              END IF
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1684, IUER, 'SUR_ASTROMET', 'Error in '// &
     &                 'computing spline coefficients for hour angles' )
!$OMP              CRITICAL (ERROR)
                   FL_ERROR = .TRUE.
!$OMP              END CRITICAL (ERROR)
                   GOTO 420
              END IF
 430       CONTINUE
 420     CONTINUE
!$OMP END PARALLEL DO
!
         IF ( FL_ERROR ) THEN
              RETURN 
         END IF
         SUR%STATUS_SPL(J1) = SUR__SPL
 410  CONTINUE
      TW = WALL_TIMER ( %VAL(2) )
      IF ( IVRB > 4 ) THEN
           WRITE ( 6, '("  Time elapsed: ", F9.2, " sec" )' ) TW
      END IF
!
! --- Now compute rise and set time
!
      IF ( IVRB .GE. 3 ) THEN
!!           WRITE ( 6, * ) ' Compute rise and set time  NTHR= ', INT2(NTHR)
           WRITE ( 6, * ) ' Compute rise and set time'
      END IF
      SUR%ELEV_MAX = -1.0D0
      TW = WALL_TIMER ( %VAL(0) ) ! reset to zero
      DO 450 J5=1,SUR__M_EPC
         TIM = (J5-1)*DUR_SES/(SUR__M_EPC-1)
         MJD_OBS = SUR%MJD_START
         TAI_OBS = SUR%TAI_START + TIM
         IF ( TAI_OBS > 86400.0D0 ) THEN
              IDAY = TAI_OBS/86400.0D0
              TAI_OBS = TAI_OBS - IDAY*86400.D0
              MJD_OBS = MJD_OBS + IDAY
         END IF
!
         IF ( ISATTY ( 6 ) ) THEN
              IF ( IVRB .GE. 2 .AND. ( MOD(J5,100) == 0 .OR. J5 == SUR__M_EPC ) ) THEN
                   WRITE  ( 6, 110 ) '  Compute visibility ', J5, SUR__M_EPC, '  '//CHAR(13)
                   CALL FLUSH  ( 6 )
 110               FORMAT ( A, I5,' ( ', I5, ' ) ',A$ )
              END IF
            ELSE 
              IF ( IVRB .GE. 2 .AND. ( MOD(J5,512) == 0 .OR. J5 == SUR__M_EPC ) ) THEN
                   WRITE  ( 6, 115 ) '  Compute visibility ', J5, SUR__M_EPC
                   CALL FLUSH  ( 6 )
 115               FORMAT ( A, I5,' ( ', I5, ' ) ' )
              END IF
         END IF
!
         FL_GAP = .FALSE.
         IF ( SUR%N_GAP > 0 ) THEN
              DO 460 J6=1,SUR%N_GAP
                 IF ( (MJD_OBS - SUR%MJD_GAP(1,J6))*86400.0D0 + (TAI_OBS - SUR%TAI_GAP(1,J6)) > 0.0D0 .AND. &
     &                (MJD_OBS - SUR%MJD_GAP(2,J6))*86400.0D0 + (TAI_OBS - SUR%TAI_GAP(2,J6)) < 0.0D0       ) THEN 
                      FL_GAP = .TRUE.
                 END IF
 460          CONTINUE 
         END IF
         IF ( FL_GAP ) GOTO 450
!
         DO 470 J7=1,SUR__NTYP
            IF ( J7 == SUR__TYP_TAG ) THEN
                 I_TYP = SUR__TYP_TAG
                 L_SOU = SUR%L_SOU
               ELSE IF ( J7 == SUR__TYP_SEC ) THEN
                 I_TYP = SUR__TYP_SEC
                 L_SOU = SUR%L_SO2
               ELSE IF ( J7 == SUR__TYP_CAL ) THEN
                 I_TYP = SUR__TYP_CAL
                 L_SOU = SUR%L_CAL
               ELSE IF ( J7 == SUR__TYP_POC ) THEN
                 I_TYP = SUR__TYP_POC
                 L_SOU = SUR%L_SOP
               ELSE IF ( J7 == SUR__TYP_PLA ) THEN
                 I_TYP = SUR__TYP_PLA
                 L_SOU = SUR%L_PLA
            END IF
            IF ( L_SOU == 0 ) GOTO 470
!
            DO 480 J8=1,L_SOU
               K_STA = 0
               FL_STA = .FALSE.
               FL_STI_FAIL = .FALSE.
               DO 490 J9=1,SUR%L_STA
                  IF ( SUR%STA(J9)%TAGALONE ) GOTO 490
!
                  CALL ERR_PASS ( IUER, IER )
                  CALL SUR_AZEL ( SUR, VTD, I_TYP, MJD_OBS, TAI_OBS, J9, J8, &
     &                            AZ, EL, HA, IER )
                  IF ( IER .NE. 0 ) THEN
                       CALL ERR_LOG ( 1685, IUER, 'SUR_ASTROMET', 'Error in '// &
     &                      'computing azimuth and elevation' )
                       RETURN
                  END IF
                  IF ( SUR%STA(J9)%REF ) THEN
                       SUR%ELEV_MAX(J8,J7) = MAX ( SUR%ELEV_MAX(J8,J7), EL )
                  END IF
!
                  IF ( I_TYP == SUR__TYP_TAG ) THEN
                       IF ( EL < SUR%SOU(J8)%EL_MIN ) THEN
                            IF ( SUR%STA(J9)%STICKY ) FL_STI_FAIL = .TRUE.
                            GOTO 490
                       END IF
                     ELSE IF ( I_TYP == SUR__TYP_SEC ) THEN
                       IF ( EL < SUR%SO2(J8)%EL_MIN ) GOTO 490
                     ELSE IF ( I_TYP == SUR__TYP_CAL ) THEN
                       IF ( EL < SUR%CAL(J8)%EL_MIN ) GOTO 490
                     ELSE IF ( I_TYP == SUR__TYP_POC ) THEN
                       IF ( EL < SUR%SOP(J8)%EL_MIN ) GOTO 490
                     ELSE IF ( I_TYP == SUR__TYP_PLA ) THEN
                       IF ( EL < SUR%PLA(J8)%EL_MIN ) GOTO 490
                  END IF
!
                  IF ( SUR%STA(J9)%MOUNT_TYPE == MT__ALTAZ ) THEN
                       IF ( EL < SUR%STA(J9)%EL_MIN .OR.  &
     &                      EL > SUR%STA(J9)%EL_MAX       ) THEN
!
                            IF ( I_TYP == SUR__TYP_TAG .AND. SUR%STA(J9)%STICKY ) FL_STI_FAIL = .TRUE.
                            GOTO 490
                       END IF
                  END IF
!
! --------------- Now, let us perform a more stringent check
!
                  IER = -1
                  IF ( .NOT. SUR_CHECK_VIS ( SUR, J9, SUR__TYP_TAG, J8, AZ, EL, &
     &                                       HA, IER ) ) THEN
!
! -------------------- Does not see? Go to the next source
!
                       IF ( I_TYP == SUR__TYP_TAG .AND. SUR%STA(J9)%STICKY ) FL_STI_FAIL = .TRUE.
                       GOTO 490
                  END IF
                  FL_STA(J9) = .TRUE.
                  K_STA = K_STA + 1
 490           CONTINUE
!
               IF ( J7 == SUR__TYP_TAG  ) THEN
                    IF ( K_STA < SUR%SOU(J8)%MIN_STA ) GOTO 480
                    IF ( FL_STI_FAIL ) GOTO 480
                 ELSE IF ( J7 == SUR__TYP_SEC ) THEN
                    IF ( K_STA < SUR%SO2(J8)%MIN_STA ) GOTO 480
                 ELSE IF ( J7 == SUR__TYP_CAL ) THEN
                    IF ( K_STA < SUR%CAL(J8)%MIN_STA ) GOTO 480
                 ELSE IF ( J7 == SUR__TYP_POC ) THEN
                    IF ( K_STA < SUR%CAL(J8)%MIN_STA ) GOTO 480
                 ELSE IF ( J7 == SUR__TYP_PLA ) THEN
                    IF ( K_STA < SUR%CAL(J8)%MIN_STA ) GOTO 480
               END IF
               NUM_EPC(J8,J7) = NUM_EPC(J8,J7) + 1
!
               IF ( SUR%N_VIS(J8,J7) == 0 ) THEN
                    SUR%N_VIS(J8,J7) = 1
                    SUR%MJD_VIS(SUR%N_VIS(J8,J7),SUR__RIS,J8,J7) = MJD_OBS
                    SUR%TAI_VIS(SUR%N_VIS(J8,J7),SUR__RIS,J8,J7) = TAI_OBS
                    SUR%MJD_VIS(SUR%N_VIS(J8,J7),SUR__SET,J8,J7) = MJD_OBS
                    SUR%TAI_VIS(SUR%N_VIS(J8,J7),SUR__SET,J8,J7) = TAI_OBS
                 ELSE
                    TIM_LAST = (MJD_OBS - SUR%MJD_VIS(SUR%N_VIS(J8,J7),SUR__SET,J8,J7))* &
     &                         86400.0D0 + &
     &                         (TAI_OBS - SUR%TAI_VIS(SUR%N_VIS(J8,J7),SUR__SET,J8,J7))
                    IF ( TIM_LAST > 4.0D0*DUR_SES/(SUR__M_EPC-1) ) THEN
                         SUR%N_VIS(J8,J7) = SUR%N_VIS(J8,J7) + 1
                         IF ( SUR%N_VIS(J8,J7) > SUR__M_VIS ) THEN
                              CALL CLRCH ( STR )
                              CALL INCH  ( SUR__M_VIS, STR )
                              WRITE ( 6, * ) ' J7=', J7
                              CALL ERR_LOG ( 1686, IUER, 'SUR_ASTROMET', &
     &                            'Too many rises for source '// &
     &                             SUR%SOU(J8)%J2000_NAME//' more than '// &
     &                             STR )
                              RETURN
                         END IF
                         SUR%MJD_VIS(SUR%N_VIS(J8,J7),SUR__RIS,J8,J7) = MJD_OBS
                         SUR%TAI_VIS(SUR%N_VIS(J8,J7),SUR__RIS,J8,J7) = TAI_OBS
                    END IF
                    SUR%MJD_VIS(SUR%N_VIS(J8,J7),SUR__SET,J8,J7) = MJD_OBS
                    SUR%TAI_VIS(SUR%N_VIS(J8,J7),SUR__SET,J8,J7) = TAI_OBS
               END IF
 480        CONTINUE
 470     CONTINUE
 450  CONTINUE
      TW = WALL_TIMER ( %VAL(2) )
!
      IF ( IVRB .GE. 5 ) THEN
           WRITE ( 6, '(A)' ) ' '
           WRITE ( 6, '("  Elapsed time: ", F9.2)' ) TW
      END IF
!
      DO 4100 J10=1,SUR%L_SOU
         DUR_OBS = DUR_SES*NUM_EPC(J10,1)/DFLOAT(SUR__M_EPC)
         NOBS_NORM = DUR_OBS/SUR%SOU(J10)%GAP_NOR
         NOBS_MIN  = DUR_OBS/SUR%SOU(J10)%GAP_MIN
         IF ( NOBS_NORM > SUR%SCAN_PER_SOURCE_NORM ) THEN
              NOBS = SUR%SCAN_PER_SOURCE_NORM
            ELSE IF ( NOBS_MIN > SUR%SCAN_PER_SOURCE_NORM ) THEN
              NOBS = SUR%SCAN_PER_SOURCE_NORM
            ELSE
              NOBS = NOBS_MIN
         END IF
         IF ( NOBS > 2 ) THEN
              SUR%GAPS_SRC(J10) = DUR_OBS/(NOBS-1)
            ELSE
              SUR%GAPS_SRC(J10) = 0.0D0
         END IF
!
         SUR%SOU(J10)%FL_END = 0
         TIM_UP = 0.0D0
         DO 4110 J11=1,SUR%N_VIS(J10,1)
            TIM_RIS = (SUR%MJD_VIS(J11,SUR__RIS,J10,1) - SUR%MJD_START)*86400.0D0 + &
     &                (SUR%TAI_VIS(J11,SUR__RIS,J10,1) - SUR%TAI_START)
            TIM_SET = (SUR%MJD_VIS(J11,SUR__SET,J10,1) - SUR%MJD_START)*86400.0D0 + &
     &                (SUR%TAI_VIS(J11,SUR__SET,J10,1) - SUR%TAI_START)
            TIM_UP = TIM_UP + &
     &               (SUR%MJD_VIS(J11,SUR__SET,J10,1) - SUR%MJD_VIS(J11,SUR__RIS,J10,1))*86400.0D0 + &
     &               (SUR%TAI_VIS(J11,SUR__SET,J10,1) - SUR%TAI_VIS(J11,SUR__RIS,J10,1))
            IF ( TIM_RIS > DUR_SES - SUR%SOU(J10)%GAP_MIN ) THEN
                 SUR%SOU(J10)%FL_END = 1
            END IF
!
            IND_PT = 0
            IND_PA = 0
            IND_EF = 0
            DO 4120 J12=1,SUR%L_STA
               IF ( SUR%STA(J12)%NAME == 'PIETOWN ' ) IND_PT = J12
               IF ( SUR%STA(J12)%NAME == 'PARKES  ' ) IND_PA = J12
               IF ( SUR%STA(J12)%NAME == 'EFLSBERG' ) IND_EF = J12
 4120       CONTINUE 
            IF ( IND_PT > 0 ) THEN
                 STA_LST = 'PIETOWN '
               ELSE IF ( IND_PA > 0 ) THEN
                 STA_LST = 'PARKES  '
               ELSE IF ( IND_EF > 0 ) THEN
                 STA_LST = 'EFLSBERG'
               ELSE 
                 STA_LST = SUR%STA(1)%NAME
            END IF
            LST_PT_RIS = GET_LST ( SUR%MJD_VIS(J11,SUR__RIS,J10,1), &
     &                             SUR%TAI_VIS(J11,SUR__RIS,J10,1), STA_LST, &
     &                             SUR, VTD, S_ANG, IER )
            LST_PT_SET = GET_LST ( SUR%MJD_VIS(J11,SUR__SET,J10,1), &
     &                             SUR%TAI_VIS(J11,SUR__SET,J10,1), STA_LST, &
     &                             SUR, VTD, S_ANG, IER )
!
            STR_RIS = MJDSEC_TO_DATE ( SUR%MJD_VIS(J11,SUR__RIS,J10,1), SUR%TAI_VIS(J11,SUR__RIS,J10,1), IER )
            STR_SET = MJDSEC_TO_DATE ( SUR%MJD_VIS(J11,SUR__SET,J10,1), SUR%TAI_VIS(J11,SUR__SET,J10,1), IER )
            IF ( IVRB .GE. 5 ) THEN
                 WRITE ( 6, 212 ) J10, SUR%SOU(J10)%J2000_NAME, &
     &                            LST_PT_RIS*RAD__TO__SEC/3600.0D0, &
     &                            LST_PT_SET*RAD__TO__SEC/3600.0D0, &
     &                            STR_RIS(12:16), STR_SET(12:16)
 212             FORMAT ( I4, ') LST  Source: ', A, 2X,  &
     &                    'Rise: ', F5.2, '  Set: ', F5.2, &
     &                    ' hours || UTC ris: ', A, ' UTC set: ', A )
            END IF                         
 4110    CONTINUE
         IF ( IVRB .GE. 5 .AND. TIM_UP > 10.0 ) THEN
              WRITE ( 6, 120 ) J10, SUR%SOU(J10)%J2000_NAME, &
     &                         SUR%SOU(J10)%MIN_STA, &
     &                         SUR%SOU(J10)%EL_MIN/DEG__TO__RAD, &
     &                         TIM_UP/3600.0D0, SUR%ELEV_MAX(J10,1)/DEG__TO__RAD
 120         FORMAT ( I4, ' Source: ',A, ' Min_sta: ', I2, &
     &                    ' El_min: ', F4.1, ' deg  Time_up: ', &
     &                    F5.2, ' hr  El_max: ', F5.2, ' deg' )
         END IF
 4100 CONTINUE
!
      IF ( IVRB .GE. 6 ) THEN
           WRITE ( 6, '(A)' ) 'Generated on '//GET_CDATE()
           WRITE ( 6, '(A)' ) '================================'
!
           DO 4130 J13=0,47
              MJD_OBS = SUR%MJD_START
              TAI_OBS = J13*3600.0D0/2.0D0
              IF ( J13 ==  0 ) TAI_OBS = TAI_OBS + 60.0D0
              IF ( J13 == 23 ) TAI_OBS = TAI_OBS - 60.0D0
              NSOU_UP = 0
              DO 4140 J14=1,SUR%L_SOU
                 FL_UP = .FALSE.
                 DO 4150 J15=1,SUR%N_VIS(J14,SUR__TYP_TAG)
!
! ----------------- TIM_RIS -- time elapsed from the J15-th rise
! ----------------- TIM_SET -- time elapsed from the J15-th set
!
                    TIM_RIS = (MJD_OBS - SUR%MJD_VIS(J15,SUR__RIS,J14,SUR__TYP_TAG))*86400.0D0 + &
     &                        (TAI_OBS - SUR%TAI_VIS(J15,SUR__RIS,J14,SUR__TYP_TAG))
                    TIM_SET = (MJD_OBS - SUR%MJD_VIS(J15,SUR__SET,J14,SUR__TYP_TAG))*86400.0D0 + &
     &                        (TAI_OBS - SUR%TAI_VIS(J15,SUR__SET,J14,SUR__TYP_TAG))
                    IF ( TIM_RIS > 0.0D0  .AND. TIM_SET < 0.0D0 ) THEN
                         FL_UP = .TRUE.
                    END IF
 4150            CONTINUE
                 IF ( FL_UP .AND. SUR%SOU(J14)%NOBS < SUR%SOU(J14)%NSCA_MAX ) THEN
                      NSOU_UP = NSOU_UP + 1
                 END IF
 4140         CONTINUE
              STR = MJDSEC_TO_DATE ( MJD_OBS, TAI_OBS, IER )
              WRITE ( 6, 130 ) STR(1:19), NSOU_UP
 130          FORMAT ( 'UTC Date: ', A, ' # remaining target sources up: ', I4 )
 4130      CONTINUE 
!
           IF ( IVRB == 21 ) THEN
                CALL EXIT ( 0 )
           END IF
      END IF
      IF ( FL_VIS_ONLY ) THEN
           WRITE ( 6, * ) 'SUR_ASTROMTRY: SUR_SKED_VIS_ONLY variable was set up'
           WRITE ( 6, * ) 'SUR_ASTROMTRY: stop'
           CALL EXIT ( 0 )
      END IF
      SUR%L_OBS_POC = 0
      SUR%L_SCN = 0
      SUR%L_SCN_SO1  = 0
      SUR%SOU_POCAL  = 0
      SUR%LAST_POCAL_SCAN = 0
      SUR%MJD_CUR = SUR%MJD_START
      SUR%TAI_CUR = SUR%TAI_START + SUR%PRESES_INTERVAL
!
      IF ( SUR%ALGORITHM == 'ASTROMET_01'   .OR. &
     &     SUR%ALGORITHM == 'ASTROMET_02'   .OR. &
     &     SUR%ALGORITHM == 'ASTROMET_03'   .OR. &
     &     SUR%ALGORITHM == 'ASTROMET_04'   .OR. &
     &     SUR%ALGORITHM == 'ASTROMET_05'   .OR. &
     &     SUR%ALGORITHM == 'ASTROMET_06'   .OR. &
     &     SUR%ALGORITHM == 'ASTROMET_07'   .OR. &
     &     SUR%ALGORITHM == 'GEODETIC_01'   .OR. &
     &     SUR%ALGORITHM == 'GEODETIC_02'   .OR. &
     &     SUR%ALGORITHM == 'GEODETIC_03'   .OR. &
     &     SUR%ALGORITHM == 'GNSS_01'       .OR. &
     &     SUR%ALGORITHM == 'GNSS_02'       .OR. &
     &     SUR%ALGORITHM == 'SPACECRAFT_01'      ) THEN
!
           IF ( SUR%TROPO_BURST_INTERVAL < 0.0D0 .OR.    &
     &          ( SUR%ALGORITHM .NE. 'ASTROMET_03' .AND. &
     &            SUR%ALGORITHM .NE. 'ASTROMET_04' .AND. &
     &            SUR%ALGORITHM .NE. 'ASTROMET_05' .AND. &
     &            SUR%ALGORITHM .NE. 'ASTROMET_06' .AND. &
     &            SUR%ALGORITHM .NE. 'ASTROMET_07'       ) ) THEN
! 
                IF ( IVRB .GE. 2 ) THEN
                     WRITE ( 6, '(A)' ) '  Get the first observation' 
                END IF
!
                CALL ERR_PASS ( IUER, IER )
                CALL SUR_ASTRO_FIRST ( SUR, VTD, IVRB, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 1687, IUER, 'SUR_ASTROMET', 'Error '// &
     &                   'in attempt to find the first observation for '// &
     &                   'the astrometric schedule' )
                     RETURN
                END IF
           END IF
           IF ( IVRB .GE. 2 ) THEN
                WRITE ( 6, '(A)' ) '  Get the first troposphere calibrators burst'
           END IF
!
           IF ( SUR%TROPO_BURST_INTERVAL > 0.0D0 ) THEN
                CALL ERR_PASS ( IUER, IER )
                CALL SUR_ASTRO_TROPO ( SUR, VTD, IVRB, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 1688, IUER, 'SUR_ASTROMET', 'Error in attempt '// &
     &                   'to find the observation for the first troposphere '// &
     &                   'calibrators burst in the astrmetric schedule' )
                     RETURN
                END IF
           END IF
           IF ( IVRB .GE. 2 ) THEN
                WRITE ( 6, '(A)' ) '  Get the sequence of observations'
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL SUR_ASTRO_SEQ ( SUR, VTD, IVRB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1689, IUER, 'SUR_ASTROMET', 'Error in attempt '// &
     &              'to find an optimal sequence of observation for the '// &
     &              'astrometric schedule' )
                RETURN
           END IF
         ELSE IF ( SUR%ALGORITHM == 'ASTROMET_11' ) THEN
           IF ( IVRB .GE. 2 ) THEN
                WRITE ( 6, '(A)' ) '  Get the first troposphere calibrators burst'
           END IF
           CALL ERR_PASS ( IUER, IER )
           CALL SUR_ASTRO_TROPO ( SUR, VTD, IVRB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1690, IUER, 'SUR_ASTROMET', 'Error in attempt '// &
     &              'to find the observation for the first troposphere '// &
     &              'calibrators burst in the astrmetric schedule' )
                RETURN
           END IF
!
           IF ( IVRB .GE. 2 ) THEN
                WRITE ( 6, '(A)' ) '  Get the sequence of observations'
           END IF
           CALL ERR_PASS ( IUER, IER )
           CALL SUR_TRANSIT ( SUR, VTD, IVRB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1691, IUER, 'SUR_ASTROMET', 'Error in attempt '// &
     &              'to generate the transit-style astrometric schedule' )
                RETURN
           END IF
         ELSE IF ( SUR%ALGORITHM == 'ASTROMET_12' .OR. &
     &             SUR%ALGORITHM == 'ASTROMET_13' .OR. &
     &             SUR%ALGORITHM == 'IMAGING_01'  .OR. &
     &             SUR%ALGORITHM == 'IMAGING_S1'       ) THEN
           IF ( IVRB .GE. 2 ) THEN
                WRITE ( 6, '(A)' ) '  Get the first observation'
           END IF
           IF ( SUR%ALGORITHM == 'IMAGING_S1' ) THEN
!
! ------------- Single image mode. We avoid the first source
!
                SUR%ALGORITHM = 'IMAGING_01'
              ELSE 
                CALL ERR_PASS ( IUER, IER )
                CALL SUR_ASTRO_FIRST ( SUR, VTD, IVRB, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 1692, IUER, 'SUR_ASTROMET', 'Error '// &
     &              'in attempt to find the first observation for '// &
     &              'the astrmotric schedule' )
                     RETURN
                END IF
           END IF
!
           IF ( SUR%TROPO_BURST_INTERVAL .GE. 0.0D0 ) THEN
                IF ( IVRB .GE. 2 ) THEN
                     WRITE ( 6, '(A)' ) '  Get the first troposphere calibrators burst'
                END IF
                CALL ERR_PASS ( IUER, IER )
                CALL SUR_ASTRO_TROPO ( SUR, VTD, IVRB, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 1693, IUER, 'SUR_ASTROMET', 'Error in attempt '// &
          &              'to find the observation for the first troposphere '// &
          &              'calibrators burst in the astrmetric schedule' )
                     RETURN
                END IF
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL SUR_MERIDIAN ( SUR, VTD, IVRB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1694, IUER, 'SUR_ASTROMET', 'Error in attempt '// &
     &              'to generate the transit-style astrometric schedule' )
                RETURN
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SUR_ASTROMET  !#!#
