      SUBROUTINE VTD_UEOP_INIT ( VTD, FILEOP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_UEOP_INIT reads the file FILEOP with the Earth        *
! *   Orientation Parameters parses it, computes interpolation           *
! *   polynomials and stores it in the fields of object VTD. After call  *
! *   VTD_UEOP_INIT other routines, f.e., vtd_erm_na are able to use     *
! *   these coefficients and compute UT1 and  pole coordinates on the    *
! *   moment of time of interest.                                        *
! *                                                                      *
! *   Supported format of the Earth Orientation Parameters file:         *
! *   1) EOP-MOD Ver 2.0                                                 *
! *   2) # NERS EOP series.  Format version of 2016.06.22!               *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   FILEOP ( CHARACTER ) -- Name of the file with the Earth            *
! *                           Orientation Parameters.                    *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *      VTD ( RECORD    ) -- Object which keeps configuration and data  *
! *                           related to VLBI Theoretical Delay (VTD)    *
! *                           package.                                   *
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
! *  ### 25-JAN-2004  VTD_UEOP_INIT v2.4 (c)  L. Petrov  16-OCT-2023 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      CHARACTER  FILEOP*(*)
      INTEGER*4  IUER
      TYPE      ( VTD__TYPE ) :: VTD
      CHARACTER*256, ALLOCATABLE :: BUF(:)
      CHARACTER  STR*32
      LOGICAL*4  LEX
      REAL*8     JD_BEG, STEP_JD, JD, XPL, YPL, D1, DN, WORK(UEOP__MP), &
     &           UTC_SEC, TAI_SEC, E3, E3_DOT, E3_DT2, TIM_TAI, TIM_TAG
      INTEGER*4  NBUF, IOS, NP, MJD, INT_UT1_M_TAI, J1, J2, J3, IER
      INTEGER*4  I_LEN
!
! --- Check, whether the file exists
!
      INQUIRE ( FILE=FILEOP, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 1611, IUER, 'VTD_UEOP_INIT', 'File with polar '// &
     &         'motion and UT1 time series '//FILEOP(1:I_LEN(FILEOP))// &
     &         ' has not been found' )
           RETURN
      END IF
!
! --- Allocate the buffer for reading the input file
!
      ALLOCATE ( BUF(UEOP__MP), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 256*UEOP__MP, STR )
           CALL ERR_LOG ( 1612, IUER, 'VTD_UEOP_INIT', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN
      END IF
!
! --- Read the input file and put its contents into the buffer BUF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILEOP, UEOP__MP, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           DEALLOCATE ( BUF )
           CALL ERR_LOG ( 1613, IUER, 'VTD_UEOP_INIT', 'Error in an attempt '// &
     &         'to read input file with series of polar motion and EOP '// &
     &          FILEOP )
           RETURN
      END IF
!
      NP = 0
      IF ( BUF(1)(1:26) == '# NERS Configuration file.' ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL NERS_INIT ( FILEOP, VTD%NERS, &
     &              (VTD%MJD_BEG - J2000__MJD)*86400.0D0 + VTD%TAI_BEG - VTD__EOP_TIM_MAR, &
     &              (VTD%MJD_END - J2000__MJD)*86400.0D0 + VTD%TAI_END + VTD__EOP_TIM_MAR, &
     &               IER )
           IF ( IER .NE. 0 ) THEN
                DEALLOCATE ( BUF )
                CALL ERR_LOG ( 1614, IUER, 'VTD_UEOP_INIT', 'Error in an attempt '// &
     &              'to initialize NERS internal data structure' )
                RETURN
           END IF
           DEALLOCATE ( BUF )
           VTD%UEOP%STATUS = UEOP__LOADED
! 
           IF ( VTD%CONF%PREC_EXP .NE. VTD__NERS ) THEN
                CALL ERR_LOG ( 1615, IUER, 'VTD_UEOP_INIT', 'Conflicting parameters: '// &
     &              'when the Earth orientation parameters are requested via '// &
     &              'NERS, precession expression should be specified as NERS' )
                RETURN
           END IF
           IF ( VTD%CONF%NUT_EXP .NE. VTD__NERS ) THEN
                CALL ERR_LOG ( 1616, IUER, 'VTD_UEOP_INIT', 'Conflicting parameters: '// &
     &              'when the Earth orientation parameters are requested via '// &
     &              'NERS, nutation expression should be specified as NERS' )
                RETURN
           END IF
           CALL ERR_LOG ( 0, IUER )
           RETURN
        ELSE IF ( BUF(1)(1:17) .EQ. 'EOP-MOD Ver 2.0  ' ) THEN
!
! -------- Aga. EOP-MOD Ver 2.0  format
!
! -------- Parse the first line with the header
!
           READ ( UNIT=BUF(1)(18:29), FMT='(F12.4)', IOSTAT=IOS ) JD_BEG
           READ ( UNIT=BUF(1)(29:34), FMT='(F6.3)',  IOSTAT=IOS ) STEP_JD
           READ ( UNIT=BUF(1)(35:39), FMT='(I5)',    IOSTAT=IOS ) NP
!
! -------- Initialize the number of points counter
!
           VTD%UEOP%NP = 0
!
! -------- Get Julian dates of the first and the last date
!
           VTD%UEOP%MJD_BEG = JD_BEG - 2400000.5D0
           VTD%UEOP%TAI_BEG = (JD_BEG - 2400000.5D0 - VTD%UEOP%MJD_BEG)*86400.0D0
           IF ( VTD%UEOP%TAI_BEG .LT. 0.0D0 ) THEN
                VTD%UEOP%TAI_BEG = VTD%UEOP%TAI_BEG + 86400.0D0
                VTD%UEOP%MJD_BEG = VTD%UEOP%MJD_BEG - 1
           END IF
!
           VTD%UEOP%MJD_END = VTD%UEOP%MJD_BEG + NP*STEP_JD
           VTD%UEOP%TAI_END = ( JD_BEG + NP*STEP_JD - 2400000.5D0 - VTD%UEOP%MJD_END )* &
     &                    86400.0D0
           IF ( VTD%UEOP%TAI_END .LT. 0.0D0 ) THEN
                VTD%UEOP%TAI_END = VTD%UEOP%TAI_END + 86400.0D0
                VTD%UEOP%MJD_END = VTD%UEOP%MJD_END - 1
           END IF
!
! -------- Read the body of the EOP file
!
           DO 410 J1=2,NBUF
              IF ( BUF(J1)(1:1) .EQ. '#' ) GOTO 410
!
! ----------- Get the line and parse it
!
              READ ( UNIT=BUF(J1)(1:35), FMT=110, IOSTAT=IOS ) &
     &               JD, XPL, YPL, INT_UT1_M_TAI
 110          FORMAT ( F10.2, F7.4, 1X, F7.4, I10 )
              IF ( IOS .NE. 0 ) THEN
                   DEALLOCATE ( BUF )
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 1618, IUER, 'VTD_UEOP_INIT', 'Error in '// &
     &                 'reading line '//STR(1:I_LEN(STR))//' fo the '// &
     &                 'to read input file with series of polar motion '//  &
     &                 'and EOP '//FILEOP )
                   RETURN
              END IF
!
! ----------- Transform time scale if it is not TAI
!
              IF ( VTD%CONF%EOP_TIME_SCALE == VTD__TAI ) THEN
                   CONTINUE
                ELSE IF ( VTD%CONF%EOP_TIME_SCALE == VTD__TDB ) THEN
                   JD = JD - 32.184D0/86400.0D0
                ELSE IF ( VTD%CONF%EOP_TIME_SCALE == VTD__TDT ) THEN
                   JD = JD - 32.184D0/86400.0D0
                ELSE IF ( VTD%CONF%EOP_TIME_SCALE == VTD__UTC ) THEN
                   MJD     = (JD - 2400000.5D0 )
                   UTC_SEC = (JD - 2400000.5D0 - MJD)*86400.0D0
                   CALL ERR_PASS ( IUER, IER )
                   CALL VTD_UTC_TO_TAI ( VTD, MJD, UTC_SEC, TAI_SEC, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 1619, IUER, 'VTD_UEOP_INIT', 'Error in '// &
     &                      'computing leap second' )
                        RETURN
                   END IF
                   JD = JD + (TAI_SEC  - UTC_SEC)/86400.0D0
                ELSE IF ( VTD%CONF%EOP_TIME_SCALE == VTD__UT1 ) THEN
                   JD = JD - (INT_UT1_M_TAI*1.D-6)/86400.0D0
              END IF
!
! ----------- Transform units to SI
!
              MJD     = (JD - 2400000.5D0 )
              TAI_SEC = (JD - 2400000.5D0 - MJD)*86400.0D0
!
              VTD%UEOP%NP = VTD%UEOP%NP + 1
              VTD%UEOP%TIM(VTD%UEOP%NP)   = (JD - J2000__JD)*86400.0D0
              VTD%UEOP%VAL(VTD%UEOP%NP,UEOP__XPL) = XPL*100.0D0*MAS__TO__RAD
              VTD%UEOP%VAL(VTD%UEOP%NP,UEOP__YPL) = YPL*100.0D0*MAS__TO__RAD
              VTD%UEOP%VAL(VTD%UEOP%NP,UEOP__UT1_M_TAI) = INT_UT1_M_TAI*1.D-6
              IF ( VTD%CONF%UZT_USE   == UZT__INTERPOLATE  .AND.  &
     &             VTD%CONF%UZT_MODEL == UZT__DICKMAN1993         ) THEN
!
! ---------------- Subtract apriori model of variations in E3 due to zonal tides
!
                   CALL E3ZT_DICKMAN1993  ( 0, MJD, TAI_SEC, E3, E3_DOT, E3_DT2 )
                   VTD%UEOP%VAL(VTD%UEOP%NP,UEOP__UT1_M_TAI) = &
     &                          VTD%UEOP%VAL(VTD%UEOP%NP,UEOP__UT1_M_TAI) - &
     &                          E3/UT1__TO__E3
                 ELSE IF ( VTD%CONF%UZT_USE   == UZT__INTERPOLATE      .AND. &
     &                     VTD%CONF%UZT_MODEL == UZT__DICKMAN_PRINCIPLE    ) THEN
!
! ---------------- Subtract apriori model of variations in E3 due to zonal tides
!
                   CALL E3ZT_DICKMAN1993 ( 13, MJD, TAI_SEC, E3, E3_DOT, E3_DT2 )
                   VTD%UEOP%VAL(VTD%UEOP%NP,UEOP__UT1_M_TAI) = &
     &                          VTD%UEOP%VAL(VTD%UEOP%NP,UEOP__UT1_M_TAI) - &
     &                          E3/UT1__TO__E3
                 ELSE IF ( VTD%CONF%UZT_USE   == UZT__INTERPOLATE      .AND. &
     &                     VTD%CONF%UZT_MODEL == UZT__DICKMAN_SHORT        ) THEN
!
! ---------------- Subtract apriori model of variations in E3 due to zonal tides
!
                   CALL E3ZT_DICKMAN1993 ( 12, MJD, TAI_SEC, E3, E3_DOT, E3_DT2 )
                   VTD%UEOP%VAL(VTD%UEOP%NP,UEOP__UT1_M_TAI) = &
     &                          VTD%UEOP%VAL(VTD%UEOP%NP,UEOP__UT1_M_TAI) - &
     &                          E3/UT1__TO__E3
                 ELSE IF ( VTD%CONF%UZT_USE   == UZT__INTERPOLATE  .AND.  &
     &                     VTD%CONF%UZT_MODEL == UZT__RE2014              ) THEN
!
! ---------------- Subtract apriori model of variations in E3 due to zonal tides
!
                   CALL E3ZT_RE2014 ( 0, MJD, TAI_SEC, E3, E3_DOT, E3_DT2 )
                   VTD%UEOP%VAL(VTD%UEOP%NP,UEOP__UT1_M_TAI) = &
     &                          VTD%UEOP%VAL(VTD%UEOP%NP,UEOP__UT1_M_TAI) - &
     &                          E3/UT1__TO__E3
                 ELSE IF ( VTD%CONF%UZT_USE   == UZT__INTERPOLATE  .AND.  &
     &                     VTD%CONF%UZT_MODEL == UZT__RE2014_SHORT        ) THEN
!
! ---------------- Subtract apriori model of variations in E3 due to zonal tides
!
                   CALL E3ZT_RE2014 ( 1, MJD, TAI_SEC, E3, E3_DOT, E3_DT2 )
                   VTD%UEOP%VAL(VTD%UEOP%NP,UEOP__UT1_M_TAI) = &
     &                          VTD%UEOP%VAL(VTD%UEOP%NP,UEOP__UT1_M_TAI) - &
     &                          E3/UT1__TO__E3
              END IF
 410       CONTINUE
         ELSE IF ( BUF(1)(1:48) .EQ. '# NERS EOP series.  Format version of 2016.06.22' ) THEN
!
! -------- Modern NERS format
!
           IF ( BUF(3)(1:20) .EQ. '# Output type: polu ' ) THEN
                CONTINUE 
              ELSE IF ( BUF(3)(1:20) .EQ. '# Output type: poluz' ) THEN
                CONTINUE 
              ELSE
                CALL ERR_LOG ( 1620, IUER, 'VTD_UEOP_INIT', 'The NERS EOP series '// &
     &              'file '//FILEOP(1:I_LEN(FILEOP))//' is not in the "polu" '// &
     &              'style. It is not suitable for VTD' )
                RETURN
           END IF
!
! -------- Initialize the number of points counter
!
           VTD%UEOP%NP = 0
           DO 420 J2=1,NBUF
              IF ( BUF(J2)(1:1) .EQ. '#' ) GOTO 420
!
! ----------- Get the line and parse it
!
              READ ( UNIT=BUF(J2)(9:20), FMT='(F12.1)', IOSTAT=IOS ) TIM_TAI
              IF ( IOS .NE. 0 ) THEN
                   DEALLOCATE ( BUF )
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 1621, IUER, 'VTD_UEOP_INIT', 'Error in '// &
     &                 'reading line '//STR(1:I_LEN(STR))//' fo the '// &
     &                 'to read input file with series of polar motion '//  &
     &                 'and EOP '//FILEOP )
                   RETURN
              END IF
              VTD%UEOP%NP = VTD%UEOP%NP + 1
              NP = NP + 1
              READ ( UNIT=BUF(J2)(51:65), FMT='(F15.5)', IOSTAT=IOS ) VTD%UEOP%VAL(VTD%UEOP%NP,UEOP__XPL)
              IF ( IOS .NE. 0 ) THEN
                   DEALLOCATE ( BUF )
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 1622, IUER, 'VTD_UEOP_INIT', 'Error in '// &
     &                 'reading line '//STR(1:I_LEN(STR))//' fo the '// &
     &                 'to read input file with series of polar motion '//  &
     &                 'and EOP '//FILEOP )
                   RETURN
              END IF
              VTD%UEOP%VAL(VTD%UEOP%NP,UEOP__XPL) = VTD%UEOP%VAL(VTD%UEOP%NP,UEOP__XPL)*ARCSEC__TO__RAD
!
              READ ( UNIT=BUF(J2)(67:81), FMT='(F15.5)', IOSTAT=IOS ) VTD%UEOP%VAL(VTD%UEOP%NP,UEOP__YPL)
              IF ( IOS .NE. 0 ) THEN
                   DEALLOCATE ( BUF )
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 1623, IUER, 'VTD_UEOP_INIT', 'Error in '// &
     &                 'reading line '//STR(1:I_LEN(STR))//' fo the '// &
     &                 'to read input file with series of polar motion '//  &
     &                 'and EOP '//FILEOP )
                   RETURN
              END IF
              VTD%UEOP%VAL(VTD%UEOP%NP,UEOP__YPL) = VTD%UEOP%VAL(VTD%UEOP%NP,UEOP__YPL)*ARCSEC__TO__RAD
!
              READ ( UNIT=BUF(J2)(83:97), FMT='(F15.5)', IOSTAT=IOS ) VTD%UEOP%VAL(VTD%UEOP%NP,UEOP__UT1_M_TAI)
              IF ( IOS .NE. 0 ) THEN
                   DEALLOCATE ( BUF )
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 1624, IUER, 'VTD_UEOP_INIT', 'Error in '// &
     &                 'reading line '//STR(1:I_LEN(STR))//' fo the '// &
     &                 'to read input file with series of polar motion '//  &
     &                 'and EOP '//FILEOP )
                   RETURN
              END IF
!
              MJD     = TIM_TAI/86400.0D0 + J2000__MJD
              TAI_SEC = TIM_TAI - (MJD - J2000__MJD)*86400.0D0
!
! ----------- Transform time scale if it is not TAI
!
              IF ( VTD%CONF%EOP_TIME_SCALE == VTD__TAI ) THEN
                   TIM_TAG  = TIM_TAI
                ELSE IF ( VTD%CONF%EOP_TIME_SCALE == VTD__TDB ) THEN
                   TIM_TAG = TIM_TAI - 32.184D0
                ELSE IF ( VTD%CONF%EOP_TIME_SCALE == VTD__TDT ) THEN
                   TIM_TAG = TIM_TAI - 32.184D0
                ELSE IF ( VTD%CONF%EOP_TIME_SCALE == VTD__UTC ) THEN
                   UTC_SEC = TIM_TAG
                   CALL ERR_PASS ( IUER, IER )
                   CALL VTD_UTC_TO_TAI ( VTD, MJD, UTC_SEC, TAI_SEC, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 1625, IUER, 'VTD_UEOP_INIT', 'Error in '// &
     &                      'computing leap second' )
                        RETURN
                   END IF
                   TIM_TAG = TIM_TAG + (TAI_SEC  - UTC_SEC)
                ELSE IF ( VTD%CONF%EOP_TIME_SCALE == VTD__UT1 ) THEN
                   TIM_TAG = TIM_TAG - INT_UT1_M_TAI*1.D-6
                ELSE 
                   CALL CLRCH ( STR )
                   WRITE ( UNIT=STR, FMT='(I12)' ) VTD%CONF%EOP_TIME_SCALE
                   CALL ERR_LOG ( 1626, IUER, 'VTD_UEOP_INIT', 'Trap of '// &
     &                 'internal control: undefined EOP time scale: '//STR )
                   RETURN 
              END IF
!
              VTD%UEOP%TIM(VTD%UEOP%NP) = TIM_TAG - 43200.0D0
              IF ( VTD%CONF%UZT_USE   == UZT__INTERPOLATE  .AND.  &
     &             VTD%CONF%UZT_MODEL == UZT__DICKMAN1993         ) THEN
!
! ---------------- Subtract apriori model of variations in E3 due to zonal tides
!
                   CALL E3ZT_DICKMAN1993  ( 0, MJD, TAI_SEC, E3, E3_DOT, E3_DT2 )
                   VTD%UEOP%VAL(VTD%UEOP%NP,UEOP__UT1_M_TAI) = &
     &                          VTD%UEOP%VAL(VTD%UEOP%NP,UEOP__UT1_M_TAI) - &
     &                          E3/UT1__TO__E3
                 ELSE IF ( VTD%CONF%UZT_USE   == UZT__INTERPOLATE      .AND. &
     &                     VTD%CONF%UZT_MODEL == UZT__DICKMAN_PRINCIPLE    ) THEN
!
! ---------------- Subtract apriori model of variations in E3 due to zonal tides
!
                   CALL E3ZT_DICKMAN1993 ( 13, MJD, TAI_SEC, E3, E3_DOT, E3_DT2 )
                   VTD%UEOP%VAL(VTD%UEOP%NP,UEOP__UT1_M_TAI) = &
     &                          VTD%UEOP%VAL(VTD%UEOP%NP,UEOP__UT1_M_TAI) - &
     &                          E3/UT1__TO__E3
                 ELSE IF ( VTD%CONF%UZT_USE   == UZT__INTERPOLATE      .AND. &
     &                     VTD%CONF%UZT_MODEL == UZT__DICKMAN_SHORT        ) THEN
!
! ---------------- Subtract apriori model of variations in E3 due to zonal tides
!
                   CALL E3ZT_DICKMAN1993 ( 12, MJD, TAI_SEC, E3, E3_DOT, E3_DT2 )
                   VTD%UEOP%VAL(VTD%UEOP%NP,UEOP__UT1_M_TAI) = &
     &                          VTD%UEOP%VAL(VTD%UEOP%NP,UEOP__UT1_M_TAI) - &
     &                          E3/UT1__TO__E3
                 ELSE IF ( VTD%CONF%UZT_USE   == UZT__INTERPOLATE  .AND.  &
     &                     VTD%CONF%UZT_MODEL == UZT__RE2014              ) THEN
!
! ---------------- Subtract apriori model of variations in E3 due to zonal tides
!
                   CALL E3ZT_RE2014 ( 0, MJD, TAI_SEC, E3, E3_DOT, E3_DT2 )
                   VTD%UEOP%VAL(VTD%UEOP%NP,UEOP__UT1_M_TAI) = &
     &                          VTD%UEOP%VAL(VTD%UEOP%NP,UEOP__UT1_M_TAI) - &
     &                          E3/UT1__TO__E3
                 ELSE IF ( VTD%CONF%UZT_USE   == UZT__INTERPOLATE  .AND.  &
     &                     VTD%CONF%UZT_MODEL == UZT__RE2014_SHORT        ) THEN
!
! ---------------- Subtract apriori model of variations in E3 due to zonal tides
!
                   CALL E3ZT_RE2014 ( 1, MJD, TAI_SEC, E3, E3_DOT, E3_DT2 )
                   VTD%UEOP%VAL(VTD%UEOP%NP,UEOP__UT1_M_TAI) = &
     &                          VTD%UEOP%VAL(VTD%UEOP%NP,UEOP__UT1_M_TAI) - &
     &                          E3/UT1__TO__E3
              END IF
 420       CONTINUE 
         ELSE
           DEALLOCATE ( BUF )
           CALL ERR_LOG ( 1627, IUER, 'VTD_UEOP_INIT', 'Format of the input '// &
     &         'file with polar motion and UT1 time series '// &
     &          FILEOP(1:I_LEN(FILEOP))//' was not recognized' )
           RETURN
      END IF
      DEALLOCATE ( BUF )
      IF ( VTD%UEOP%NP .LT. NP ) THEN
           WRITE ( 6, * ) ' NP = ', NP
           WRITE ( 6, * ) ' VTD%UEOP%NP = ', VTD%UEOP%NP
           CALL ERR_LOG ( 1628, IUER, 'VTD_UEOP_INIT', 'Input file file with '// &
     &         'polar motion and UT1 time series '// &
     &          FILEOP(1:I_LEN(FILEOP))//' was not recognized' )
           RETURN
      END IF
!
! --- Compute interpolating coefficients for each of components: X_pole, Y_pole
! --- and UT1
!
      DO 430 J3=1,3
         CALL ERR_PASS ( IUER, IER )
         CALL MAKE_SPLINE ( 3, VTD%UEOP%NP, VTD%UEOP%TIM, VTD%UEOP%VAL(1,J3), &
     &                      D1, DN, VTD%UEOP%SPL(1,J3), WORK, IER )
         IF ( IER .NE. 0 ) THEN
             CALL ERR_LOG ( 1629, IUER, 'VTD_UEOP_INIT', 'Error in an attempt '// &
     &           'to compute interpolating spline' )
             RETURN
         END IF
 430  CONTINUE
      VTD%UEOP%STATUS = UEOP__LOADED
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_UEOP_INIT  !#!#
