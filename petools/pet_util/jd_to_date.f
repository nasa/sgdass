      FUNCTION   JD_TO_DATE ( JD, IUER )
! ************************************************************************
! *                                                                      *
! *   Function  JD_TO_DATE  transforms Julian date to the internal SOLVE *
! *   format of date representation:   yyyy.mm.dd-hh:mm:ss.ppp           *
! *                                                                      *
! *   For example:                                                       *
! *                                                                      *
! *   1999.10.08-09:11:23.321                                            *
! *                                                                      *
! *   The length of the line is 23 symbols what gives precision up to    *
! *   one millisecond.                                                   *
! *   The date should be in the range [2433282.5, 2469808.8]             *
! *   (1950.0 - 2050.0).                                                 *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *          JD  ( REAL*8    ) -- Julian date.                           *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <JD_TO_DATE> ( CHARACTER ) -- Date and time in SOLVE format.         *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
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
! *  ###  08-OCT-99   JD_TO_DATE   v1.4 (c)  L. Petrov  16-MAY-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  JD_TO_DATE*23
      REAL*8     JD
      INTEGER*4  IUER
!
      INTEGER*4  NDAYS, IYEAR, IMONTH, IDAY, IHOUR, IMINUTE, ISECOND, IMSEC, &
     &           K4_YEAR, L4_DAY, J1, J2
      REAL*8     FDAY
      CHARACTER  STR*20
!
      INTEGER*4  MON(12,4), I_YEAR4, NDAYS_MIN, NDAYS_MAX
      REAL*8     JD2000, JD__MIN, JD__MAX
      DATA MON/ &
     &     0,  31,  60,  91, 121, 152, 182, 213, 244, 274, 305, 335, &  !  1 - 12
     &   366, 397, 425, 456, 486, 517, 547, 578, 609, 639, 670, 700, &  ! 13 - 24
     &   731, 762, 790, 821, 851, 882, 912, 943, 974,1004,1035,1065, &  ! 25 - 36
     &  1096,1127,1155,1186,1216,1247,1277,1308,1339,1369,1400,1430/ ! 37 - 48
!
      PARAMETER  ( JD2000    = 2451544.5D0 )
      PARAMETER  ( JD__MIN   = -1.D8       )
      PARAMETER  ( JD__MAX   =  1.D8       )
      PARAMETER  ( I_YEAR4   = 1461        )
      PARAMETER  ( NDAYS_MIN = -18262      )
      PARAMETER  ( NDAYS_MAX =  18263      )
      LOGICAL*4, EXTERNAL ::   IS_R8_NAN 
!
      CALL CLRCH ( JD_TO_DATE )
!
! --- NDAYS -- number of days elapsed from J2000.0 epoch to the midnight 
! --- preceeding JD
!
      IF ( IS_R8_NAN ( JD ) ) THEN
           CALL ERR_LOG ( 1001, IUER, 'JD_TO_DATE', 'Argument JD is '// &
     &         'not a number' )
           RETURN
      END IF
      IF ( JD < JD__MIN  ) THEN
           WRITE ( UNIT=STR, FMT='(1PD20.10)' ) JD
           CALL CHASHL ( STR ) 
           CALL ERR_LOG ( 1002, IUER, 'JD_TO_DATE', 'Argument JD is '// &
     &         'too small: '//STR )
           RETURN
      END IF
      IF ( JD > JD__MAX  ) THEN
           WRITE ( UNIT=STR, FMT='(1PD20.10)' ) JD
           CALL CHASHL ( STR ) 
           CALL ERR_LOG ( 1004, IUER, 'JD_TO_DATE', 'Argument JD is '// &
     &         'too big: '//STR )
           RETURN
      END IF
      NDAYS = JD - JD2000
      IF ( (JD - JD2000) .LT. 0.0D0 ) NDAYS = NDAYS - 1 ! Skip 00-JAN-2000
!
      FDAY = ( JD - JD2000 - NDAYS ) ! fraction of the day
!
! --- Take care the fraction should be positive. Correct if necessary
!
      IF ( FDAY .GE. 1.0D0  ) THEN
           FDAY  = FDAY  - 1.0
           NDAYS = NDAYS + 1
      END IF
      IF ( FDAY .LT. 0.0D0 ) THEN
           FDAY  = FDAY  + 1.0
           NDAYS = NDAYS + 1
      END IF
!
! --- check whetehr the date is in the range of valid dates
!
      IF ( NDAYS .LT. NDAYS_MIN ) THEN
           WRITE ( UNIT=STR, FMT='(1PD20.10)' ) JD
           CALL CHASHL ( STR ) 
           CALL ERR_LOG ( 1005, IUER, 'JD_TO_DATE', 'Argument JD is '// &
     &         'too small: '//STR )
!           j1 = 801; write ( 6, * ) 'mon = ', mon(j1,j1) ! %%% in order to see the stack
           RETURN
         ELSE IF ( NDAYS .GT. NDAYS_MAX ) THEN
           WRITE ( UNIT=STR, FMT='(1PD20.10)' ) JD
           CALL CHASHL ( STR ) 
           CALL ERR_LOG ( 1006, IUER, 'JD_TO_DATE', 'Argument JD is '// &
     &         'too big: '//STR )
           RETURN
      END IF
!
! --- Compute hours, minuts seconds and fractions of seconds
!
      IHOUR   = FDAY*24
      IMINUTE = FDAY*1440.0  - IHOUR*60
      ISECOND = FDAY*86400.0 - IHOUR*3600 - IMINUTE*60
      IMSEC   = NINT( (FDAY*86400.0 - IHOUR*3600 - IMINUTE*60 - ISECOND )*1000)
      IF ( IMSEC .GE. 1000 ) THEN
           IMSEC = IMSEC - 1000
           ISECOND = ISECOND + 1
      END IF
      IF ( ISECOND .GE. 60 ) THEN
           ISECOND = ISECOND - 60 
           IMINUTE = IMINUTE + 1
      END IF
      IF ( IMINUTE .GE. 60 ) THEN
           IMINUTE = IMINUTE - 60 
           IHOUR   = IHOUR + 1
      END IF           
      IF ( IHOUR .GE. 24 ) THEN
           IHOUR = IHOUR - 24
           NDAYS = NDAYS + 1
      END IF
!
! --- K4 -- Number of four-year cycles elapsed from J2000.0
! --- L4 -- number of days elapsed from the beginning of a four-year cycle
!
      K4_YEAR = (NDAYS+1)/I_YEAR4
      L4_DAY  = (NDAYS+1) - I_YEAR4*K4_YEAR
!        write ( 6, * ) ' k4_year=',k4_year ! %%%
      IF ( L4_DAY .LT. 0 ) THEN
           K4_YEAR = K4_YEAR - 1
           L4_DAY  = L4_DAY + I_YEAR4
      END IF
!
      DO 410 J1=1,4
         DO 420 J2=1,12
            IF ( L4_DAY .LT. MON(J2,J1) ) GOTO 810
  420    CONTINUE
  410 CONTINUE
      J1=4
      J2=13
  810 CONTINUE
      IMONTH = J2-1
!
      IF ( IMONTH .EQ. 0 ) THEN
           J1=J1-1
           IMONTH=12
      END IF
      IYEAR = 2000 + 4*K4_YEAR+J1-1
      IDAY  = L4_DAY - MON(IMONTH,J1)
!
! --- Three corrections if the date fell at the beginning the month
!
      IF ( J1 .EQ. 1  .AND.  IMONTH .EQ. 1  .AND.  IDAY .EQ. 0 ) THEN
           IYEAR = 2000 + (K4_YEAR-1)*4 + 3
           IDAY = 31
           IMONTH = 12
      END IF
!
      IF ( IDAY .EQ. 0  .AND.  IMONTH .NE. 1 ) THEN
           IDAY   = MON(IMONTH,J1) - MON(IMONTH-1,J1)
           IMONTH = IMONTH-1
      END IF
!
      IF ( IDAY .EQ. 0  .AND.  IMONTH .EQ. 1 ) THEN
           IYEAR = IYEAR-1
           IDAY  = MON(IMONTH,J1) - MON(12,J1-1)
           IMONTH = 12
      END IF
!
      WRITE ( UNIT=JD_TO_DATE, FMT=110 ) IYEAR, IMONTH, IDAY, IHOUR, &
     &                                   IMINUTE, ISECOND, IMSEC
 110  FORMAT ( I4, ".", I2, ".", I2, "-", I2, ":", I2, ":", I2, ".", I3 )
      CALL BLANK_TO_ZERO ( JD_TO_DATE )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  JD_TO_DATE  #!#
