      SUBROUTINE RD_FINALS ( FINAM, MP, NP, JD, XP_VAL, XP_ERR, YP_VAL, &
     &           YP_ERR, U1_VAL, U1_ERR, DPSI_VAL, DPSI_ERR, &
     &           DEPS_VAL, DEPS_ERR, CH_FLAG, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine RD_FINALS  reads EOP file in USNO finals.dat format and    *
! *   extracts EOP series from there. It ignores data with dates out of  *
! *   range [1979.5, now + 100 days].                                    *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    FINAM ( CHARACTER ) -- EOP file name                              *
! *       MP ( INTEGER*4 ) -- Maximim number of points allowed to be in  *
! *                           the EOP file                               *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *       NP ( INTEGER*4 ) -- Actual number of points read from the EOP  *
! *                           file which are in the dates range.         *
! *       JD ( REAL*8    ) -- Array of Julian dates for EOP. Units: days,*
! *                           dimension: NP.                             *
! *   XP_VAL ( REAL*8    ) -- Array of X pole coordinates. Units: rad,   *
! *                           dimension: NP.                             *
! *   XP_ERR ( REAL*8    ) -- Array of formal uncertainties of X pole    *
! *                           coordinates. Units: rad, dimension: NP.    *
! *   YP_VAL ( REAL*8    ) -- Array of Y pole coordinates. Units: rad,   *
! *                           dimension: NP.                             *
! *   YP_ERR ( REAL*8    ) -- Array of formal uncertainties of Y pole    *
! *                           coordinates. Units: rad, dimension: NP.    *
! *   U1_VAL ( REAL*8    ) -- Array of UT1-UTC angles. Units: sec of     *
! *                           time, dimension: NP.                       *
! *   U1_ERR ( REAL*8    ) -- Array of formal uncertainties of UT1-UTC   *
! *                           angles. Units: sec of time, dimension: NP. *
! * DPSI_VAL ( REAL*8    ) -- Array of nutation in longitude angles.     *
! *                           Units: rad, dimension: NP.                 *
! * DPSI_ERR ( REAL*8    ) -- Array of formal uncertainties of nutation  *
! *                           in longitude angles. Units: rad           *
! *                           dimension: NP.                             *
! * DEPS_VAL ( REAL*8    ) -- Array of nutation in obliquity angles.     *
! *                           Units: rad, dimension: NP.                 *
! * DEPS_ERR ( REAL*8    ) -- Array of formal uncertainties of nutation  *
! *                           in obliquity angles. Units: rad,           *
! *                           dimension: NP.                             *
! *  CH_FLAG ( CHARACTER ) -- Flag of the data:                          *
! *                           'I' -- data derived from observations;     *
! *                           'P' -- predicted data.                     *
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
! *  ### 03-NOV-2000    RD_FINALS  v1.3 (c)  L. Petrov  18-OCT-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'ners.i'
      INTEGER*4  MP, NP, IUER
      REAL*8     JD(MP), XP_VAL(MP), XP_ERR(MP), YP_VAL(MP), YP_ERR(MP), &
     &           U1_VAL(MP), U1_ERR(MP), DPSI_VAL(MP), DPSI_ERR(MP), &
     &           DEPS_VAL(MP), DEPS_ERR(MP)
      CHARACTER  FINAM*(*), CH_FLAG(MP)*(*)
      TYPE ( NERS__TYPE ) :: NERS
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 32768 )
      CHARACTER  BUF(MBUF)*196, STR1*80, STR2*80
      REAL*8     MJD_R8, SEC, UTC_VAL, UTC_M_TAI
      LOGICAL*4  FL_BULB
      INTEGER*4  NBUF, IO, MJD_BEG, MJD_END, MJD_VAL, J1, IER
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Set the earliest and the latest dates for which the series will be
! --- read.
! --- The ealiest date if 01-JUL-1979, the latest date is 100 days after
! --- the current date
!
      CALL DATE_TO_TIME ( '1979.07.01:00:00:00.0', MJD_BEG, SEC, -3 )
      CALL DATE_TO_TIME ( GET_CDATE(), MJD_END, SEC, -3 )
      MJD_END = MJD_END + 100
!
! --- Read the file with USNO series
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FINAM, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8211, IUER, 'RD_FINALS', 'Error in reading '// &
     &         ' file '//FINAM )
           RETURN
      END IF
!
      NP = 0
      DO 410 J1=1,NBUF
         IF ( ILEN(BUF(J1)(21:27)) .GT. 0 ) THEN
!
! ----------- This logic is fo deciding dfrom which columns to take the data:
! ----------- from the right or from the left?
!
              IF ( ILEN(BUF(J1)(135:185)) .EQ. 0 ) THEN
                   FL_BULB = .FALSE.  ! left columns will be used.
                 ELSE
                   FL_BULB = .TRUE.   ! right columns will be used
              END IF
!
! ----------- Important!!! It was discovered in May 2001, that right columns
! ----------- contains pseudo-bulletine B ans SHOULD BE AVOIDED.
!
              FL_BULB = .FALSE.  ! We use only left coumns of USNO file
              NP = NP + 1
!
! ----------- Get the date
!
              READ ( UNIT=BUF(J1)(8:15), FMT='(F8.4)', IOSTAT=IO ) MJD_R8
              IF ( IO .NE. 0 ) GOTO 710
              IF ( MJD_R8 .LT. MJD_BEG   .OR.  MJD_R8 .GT. MJD_END ) THEN
!
! ---------------- We don't include the point which isoutside the range
! ---------------- [MJD_BEG, MJD_END]
!
                   NP = NP-1
                   GOTO 410
              END IF
              JD(NP) = 2400000.5D0 + MJD_R8
              MJD_VAL = IDINT ( MJD_R8 )
              UTC_VAL = MJD_R8 - MJD_VAL
!
              CALL ERR_PASS ( IUER, IER )
              CALL GET_UTC_M_TAI ( NERS, MJD_VAL, UTC_VAL, UTC_M_TAI, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8213, IUER, 'RD_FINALS', 'Error in '// &
     &                 'reading computation UTC minus TAI for the date '// &
     &                 BUF(J1)(8:15) )
                   RETURN
              END IF
!
! ----------- Get X pole coordinates
!
              IF ( FL_BULB ) THEN
                 READ ( UNIT=BUF(J1)(135:144), FMT='(F10.5)', IOSTAT=IO ) XP_VAL(NP)
                ELSE
                 READ ( UNIT=BUF(J1)(19:27),   FMT='(F9.4)',  IOSTAT=IO ) XP_VAL(NP)
              END IF
              IF ( IO .NE. 0 ) GOTO 710
              XP_VAL(NP) = XP_VAL(NP)/RAD__TO__ARCSEC
!
              READ ( UNIT=BUF(J1)(28:36), FMT='(F9.4)', IOSTAT=IO ) XP_ERR(NP)
              IF ( IO .NE. 0 ) GOTO 710
              XP_ERR(NP) = XP_ERR(NP)/RAD__TO__ARCSEC
!
! ----------- Get Y pole coordinates
!
              IF ( FL_BULB ) THEN
                 READ ( UNIT=BUF(J1)(145:154), FMT='(F9.5)', IOSTAT=IO ) YP_VAL(NP)
                ELSE
                 READ ( UNIT=BUF(J1)(38:46),   FMT='(F9.5)', IOSTAT=IO ) YP_VAL(NP)
              END IF
              IF ( IO .NE. 0 ) GOTO 710
              YP_VAL(NP) = YP_VAL(NP)/RAD__TO__ARCSEC
!
              READ ( UNIT=BUF(J1)(47:55), FMT='(F9.5)', IOSTAT=IO ) YP_ERR(NP)
              IF ( IO .NE. 0 ) GOTO 710
              YP_ERR(NP) = YP_ERR(NP)/RAD__TO__ARCSEC
!
! ----------- Get UT1-UTC angle
!
              IF ( FL_BULB ) THEN
                 READ ( UNIT=BUF(J1)(155:165), FMT='(F11.8)', IOSTAT=IO ) U1_VAL(NP)
                ELSE
                 READ ( UNIT=BUF(J1)(59:68),   FMT='(F10.7)', IOSTAT=IO ) U1_VAL(NP)
              END IF
              IF ( IO .NE. 0 ) GOTO 710
              U1_VAL(NP) = U1_VAL(NP)/RAD__TO__SEC
!
              READ ( UNIT=BUF(J1)(69:78), FMT='(F10.7)', IOSTAT=IO ) U1_ERR(NP)
              IF ( IO .NE. 0 ) GOTO 710
              U1_ERR(NP) = U1_ERR(NP)/RAD__TO__SEC
!
! ----------- Transform "UT1 minus UTC" ---> "UT1 minus TAI"
!
              U1_VAL(NP) = U1_VAL(NP) + UTC_M_TAI/RAD__TO__SEC
!
! ----------- Get Dpsi
!
              IF ( FL_BULB ) THEN
                   READ ( UNIT=BUF(J1)(166:175), FMT='(F10.5)', IOSTAT=IO  ) &
     &                    DPSI_VAL(NP)
                 ELSE
                   READ ( UNIT=BUF(J1)(98:106),  FMT='(F9.5)', IOSTAT=IO  ) &
     &                    DPSI_VAL(NP)
              END IF
              IF ( IO .NE. 0 ) GOTO 710
              DPSI_VAL(NP) = DPSI_VAL(NP)/RAD__TO__MAS
!
              READ ( UNIT=BUF(J1)(107:115), FMT='(F9.5)', IOSTAT=IO ) DPSI_ERR(NP)
              IF ( IO .NE. 0 ) GOTO 710
              DPSI_ERR(NP) = DPSI_ERR(NP)/RAD__TO__MAS
!
! ----------- Get Deps
!
              IF ( FL_BULB ) THEN
                   READ ( UNIT=BUF(J1)(176:185), FMT='(F10.5)', IOSTAT=IO ) &
     &                    DEPS_VAL(NP)
                 ELSE
                   READ ( UNIT=BUF(J1)(117:125), FMT='(F9.5)', IOSTAT=IO ) &
     &                    DEPS_VAL(NP)
              END IF
              IF ( IO .NE. 0 ) GOTO 710
              DEPS_VAL(NP) = DEPS_VAL(NP)/RAD__TO__MAS
!
              READ ( UNIT=BUF(J1)(126:134), FMT='(F9.5)', IOSTAT=IO ) DEPS_ERR(NP)
              IF ( IO .NE. 0 ) GOTO 710
!
              DEPS_ERR(NP) = DEPS_ERR(NP)/RAD__TO__MAS
              CH_FLAG(NP) = BUF(J1)(17:17)//BUF(J1)(58:58)//BUF(J1)(96:96)
         END IF
 410  CONTINUE
 710  CONTINUE
!
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR1 )
           CALL INCH  ( IO, STR1 )
           CALL CLRCH ( STR2 )
           CALL INCH  ( J1, STR2 )
           CALL ERR_LOG ( 8213, IUER, 'RD_FINALS', 'Error '// &
     &          STR1(1:I_LEN(STR1))//' at the '//STR2(1:I_LEN(STR2))// &
     &          '-th line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the USNO '// &
     &          'finals eop file '//FINAM )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  RD_FINALS  #!#
