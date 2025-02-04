      SUBROUTINE RD_IERS_C04 ( FINAM, MP, NP, JD, XP_VAL, XP_ERR, YP_VAL, &
     &           YP_ERR, U1_VAL, U1_ERR, DPSI_VAL, DPSI_ERR, &
     &           DEPS_VAL, DEPS_ERR, CH_FLAG, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine RD_IERS_C04  reads EOP file in IERS C04 format and         *
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
! *                           coordinates. Units: rae, dimension: NP.    *
! *   YP_VAL ( REAL*8    ) -- Array of Y pole coordinates. Units: rad,   *
! *                           dimension: NP.                             *
! *   YP_ERR ( REAL*8    ) -- Array of formal uncertainties of Y pole    *
! *                           coordinates. Units: rad, dimension: NP.    *
! *   U1_VAL ( REAL*8    ) -- Array of UT1-UTC angles. Units: sec of     *
! *                           time, dimension: NP.                       *
! *   U1_ERR ( REAL*8    ) -- Array of formal uncertainties of UT1-UTC   *
! *                           angles. Units: rad, dimension: NP.         *
! * DPSI_VAL ( REAL*8    ) -- Array of nutation in longitude angles.     *
! *                           Units: rad, dimension: NP.                 *
! * DPSI_ERR ( REAL*8    ) -- Array of formal uncertainties of nutation  *
! *                           in longitude angles. Units: rad,           *
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
! *  ### 03-NOV-2000  RD_IERS_C04  v1.2 (c)  L. Petrov  18-JUL-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'ners.i'
      INTEGER*4  MP, NP, IUER
      REAL*8     JD(MP), XP_VAL(MP), XP_ERR(MP), YP_VAL(MP), YP_ERR(MP), &
     &           U1_VAL(MP), U1_ERR(MP), DPSI_VAL(MP), DPSI_ERR(MP), &
     &           DEPS_VAL(MP), DEPS_ERR(MP)
      CHARACTER  FINAM*(*), CH_FLAG(MP)*(*)
      TYPE ( NERS__TYPE ) :: NERS
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 32*1024 )
      CHARACTER  BUF(MBUF)*196, STR1*80, STR2*80
      CHARACTER  GET_CDATE*19
      REAL*8     SEC, LOD, UTC_VAL, MJD_R8, UTC_M_TAI
      REAL*8     XP_SIG, YP_SIG, U1_SIG, DPSI_SIG, DEPS_SIG
      PARAMETER  ( XP_SIG   = 0.0002D0   )
      PARAMETER  ( YP_SIG   = 0.0002D0   )
      PARAMETER  ( U1_SIG   = 0.000015D0 )
      PARAMETER  ( DPSI_SIG = 0.0005D0   )
      PARAMETER  ( DEPS_SIG = 0.0002D0   )
      REAL*8      PI, PI2, P2I, ARCSEC_TO_RAD, SEC_TO_RAD, MAS_TO_RAD
      PARAMETER ( PI=3.141592653589793D0, PI2=2.D0*PI, P2I=PI/2.D0 ) ! PI number
      PARAMETER ( ARCSEC_TO_RAD = PI/(180.D0*3600.D0)        )
      PARAMETER ( MAS_TO_RAD    = PI/(180.D0*3600.D0*1000.0) )
      PARAMETER ( SEC_TO_RAD    = PI2/86400.D0               )
      INTEGER*4  MJD, NBUF, IO, MJD_BEG, MJD_END, MJD_VAL, IFMT, J1, IER
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
! --- Read the file with external EOP series
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FINAM, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8211, IUER, 'RD_IERS_C04', 'Error in reading '// &
     &         'file '//FINAM )
           RETURN
      END IF
!
      IF ( BUF(1)(1:29) == '# EARTH ORIENTATION PARAMETER' ) THEN
           IFMT = 1
         ELSE 
           IFMT = 2
      END IF
!
      NP = 0
      DO 410 J1=1,NBUF
         IO = 0
         IF ( ILEN(BUF(J1))  .LE.  1  ) GOTO 410
         IF ( IFMT == 1 ) THEN
               IF ( BUF(J1)(24:24) .NE. '.' ) GOTO 410
               IF ( BUF(J1)(32:32) .NE. '.' ) GOTO 410
               READ ( UNIT=BUF(J1)(19:26), FMT='(F8.2)' ) MJD_R8
               IF ( MJD_R8 .LT. MJD_BEG  .OR.  MJD_R8 .GT. MJD_END ) GOTO 410
               MJD = NINT ( MJD_R8 )
!
               NP = NP + 1
               JD(NP) = 2400000.5D0 + MJD_R8
               READ ( UNIT=BUF(J1)(30:38), FMT='(F9.5)',  IOSTAT=IO ) XP_VAL(NP)
               IF ( IO .NE. 0 ) GOTO 710
               READ ( UNIT=BUF(J1)(42:50), FMT='(F9.5)',  IOSTAT=IO ) YP_VAL(NP)
               IF ( IO .NE. 0 ) GOTO 710
               READ ( UNIT=BUF(J1)(53:62), FMT='(F10.5)', IOSTAT=IO ) U1_VAL(NP)
               IF ( IO .NE. 0 ) GOTO 710
               READ ( UNIT=BUF(J1)(113:122), FMT='(F10.5)', IOSTAT=IO ) LOD
               IF ( IO .NE. 0 ) GOTO 710
               READ ( UNIT=BUF(J1)(66:74), FMT='(F9.5)',  IOSTAT=IO ) DPSI_VAL(NP)  ! X"
               IF ( IO .NE. 0 ) GOTO 710
               READ ( UNIT=BUF(J1)(78:86), FMT='(F9.5)',  IOSTAT=IO ) DEPS_VAL(NP)  ! Y"
               IF ( IO .NE. 0 ) GOTO 710
            ELSE IF ( IFMT == 2 ) THEN
               IF ( BUF(J1)(24:24) .NE. '.' ) GOTO 410
               IF ( BUF(J1)(58:58) .NE. '.' ) GOTO 410
               IF ( BUF(J1)(70:70) .NE. '.' ) GOTO 410
               CALL CHIN ( BUF(J1)(15:19), MJD )
               IF ( MJD .LT. MJD_BEG  .OR.  MJD .GT. MJD_END ) GOTO 410
!
               NP = NP + 1
               JD(NP) = 2400000.5D0 + MJD
               READ ( UNIT=BUF(J1)(22:30), FMT='(F9.5)',  IOSTAT=IO ) XP_VAL(NP)
               IF ( IO .NE. 0 ) GOTO 710
               READ ( UNIT=BUF(J1)(33:41), FMT='(F9.5)',  IOSTAT=IO ) YP_VAL(NP)
               IF ( IO .NE. 0 ) GOTO 710
               READ ( UNIT=BUF(J1)(44:53), FMT='(F10.5)', IOSTAT=IO ) U1_VAL(NP)
               IF ( IO .NE. 0 ) GOTO 710
               READ ( UNIT=BUF(J1)(56:65), FMT='(F10.5)', IOSTAT=IO ) LOD
               IF ( IO .NE. 0 ) GOTO 710
               READ ( UNIT=BUF(J1)(68:76), FMT='(F9.5)',  IOSTAT=IO ) DPSI_VAL(NP)
               IF ( IO .NE. 0 ) GOTO 710
               READ ( UNIT=BUF(J1)(79:87), FMT='(F9.5)',  IOSTAT=IO ) DEPS_VAL(NP)
               IF ( IO .NE. 0 ) GOTO 710
         END IF
!
         XP_VAL(NP) = XP_VAL(NP)*ARCSEC_TO_RAD
         YP_VAL(NP) = YP_VAL(NP)*ARCSEC_TO_RAD
         DPSI_VAL(NP) = DPSI_VAL(NP)*ARCSEC_TO_RAD
         DEPS_VAL(NP) = DEPS_VAL(NP)*ARCSEC_TO_RAD
!
! ------ Set formal errors. IERS C04 eop file does not provide formal
! ------ uncertainties. We set them to some predefined values.
!
         XP_ERR(NP) = XP_SIG*ARCSEC_TO_RAD
         YP_ERR(NP) = YP_SIG*ARCSEC_TO_RAD
         U1_ERR(NP) = U1_SIG*SEC_TO_RAD
         DPSI_ERR(NP) = DPSI_SIG*MAS_TO_RAD
         DEPS_ERR(NP) = DEPS_SIG*MAS_TO_RAD
!
! ------ Get leap seconds values: values of function UTC minus TAI
!
         IUER = -1
         CALL GET_UTC_M_TAI ( NERS, MJD, 0.0D0, UTC_M_TAI, IUER )
         U1_VAL(NP) = U1_VAL(NP) + UTC_M_TAI
         U1_VAL(NP) = U1_VAL(NP)*SEC_TO_RAD
         CH_FLAG(NP) = 'I'
 410  CONTINUE
 710  CONTINUE
!
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR1 )
           CALL INCH  ( IO, STR1 )
           CALL CLRCH ( STR2 )
           CALL INCH  ( J1, STR2 )
           CALL ERR_LOG ( 8213, IUER, 'RD_IERS_C04', 'Error '// &
     &          STR1(1:I_LEN(STR1))//' at the '//STR2(1:I_LEN(STR2))// &
     &          '-th line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the IERS '// &
     &          'C04 eop file '//FINAM )
           RETURN
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  RD_IERS_C04  #!#
