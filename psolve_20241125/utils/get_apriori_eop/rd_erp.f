      SUBROUTINE RD_ERP ( FINAM, MP, NP, JD, XR_VAL, YR_VAL, UR_VAL, &
     &                    XR_ERR, YR_ERR, UR_ERR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  RD_ERP reads EOP file in erp format and extracts EOP      *
! *   series from there: X pole coordinates, Y ppole coordinates,        *
! *   UT1-TAI as well as their formal uncertainties.                     *
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
! *   U1_VAL ( REAL*8    ) -- Array of UT1-TAI angles. Units: rad,       *
! *                           dimension: NP.                             *
! *   U1_ERR ( REAL*8    ) -- Array of formal uncertainties of UT1-TAI   *
! *                           angles. Units: rad. Dimension: NP.         *
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
! *  ### 03-NOV-2000     RD_ERP   v1.1 (c)  L. Petrov  16-MAY-2006  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INTEGER*4  MP, NP, IUER
      REAL*8     JD(MP), XR_VAL(MP), XR_ERR(MP), YR_VAL(MP), YR_ERR(MP), &
     &           UR_VAL(MP), UR_ERR(MP)
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 16384 )
      CHARACTER  FINAM*(*), BUF(MBUF)*80, STR1*80, STR2*80
      INTEGER*4  NBUF, J1, IO, IUR, IER
      INTEGER*4  I_LEN
!
! --- Read the file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FINAM, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8221, IUER, 'RD_ERP', 'Error in reading '// &
     &         'file '//FINAM )
           RETURN
      END IF
!
      NP = 0
      DO 410 J1=2,NBUF
         IF ( BUF(J1)(1:1) .NE. '#' ) THEN
!
              NP = NP + 1
              READ ( UNIT=BUF(J1)(1:9), FMT='(F9.1)', IOSTAT=IO ) JD(NP)
              IF ( IO .NE. 0 ) GOTO 710
!
              READ ( UNIT=BUF(J1)(11:17), FMT='(F8.5)', IOSTAT=IO ) XR_VAL(NP)
              IF ( IO .NE. 0 ) GOTO 710
              XR_VAL(NP) = XR_VAL(NP)*0.1/RAD__TO__ARCSEC
!
              READ ( UNIT=BUF(J1)(19:25), FMT='(F8.5)', IOSTAT=IO ) YR_VAL(NP)
              IF ( IO .NE. 0 ) GOTO 710
              YR_VAL(NP) = YR_VAL(NP)*0.1/RAD__TO__ARCSEC
!
              READ ( UNIT=BUF(J1)(27:35), FMT='(I9)', IOSTAT=IO ) IUR
              UR_VAL(NP) = IUR*0.000001/RAD__TO__SEC
!
              READ ( UNIT=BUF(J1)(38:42), FMT='(F5.2)', IOSTAT=IO ) XR_ERR(NP)
              IF ( IO .NE. 0 ) GOTO 710
              XR_ERR(NP) = XR_ERR(NP)/RAD__TO__ARCSEC
!
              READ ( UNIT=BUF(J1)(45:49), FMT='(F5.2)', IOSTAT=IO ) YR_ERR(NP)
              IF ( IO .NE. 0 ) GOTO 710
              YR_ERR(NP) = YR_ERR(NP)/RAD__TO__ARCSEC
!
              READ ( UNIT=BUF(J1)(51:57), FMT='(F7.1)', IOSTAT=IO ) UR_ERR(NP)
              UR_ERR(NP) = UR_ERR(NP)*1.D-6/RAD__TO__SEC
              IF ( IO .NE. 0 ) GOTO 710
         END IF
 410  CONTINUE
 710  CONTINUE
!
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR1 )
           CALL INCH  ( IO, STR1 )
           CALL CLRCH ( STR2 )
           CALL INCH  ( J1, STR2 )
           CALL ERR_LOG ( 8222, IUER, 'RD_ERP', 'Error '// &
     &          STR1(1:I_LEN(STR1))//' at the '//STR2(1:I_LEN(STR2))// &
     &          '-th line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the USNO '// &
     &          'finals eop file '//FINAM )
           RETURN
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  RD_ERP #!#
