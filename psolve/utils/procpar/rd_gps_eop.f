      SUBROUTINE RD_GPS_EOP ( FINAM, MP, NP, JD, XP_VAL, XP_ERR, &
     &                  YP_VAL, YP_ERR, UR_VAL, UR_ERR, CH_FLAG, IUER )
! ************************************************************************
! *                                                                      *
! *   
! *                                                                      *
! *  ### 25-JUN-2006               v1.0 (c)  L. Petrov  01-DEC-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  MP, NP, IUER
      REAL*8     JD(MP), XP_VAL(MP), XP_ERR(MP), YP_VAL(MP), YP_ERR(MP), &
     &           UR_VAL(MP), UR_ERR(MP), SEC
      CHARACTER  CH_FLAG(MP)*(*)
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 16384 )
      CHARACTER  FINAM*(*), BUF(MBUF)*160, STR1*80, STR2*80
      LOGICAL*4  FL_LAST_SES
      REAL*8     TAI
!@ REAL*8     DAT(MBUF) ! %%%
      INTEGER*4  NBUF, J1, IO, IUR, MJD, IVAL, IER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      VTD%CONF%FINAM_LEAPSEC = '/vlbi/vtd_data/leapsec.dat'
      CALL ERR_PASS ( IUER, IER ) 
      CALL VTD_LOAD_LEAPSEC ( VTD, MBUF, BUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8221, IUER, 'RD_GPS_EOP', 'Error in an attempt '// &
     &         'to load the leap second file '//VTD%CONF%FINAM_LEAPSEC  )
           RETURN
      END IF
!
! --- Read the file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FINAM, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8222, IUER, 'RD_GPS_EOP', 'Error in reading '// &
     &         'file '//FINAM )
           RETURN
      END IF
!
      NP = 0
      FL_LAST_SES = .FALSE.
      IO = 0
      DO 410 J1=2,NBUF
         IF ( ILEN(BUF(J1)) == 0 ) GOTO 410
!
         IF ( BUF(J1)(1:1) .NE. '#' .AND.  &
     &        BUF(J1)(1:1) .NE. '!' .AND.  &
              BUF(J1)(9:9) .EQ. '.'        ) THEN
!
              NP = NP + 1
              READ ( UNIT=BUF(J1)(4:12), FMT='(F9.3)', IOSTAT=IO ) JD(NP)
              JD(NP) = JD(NP) + 2400000.5D0 
              IF ( IO .NE. 0 ) GOTO 710
!@ DAT(NP) = (JD(NP) - J2000__JD - 0.5D0)/365.25  + 2000.0D0
!
              READ ( UNIT=BUF(J1)(14:22), FMT='(F9.3)', IOSTAT=IO ) XP_VAL(NP)
              IF ( IO .NE. 0 ) GOTO 710
              XP_VAL(NP) = XP_VAL(NP)*ARCSEC__TO__RAD
!
              READ ( UNIT=BUF(J1)(24:32), FMT='(F9.3)', IOSTAT=IO ) YP_VAL(NP)
              IF ( IO .NE. 0 ) GOTO 710
              YP_VAL(NP) = YP_VAL(NP)*ARCSEC__TO__RAD
!
              READ ( UNIT=BUF(J1)(34:43), FMT='(F10.3)', IOSTAT=IO ) UR_VAL(NP) 
              IF ( IO .NE. 0 ) GOTO 710
!
              CALL JD_TO_MJD_SEC  ( JD(NP), MJD, SEC )
              CALL ERR_PASS ( IUER, IER )
              CALL VTD_UTC_TO_TAI ( VTD, MJD, -UR_VAL(NP), TAI, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8223, IUER, 'RD_GPS_ERP', 'Error in '// &
     &                 'an attempt to convert UTC to TAI' )
                   RETURN
              END IF
              UR_VAL(NP) = -TAI*SEC__TO__RAD 
!
              READ ( UNIT=BUF(J1)(72:80), FMT='(F9.3)', IOSTAT=IO ) XP_ERR(NP)
              IF ( IO .NE. 0 ) GOTO 710
              XP_ERR(NP) = XP_ERR(NP)*ARCSEC__TO__RAD
!
              READ ( UNIT=BUF(J1)(82:90), FMT='(F9.3)', IOSTAT=IO ) YP_ERR(NP)
              IF ( IO .NE. 0 ) GOTO 710
              YP_ERR(NP) = YP_ERR(NP)*ARCSEC__TO__RAD
!
              READ ( UNIT=BUF(J1)(92:101), FMT='(F10.3)', IOSTAT=IO ) UR_ERR(NP)
              IF ( IO .NE. 0 ) GOTO 710
              UR_ERR(NP) = UR_ERR(NP)*1.D3*MSEC__TO__RAD
!
           ELSE IF ( BUF(J1)(1:1) .NE. '#' .AND.  &
     &               BUF(J1)(1:1) .NE. '!'  .AND.  &
                     BUF(J1)(6:6) .EQ. '.'         ) THEN
!
              NP = NP + 1
              READ ( UNIT=BUF(J1)(1:8), FMT='(F8.3)', IOSTAT=IO ) JD(NP)
              JD(NP) = JD(NP) + 2400000.5D0 
              IF ( IO .NE. 0 ) GOTO 710
!@ DAT(NP) = (JD(NP) - J2000__JD - 0.5D0)/365.25  + 2000.0D0
!
              READ ( UNIT=BUF(J1)(11:17), FMT='(I7)', IOSTAT=IO ) IVAL
              IF ( IO .NE. 0 ) GOTO 710
              XP_VAL(NP) = IVAL*1.D-6*ARCSEC__TO__RAD
!
              READ ( UNIT=BUF(J1)(20:26), FMT='(I7)', IOSTAT=IO ) IVAL
              IF ( IO .NE. 0 ) GOTO 710
              YP_VAL(NP) = IVAL*1.D-6*ARCSEC__TO__RAD
!
              READ ( UNIT=BUF(J1)(29:37), FMT='(I9)', IOSTAT=IO ) IVAL
              IF ( IO .NE. 0 ) GOTO 710
              UR_VAL(NP) = IVAL*1.D-7
!
              CALL JD_TO_MJD_SEC  ( JD(NP), MJD, SEC )
              CALL ERR_PASS ( IUER, IER )
              CALL VTD_UTC_TO_TAI ( VTD, MJD, -UR_VAL(NP), TAI, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8224, IUER, 'RD_GPS_ERP', 'Error in '// &
     &                 'an attempt to convert UTC to TAI' )
                   RETURN
              END IF
              UR_VAL(NP) = -TAI*SEC__TO__RAD 
!
              READ ( UNIT=BUF(J1)(47:51), FMT='(I5)', IOSTAT=IO ) IVAL
              IF ( IO .NE. 0 ) GOTO 710
              XP_ERR(NP) = IVAL*1.D-6*ARCSEC__TO__RAD
!
              READ ( UNIT=BUF(J1)(55:58), FMT='(I4)', IOSTAT=IO ) IVAL
              IF ( IO .NE. 0 ) GOTO 710
              YP_ERR(NP) = IVAL*1.D-6*ARCSEC__TO__RAD
!
              READ ( UNIT=BUF(J1)(62:68), FMT='(I7)', IOSTAT=IO ) IVAL
              IF ( IO .NE. 0 ) GOTO 710
              UR_ERR(NP) = IVAL*1.D-4*MSEC__TO__RAD
         END IF
 410  CONTINUE
 710  CONTINUE
!
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR1 )
           CALL INCH  ( IO, STR1 )
           CALL CLRCH ( STR2 )
           CALL INCH  ( J1, STR2 )
           CALL ERR_LOG ( 8225, IUER, 'RD_GPS_EOP', 'Error '// &
     &          STR1(1:I_LEN(STR1))//' at the '//STR2(1:I_LEN(STR2))// &
     &          '-th line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the GPS '// &
     &          'finals eop file '//FINAM )
           RETURN
      END IF
!
!@  call    diagi_1 ( nP, DAT, UR_VAL, -2 ) ! %%%%%%%%%%%%%%
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  RD_GPS_EOP #!#
