      SUBROUTINE TLEDATE_TO_MJDSEC ( STR_TLE_EPOCH, MJD, SEC )
!
! ***************************************************************************************
! *                                                                                     *
! *   Subroutine TLEDATE_TO_MJDSEC reads the epoch from the TLE file and converts it    *
! *   to MJD and UTC                                                                    *
! *                                                                                     *
! *   INPUT:                                                                            *
! *           STR_TLE_EPOCH = Epoch from TLE file                                       *
! *                                                                                     *
! *   OUTPUT:                                                                           *
! *                                                                                     *
! *           MJD        =  Mean Julian Date                          { INT }           *
! *                                                                                     *
! *           UTC        =  Universal Coordinated Time                { REAL }          
! *                                                                                     *
! *  ###  16-NOV-2021    TLEDATE_TO_MJDSEC    v1.0 (c)    N. Habana  16-NOV-2021   ###  *
! *                                                                                     *
! ***************************************************************************************
!
      IMPLICIT    NONE
      CHARACTER   STR_TLE_EPOCH*14, STR*16
      INTEGER*4   MJD, TLE_YR, IUER, IER
      REAL*8      TLE_DATE, SEC
      CHARACTER   CYEAR*8, CMONTH*8,CDAY*8,CHRS*8,CMINS*8,CSECS*8
      INTEGER*4   IYEAR, IMONTH, IDAY
      INTEGER*4   NDAYS, NSECS
      REAL*8      RSECS, DEC_SECS
      INTEGER*4   IHRS, IMINS, ISECS, IDEC_SECS
      CHARACTER   DATE_CHR*26, CDEC_SECS*32
!
! --- Get the year
!
      READ( UNIT=STR_TLE_EPOCH(1:2), FMT='(I2)', IOSTAT=IER )  IYEAR
      IF ( IER .NE. 0 ) THEN
         CALL CLRCH ( STR )
         CALL IINCH ( IER, STR)
         CALL ERR_LOG ( 2202, IUER, 'TLEDATE_TO_MJDSEC',                &
     &           'Error converting IYEAR. Not INT, as expected. '//     &
     &           'IOSTAT = '//TRIM(STR) )
         RETURN
      END IF
      IF ( IYEAR .LT. 57 ) THEN
         IYEAR = 2000 + IYEAR
      ELSE
         IYEAR = 1900 + IYEAR
      END IF
!
! --- Get the number of days in the year and seconds elapsed for 
!
      READ( UNIT=STR_TLE_EPOCH(3:5),  FMT='(I3)', IOSTAT=IER )   NDAYS
      IF ( IER .NE. 0 ) THEN
         CALL CLRCH ( STR )
         CALL IINCH ( IER, STR)
         CALL ERR_LOG ( 2203, IUER, 'TLEDATE_TO_MJDSEC',                &
     &           'Error converting NDAYS. Not INT, as expected. '//     &
     &           'IOSTAT = '//TRIM(STR) )
         RETURN
      END IF
!
      READ( UNIT=STR_TLE_EPOCH(6:14), FMT='(F9.8)', IOSTAT=IER ) RSECS
      IF ( IER .NE. 0 ) THEN
         CALL CLRCH ( STR )
         CALL IINCH ( IER, STR)
         CALL ERR_LOG ( 2204, IUER, 'TLEDATE_TO_MJDSEC',                &
     &           'Error converting RSECS. Not REAL, as expected. '//    &
     &           'IOSTAT = '//TRIM(STR) )
         RETURN
      END IF
!
      DATE_CHR = '20'//STR_TLE_EPOCH(1:2)//'.01.01_00:00.00000000'
      CALL ERR_PASS ( IUER, IER )
      CALL DATE_TO_TIME ( DATE_CHR, MJD, SEC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2201, IUER, 'TLEDATE_TO_MJDSEC',              &
     &                    'Error in coverting date to string' )
           RETURN
      END IF
      MJD = MJD + NDAYS - 1
      SEC = RSECS*86400.D0
!
!@      RSECS = RSECS*86400.D0
!@      NSECS = INT(RSECS)
!@      DEC_SECS = RSECS - NSECS
!@      IDEC_SECS = INT(DEC_SECS*1000000, 4)   ! 6 decimal places of the seconcs
!@!
!@! --- Given a leap year
!@!
!@      IF ( MOD(IYEAR,4) .EQ. 0 ) THEN
!@         IF ( (NDAYS .GT. 0) .AND. (NDAYS .LE. 31) ) THEN         ! january
!@            IMONTH = 1
!@            IDAY   = 31 - NDAYS
!@         ELSEIF ( (NDAYS .GT. 31)  .AND. (NDAYS .LE. 60) ) THEN   ! February
!@            IMONTH = 2
!@            IDAY   = 60 - NDAYS
!@         ELSEIF ( (NDAYS .GT. 60)  .AND. (NDAYS .LE. 91) ) THEN   ! March
!@            IMONTH = 3
!@            IDAY   = 91 - NDAYS
!@         ELSEIF ( (NDAYS .GT. 91)  .AND. (NDAYS .LE. 121) ) THEN  ! April
!@            IMONTH = 4
!@            IDAY   = 121 - NDAYS
!@         ELSEIF ( (NDAYS .GT. 121) .AND. (NDAYS .LE. 152) ) THEN  ! May
!@            IMONTH = 5
!@            IDAY   = 152 - NDAYS
!@         ELSEIF ( (NDAYS .GT. 152) .AND. (NDAYS .LE. 182) ) THEN  ! June
!@            IMONTH = 6
!@            IDAY   = 182 - NDAYS
!@         ELSEIF ( (NDAYS .GT. 182) .AND. (NDAYS .LE. 213) ) THEN  ! July
!@            IMONTH = 7
!@            IDAY   = 213 - NDAYS
!@         ELSEIF ( (NDAYS .GT. 213) .AND. (NDAYS .LE. 244) ) THEN  ! August
!@            IMONTH = 8
!@            IDAY   = 244 - NDAYS
!@         ELSEIF ( (NDAYS .GT. 244) .AND. (NDAYS .LE. 274) ) THEN  ! September
!@            IMONTH = 9
!@            IDAY   = 274- NDAYS
!@         ELSEIF ( (NDAYS .GT. 274) .AND. (NDAYS .LE. 305) ) THEN  ! October
!@            IMONTH = 10
!@            IDAY   = 305 - NDAYS
!@         ELSEIF ( (NDAYS .GT. 305) .AND. (NDAYS .LE. 335) ) THEN  ! November
!@            IMONTH = 11
!@            IDAY   = 335 - NDAYS
!@         ELSEIF ( (NDAYS .GT. 335) .AND. (NDAYS .LE. 366) ) THEN  ! December
!@            IMONTH = 12
!@            IDAY   = 366 - NDAYS
!@         END IF
!@      ELSE 
!@         IF ( (NDAYS .GT. 0) .AND. (NDAYS .LE. 31) ) THEN         ! january
!@            IMONTH = 1
!@            IDAY   = 31 - NDAYS
!@         ELSEIF ( (NDAYS .GT. 31)  .AND. (NDAYS .LE. 59) ) THEN   ! February
!@            IMONTH = 2
!@            IDAY   = 59 - NDAYS
!@         ELSEIF ( (NDAYS .GT. 59)  .AND. (NDAYS .LE. 90) ) THEN   ! March
!@            IMONTH = 3
!@            IDAY   = 90 - NDAYS
!@         ELSEIF ( (NDAYS .GT. 90)  .AND. (NDAYS .LE. 120) ) THEN  ! April
!@            IMONTH = 4
!@            IDAY   = 120 - NDAYS
!@         ELSEIF ( (NDAYS .GT. 120) .AND. (NDAYS .LE. 151) ) THEN  ! May
!@            IMONTH = 5
!@            IDAY   = 151 - NDAYS
!@         ELSEIF ( (NDAYS .GT. 151) .AND. (NDAYS .LE. 181) ) THEN  ! June
!@            IMONTH = 6
!@            IDAY   = 181 - NDAYS
!@         ELSEIF ( (NDAYS .GT. 181) .AND. (NDAYS .LE. 212) ) THEN  ! July
!@            IMONTH = 7
!@            IDAY   = 212 - NDAYS
!@         ELSEIF ( (NDAYS .GT. 212) .AND. (NDAYS .LE. 243) ) THEN  ! August
!@            IMONTH = 8
!@            IDAY   = 243 - NDAYS
!@         ELSEIF ( (NDAYS .GT. 243) .AND. (NDAYS .LE. 273) ) THEN  ! September
!@            IMONTH = 9
!@            IDAY   = 273 - NDAYS
!@         ELSEIF ( (NDAYS .GT. 273) .AND. (NDAYS .LE. 304) ) THEN  ! October
!@            IMONTH = 10
!@            IDAY   = 304 - NDAYS
!@         ELSEIF ( (NDAYS .GT. 304) .AND. (NDAYS .LE. 334) ) THEN  ! November
!@            IMONTH = 11
!@            IDAY   = 334 - NDAYS
!@         ELSEIF ( (NDAYS .GT. 334) .AND. (NDAYS .LE. 365) ) THEN  ! December
!@            IMONTH = 12
!@            IDAY   = 365 - NDAYS
!@         END IF
!@      END IF
!@!
!@! --- Convert seconds to Hrs, Mins, Secs
!@!
!@      IHRS   =  NSECS/3600
!@      IMINS  =  (MOD(NSECS,3600))/60
!@      ISECS  =  NSECS - IHRS*3600 - IMINS*60
!@!
!@! --- Convert times to character
!@!
!@      CALL CLRCH ( CYEAR )
!@      CALL CLRCH ( CMONTH )
!@      CALL CLRCH ( CDAY )
!@      CALL CLRCH ( CHRS )
!@      CALL CLRCH ( CMINS )
!@      CALL CLRCH ( CSECS ) 
!@!
!@      CALL IINCH ( IYEAR,  CYEAR  )
!@      CALL IINCH ( IMONTH, CMONTH )
!@      CALL IINCH ( IDAY,   CDAY   )
!@      CALL IINCH ( IHRS,   CHRS   )
!@      CALL IINCH ( IMINS,  CMINS  )
!@      CALL IINCH ( ISECS,  CSECS  )
!@      CALL IINCH ( IDEC_SECS, CDEC_SECS )
!@!
!@! --- for unit numbers add a leading zero
!@!
!@      IF ( IMONTH .LT. 10 ) THEN
!@         CMONTH = '0'//TRIM(CMONTH)
!@      END IF
!@      IF ( IDAY .LT. 10 ) THEN
!@         CDAY = '0'//TRIM(CDAY)
!@      END IF
!@      IF ( IHRS .LT. 10 ) THEN
!@         CHRS = '0'//TRIM(CHRS)
!@      END IF
!@      IF ( IMINS .LT. 10 ) THEN
!@         CMINS = '0'//TRIM(CMINS)
!@      END IF
!@      IF ( ISECS .LT. 10 ) THEN
!@         CSECS = '0'//TRIM(CSECS)
!@      END IF
!@!
!@! --- Convert to SOLVE date format
!@!
!@      CALL CHASHL ( CYEAR )
!@      CALL CLRCH ( DATE_CHR )
!@      DATE_CHR = CYEAR(1:1)//CYEAR(3:5)//'.'//CMONTH(1:2)//'.'//        &
!@     &           CDAY(1:2)//'_'//CHRS(1:2)//':'//CMINS(1:2)//':'//      &
!@     &           CSECS(1:2)//'.'//CDEC_SECS(1:3)//CDEC_SECS(5:7)
!@!
!@! --- Convert to MJD and UTC
!@!
!@      IUER = -1
!@      CALL DATE_TO_TIME ( DATE_CHR, MJD, SEC, IUER )
!@      IF ( IUER .NE. 0 ) THEN
!@           CALL ERR_LOG ( 2201, IUER, 'TLEDATE_TO_MJDSEC',              &
!@     &                    'Error in coverting date to string' )
!@           RETURN
!@      END IF
!@!      
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  TLEDATE_TO_MJDSEC  !#!#
