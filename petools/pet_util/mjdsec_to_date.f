      FUNCTION   MJDSEC_TO_DATE ( MJD, SEC, IUER )
! ************************************************************************
! *                                                                      *
! *   Function  MJDSEC_TO_DATE  transforms the date specified by a pair  *
! *   of MJD and SEC to the internal SOLVE format of                     *
! *   date representation:   yyyy.mm.dd-hh:mm:ss.ppp                     *
! *                                                                      *
! *   For example:                                                       *
! *                                                                      *
! *   1999.10.08-09:11:23.0123456789                                     *
! *                                                                      *
! *   The length of the line is 30 symbols what gives precision up to    *
! *   one tentch of a nanosecond.                                        *
! *   The date should be in the range [33282, 69808]                     *
! *   (1950.0 - 2050.0).                                                 *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *              MJD ( INTEGER*4 ) -- Integer MJD at the midnight of the *
! *                                   date under consideration.          *
! *              SEC ( REAL*8    ) -- Time in seconds from the midnight. *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <MJDSEC_TO_DATE> ( CHARACTER ) -- Date and time in SOLVE format.     *
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
! *  ### 08-OCT-1999  MJDSEC_TO_DATE v2.1 (c) L. Petrov 10-NOV-2008 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  MJDSEC_TO_DATE*30, JD_TO_DATE*23
      INTEGER*4  MJD, IUER
      CHARACTER  STR_SEC*16, STR*32, STR1*32
      REAL*8     SEC, J2000__JD   
      INTEGER*4  IDAY, IER
      INTEGER*4  J2000__MJD  
      PARAMETER  ( J2000__MJD =   51544     ) ! 2000.01.01_00:00:00
      PARAMETER  ( J2000__JD  = 2451545.0D0 ) ! 2000.01.01_12:00:00
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      CALL ERR_PASS ( IUER, IER )
      MJDSEC_TO_DATE = JD_TO_DATE ( J2000__JD + (MJD-J2000__MJD) + &
     &                              SEC/86400.0D0 - 0.5D0, IER )
      IF ( IER > 0 ) THEN
           WRITE ( UNIT=STR,  FMT='(I12)' ) MJD
           WRITE ( UNIT=STR1, FMT='(1PD19.12)' ) SEC
           CALL ERR_LOG ( 1021, IUER, 'MJDSEC_TO_DATE', 'Wrong input '// &
     &         'arguments: MJD: '//STR(1:I_LEN(STR))//' SEC : '//STR1 )
           RETURN 
      END IF
!
! --- Get more pricise fractional part of seconds
!
      IDAY = SEC/86400.0D0
      IF ( SEC < 0.0D0 ) IDAY = IDAY - 1
      WRITE ( UNIT=STR_SEC, FMT='(F16.10)', IOSTAT=IER ) SEC - IDAY*86400.0D0
!
      IF ( IER .NE. 0 ) THEN
           MJDSEC_TO_DATE(24:30) =  '       '
         ELSE 
           IF ( MJDSEC_TO_DATE(21:21) .EQ. STR_SEC(7:7) ) THEN
                MJDSEC_TO_DATE(21:30) =  STR_SEC(7:16)
                CALL ERR_LOG ( 0, IUER )
                RETURN 
              ELSE 
!
! ------------- There is a disagreement in the first fractinoal digit
! ------------- after the decimal dot
!
                CALL ERR_PASS ( IUER, IER )
                MJDSEC_TO_DATE = JD_TO_DATE ( J2000__JD + (MJD-J2000__MJD) + &
     &                                        SEC/86400.0D0 - 0.5D0 + &
     &                                        0.001D0/86400.0D0, &
     &                                        IER )
                IF ( IER > 0 ) THEN
                     MJDSEC_TO_DATE = '$$$ Wrong date $$$'
                     WRITE ( UNIT=STR,  FMT='(I12)' ) MJD
                     WRITE ( UNIT=STR1, FMT='(1PD19.12)' ) SEC
                     CALL ERR_LOG ( 1022, IUER, 'MJDSEC_TO_DATE', 'Wrong input '// &
              &          'arguments: MJD: '//STR(1:I_LEN(STR))//' SEC : '//STR1 )
                     RETURN 
                END IF
           END IF
!
           IF ( MJDSEC_TO_DATE(21:21) .NE. STR_SEC(7:7) ) THEN
!
! ------------- Let us try to play an opposite side
!
                CALL ERR_PASS ( IUER, IER )
                MJDSEC_TO_DATE = JD_TO_DATE ( J2000__JD + (MJD-J2000__MJD) + &
     &                                        SEC/86400.0D0 - 0.5D0 - &
     &                                        0.001D0/86400.0D0, &
     &                                        IER )
                IF ( IER > 0 ) THEN
                     MJDSEC_TO_DATE = '$$$ Wrong date $$$'
                     WRITE ( UNIT=STR,  FMT='(I12)' ) MJD
                     WRITE ( UNIT=STR1, FMT='(1PD19.12)' ) SEC
                     CALL ERR_LOG ( 1023, IUER, 'MJDSEC_TO_DATE', 'Wrong input '// &
              &          'arguments: MJD: '//STR(1:I_LEN(STR))//' SEC : '//STR1 )
                     RETURN 
                END IF
!
                IF ( MJDSEC_TO_DATE(21:21) .NE. STR_SEC(7:7) ) THEN
!
! ------------------ Nothing help?? Hmmm. Let us put blanks to that place.
!
                     MJDSEC_TO_DATE(24:30) =  '       '
                   ELSE 
                     MJDSEC_TO_DATE(21:30) =  STR_SEC(7:16)
                END IF
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  FUNCTION   MJDSEC_TO_DATE  !#!#
