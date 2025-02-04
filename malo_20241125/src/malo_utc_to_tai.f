      SUBROUTINE MALO_UTC_TO_TAI ( MALO, MJD, UTC, TAI, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine MALO_UTC_TO_TAI find time TAI which corresponds  *
! *   to the value of function UTC(t) at epoch MJD. Or by another        *
! *   words, it finds the leap second which corresponds to the date      *
! *   of interest, subtracts it from UTC and get TAI.                    *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      MALO ( RECORD    ) -- Object which keeps configuration and data *
! *                           related to VLBI Theoretical Delay (MALO)   *
! *                           package.                                   *
! *      MJD ( INTEGER*4 ) -- Modified Julian date which corresponds to  *
! *                           the UTC calendar date of the midnight of   * 
! *                           the day of interest.                       *
! *      UTC ( REAL*8    ) -- Value of the function UTC(T) which         *
! *                           corresponds the the moment of interest.    *
! *                           Units: seconds.                            *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      TAI ( REAL*8    ) -- Time in TAI in seconds elapsed from        *
! *                           midnight.                                  *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
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
! * ### 29-JAN-2004  MALO_UTC_TO_TAI  v1.1 (c) L. Petrov 14-JUN-2012 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      TYPE     ( MALO__TYPE ) :: MALO
      INTEGER*4  MJD, IUER
      REAL*8     UTC, TAI
      INTEGER*4  J1, IER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Check the leap second status
!
      IF ( MALO%LEAPSEC%STATUS .NE. MALO__LOAD ) THEN
           CALL ERR_LOG ( 2371, IUER, 'MALO_UTC_TO_TAI', 'Leap second file '// &
     &         'has not been yet loaded' ) 
           RETURN 
      END IF
!
! --- Check whether the requested data falls into the range dates for which
! --- the leap second is defined
!
      IF ( MJD .LT. MALO%LEAPSEC%MJD_LPS(1) ) THEN
           IER = -1
           WRITE ( 6, * ) ' MJD=',MJD, ' UTC=',UTC
           WRITE ( 6, * ) ' MALO%LEAPSEC%MJD_LPS(1) = ', MALO%LEAPSEC%MJD_LPS(1) 
           CALL ERR_LOG ( 2372, IUER, 'MALO_UTC_TO_TAI', 'Requested date '// &
     &          MJDSEC_TO_DATE ( MJD, UTC, IER )//' is too early. '//        &
     &          'Leap second file '// &
     &          MALO%LEAPSEC%FINAM_LEAPSEC(1:I_LEN(MALO%LEAPSEC%FINAM_LEAPSEC))// &
     &          ' has information only for the range [ '//  &
     &          MJDSEC_TO_DATE ( MALO%LEAPSEC%MJD_LPS(1),                    &
     &                           MALO%LEAPSEC%TAI_LPS(1), IER )//' , '//      &
     &          MJDSEC_TO_DATE ( MALO%LEAPSEC%MJD_LPS(MALO%LEAPSEC%L_LPS),    &
     &                           MALO%LEAPSEC%TAI_LPS(MALO%LEAPSEC%L_LPS), IER )// &
     &                           ' ]' )
           RETURN 
      END IF
!
      IF ( MJD .GT. MALO%LEAPSEC%MJD_LPS(MALO%LEAPSEC%L_LPS) ) THEN
           IER = -1
           CALL ERR_LOG ( 2373, IUER, 'MALO_UTC_TO_TAI', 'Requested date '// &
     &          MJDSEC_TO_DATE ( MJD, UTC, IER )//' is too late. '//         &
     &          'Leap second file '// &
     &          MALO%LEAPSEC%FINAM_LEAPSEC(1:I_LEN(MALO%LEAPSEC%FINAM_LEAPSEC))// &
     &          ' has information only for the range [ '//  &
     &          MJDSEC_TO_DATE ( MALO%LEAPSEC%MJD_LPS(1),                    &
     &                           MALO%LEAPSEC%TAI_LPS(1), IER )//' , '//      &
     &          MJDSEC_TO_DATE ( MALO%LEAPSEC%MJD_LPS(MALO%LEAPSEC%L_LPS),    &
     &                           MALO%LEAPSEC%TAI_LPS(MALO%LEAPSEC%L_LPS), IER )// &
     &                           ' ]' )
           RETURN 
      END IF
!
! --- Search for leap second
!
      DO 410 J1=1,MALO%LEAPSEC%L_LPS
         IF ( MJD .GE. MALO%LEAPSEC%MJD_LPS(J1) ) THEN
              TAI = UTC - MALO%LEAPSEC%UTC_M_TAI(J1)
            ELSE 
              GOTO 810
         END IF
 410  CONTINUE 
 810  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN 
      END  SUBROUTINE  MALO_UTC_TO_TAI  !#!#
