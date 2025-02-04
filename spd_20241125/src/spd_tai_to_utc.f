      SUBROUTINE SPD_TAI_TO_UTC ( SPD, MJD, TAI, UTC, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine SPD_TAI_TO_UTC computes the value of UTC         *
! *   function on a moment of time TAI at epoch MJD. Or by another       *
! *   words, it finds the leap second which corresponds to the date      *
! *   of interest, and compute UTC from TAI.                             *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * SPD ( SPD_3D__TYPE   ) -- The object that keeps information          *
! *                           related to computing slanted path delay.   *
! *      MJD ( INTEGER*4 ) -- Modified Julian date which corresponds to  *
! *                           the UTC calendar date of the midnight of   * 
! *                           the day of interest.                       *
! *      TAI ( REAL*8    ) -- Time in TAI in seconds elapsed from        *
! *                           midnight.                                  *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      UTC ( REAL*8    ) -- Value of the function UTC(T) which         *
! *                           corresponds the the moment of interest.    *
! *                           Units: seconds.                            *
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
! * ### 29-JAN-2004  SPD_TAI_TO_UTC  v1.0 (c) L. Petrov  07-JAN-2009 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      TYPE     ( SPD_3D__TYPE ) :: SPD
      INTEGER*4  MJD, IUER
      REAL*8     UTC, TAI
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4  J1, IER
!
! --- Check the leap second status
!
      IF ( SPD%LEAPSEC%STATUS .NE. SPD__LOAD ) THEN
           CALL ERR_LOG ( 2371, IUER, 'SPD_TAI_TO_UTC', 'Leap second file '// &
     &         'has not been yet loaded' ) 
           RETURN 
      END IF
!
! --- Check whether the requested data falls into the range dates for which
! --- the leap second is defined
!
      IF ( MJD .LT. SPD%LEAPSEC%MJD_LPS(1) ) THEN
           IER = -1
           WRITE ( 6, * ) ' MJD=',MJD, ' UTC=',UTC
           WRITE ( 6, * ) ' SPD%LEAPSEC%MJD_LPS(1) = ', SPD%LEAPSEC%MJD_LPS(1) 
           CALL ERR_LOG ( 2372, IUER, 'SPD_TAI_TO_UTC', 'Requested date '// &
     &          MJDSEC_TO_DATE ( MJD, UTC, IER )//' is too early. '//        &
     &          'Leap second file has information only for the range [ '//  &
     &          MJDSEC_TO_DATE ( SPD%LEAPSEC%MJD_LPS(1),                    &
     &                           SPD%LEAPSEC%TAI_LPS(1), IER )//' , '//      &
     &          MJDSEC_TO_DATE ( SPD%LEAPSEC%MJD_LPS(SPD%LEAPSEC%L_LPS),    &
     &                           SPD%LEAPSEC%TAI_LPS(SPD%LEAPSEC%L_LPS), IER )// &
     &                           ' ]' )
           RETURN 
      END IF
!
      IF ( MJD .GT. SPD%LEAPSEC%MJD_LPS(SPD%LEAPSEC%L_LPS) ) THEN
           IER = -1
           CALL ERR_LOG ( 2373, IUER, 'SPD_TAI_TO_UTC', 'Requested date '// &
     &          MJDSEC_TO_DATE ( MJD, TAI, IER )//' is too late. '//         &
     &          'Leap second file has information only for the range [ '//  &
     &          MJDSEC_TO_DATE ( SPD%LEAPSEC%MJD_LPS(1),                    &
     &                           SPD%LEAPSEC%TAI_LPS(1), IER )//' , '//      &
     &          MJDSEC_TO_DATE ( SPD%LEAPSEC%MJD_LPS(SPD%LEAPSEC%L_LPS),    &
     &                           SPD%LEAPSEC%TAI_LPS(SPD%LEAPSEC%L_LPS), IER )// &
     &                           ' ]' )
           RETURN 
      END IF
!
! --- Search for leap second
!
      DO 410 J1=1,SPD%LEAPSEC%L_LPS
         IF ( MJD .GE. SPD%LEAPSEC%MJD_LPS(J1) ) THEN
              UTC = TAI + SPD%LEAPSEC%UTC_M_TAI(J1) 
              SPD%UTC_M_TAI = SPD%LEAPSEC%UTC_M_TAI(J1) 
            ELSE 
              GOTO 810
         END IF
 410  CONTINUE 
 810  CONTINUE 
!
      SPD%UTC_M_TAI_STATUS = SPD__COMP
      CALL ERR_LOG ( 0, IUER )
      RETURN 
      END  SUBROUTINE  SPD_TAI_TO_UTC  !#!#
