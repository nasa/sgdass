      FUNCTION   VEX_TO_DATE ( STR_VEX, IUER )
! ************************************************************************
! *                                                                      *
! *   Function VEX_TO_DATE  transforms the date in the VEX format to     *
! *   the date in ISO 8600 (Solve) format.                               *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       STR_VEX ( CHARACTER ) -- Date and time in Vex format.          *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <VEX_TO_DATE> ( CHARACTER ) -- Date and time in SOLVE format.        *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                      Input: IUER=0 -- no error message will be       *
! *                                       printed even in the case       *
! *                                       of error.                      *
! *                             IUER=-1,-2,-3 -- in the case of error    *
! *                                       the message will be put on     *
! *                                       stdout.                        *
! *                             IUER=-3 -- in the case of error after    *
! *                                        printing the error message    *
! *                                        the program will terminate.   *
! *                       Output:                                        *
! *                             if input value of IUER =-2,-3 -- IUER    *
! *                                        is not modified.              *
! *                             otherwise, the output value of IUER is 0 *
! *                             in the case of successful and            *
! *                             positive non-zero in the case of errors. *
! *                                                                      *
! *  ### 12-FEB-2007   VEX_TO_DATE v1.0 (c)  L. Petrov  12-FEB-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  VEX_TO_DATE*19
      CHARACTER  STR_VEX*(*)
      CHARACTER  STR_YEAR*21, STR_NEW*30
      INTEGER*4  IUER
      INTEGER*4  MJD_YEAR, IDAY, IER
      REAL*8     TIM_YEAR
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      IF ( STR_VEX(5:5) .NE. 'y' ) THEN
           CALL ERR_LOG ( 1671, IUER, 'VEX_TO_DATE', 'Wrong string format: '// &
     &         'letter "y" for year is absent' )
           RETURN 
      END IF
! 
      IF ( STR_VEX(9:9) .NE. 'd' ) THEN
           CALL ERR_LOG ( 1672, IUER, 'VEX_TO_DATE', 'Wrong string format: '// &
     &         'letter "d" for day is absent' )
           RETURN 
      END IF
! 
      IF ( STR_VEX(12:12) .NE. 'h' ) THEN
           CALL ERR_LOG ( 1673, IUER, 'VEX_TO_DATE', 'Wrong string format: '// &
     &         'letter "h" for hour is absent' )
           RETURN 
      END IF
! 
      IF ( STR_VEX(15:15) .NE. 'm' ) THEN
           CALL ERR_LOG ( 1674, IUER, 'VEX_TO_DATE', 'Wrong string format: '// &
     &         'letter "m" for monute is absent' )
           RETURN 
      END IF
! 
      IF ( STR_VEX(18:18) .NE. 's' ) THEN
           CALL ERR_LOG ( 1675, IUER, 'VEX_TO_DATE', 'Wrong string format: '// &
     &         'letter "s" for second is absent' )
           RETURN 
      END IF
!
      STR_YEAR = STR_VEX(1:4)//'.01.01_00:00:00.0'
      CALL ERR_PASS ( IUER, IER )
      CALL DATE_TO_TIME ( STR_YEAR, MJD_YEAR, TIM_YEAR, IER )
      IF ( IER .NE. 0 .AND. IER .NE. -2 ) THEN
           CALL ERR_LOG ( 1676, IUER, 'VEX_TO_DATE', 'Error in transforming '// &
     &         'the date string '//STR_YEAR )
           RETURN 
      END IF
      TIM_YEAR = IDNINT ( TIM_YEAR*1.D4 )/10000
      CALL CHIN ( STR_VEX(6:8), IDAY )
      IF ( IDAY .LE. 0 .OR. IDAY > 366 ) THEN
           CALL ERR_LOG ( 1677, IUER, 'VEX_TO_DATE', 'Wrong field day in '// &
     &         'the vex date string '//STR_VEX )
           RETURN 
      END IF
!
      STR_NEW = MJDSEC_TO_DATE ( MJD_YEAR + IDAY - 1, TIM_YEAR, IUER )
      VEX_TO_DATE = STR_NEW(1:10)//'_'//STR_VEX(10:11)//'_'//STR_VEX(13:14)// &
     &              '_'//STR_VEX(16:17)
      RETURN
      END  FUNCTION   VEX_TO_DATE  !#!#
