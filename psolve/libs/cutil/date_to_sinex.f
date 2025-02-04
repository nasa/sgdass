      SUBROUTINE DATE_TO_SINEX ( DATE_IN, DATE_OUT, IUER )
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
! *                                                                      *
! *   Routine  DATE_TO_SINEX  transforms dates from Solve character      *
! *   format to SINEX character format.                                  *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * DATE_IN  ( CHARACTER ) -- Date in Solve character format             *
! *                           yyyy.mm.dd:hh:mm:ss.sssss                  *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * DATE_OUT ( CHARACTER ) -- Date in Sinex character format:            *
! *                           yy:DDD:SSSSS (where DDD is a day of year,  *
! *                           SSSSS are seconds from a midnight).        *
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
! *  ### 29-MAR-2002  DATE_TO_SINEX v1.0 (c) L. Petrov  29-MAR-2002 ###  *
! *                                                                      *
! ************************************************************************
      CHARACTER  DATE_IN*(*), DATE_OUT*(*)
      INTEGER*4  IUER
      INTEGER*4  IER, MJD, MJD_0, IDAY, ISEC
      REAL*8     SEC
!
      CALL ERR_PASS ( IUER, IER )
      CALL DATE_TO_TIME ( DATE_IN, MJD, SEC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2801, IUER, 'DATE_TO_SINEX', 'Error in '// &
     &         'date '//DATE_IN )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL DATE_TO_TIME ( DATE_IN(1:5)//'01.01'//DATE_IN(11:), MJD_0, SEC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2802, IUER, 'DATE_TO_SINEX', 'Error in '// &
     &         'date '//DATE_IN(1:5)//'01.01'//DATE_IN(11:) )
           RETURN
      END IF
      IDAY = MJD-MJD_0+1
      ISEC = SEC
      WRITE ( UNIT=DATE_OUT, FMT='(A2,":",I3,":",I5)' ) DATE_IN(3:4), IDAY, ISEC
      CALL BLANK_TO_ZERO ( DATE_OUT )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DATE_TO_SINEX   #!#
