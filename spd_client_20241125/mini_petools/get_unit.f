      FUNCTION   GET_UNIT ()
! ************************************************************************
! *                                                                      *
! *   Function GET_UNIT  returns the Fortran input/output unit which     *
! *   is free (not used now).                                            *
! *                                                                      *
! *  ### 15-SEP-2000    GET_UNIT   v1.1 (c)  L. Petrov  27-DEC-2014 ###  *
! *                                                                      *
! ************************************************************************
      INTEGER*4  GET_UNIT
      LOGICAL*4  FLAG
!
      GET_UNIT = -1
      DO 410 J1=41,127
         INQUIRE ( UNIT=J1, OPENED=FLAG )
         IF ( .NOT. FLAG ) THEN
              GET_UNIT = J1
              GOTO 810
         END IF
 410  CONTINUE
 810  CONTINUE 
      RETURN
      END  !#!  GET_UNIT  #!#
