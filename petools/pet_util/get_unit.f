      FUNCTION   GET_UNIT ()
! ************************************************************************
! *                                                                      *
! *   Function GET_UNIT  returns the Fortran input/output unit which     *
! *   is free (not used now).                                            *
! *                                                                      *
! *  ### 15-SEP-2000    GET_UNIT   v1.2 (c)  L. Petrov  04-DEC-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  GET_UNIT
      LOGICAL*4  FLAG
      INTEGER*4  J1
!
!$OMP CRITICAL ( GET_UNIT_BLOCK )
      GET_UNIT = -1
      DO 410 J1=41,511
         INQUIRE ( UNIT=J1, OPENED=FLAG )
         IF ( .NOT. FLAG ) THEN
              GET_UNIT = J1
              GOTO 810
         END IF
 410  CONTINUE
 810  CONTINUE 
!$OMP END CRITICAL ( GET_UNIT_BLOCK )
      RETURN
      END  !#!  GET_UNIT  #!#
