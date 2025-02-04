      SUBROUTINE PUT9 ( NAME_I2, OUT )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine which prepares a header of mapping setup for     *
! *   spool file.                                                        *
! *                                                                      *
! *  ### 30-MAR-2000     PUT9      v1.0 (c)  L. Petrov  30-MAR-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INTEGER*2  NAME_I2(NAME_WORDS)
      CHARACTER  OUT*10, STR*256
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4  LEN_NAME, ILEN
!
! --- Copy the line to STR
!
      LEN_NAME = NAME_SIZE
      CALL CLRCH ( STR )
      CALL LIB$MOVC3 ( LEN_NAME, NAME_I2, STR )
!
      CALL CLRCH ( OUT )
      IF ( ILEN(STR) .GT. 9 ) THEN
!
! -------- The line is long -- let's take the end of the line
!
           OUT = STR(ILEN(STR)-8:ILEN(STR))
         ELSE
!
! -------- the line is short -- it is good
!
           OUT = STR(1:9)
      END IF
!
      RETURN
      END  !#!  PUT9  #!#
