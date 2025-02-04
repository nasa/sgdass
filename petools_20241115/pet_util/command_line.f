      SUBROUTINE GET_COMMAND ( COMMAND, LENGTH, STATUS )
! ************************************************************************
! *                                                                      *
! *   Get the command-line **AFTER PROCESSING BY SHELL**.                *
! *                                                                      *
! *  ### 29-JAN-2003   GET_COMMAND  v1.0 (c) L. Petrov  29-JAN-2003 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  COMMAND*(*)
      INTEGER*4  LENGTH, STATUS
      INTEGER*4  IB, J1
      INTEGER*4, EXTERNAL :: ILEN
#ifdef GNU
      INTEGER*4, INTRINSIC :: IARGC
#else 
      INTEGER*4, EXTERNAL  :: IARGC
      EXTERNAL   GETARG
#endif
      IB = 1
      CALL CLRCH ( COMMAND )
      DO 410 J1=0,IARGC()
          CALL GETARG ( J1, COMMAND(IB:) )
          LENGTH = ILEN(COMMAND)
          IB = LENGTH + 2
 410  CONTINUE
      STATUS = 0
      RETURN
      END  !#!  GET_COMMAND  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_ALL_ARGUMENTS ( COMMAND, LENGTH, STATUS )
! ************************************************************************
! *                                                                      *
! *   Get the a string with all arguments, except the name of the        *
! *   program **AFTER PROCESSING IT BY SHELL**                           *
! *                                                                      *
! * ### 29-JAN-2003 GET_ALL_ARGUMENTS v1.0 (c) L. Petrov 25-MAY-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  COMMAND*(*)
      INTEGER*4  LENGTH, STATUS
      INTEGER*4  IB, J1
      INTEGER*4, EXTERNAL :: ILEN
#ifdef GNU
      INTEGER*4, INTRINSIC :: IARGC
#else 
      INTEGER*4, EXTERNAL  :: IARGC
      EXTERNAL   GETARG
#endif
!
      IB = 1
      CALL CLRCH ( COMMAND )
      IF ( IARGC() == 0 ) THEN
           LENGTH = 0
         ELSE
           DO 410 J1=1,IARGC()
              CALL GETARG ( J1, COMMAND(IB:) )
              LENGTH = ILEN(COMMAND)
              IB = LENGTH + 2
 410       CONTINUE
      END IF
      STATUS = 0
      RETURN
      END  !#!  GET_ALL_ARGUMENTS  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_COMMAND_ARGUMENT ( NUMBER, VALUE, LENGTH, STATUS )
! ************************************************************************
! *                                                                      *
! *   Get the command argument. Arguments are counted from 1.            *
! *   IF NUMBER is - then the routine returns the name of the executable *
! *   file wtih fill path.                                               *
! *                                                                      *
! * # 29-JAN-2003 GET_COMMAND_ARGUMENT v1.0 (c)  L. Petrov 29-JAN-2003 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  VALUE*(*)
      INTEGER*4  NUMBER, LENGTH, STATUS
      INTEGER*4, EXTERNAL :: ILEN
#ifdef GNU
      INTEGER*4, INTRINSIC :: IARGC
#else 
      INTEGER*4, EXTERNAL  :: IARGC
      EXTERNAL   GETARG
#endif
!
      IF ( NUMBER .GE. 0  .AND.  NUMBER .LE. IARGC() ) THEN
           CALL GETARG ( NUMBER, VALUE )
           LENGTH = ILEN(VALUE)
           STATUS = 0
         ELSE
           STATUS = 1
      END IF
!
      RETURN
      END  !#!  GET_COMMAND_ARGUMENT  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   COMMAND_ARGUMENT_COUNT ()
! ************************************************************************
! *                                                                      *
! *   Return the number of command-line arguments.                       *
! *                                                                      *
! *  ### 29-JAN-2003               v1.0 (c)  L. Petrov  29-JAN-2003 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  COMMAND_ARGUMENT_COUNT
#ifdef GNU
      INTEGER*4, INTRINSIC :: IARGC
#else 
      INTEGER*4, EXTERNAL  :: IARGC
#endif
      COMMAND_ARGUMENT_COUNT = IARGC()
      RETURN
      END  !#!   COMMAND_ARGUMENT_COUNT
