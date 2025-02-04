      SUBROUTINE GET_NAME_FROM_DIR ( DIR_REC, NAME )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_NAME_FROM_DIR extracts the filename fro the internal  *
! *   data strictire reacted by the system subprograms opendir and       *
! *   readdir.                                                           *
! *                                                                      *
! * ### 12-NOV-1999 GET_NAME_FROM_DIR v2.1 (c) L. Petrov 06-APR-2014 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MAXNAMLEN, MAXNAMLEN_DEF
      CHARACTER  NAME*(*)
      PARAMETER  ( MAXNAMLEN_DEF = 256 )
      INTEGER*1  DIR_REC(*)
      INTEGER*4  D_NAME, ARG_LEN, J1
!
      CALL GET_SYSTEM_CONSTANT ( 'MAXNAMLEN', MAXNAMLEN, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'd_name',    D_NAME,    ARG_LEN )
      IF ( MAXNAMLEN .LE. 0 ) MAXNAMLEN = MAXNAMLEN_DEF
#ifdef GNU
#ifdef ADR_32BIT
!
! --- This is a bug in gfortran-4.3.0 . The compiler ignores 
! --- -D _FILE_OFFSET_BITS=64 -D _LARGEFILE_SOURCE
! --- C routine is compiled with these flags and it assumes 8-bytes addresses
! --- and offsets, while the Fortrn calls readdir ith 32-bit interface
!
      D_NAME = D_NAME - 8
#endif
#endif
!
      CALL CLRCH ( NAME )
      DO 410 J1=1,MAXNAMLEN
         IF ( DIR_REC(D_NAME+J1) .EQ. 0 ) THEN
              GOTO 810
            ELSE
              NAME(J1:J1) = CHAR( DIR_REC(D_NAME+J1) )
         END IF
 410  CONTINUE 
 810  CONTINUE 
!
      RETURN
      END  SUBROUTINE  GET_NAME_FROM_DIR  !#!#
