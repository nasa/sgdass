      SUBROUTINE SAVE_CFNAME ( CFNAME )
! ************************************************************************
! *                                                                      *
! *   Routine  SAVE_CFNAME  writes down the hostname and the name of the *
! *   control file in file CNTRxx, where xx are user initials.           *
! *                                                                      *
! *  ###  10-MAY-99    SAVE_CFNAME  v1.0 (c)  L. Petrov  10-MAY-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
!
      CHARACTER  CFNAME*(*)
      CHARACTER  SYSNAME*20, NODENAME*20, HARDWARE*20, OUT*200, FINAM*200, &
     &           WORK_DIR*200
      INTEGER*4  I40, IP, ILEN, I_LEN, LINDEX
!
! --- Set the file name where information will be written
!
      CALL CLRCH ( WORK_DIR )
      CALL GETENVAR ( 'PSOLVE_WORK_DIR', WORK_DIR )
      IF ( ILEN(WORK_DIR) .EQ. 0 ) THEN
           WORK_DIR = SOLVE_WORK_DIR
      END IF
!
      CALL CLRCH ( FINAM )
      FINAM = WORK_DIR(1:I_LEN(WORK_DIR))//'/'//'CNTR'//PRE_LETRS
!
! --- Get the hostname (nodename)
!
      CALL CLRCH ( NODENAME )
      CALL GETINFO_SYSTEM ( SYSNAME, NODENAME, HARDWARE )
!
! --- Form the output string
!
      CALL CLRCH ( OUT )
      OUT=NODENAME(1:I_LEN(NODENAME))
      IP = ILEN(OUT)+2
      OUT(IP:) = CFNAME
!
      IP = LINDEX ( OUT, '.XPND' )
      IF ( IP .GT. 0 ) THEN
!
! -------- Get away ".XPND" extension
!
           CALL CLRCH ( OUT(IP:) )
      END IF
!
! --- Write the output string in the file
!
      OPEN  ( UNIT=40, FILE=FINAM, STATUS='UNKNOWN', IOSTAT=I40 )
      WRITE ( UNIT=40, FMT='(A)', IOSTAT=I40 ) OUT(1:I_LEN(OUT))
      CLOSE ( UNIT=40, IOSTAT=I40 )
!
      RETURN
      END  !#!  SAVE_CFNAME  #!#
