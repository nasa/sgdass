      SUBROUTINE FERRX ( IERR )
      IMPLICIT NONE
!
! 1.  FERR PROGRAM SPECIFICATION
!
! 1.1 Generate an error message, if needed, resulting from received signal.
!      PAUSE the program after displaying the message.
!
! 1.2 REFERENCES:
!
! 2.  FERR INTERFACE
!
! 2.1 Parameter File
      INCLUDE "solve.i"
      INCLUDE "fclib.i"
!
! 2.2 INPUT Variables:
!
      INTEGER*4 IERR
!
! IERR - Error number
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE "precm.i"
      INCLUDE "socom.i"
      INCLUDE "ba2cm.i"
      INTEGER*4 IRET
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: pname
!
! 3.  LOCAL VARIABLES
!
      INTEGER NAME(3)
      CHARACTER*5 SNAME
      CHARACTER  FNAME*(NAME_SIZE), DESCRP*(NAME_SIZE)
      INTEGER*2  ERROR, TRIMLEN, LEN
      INTEGER*4  I13
      LOGICAL*4  LEX
      LOGICAL*2 KBIT
!
      EQUIVALENCE (SNAME,NAME)
!
! IDUM - Dummy variable for call to LOGLU
! LUOP - LU of the operator terminal
! NAME,SNAME - Name of the program currently executing
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE  910614  Added background/foreground detection.
!   AEE  910913  Added code to purge *.XPND control file before terminating.
!   AEE  920204  Removed hard coded path for fclib.i
!   AEE  920220  check bit 6 of word #2 in pre_ip (from precm.i) to see if
!                foreground or background. No longer using use_common.
!   kdb  960510  Only try to remove the expanded control file if batch is
!                the caller, because the cfname variable comes from ba2cm.i,
!                which is exclusive to batch. (This fixes a range error which
!                occurred sporadically, since cfname was sometimes set to
!                blanks, causing the error, but was usually set to non-
!                printable characters.)
!   pet  2003.05.15  Cosmetic improvements. Manual conversion I2 --> I4
!
! 5.  FERR PROGRAM STRUCTURE
!
! First check for condition in which message is not needed
!
      IF(IERR.EQ.0) RETURN
!
! Otherwise, format the message and display it
!
      IF ( IERR .EQ. 1 ) THEN
           DESCRP = 'Hangup.'
        ELSE IF ( IERR .EQ. 2 ) THEN
           DESCRP = 'User Interrupt.'
        ELSE IF ( IERR .EQ. 3 ) THEN
           DESCRP = 'Bus Error.'
        ELSE IF ( IERR .EQ. 4 ) THEN
           DESCRP = 'User quit.'
        ELSE IF ( IERR .EQ. 5 ) THEN
           DESCRP = 'Segmentation Violation.'
      ENDIF
!
      CALL PNAME(NAME)
      WRITE ( *, 1 ) SNAME,": ERROR ",IERR," ",DESCRP
1     FORMAT(A,A,I7,A,A)
      LEN = TRIMLEN ( PRE_SCR_DIR )
      FNAME = PRE_SCR_DIR(1:LEN)//'ERRF'//PRE_LETRS
      INQUIRE ( FILE=FNAME, EXIST = LEX )
      IF ( LEX ) THEN
           OPEN ( UNIT=13, FILE=FNAME, STATUS='OLD', ACCESS='APPEND', &
     &            IOSTAT=I13 )
         ELSE
           OPEN ( UNIT=13, FILE=FNAME, STATUS='NEW', IOSTAT=I13 )
      END IF
!
      IF ( I13 .EQ. 0 ) THEN
           WRITE ( 13, 1 ) SNAME,": ERROR ",IERR," ",DESCRP
           CLOSE ( 13 )
      END IF
!
      if ( KBIT( PRE_IP(2), INT2(6) ) ) then  ! background
           IF ( SNAME .EQ. 'BATCH' ) THEN
                IF ( INDEX ( CFNAME, '.XPND' ) .EQ. 0 ) THEN
                      CFNAME= CFNAME(:TRIMLEN(CFNAME))//'.XPND'
                ENDIF
                ERROR=FC_UNLINK( PTR_CH(CFNAME(:TRIMLEN(CFNAME))//CHAR(0)) )
              ELSE
                WRITE ( 6, * ) 'Unable to remove your .XPND file.'
                WRITE ( 6, * ) 'Please remove it manually.'
           ENDIF
           IRET=FC_PAUSE( 0, ' ')
         ELSE                      ! foreground; bit 6 = 1
!           CALL CURLIB_SET_TERM ( PTR_CH (PRE_SCR_DIR(:PRE_SD_LEN)//'term'// &
!     &                            PRE_LETRS//CHAR(0)) )
           IRET=FC_PAUSE( 0, ' ' )
      END IF
      END  !#!  FERRX  #!#
