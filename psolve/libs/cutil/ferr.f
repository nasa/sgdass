      SUBROUTINE FERR ( IERR, DESCRP, INER, JNER )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
!
! 1.  FERR PROGRAM SPECIFICATION
!
! 1.1 Generate an error message, if needed.
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
      INTEGER*2 IERR,INER,JNER
      CHARACTER*(*) DESCRP
!
! DESCRP - Descriptive part of message
! IERR - Error number
! INER - Error number to ignore
! JNER - If > 0, then only report error numbers < 0
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE "precm.i"
      INCLUDE "socom.i"
      INCLUDE "ba2cm.i"
      INCLUDE "glbc4.i"
      integer*4 iret
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: pname
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 LUOP,LOGLU,IDUM,NAME(3)
      CHARACTER SNAME*5, STR*32
      character*(NAME_SIZE) fname
      integer*2 error,trimlen,len
      logical*2 kbit
      LOGICAL*4  LEX
      INTEGER*4  I13, IARR(2)
      INTEGER*4, EXTERNAL :: I_LEN
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
!   pet 1999.05.31  Added a line with forcible setting mimnal value of the
!                   length of ERRF file in order to prevent abnormal
!                   termination. Added an additional check of existance of the
!                   ERRF file to reduce probability of the infinite loop.
!
! 5.  FERR PROGRAM STRUCTURE
!
! First check for condition in which message is not needed
!
      IF(IERR.EQ.0) RETURN
      IF(INER.EQ.IERR) RETURN
      IF(JNER.GT.0.AND.IERR.GT.0) RETURN
!
!   Restore screen to original configuration
!
!      if (kbit(pre_ip(2),6)) then
!      call curlib_set_term(ptr_ch(SOLVE_WORK_DIR//'term'//pre_letrs//char(0)))
!      endif
!
! Otherwise, format the message and display it
!
      CALL PNAME(NAME )
      WRITE(*,1) SNAME,": ERROR ",IERR," ",DESCRP
1     FORMAT(A,A,I7,A,A)
      LEN = TRIMLEN ( PRE_SCR_DIR )
      IF ( LEN .LE. 0 ) LEN = 1
      FNAME = PRE_SCR_DIR(1:LEN)//'ERRF'//PRE_LETRS
      INQUIRE ( FILE=FNAME, EXIST = LEX )
      IF ( LEX ) THEN
           OPEN ( UNIT=13, FILE=FNAME, STATUS='OLD', ACCESS='APPEND', &
     &            IOSTAT=I13 )
         ELSE
           OPEN ( UNIT=13, FILE=FNAME, STATUS='NEW', IOSTAT=I13 )
      END IF
      IF ( I13 .EQ. 0 ) THEN
           WRITE ( 13, 1, IOSTAT=I13 ) SNAME,": ERROR ",IERR," ",DESCRP
           CLOSE ( 13 )
      END IF
!
      IF ( .NOT. KBIT( PRE_IP(2), INT2(6) ) ) then  ! background bit 6 = 0
           IF ( SNAME .EQ. 'BATCH' ) THEN
                IF ( INDEX ( CFNAME, '.XPND' ) .EQ. 0 ) THEN
                     CFNAME= CFNAME(:TRIMLEN(CFNAME))//'.XPND'
                END IF
                ERROR = FC_UNLINK( PTR_CH(CFNAME(1:I_LEN(CFNAME))//CHAR(0)) )
           END IF
           CALL GETENVAR ( 'SOLVE_DEBUG', STR )
           IF ( STR(1:3) == 'YES' .OR.  STR(1:2) == 'ON' ) THEN
                WRITE ( 6, * ) ' AA: ',IARR(IERR-20000)
           END IF
!@           I13 = -1; WRITE ( 6, * ) IARR(I13) ! Deliberate crash
           IRET = FC_PAUSE ( 0, ' ' )
         ELSE                      ! FOREGROUND; BIT 6 = 1
!@           I13 = -1; WRITE ( 6, * ) IARR(I13) ! Deliberate crash
           IF ( KBEEP ) THEN
                IRET=FC_PAUSE ( 2, ' ' )
             ELSE
                IRET=FC_PAUSE ( 1, ' ' )
           ENDIF
      END IF
      END  !#!  FERR  #!#
