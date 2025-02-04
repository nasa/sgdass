      SUBROUTINE FERR2(IERR,DESCRP,INER,JNER,IGO)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
!
! 1.  FERR2 PROGRAM SPECIFICATION
!
! 1.1 Based on ferr. Writes error messages.
!      Basically the same thing as ferr, but makes pausing/stopping optional.
!
! 1.2 REFERENCES:
!
! 2.  FERR2 INTERFACE
!
! 2.1 Parameter File
      INCLUDE "solve.i"
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IERR,INER,JNER,IGO
      CHARACTER*(*) DESCRP
!
! DESCRP - Descriptive part of message
! IERR - Error number
! INER - Error number to ignore
! JNER - If > 0, then only report error numbers < 0
! IGO - bit 1 - foreground mode handling -
!            cleared - pause
!            set - go on
!       bit 2 background mode handling
!            cleared - stop
!            set - go on
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE "precm.i"
      INCLUDE "socom.i"
      INCLUDE "ba2cm.i"
      INCLUDE "glbc4.i"
      INCLUDE 'fclib.i'
      INTEGER*4 IRET
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: pname
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 LUOP,LOGLU,IDUM,NAME(3)
      CHARACTER*5 SNAME
      character*(NAME_SIZE) fname
      INTEGER*2 IL, ERROR, TRIMLEN
      logical*2 kbit
      LOGICAL*4  LEX
      INTEGER*4 I4P0, I4P1, I4P2, I13
      DATA  I4P0, I4P1, I4P2/ 0, 1, 2 /
!
      EQUIVALENCE (SNAME,NAME)
!
! IDUM - Dummy variable for call to LOGLU
! LUOP - LU of the operator terminal
! NAME,SNAME - Name of the program currently executing
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   KDB  910906  Created from ferr
!   AEE  920204  Removed hard coded path for fclib.i
!   AEE  920220  check bit 6 of word #2 in pre_ip (from precm.i) to see if
!                foreground or background. No longer using use_common.
!   kdb  960510  Only try to remove the expanded control file if batch is
!                the caller, because the cfname variable comes from ba2cm.i,
!                which is exclusive to batch. (This fixes a range error which
!                occurred sporadically, since cfname was sometimes set to
!                blanks, causing the error, but was usually set to non-
!                printable characters.)
!   kdb  970317  Separate action (stop/pause vs. go on) for foreground and
!                background modes.
!
! 5.  FERR2 PROGRAM STRUCTURE
!
! First check for condition in which message is not needed
!
      IF(IERR.EQ.0) RETURN
      IF(INER.EQ.IERR) RETURN
      IF(JNER.GT.0.AND.IERR.GT.0) RETURN
!
! Otherwise, format the message and display it
!
      LUOP=LOGLU(IDUM)
      CALL PNAME(NAME )
      IL = TRIMLEN(DESCRP)
      IF ( IL .EQ. 0 ) IL=1
      WRITE(LUOP,1) SNAME,": ERROR ",IERR," ",DESCRP(1:IL)
1     FORMAT(A,A,I7,A,A)
      FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'ERRF'//PRE_LETRS
      INQUIRE ( FILE=FNAME, EXIST = LEX )
      IF ( LEX ) THEN
           OPEN ( UNIT=13, FILE=FNAME, STATUS='OLD', ACCESS='APPEND', &
     &            IOSTAT=I13 )
         ELSE
           OPEN ( UNIT=13, FILE=FNAME, STATUS='NEW', IOSTAT=I13 )
      END IF
      IF ( I13 .EQ. 0 ) THEN
           WRITE ( 13, 1 ) SNAME,": ERROR ",IERR," ",DESCRP(1:IL)
           CLOSE ( 13 )
      END IF
!
      if ( kbit( PRE_IP(2), INT2(6))  ) then  ! background
        if (.not. kbit( igo, INT2(2) )) then
          if (sname.eq.'BATCH') then
            if(index(cfname,'.XPND') .EQ. 0) then
              cfname= cfname(:trimlen(cfname))//'.XPND'
            endif
            ERROR=FC_UNLINK( PTR_CH(CFNAME(:trimlen(cfname))//char(0)) )
          else
            write(6,"('Unable to remove your .XPND file.')")
            write(6,"('Please remove it manually.')")
          endif
!@           I13 = -1; WRITE ( 6, * ) NAME(I13) ! Deliberate crash
          iret=fc_pause(I4P0,' ')
        endif
      else                      ! foreground; bit 6 = 1
        if (.not. kbit( igo, INT2(1) )) then
!@           I13 = -1; WRITE ( 6, * ) NAME(I13) ! Deliberate crash
          if (kbeep) then
            iret=fc_pause(I4P2,' ')
          else
            iret=fc_pause(I4P1,' ')
          endif
        endif
      end if
      RETURN
      END
