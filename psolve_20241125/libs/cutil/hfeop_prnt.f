      SUBROUTINE HFEOP_PRNT(hfeopcal)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SHFEOP PROGRAM SPECIFICATION
!
! 1.1 Copy a high-frequency earth orientation calibration file to the spool file
!
! 1.2 REFERENCES:
!
! 2.  SHFEOP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      character*(*) hfeopcal
!
!
! 2.3 OUTPUT Variables:
!
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: gerot
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 ios
      CHARACTER ERRSTR*150, BUFSTR*100, LNAME*128
!
! 4.  HISTORY
!   WHO   WHEN     WHAT
!   mwh  930507  Created
!   jmg  930624  Put in error calls ferr on unexpected EOF and
!                not enough values in read statement
!   pet  990706  Removed unused variables and added END operator
!   pet  2000.03.14  Added capacity to specify a full path name in the variable
!                    HFEOPCAL. If HFEOPCAL contains a symbol "/" then it is
!                    considered as a full path. If it doesn't have "/" then
!                    $SAVE_DIR path is prepended before the name.
!
! 5.  SHFEOP PROGRAM STRUCTURE
!
! Get values from the earth orientation data substitution file
!
! *** OPEN EARTH ORIENTATION CALIBRATION FILE
!
      IF ( HFEOPCAL(1:4) .EQ. 'NONE' .OR. HFEOPCAL .EQ. ' ' .OR. &
     &     .NOT.KSPOOL ) THEN
          RETURN
      ENDIF
!
      KHFEOP = 1
      CALL CLRCH ( LNAME )
      IF ( INDEX ( HFEOPCAL, '/' ) .GT. 0 ) THEN
           LNAME = HFEOPCAL
         ELSE
           LNAME = PRE_SAV_DIR(:PRE_SV_LEN)//HFEOPCAL
      END IF
 1    CONTINUE
!
      OPEN ( 40, IOSTAT=IOS, FILE=LNAME, STATUS='OLD' )
      IF ( IOS.NE.0 ) THEN
           WRITE ( ERRSTR, &
     &       "('HFEOP_PRNT: Failure in opening HF E.O. cal file ',A)" ) LNAME
           CALL FERR ( INT2(179), ERRSTR, INT2(0), INT2(0) )
           GOTO 1
      ENDIF
!
      call use_spool('O' )
      WRITE(23,5514) hfeopcal
 5514 FORMAT(" HF Earth orientation calibration values from file ",A20)
!
!     READ FIRST RECORD OF SUBSTITUTION FILE
!
      bufstr(1:1) = '*'
      do while (.TRUE.)
        READ(40,FMT=102,END=999,IOSTAT=ios ) bufstr
        call ferr( INT2(IOS), "Reading HF E.O. cal file", INT2(0), INT2(0) )
        write(23,102) bufstr
 102    FORMAT(A)
      enddo
!
!  ***   CLOSE THE CALIBRATION FILE.
  999 CLOSE(40)
!      call use_spool('C')
!     call use_glbfil_4('W')
!
      RETURN
      END  !#!  HFEOP_PRNT  #!#
