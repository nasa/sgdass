      SUBROUTINE USE_OBSFIL(IOBSFIL,IOBS,STRING)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  USE_OBSFIL PROGRAM SPECIFICATION
!
! 1.1 Read or write OBSFIL.  Program pauses if an error is detected.
!
! 1.2 REFERENCES:
!
! 2.  USE_OBSFIL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4 IOBS
      INTEGER*2 IOBSFIL(*)
      CHARACTER*(*) STRING
!
! IOBS - Number of observation to be read or written
! IOBSFIL - Array to be read or written
! STRING - Requested access type ('R'=read; 'W'=write)
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INTEGER*4  FILDES
      INTEGER*8  IOBS_OFF
      CHARACTER*(NAME_SIZE) FNAME
      COMMON  / SAVOBS / FNAME, FILDES, IOBS_OFF
      SAVE    / SAVOBS /
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES:
!       CALLED SUBROUTINES: file_report,bin_seek,bin_read,bin_write
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*10 ME
      INTEGER*8 JPOS
      CHARACTER*(NAME_SIZE) FNAME_TEST
      INTEGER*4, EXTERNAL :: I_LEN
!
      DATA ME / 'USE_OBSFIL' /
!
! JPOS - Position at which to locate in file
! ME - Name of this routine
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   jwr, 951127  Remove the ability to write the obsfil in batch mode,
!    kdb         which can overwrite superfiles.
!   95.12.04:kdb Allow integer*4 number of observations
!   kdb  960229  Fix the 951127 fix.  (That fix left user programs unable to
!                write to the obsfil.)
!
! 5.  USE_OBSFIL PROGRAM STRUCTURE
!
! Make sure STRING is either 'R' or 'W'
!
    1 CONTINUE
!
      IF ( LEN(STRING).NE.1 .OR. INDEX('RW',STRING).EQ.0 ) THEN
           CALL FILE_REPORT(FNAME,ME,'ILLEGAL STRING' )
           GOTO 1
      ENDIF
!
! --- Fix 1:
! --- Check for the illegal combination of writing when in the batch mode.
! --- Otherwise the superfile would be overwritten. Closing a painful
! --- worm hole. jwr,kdb
! --- Fix 2:
! --- The first fix assumed that in batch mode Solve always writes to the
! --- superfile, not the obsfil.  However, this overlooks user programs which
! --- write to the obsfil.  So now this subroutine will just check the name
! --- of the "obsfil" currently open, and if it's OBSFxx, will permit a write.
!
      FNAME_TEST = PRE_SCR_DIR(1:PRE_SD_LEN)//'OBSF'//PRE_LETRS
      IF ( FNAME .NE. FNAME_TEST  .AND.  STRING .EQ. 'W' ) THEN
!@           jpos = -1; write ( 6, * ) ' fname(jpos:jpos) = ' , fname(jpos:jpos) 
           CALL FERR ( INT2(2201), &
     &                'USE_OBSFIL: Trying superfile: '// &
     &                 FNAME(1:I_LEN(FNAME))// &
     &                ' not OBSFIL, write. No NO!', &
     &                INT2(0), INT2(0) )
           STOP 'forcing a stop before you overwrite your superfile'
      ENDIF
!
! --- Position file
!
      JPOS = INT8(IABS(IOBS)-1) * INT8(JOBSREC_BLOCKS) + IOBS_OFF
      CALL BIN_SEEK8 ( FNAME, FILDES, JPOS )
!
      IF ( STRING .EQ. 'R' ) THEN
!
! -------- Read
!
           CALL BIN_READ ( FNAME, FILDES, IOBSFIL, JOBSREC_BLOCKS )
         ELSE
!
! -------- Write
!
           CALL BIN_WRITE ( FNAME, FILDES, IOBSFIL, JOBSREC_BLOCKS )
      ENDIF
!
      RETURN
      END  !#!  USE_OBSFIL  #!#
