      INTEGER*4 FUNCTION MAKE_SEMA(FNAME,MODE,BLOCK)
      IMPLICIT NONE
!
! 1.  MAKE_SEMA PROGRAM SPECIFICATION
!
! 1.1 Make a semaphore to limit resource use.  The file specified
!     by FNAME is opened and locked. The returned file descriptor
!     is set for close on exec so that the next program doesn't
!     lose a file descriptor.
!
! 1.2 REFERENCES:
!
! 2.  MAKE_SEMA INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'fclib.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) FNAME,MODE,BLOCK
!
! BLOCK - 'Y' to wait for access, 'N' to return with error
! FNAME - File to be used as semaphore
! MODE - 'W' for write lock, 'R' for read lock
!
! 2.3 OUTPUT Variables:
!
! MAKE_SEMA - Returns -1 if error, otherwise file descriptor
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: fc_flock_p,fc_const_g,fatal_file,fc_fcntl,
!                           fc_open,fc_close
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 IERR, CMD, L_START, FLOCK
      INTEGER*2 IL, TRIMLEN, L_TYPE, MDX, MDL
      INTEGER*4 OPEN_1, OPEN_2, OPEN_FLAGS, MODE_1, MODE_2, MODE_3, &
     &          MODE_4, MODE_5, MODE_6, MODE_FLAGS, LN
      CHARACTER ME*9
      DATA ME   / 'make_sema' /
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE   920204 Removed hard coded path for fclib.i
!   ALF   12/19/92   Call fc_const_g to set OFLAG for fc_open
!   KDB   951102 Call new version of fc_open (new subroutine) that does
!                not abend, providing time for solve to print more
!                diagnostics before stopping.
!
! 5.  MAKE_SEMA PROGRAM STRUCTURE
!
! Check for valid mode
!
      MDL = TRIMLEN(MODE)
      MDX = INDEX ( 'WR', MODE(1:MDL) )
      IF ( MDX .EQ. 0  .OR. MDL .NE. 1 ) THEN
           CALL FATAL_W ( 'illegal mode', ME )
        ELSE IF ( INDEX('YN',BLOCK) .EQ. 0  .OR.  LEN(BLOCK) .NE. 1 ) THEN
           CALL FATAL_W ( 'illegal block', ME )
      ENDIF
      CALL GET_SYSTEM_CONSTANT ( 'O_CREAT', OPEN_1, LN )
      CALL GET_SYSTEM_CONSTANT ( 'O_RDWR',  OPEN_2, LN )
      CALL GET_SYSTEM_CONSTANT ( 'S_IRUSR', MODE_1, LN )
      CALL GET_SYSTEM_CONSTANT ( 'S_IWUSR', MODE_2, LN )
      CALL GET_SYSTEM_CONSTANT ( 'S_IRGRP', MODE_3, LN )
      CALL GET_SYSTEM_CONSTANT ( 'S_IWGRP', MODE_4, LN )
      CALL GET_SYSTEM_CONSTANT ( 'S_IROTH', MODE_5, LN )
      CALL GET_SYSTEM_CONSTANT ( 'S_IWOTH', MODE_6, LN )
!
#ifdef GNU
      OPEN_FLAGS = OPEN_1 + OPEN_2
      MODE_FLAGS = MODE_1 + &
     &             MODE_2 + &
     &             MODE_3 + &
     &             MODE_4 + &
     &             MODE_5 + &
     &             MODE_6
#else
      OPEN_FLAGS = OPEN_1 .OR. OPEN_2
      MODE_FLAGS = MODE_1 .OR. &
     &             MODE_2 .OR. &
     &             MODE_3 .OR. &
     &             MODE_4 .OR. &
     &             MODE_5 .OR. &
     &             MODE_6
#endif
!
! --- open the file, create id necessery
!
      IL=TRIMLEN(FNAME)
      MAKE_SEMA=FCIV_OPEN ( PTR_CH(FNAME(:IL)//CHAR(0)), OPEN_FLAGS, MODE_FLAGS )
      CALL FATAL_FILE ( MAKE_SEMA, 'opening', FNAME, ME )
!
! --- set file to be locked
!
! --- get arguments first
!
      L_TYPE=1
      IF(INDEX(MODE,'W').NE.0) L_TYPE=2
      ierr=fc_flock_p(flock,ptr_ch('l_type'//char(0)),l_type)
      CALL FATAL_FILE(IERR,'getting l_type',fname,me)
!
      L_START=0
      ierr=fc_flock_p(flock,ptr_ch('l_start'//char(0)),l_start)
      CALL FATAL_FILE(IERR,'getting l_start',fname,me)
!
      IF(BLOCK.EQ.'Y') THEN
        IERR=fc_const_g(ptr_CH('F_SETLKW'//char(0)),CMD)
        CALL FATAL_FILE(IERR,'getting F_SETLKW',fname,me)
      ELSE
        IERR=fc_const_g(ptr_CH('F_SETLK'//char(0)) ,CMD)
        CALL FATAL_FILE(IERR,'getting F_SETLK',fname,me)
      ENDIF
!
!  do it
!
      IERR=FC_FCNTL(MAKE_SEMA,CMD,flock)
      IF(IERR.EQ.-1) THEN
        IERR=FC_CLOSE(MAKE_SEMA)
        CALL FATAL_FILE(IERR,'closing 1',fname,me)
        MAKE_SEMA=-1
        RETURN
      ENDIF
!
!  set to close on exec
!
      IERR=fc_const_g(ptr_CH('F_SETFD'//char(0)) ,CMD)
      CALL FATAL_FILE(IERR,'getting F_SETFD',fname,me)
!
      MODE_FLAGS = 1
      IERR=FC_FCNTL ( MAKE_SEMA, CMD, MODE_FLAGS )
      IF(IERR.EQ.-1) THEN
        IERR=FC_CLOSE(MAKE_SEMA)
        CALL FATAL_FILE(IERR,'closing 2',fname,me)
        MAKE_SEMA=-1
      ENDIF
!
      RETURN
      END
