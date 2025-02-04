      SUBROUTINE BIN_CREATE ( FNAME, FILDES, LENGTH )
! ************************************************************************
! *                                                                      *
! *   ATTENTION! ATTENTION! ATTENTION! ATTENTION! ATTENTION! ATTENTION!  *
! *                                                                      *
! *   Extremly DANGEROUS routine!         Extremly DANGEROUS routine!    *
! *                                                                      *
! *     If LENGTH is not 0 then the position in the file after calling   *
! *   BIN_CREATE is arbitrary. For proper writing stuff in file you      *
! *   MUST call BIN_SEEK just after the call of BIN_CREATE. For example, *
! *   if you are going to start to write information from the beginning  *
! *   of the file you should call BIN_SEEK ( FNAME, FILDES, I4_1 ),      *
! *   where I4_1 = 1 but type of INTEGER*4. If you forget to do it,      *
! *   the result will be UNPREDICTABLE!                                  *
! *                                                                      *
! ************************************************************************
!
! --- To position the file at the beginning. People! BIN_CREATE leave
! --- the positioning of the file to the arbitrary place! IT IS EXTREMLY
! --- DANGEROUS!
!
      IMPLICIT NONE
!
! 1.  BIN_CREATE PROGRAM SPECIFICATION
!
! 1.1 Open an existing file or create a new file and leave open.
!     If LENGTH is greater than zero, then a minimum of LENGTH
!     blocks are allocated.
!
! 1.2 REFERENCES:
!
! 2.  BIN_CREATE INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4 LENGTH
      CHARACTER*(*) FNAME
!
!     FNAME   - Name of file to be opened or created
!     LENGTH  - Minimum  number of blocks to allocate
!
! 2.3 OUTPUT Variables:
!
      INTEGER*4 FILDES
!
!     FILDES - File descriptor returned from fc_open ( -1 if there are problems)
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'fclib.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: fc_open,fc_write,fc_close,fatal_file,fc_unlink
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 JERR, JMOVE, BLOCKS_NEEDED, ARR_LEN
      INTEGER*8  JBYTE
      PARAMETER  ( ARR_LEN  = 1024*1024 )
      INTEGER*1 IARR_I1(ARR_LEN)
      SAVE      IARR_I1
      INTEGER*2 IERR, TRIMLEN
      INTEGER*4 BLOCKS_FREE
      CHARACTER*10 ME
      CHARACTER  STR*128
      SAVE       STR
      INTEGER*4 FILE_LENGTH, OLD_BLOCKS_FREE
      LOGICAL*2 SAME_DIRECTORY
      COMMON   / FREESP_BLK / OLD_BLOCKS_FREE, FILE_LENGTH, SAME_DIRECTORY
      DATA ME  / 'BIN_CREATE'/
      INTEGER*4  OPEN_1, OPEN_2, OPEN_FLAGS, MODE_1, MODE_2, MODE_3, &
     &           MODE_4, MODE_5, MODE_6, MODE_FLAGS, LN
      INTEGER*4, EXTERNAL :: UNLINK
!
! JBYTE - Number of bytes to be allocated
! JERR - Value returned from low-level functions
! JMOVE - Number of bytes to be allocated in one call to fc_write
! ME - Name of this routine
! MODE - Mode specified in call to fc_open (o'0666')
! OFLAG - Flag sent to fc_open (258)
! BLOCKS_NEEDED - Number of blocks needed for the file to be created
! FILE_LENGTH - Length file in bytes that was last purged, if any.
! OLD_BLOCKS_FREE - Free space before the last file was purged, if any.
! SAME_DIRECTORY - TRUE, if new file is to be created on the same
!                  directory from which the last file was purged from;
!                  FALSE, otherwise.
!
! 4.  HISTORY
!  WHO   WHEN       WHAT
!  AEE   01/07/91   added code to handle the DRACO problem of not being
!                   able to change to a new directory when the cuurent
!                   directory is filled.
!  AEE   02/04/92   Removed hard coded path for fclib.i
!  ALF   12/18/92   Call fc_const_g to set OFLAG
!  KDB   11/02/95   Call new version of fc_open (new subroutine) that does
!                   not abend, providing time for solve to print more
!                   diagnostics before stopping.
!  pet   2004.11.15 Replaced hard-codded constants with calls to &
!                   GET_SYSTEM_CONSTANT
!
! 5.  BIN_CREATE PPROGRAM STRUCTURE
!
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
10    CONTINUE
      FILDES = FCIV_OPEN ( PTR_CH(FNAME(1:TRIMLEN(FNAME))//CHAR(0)), &
     &                     OPEN_FLAGS, MODE_FLAGS )
!
      IF ( FILDES .LT. 0 ) CALL FATAL_FILE(FILDES,'opening',fname,me)
      IF ( LENGTH .LE. 0 ) RETURN
!
! --- Testing: give system some time to add free space.'
!
      JBYTE=LENGTH*INT8(BLOCK_WORDS*WORD_BYTES)
!
! --- Compare current free disk space with old disk space, if the same, it
! --- means system has not added the purged file's length to the free disk
! --- space yet. So consider the file length as part of disk space before
! --- checking to see if we can create a new file using the same path.
!
!?      BLOCKS_FREE = FC_FREESP(FILDES)
!?      IF ( SAME_DIRECTORY .AND. &
!?     &     BLOCKS_FREE .EQ. OLD_BLOCKS_FREE ) THEN
!?           BLOCKS_FREE= BLOCKS_FREE + ( FILE_LENGTH/1024 )
!?      ENDIF
!?      BLOCKS_NEEDED = (JBYTE/1024) + 1
!?!
!?      IF ( BLOCKS_FREE .LE. BLOCKS_NEEDED ) GOTO 100
      DO WHILE ( JBYTE .GT. 0 )
#ifdef GNU
         JMOVE = MIN ( JBYTE, ARR_LEN )
#else
         JMOVE=JMIN0 ( JBYTE, ARR_LEN )
#endif
         JERR = FC_WRITE ( FILDES, PTR_NC(IARR_I1), JMOVE )
!
         IF ( JERR .NE. JMOVE ) GOTO 100
         JBYTE = JBYTE - JERR
      ENDDO
      RETURN
!
100   CONTINUE
      JERR=FC_CLOSE(FILDES)
      CALL FATAL_FILE ( JERR, 'closing', FNAME, ME )
      JERR = FC_UNLINK( PTR_CH(FNAME(1:TRIMLEN(FNAME))//CHAR(0)) )
      IF ( JERR .NE. 0 ) THEN
           CALL GERROR ( STR )
      END IF
      CALL FATAL_FILE ( JERR, 'deleting', FNAME, ME )
      FILDES = -1
!
      RETURN
      END  SUBROUTINE  BIN_CREATE
