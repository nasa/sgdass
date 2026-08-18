      SUBROUTINE BASFE_FILE ( IBUFF, IWDS, STRING )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  BASFE_FILE PROGRAM SPECIFICATION
!
! 1.1 Write to and read from a file, instead of using pipes, when
!     doing larger writes and reads that piping can handle(8192 bytes).
!     The program pauses if an error is detected.
!
! 1.2 REFERENCES:
!
! 2.  BASFE_FILE INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
      INCLUDE 'fclib.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2     IWDS, IBUFF(IWDS)
      CHARACTER*(*) STRING
!
! IBUFF - Array to hold buffer contents
! IWDS - Size of the buffer, in words
! STRING - Requested access type ('O'=open; 'R'=read; 'W'=write; 'C'=close)
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: fatal_file,fc_read,fc_write,fatal_w
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2 KBIT
      INTEGER*2 COUNT,MCOUNT,idum,trimlen
      Integer*4 nbytes,ierr
      CHARACTER*1 TOKEN
      character*50 basf_file
      character*75 errstr
      integer*4 oflag, mode, filedes
      INTEGER*4 OPEN_1, OPEN_2, OPEN_FLAGS, MODE_1, MODE_2, MODE_3, &
     &          MODE_4, MODE_5, MODE_6, MODE_FLAGS, LN
      CHARACTER ME*10
      DATA      ME / 'basfe_file' /
!
! COUNT - Counter for characters processed from STRING
! IERR - Error code sent to FATAL_FILE
! ME - Name of this routine
! MCOUNT - Number of characters in STRING
! NBYTES - Number of bytes in the buffer (2 X number of words)
! TOKEN - A single character from STRING
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE   121991 First version.
!   ALF   12/19/92   Call fc_const_g to set OFLAG
!   KDB   951102 Call new version of fc_open (new subroutine) that does
!                not abend, providing time for solve to print more
!                diagnostics before stopping.
!
! 5.  USE_BUFFER PROGRAM STRUCTURE
!
      NBYTES = 2*IWDS
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
! --- Form the file name:
!
      BASF_FILE= PRE_SCR_DIR(1:PRE_SD_LEN)//BASFE_FNAME//PRE_LETRS
!
! --- Break up the string
!
      MCOUNT=LEN(STRING)
      COUNT=1
      DO WHILE(COUNT.LE.MCOUNT)
         TOKEN=STRING(COUNT:COUNT)
!
! ------ Open FILE:
!
         IF ( TOKEN .EQ. 'O' ) THEN
              FILEDES= FCIV_OPEN( PTR_CH(BASF_FILE(:TRIMLEN(BASF_FILE))//CHAR(0)), &
     &                            OPEN_FLAGS, MODE_FLAGS )
              IF ( FILEDES .LT. 0 ) THEN
                   WRITE  (ERRSTR, '("BASFE_FILE, ERROR opening ",A)' ) BASF_FILE
                   CALL FERR ( INT2(555), ERRSTR, INT2(0), INT2(0) )
              ENDIF
           ELSE IF(TOKEN.EQ.'R') THEN
!
! --------- Read file:
!
            IERR= FC_READ  (FILEDES, PTR_NC(IBUFF), NBYTES )
            CALL FATAL_FILE  (IERR, 'reading', 'buffer', ME )
            IF ( IERR .NE. NBYTES ) THEN
                 IERR=-1
                 CALL FATAL_FILE ( IERR, 'incorrect read', 'buffer', ME )
            ENDIF
          ELSE IF(TOKEN.EQ.'W') THEN
!
! --------- Write file:
!
            IERR = FC_WRITE ( FILEDES, PTR_NC(IBUFF), NBYTES )
            CALL FATAL_FILE ( IERR, 'writing', 'buffer', ME )
            IF ( IERR .NE. NBYTES ) THEN
                 IERR=-1
                 CALL FATAL_FILE ( IERR, 'incorrect write', 'buffer', ME )
            ENDIF
          ELSE IF(TOKEN.EQ.'C') THEN
!
! --------- Close file:
!
            IERR= FC_CLOSE ( FILEDES )
          ELSE
!
! -------- Unkown control
!
           CALL FATAL_W ( 'unknown control', ME )
        ENDIF
        COUNT=COUNT+1
      ENDDO
!
      RETURN
      END  SUBROUTINE  BASFE_FILE
