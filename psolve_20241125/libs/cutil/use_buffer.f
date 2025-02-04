      SUBROUTINE USE_BUFFER ( IBUFF, IWDS, STRING )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  USE_BUFFER PROGRAM SPECIFICATION
!
! 1.1 Access utility for scratch buffer.  The scratch buffer provides
!     a means to pass information between SOLVE programs.
!     Conceptually it is a file with a single variable length
!     record.  It must be opened and closed like a file, because
!     it may actually be implemented as a file.  On the other hand,
!     it may not actually be a file, some lengths of buffers may be
!     transmitted more efficiently than others.  Because of this
!     possible variation the transmitting program (writing) and the
!     receiving (reading) program must agree on the length of the
!     buffer, ot the receiving program may look in the wrong place
!     for it. The program pauses if an error is detected.
!
! 1.2 REFERENCES:
!
! 2.  USE_BUFFER INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
      INCLUDE 'fclib.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IWDS, IBUFF(IWDS)
      CHARACTER*(*) STRING
      CHARACTER  STR*128
!
! IBUFF - Array to hold buffer contents
! IWDS - Size of the buffer, in words
! STRING - Requested access type ('O'=open; 'R'=read; 'W'=write; 'C'=close)
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      LOGICAL*2 KOPEN
      COMMON/BUFFCM/KOPEN
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: fatal_file, fc_read, fc_write, fatal_w
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2 KBIT
      INTEGER*2 COUNT,MCOUNT,idum
      INTEGER*4 NBYTES, IERR
      CHARACTER TOKEN*1, ME*10, STR1*12, STR2*12
      DATA ME / 'use_buffer' /
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
!   AEE   920204 Removed hard coded path for fclib.i
!   PET   980728 Made error messages more verbose. Improved comments.
!
! 5.  USE_BUFFER PROGRAM STRUCTURE
!
      NBYTES = 2*IWDS
!
      MCOUNT=LEN(STRING)
      COUNT=1
      DO WHILE(COUNT.LE.MCOUNT)
         TOKEN=STRING(COUNT:COUNT)
!
! ------ Open BUFFER
!
         IF ( TOKEN .EQ. 'O' ) THEN
              IF ( KOPEN ) THEN
                   IERR=-1
                   CALL FATAL_FILE ( IERR, 'Opening', 'buffer', ME )
              ENDIF
              KOPEN=.TRUE.
            ELSE IF ( TOKEN .EQ. 'R' ) THEN
!
! ----------- Read BUFFER
!
              IF ( .NOT. KOPEN ) THEN
                    CALL FERR ( INT2(221), 'USE_BUFFER - BUFFER NOT OPEN - R', &
     &               INT2(0), INT2(0) )
              END IF
              IERR = FC_READ ( PIPE_IDS(3), PTR_NC(IBUFF), NBYTES )
              IF ( IERR .NE. NBYTES ) THEN
!@                   CALL END_MN()  ! postpone curser
!@                   CALL UN_CURSES () ! Eliminate the influence of the curses
                   STR1 = '            '
                   STR2 = '            '
                   WRITE ( UNIT=STR1, FMT='(I6)'  ) IERR
                   WRITE ( UNIT=STR2, FMT='(I10)' ) NBYTES
!
                   IF ( IERR .NE. -1 ) IERR = -1
                   CALL FATAL_FILE ( IERR, 'Incorrect read: was read '// &
     &                               STR1//' bytes, expected '//STR2//' bytes', &
     &                               'buffer', ME )
              ENDIF
              CALL FATAL_FILE ( IERR, 'Reading', 'buffer', ME )
!
#ifdef HPUX
              NBYTES=1
              IERR = FC_READ ( PIPE_IDS(3), PTR_NC(IDUM), NBYTES )
              IF ( IERR .NE. 0 ) THEN
                   CALL END_MN()  ! postpone curser
                   CALL UN_CURSES () ! Eliminate the influence of the curses
                   WRITE ( 6, '("FC_READ =",I5 )') IERR
                   IERR=-1
                   CALL FATAL_FILE ( IERR, 'use_buffer: Reading too many '// &
     &                              'bytes from pipe.', '_', ME )
              ENDIF
#endif
!
              CALL SBIT ( PRE_IP(2), INT2(3), INT2(0) )
            ELSE IF(TOKEN.EQ.'W') THEN
!
! ----------- Write buffer
!
              IF ( .NOT. KOPEN ) THEN
                   CALL FERR ( INT2(222), 'USE_BUFFER - BUFFER NOT OPEN - W', &
     &                         INT2(0), INT2(0) )
              END IF
!
              IERR = FC_WRITE ( PIPE_IDS(4), PTR_NC(IBUFF), NBYTES )
!
              IF ( IERR .NE. NBYTES ) THEN
                   STR1 = '            '
                   STR2 = '            '
                   WRITE ( UNIT=STR1, FMT='(I6)'  ) IERR
                   WRITE ( UNIT=STR2, FMT='(I10)' ) NBYTES
!
                   IERR=-1
                   CALL FATAL_FILE ( IERR, 'Incorrect write: was written: '// &
     &                               STR1//' bytes, should be written: '// &
     &                               STR2//' bytes', 'buffer', ME )
                   IERR=-1
              ENDIF
              CALL FATAL_FILE ( IERR, 'Writing', 'buffer', ME )
              CALL SBIT ( PRE_IP(2), INT2(3), INT2(1) )
            ELSE IF(TOKEN.EQ.'C') THEN
!
! ----------- Close buffer
!
              IF ( .NOT. KOPEN ) THEN
                   IERR=-1
                   CALL FATAL_FILE ( IERR, 'Closing', 'buffer', ME )
              ENDIF
              KOPEN=.FALSE.
            ELSE
!
! ----------- Unkown control
!
              CALL FATAL_W ( 'Unknown control: '//TOKEN, ME )
        ENDIF
        COUNT=COUNT+1
      ENDDO
!
      RETURN
      END  !#!  USE_BUFFER  #!#
