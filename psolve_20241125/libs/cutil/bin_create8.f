      SUBROUTINE BIN_CREATE8 ( FNAME, FILDES, LEN8_BLOCKS )
      IMPLICIT   NONE 
      INCLUDE 'solve.i'
      INCLUDE 'fclib.i'
      CHARACTER  FNAME*(*)
      INTEGER*8  LEN8_BLOCKS
      INTEGER*4  FILDES
      INTEGER*8  ARR_LEN
      PARAMETER  ( ARR_LEN  = 1024*1024 )
      INTEGER*1  IARR_I1(ARR_LEN)
      SAVE       IARR_I1
      INTEGER*8  BYTES_REM8
      INTEGER*4  WRI_BYTES, JMOVE, JERR
      INTEGER*4  OPEN_1, OPEN_2, OPEN_FLAGS, MODE_1, MODE_2, MODE_3, &
     &           MODE_4, MODE_5, MODE_6, MODE_FLAGS, LN
      CHARACTER  ME*11, STR*128
      DATA ME  / 'BIN_CREATE8'/
      INTEGER*2, EXTERNAL :: TRIMLEN
      INTEGER*4, EXTERNAL :: UNLINK
!
      CALL GET_SYSTEM_CONSTANT ( 'O_CREAT', OPEN_1, LN )
      CALL GET_SYSTEM_CONSTANT ( 'O_RDWR',  OPEN_2, LN )
      CALL GET_SYSTEM_CONSTANT ( 'S_IRUSR', MODE_1, LN )
      CALL GET_SYSTEM_CONSTANT ( 'S_IWUSR', MODE_2, LN )
      CALL GET_SYSTEM_CONSTANT ( 'S_IRGRP', MODE_3, LN )
      CALL GET_SYSTEM_CONSTANT ( 'S_IWGRP', MODE_4, LN )
      CALL GET_SYSTEM_CONSTANT ( 'S_IROTH', MODE_5, LN )
      CALL GET_SYSTEM_CONSTANT ( 'S_IWOTH', MODE_6, LN )
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
      FILDES = FCIV_OPEN ( PTR_CH(FNAME(1:TRIMLEN(FNAME))//CHAR(0)), &
     &                     OPEN_FLAGS, MODE_FLAGS )
      BYTES_REM8 = LEN8_BLOCKS*256
      DO WHILE ( BYTES_REM8 .GT. 0 )
         JMOVE = MIN ( BYTES_REM8, ARR_LEN )
         WRI_BYTES = FC_WRITE ( FILDES, PTR_NC(IARR_I1), JMOVE )
!
         IF ( WRI_BYTES .NE. JMOVE ) GOTO 100
         BYTES_REM8 = BYTES_REM8 - WRI_BYTES
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
      END  SUBROUTINE  BIN_CREATE8  !#!#
      
