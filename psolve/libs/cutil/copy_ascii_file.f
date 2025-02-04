      SUBROUTINE COPY_ASCII_FILE ( SOURCE_FILE, DESTIN_FILE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  COPY_ASCII_FILE copies fiels in ASCII format from         *
! *   SOURCE_FILE to DESTIN_FILE. Trailing balnks are eliminated. Empty  *
! *   lines are copied as empty lines. The program merely reads          *
! *   SOURCE_FILE line by line and copyies it to the DESTIN_FILE.        *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * SOURCE_FILE ( CHARACTER ) -- File name from which data are to be     *
! *                              copied.                                 *
! * DESTIN_FILE ( CHARACTER ) -- File name where the data will be copied.*
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 08-SEP-2000  COPY_ASCII_FILE  v110 (c) L. Petrov 10-OCT-2017 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      CHARACTER  SOURCE_FILE*(*), DESTIN_FILE*(*), STR*32768, STR1*32
      INTEGER*4  IUER
      INTEGER*4  LUNS, LUND, IO, J1, LN, POS1, POS2
      INTEGER*4  I_LEN, GET_UNIT
#ifdef GNU
      INTEGER*4, INTRINSIC :: FTELL
#else
      INTEGER*4, EXTERNAL  :: FTELL
#endif
!
      LUNS = GET_UNIT ()
      OPEN ( UNIT=LUNS, FILE=SOURCE_FILE, STATUS='OLD', IOSTAT=IO )
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL INCH   ( IO, STR )
           CALL ERR_LOG ( 851, IUER, 'COPY_ASCII_FILE', 'Error '// &
     &          STR(1:I_LEN(STR))//' opening source file '//SOURCE_FILE )
           RETURN
      END IF
!
      LUND = GET_UNIT ()
      OPEN ( UNIT=LUND, FILE=DESTIN_FILE, STATUS='UNKNOWN', IOSTAT=IO )
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL INCH   ( IO, STR )
           CALL ERR_LOG ( 852, IUER, 'COPY_ASCII_FILE', 'Error '// &
     &          STR(1:I_LEN(STR))//' opening destination file '//DESTIN_FILE )
           RETURN
      END IF
!
      DO 410 J1=1,1024*1024*1024
#ifdef GNU
!
! ------ Gnu compiler does not support option Q...
!
         POS1 = FTELL ( LUNS )
         READ ( UNIT=LUNS, FMT='(A)', IOSTAT=IO ) STR
         POS2 = FTELL ( LUNS )
         LN = POS2 - POS1 - 1
#else
         READ ( UNIT=LUNS, FMT='(Q,A)', IOSTAT=IO ) LN, STR
#endif
         IF ( IO .EQ. -1 ) GOTO 810
         IF ( IO .NE.  0 ) THEN
              CALL CLRCH  ( STR )
              CALL INCH   ( IO, STR )
              CALL CLRCH  ( STR1 )
              CALL INCH   ( J1, STR1 )
              CALL ERR_LOG ( 853, IUER, 'COPY_ASCII_FILE', 'Error '// &
     &             STR(1:I_LEN(STR))//' reading the '//STR1(1:I_LEN(STR1))// &
     &            ' line of the source file '//SOURCE_FILE )
              RETURN
         END IF
!
         IF ( LN .GT. LEN(STR) ) THEN
              CALL CLRCH  ( STR )
              CALL INCH   ( LN, STR )
              CALL CLRCH  ( STR1 )
              CALL INCH   ( J1, STR1 )
              CALL ERR_LOG ( 854, IUER, 'COPY_ASCII_FILE', 'Error '// &
     &             ' reading the '//STR1(1:I_LEN(STR1))// &
     &             ' line of the source file '// &
     &             SOURCE_FILE(1:I_LEN(SOURCE_FILE))//' line length is too '// &
     &             'large: '//STR )
              RETURN
         END IF
!
         IF ( LN .GT. 0 ) THEN
              WRITE ( UNIT=LUND, FMT='(A)', IOSTAT=IO ) STR(1:LN)
            ELSE
              WRITE ( UNIT=LUND, FMT='()', IOSTAT=IO )
         END IF
         IF ( IO .EQ. -1 ) GOTO 810
         IF ( IO .NE.  0 ) THEN
              CALL CLRCH  ( STR )
              CALL INCH   ( IO, STR )
              CALL CLRCH  ( STR1 )
              CALL INCH   ( J1, STR1 )
              CALL ERR_LOG ( 855, IUER, 'COPY_ASCII_FILE', 'Error '// &
     &             STR(1:I_LEN(STR))//' writing the '//STR1(1:I_LEN(STR1))// &
     &            ' line of the destination file '//DESTIN_FILE )
              RETURN
         END IF
 410  CONTINUE
 810  CONTINUE
      CLOSE ( UNIT=LUNS )
      CLOSE ( UNIT=LUND )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  COPY_ASCII_FILE  #!#
