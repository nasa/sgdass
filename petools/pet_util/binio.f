      SUBROUTINE BINF_OPEN ( FINAM, STATUS, LUN, IUER )
! ************************************************************************
! *                                                                      *
! *    Routine  BINF_OPEN  opens file with name  FINAM on the logical    *
! *    unit  LUN. File may be openned:                                   *
! *     -- for reading only (STATUS='OLD'),                              *
! *     -- for creating and writing only (STATUS='NEW') -- file be       *
! *        created anew. If the file with the same name has existed it   *
! *        will be removed before operation! No confirmation will be     *
! *        asked for deleting previous version of the file.              *
! *     -- for reading and writing (STATUS='UNKNOWN'). If file with the  *
! *        same name has not existed it will be created.                 *
! *                                                                      *
! *  ###  03-JAN-1997   BINF_OPEN  v 2.10 (c) L. Petrov 27-APR-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  LUN, IUER
      CHARACTER  FINAM*(*), STATUS*(*)
      CHARACTER  FINAM_ZERO*8192
      INTEGER*4  IO
      CHARACTER  STR*64, STATUS_USE*7, STRI*16
      INTEGER*4  ARG_LEN
      INTEGER*4  O_CREAT_FLAG, O_RDONLY_FLAG, O_RDWR_FLAG, O_WRONLY_FLAG, &
     &           O_APPEND_FLAG
      INTEGER*4  OPEN_FLAGS, MODE_FLAGS
      INTEGER*4  MODE_1, MODE_2, MODE_3, MODE_4, MODE_5, MODE_6
      LOGICAL*4  LEX
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, OPEN, OPEN64, LOC__SUN$$_STR
!
! --- Learn values of the system constants from Unix headers
!
      CALL GET_SYSTEM_CONSTANT ( 'O_WRONLY', O_WRONLY_FLAG, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'O_CREAT',  O_CREAT_FLAG,  ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'O_RDONLY', O_RDONLY_FLAG, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'O_RDWR',   O_RDWR_FLAG,   ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'O_APPEND', O_APPEND_FLAG, ARG_LEN )
!
      CALL GET_SYSTEM_CONSTANT ( 'S_IRUSR',  MODE_1, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'S_IWUSR',  MODE_2, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'S_IRGRP',  MODE_3, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'S_IWGRP',  MODE_4, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'S_IROTH',  MODE_5, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'S_IWOTH',  MODE_6, ARG_LEN )
#ifdef GNU
      MODE_FLAGS = MODE_1 + &
     &             MODE_2 + &
     &             MODE_3 + &
     &             MODE_4 + &
     &             MODE_5 + &
     &             MODE_6
#else
      MODE_FLAGS = MODE_1 .OR. &
     &             MODE_2 .OR. &
     &             MODE_3 .OR. &
     &             MODE_4 .OR. &
     &             MODE_5 .OR. &
     &             MODE_6
#endif
!
      CALL CLRCH ( STRI )
!
! --- Transformation string STATUS in the upper case and writing to STAT
!
      CALL TRAN ( 11, STATUS, STATUS_USE )
!
! --- Test: whether the file exist?
!
      IF ( ILEN(FINAM) .EQ. 0 ) THEN
           CALL ERR_LOG ( 9001, IUER, 'BINF_OPEN', 'Argument FINAM '// &
     &         '(file name to be opened) contains only blanks or '// &
     &         'binary zeroes' )
           RETURN
      END IF
      IF ( ILEN(FINAM) > LEN(FINAM_ZERO) ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( LEN(FINAM_ZERO), STR )
           CALL ERR_LOG ( 9002, IUER, 'BINF_OPEN', 'Trap of internal control: '// &
     &         'filename '//FINAM(1:I_LEN(FINAM))//' -- is too long: '// &
     &         'longer than '//STR(1:I_LEN(STR))//' characters' )
           RETURN
      END IF
!
      INQUIRE ( FILE=FINAM(1:I_LEN(FINAM)), EXIST=LEX, IOSTAT=IO )
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IO, STR )
           CALL ERR_LOG ( 9003, IUER, 'BINF_OPEN', 'Wrong file name "'// &
     &          FINAM(1:I_LEN(FINAM))//'". IOSTAT='//STR(1:I_LEN(STR)) )
           RETURN
      END IF
      FINAM_ZERO = FINAM(1:I_LEN(FINAM))     ! This is necessary in order to
      FINAM_ZERO(I_LEN(FINAM)+1:) = CHAR(0)  ! circumvent memory leackage bug
!                                            ! In HP Fortran90 2.5.1
!
! --- Define the values of the flag
!
      IF ( STATUS_USE(1:3) .EQ. 'NEW' ) THEN
           IF ( LEX .AND.  FINAM(1:8) .NE. '/dev/nul' ) THEN
                CALL UNLINK ( FINAM(1:I_LEN(FINAM))//CHAR(0) )
           END IF
#ifdef GNU
           OPEN_FLAGS = O_WRONLY_FLAG + O_CREAT_FLAG
#else
           OPEN_FLAGS = O_WRONLY_FLAG .OR. O_CREAT_FLAG
#endif
           STRI = 'output'
         ELSE IF ( STATUS_USE(1:3) .EQ. 'OLD' ) THEN
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 9004, IUER, 'BINF_OPEN', 'File '// &
     &               FINAM(1:I_LEN(FINAM))//' not found' )
                RETURN
           END IF
           OPEN_FLAGS = O_RDONLY_FLAG
           STRI = 'input'
         ELSE IF ( STATUS_USE(1:2) .EQ. 'RW' ) THEN
           OPEN_FLAGS = O_RDWR_FLAG
           STRI = 'input/output'
         ELSE IF ( STATUS_USE(1:6) .EQ. 'APPEND' ) THEN
#ifdef GNU
           OPEN_FLAGS = O_RDWR_FLAG + O_APPEND_FLAG
#else
           OPEN_FLAGS = O_RDWR_FLAG .OR. O_APPEND_FLAG
#endif
           STRI = 'input/output'
         ELSE IF ( STATUS_USE(1:7) .EQ. 'UNKNOWN' ) THEN
#ifdef GNU
           OPEN_FLAGS = O_RDWR_FLAG + O_CREAT_FLAG
#else
           OPEN_FLAGS = O_RDWR_FLAG .OR. O_CREAT_FLAG
#endif
           STRI = 'input/output'
         ELSE
           CALL ERR_LOG ( 9005, IUER, 'BINF_OPEN', 'Parameter STATUS has '// &
     &         'unacceptable value: "'//STATUS(1:I_LEN(STATUS))// &
     &         '". Acceptable only OLD, NEW, RW, APPEND or UNKNOWN' )
           RETURN
      END IF
!
! --- System call for opening file
!
#ifdef SUN
      LUN = OPEN ( %VAL(LOC__SUN$$_STR(FINAM_ZERO)), %VAL(OPEN_FLAGS), %VAL(MODE_FLAGS) )
#else
#ifdef ADR_32BIT
      LUN = OPEN64 ( %REF(FINAM_ZERO), %VAL(OPEN_FLAGS), %VAL(MODE_FLAGS) )
#else
      LUN = OPEN   ( %REF(FINAM_ZERO), %VAL(OPEN_FLAGS), %VAL(MODE_FLAGS) )
#endif
#endif
      IF ( LUN .LT. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 9006, IUER, 'BINF_OPEN', 'Error during '// &
     &              'opening '//STRI(1:I_LEN(STRI))//' file '// &
     &               FINAM(1:I_LEN(FINAM))//'":  '//STR )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  BINF_OPEN  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE BINF_CLOSE ( LUN, IUER )
! ************************************************************************
! *                                                                      *
! *    Routine  BINF_CLOSE  closes file with name  FINAM on the logical  *
! *    unit  LUN , what has been opened earlier by  BINF_OPEN.           *
! *                                                                      *
! *  ###   06-JAN-97   BINF_CLOSE   V1.0  (c) L. Petrov  06-JAN-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  LUN, IUER, IL
      INTEGER*4, EXTERNAL :: I_LEN, CLOSE
      CHARACTER  STR*20, STR1*80
!
      IL = CLOSE ( %VAL(LUN) )
      IF ( IL .LT. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL INCH   ( LUN, STR )
           CALL CLRCH  ( STR1 )
           CALL GERROR ( STR1 )
           CALL ERR_LOG ( 9061, IUER, 'BINF_CLOSE', 'Error during '// &
     &         'closing file on the channel '//STR(1:I_LEN(STR))// &
     &         '":  '//STR1 )
           RETURN
      END IF
      CALL ERR_LOG ( 0, IUER  )
!
      RETURN
      END  !#!  BINF_CLOSE  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE WRBIN_ARRAY ( LUN, TYP, NEL, ARRAY, IUER )
! ************************************************************************
! *                                                                      *
! *  ###   03-JAN-97   WRBIN_ARRAY  V1.0  (c) L. Petrov   03-JAN-97 ###  *
! *                                                                      *
! ************************************************************************
      INTEGER*4  LUN, NEL, IUER, NBT
      REAL*8     ARRAY(*)
      CHARACTER  TYP*(*), TYPE*2
!
      CALL TRAN ( 11, TYP, TYPE )
      IF ( TYPE .EQ. 'B1' ) THEN
           NBT = NEL
        ELSE IF ( TYPE .EQ. 'I2' ) THEN
           NBT = NEL*2
        ELSE IF ( TYPE .EQ. 'L2' ) THEN
           NBT = NEL*2
        ELSE IF ( TYPE .EQ. 'I4' ) THEN
           NBT = NEL*4
        ELSE IF ( TYPE .EQ. 'L4' ) THEN
           NBT = NEL*4
        ELSE IF ( TYPE .EQ. 'R4' ) THEN
           NBT = NEL*4
        ELSE IF ( TYPE .EQ. 'R8' ) THEN
           NBT = NEL*8
        ELSE IF ( TYPE .EQ. 'I8' ) THEN
           NBT = NEL*8
        ELSE
           CALL ERR_LOG ( 9011, IUER, 'WRBIN_ARRAY', 'Parameter TYP has '// &
     &         'unacceptable value: "'//TYP(1:I_LEN(TYP))// &
     &         '". Acceptable only B1, I2, L2, I4, L4, R4, R8, I8 ' )
           RETURN
      END IF
      CALL WRBIN_RECORD ( LUN, NBT, ARRAY, IUER )
      RETURN
      END  !#!  WRBIN_ARRAY  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE WRBIN_STRING ( LUN, STRING, IUER )
! ************************************************************************
! *                                                                      *
! *  ###   03-JAN-97  WRBIN_STRING  V1.0  (c) L. Petrov   03-JAN-97 ###  *
! *                                                                      *
! ************************************************************************
      CHARACTER  STRING*(*)
      INTEGER*4  LUN, IUER
      INTEGER*4, EXTERNAL :: LOC__SUN$$_STR
!
#ifdef SUN
      CALL WRBIN_RECORD ( LUN, LEN(STRING), %VAL(LOC__SUN$$_STR(STRING)), IUER )
#else
      CALL WRBIN_RECORD ( LUN, LEN(STRING), %REF(STRING), IUER )
#endif
!
      RETURN
      END  !#!  WRBIN_STRING  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE WRBIN_RECORD ( LUN, NBT, ARRAY, IUER )
! ************************************************************************
! *                                                                      *
! *  ###  03-JAN-1997  WRBIN_RECORD  v 2.3 (c) L. Petrov 12-MAY-2017 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  LUN, NBT, IUER
      INTEGER*1  ARRAY(NBT)
      CHARACTER  STR*80, STR1*80, PRF*4
      INTEGER*4  IC
      INTEGER*4, EXTERNAL :: WRITE, I_LEN, LOC__SUN$$_STR
!
!!      WRITE ( UNIT=PRF, FMT='(A4)' ) NBT  ! May not work under gfortran 7.1.0
      CALL MEMCPY ( %REF(PRF), NBT, %VAL(4) ) 
#ifdef SUN
      IC = WRITE ( %VAL(LUN), %VAL(LOC__SUN$$_STR(PRF)), %VAL(4) )
#else
      IC = WRITE ( %VAL(LUN), %REF(PRF), %VAL(4) )
#endif
      IF ( IC .LT. 0 ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 9021, IUER, 'WRBIN_RECORD', 'Error during '// &
     &         'writing the prefix of the record: '//STR )
           RETURN
      END IF
!
      IF ( NBT .EQ. 0 ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      IC = WRITE ( %VAL(LUN), ARRAY, %VAL(NBT) )
      IF ( IC .LT. 0 ) THEN
           CALL CLRCH   ( STR      )
           CALL INCH    ( NBT,  STR )
           CALL CLRCH   ( STR1     )
           CALL GERROR  ( STR1     )
           CALL ERR_LOG ( 9022, IUER, 'WRBIN_RECORD', 'Error during '// &
     &         'writing record of '//STR(1:I_LEN(STR))//' bytes: '// &
     &          STR1 )
           RETURN
      END IF
      IF ( IC .NE. NBT ) THEN
           CALL CLRCH   ( STR      )
           CALL IINCH   ( NBT,  STR )
           CALL ERR_LOG ( 9023, IUER, 'WRBIN_RECORD', 'Error during '// &
     &         'writing record of '//STR(1:I_LEN(STR))//' bytes: not all '// &
     &         'bytes are written in file' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  WRBIN_RECORD  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE RDBIN_RECORD ( LUN, LA, ARRAY, NBT, IUER )
! ************************************************************************
! *                                                                      *
! *  ### 03-JAN-1997  RDBIN_RECORD  V2.1  (c) L. Petrov 12-MAY-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT NONE
      INTEGER*4  LUN, LA, NBT, IUER
      INTEGER*1  ARRAY(LA)
      CHARACTER  STR*80, STR1*80, PRF*4
      INTEGER*4  IC, IER
      INTEGER*4, EXTERNAL :: LOC__SUN$$_STR, READ, I_LEN
!
#ifdef SUN
      IC = READ ( %VAL(LUN), %VAL(LOC__SUN$$_STR(PRF)), %VAL(4) )
#else
      IC = READ ( %VAL(LUN), %REF(PRF), %VAL(4) )
#endif
      IF ( IC .LT. 0 ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 9031, IUER, 'RDBIN_RECORD', 'Error during '// &
     &         'reading the prefix of the record: '//STR )
           RETURN
      END IF
!!      READ ( UNIT=PRF, FMT='(A4)', IOSTAT=IER ) NBT  ! Does not work under gfortran 7.1.0 due to a bug
!!      IF ( IER .NE. 0 ) THEN
!!           CALL ERR_LOG ( 9032, IUER, 'RDBIN_RECORD', 'Error in reading '// &
!!     &         'the record prefix' )
!!           RETURN
!!      END IF
      CALL MEMCPY ( NBT, %REF(PRF), %VAL(4) )
      IF ( NBT .LT. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( NBT, STR )
           CALL ERR_LOG ( 9033, IUER, 'RDBIN_RECORD', 'Record length is '// &
     &            'negative: '//STR )
           RETURN
      END IF
      IF ( NBT .GT. LA ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( LA, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( NBT, STR1 )
           CALL ERR_LOG ( 9034, IUER, 'RDBIN_RECORD', 'Parameter LA is '// &
     &         'too small: '//STR(1:I_LEN(STR))//'. Record length is '// &
     &          STR1 )
           RETURN
      END IF
!
      IC = READ ( %val(LUN), ARRAY, %val(NBT) )
      IF ( IC .LT. 0 ) THEN
           CALL CLRCH   ( STR      )
           CALL INCH    ( NBT,  STR )
           CALL CLRCH   ( STR1     )
           CALL GERROR  ( STR1     )
           CALL ERR_LOG ( 9035, IUER, 'RDBIN_RECORD', 'Error during '// &
     &         'reading record of '//STR(1:I_LEN(STR))//' bytes: '// &
     &          STR1 )
           RETURN
      END IF
      IF ( IC .NE. NBT ) THEN
           CALL CLRCH   ( STR      )
           CALL INCH    ( NBT,  STR )
           CALL ERR_LOG ( 9036, IUER, 'RDBIN_RECORD', 'Error during '// &
     &         'reading record of '//STR(1:I_LEN(STR))//' bytes: not all '// &
     &         'bytes are read from file' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  RDBIN_RECORD  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE RDBIN_STRING ( LUN, STRING, LN, IUER )
! ************************************************************************
! *                                                                      *
! *  ###   03-JAN-97  RDBIN_STRING  v 1.1 (c) L. Petrov  04-MAY-2004 ### *
! *                                                                      *
! ************************************************************************
      CHARACTER  STRING*(*)
      INTEGER*4  LUN, LN, IUER
      INTEGER*4, EXTERNAL :: LOC__SUN$$_STR
!
#ifdef SUN
      CALL RDBIN_RECORD ( LUN, LEN(STRING), %VAL(LOC__SUN$$_STR(STRING)), LN, IUER )
#else
      CALL RDBIN_RECORD ( LUN, LEN(STRING), %REF(STRING), LN, IUER )
#endif
!
      RETURN
      END  !#!  RDBIN_STRING  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE RDBIN_ARRAY ( LUN, TYP, MEL, ARRAY, NEL, IUER )
! ************************************************************************
! *                                                                      *
! *  ###   03-JAN-97   RDBIN_ARRAY  V1.0  (c) L. Petrov   03-JAN-97 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  LUN, NEL, MEL, IUER
      INTEGER*1  ARRAY(*)
      INTEGER*4  LN, LENBYTE, IER
      INTEGER*4, EXTERNAL ::  I_LEN
      CHARACTER  TYP*(*), TYPE*2, STR*20
!
      CALL TRAN ( 11, TYP, TYPE )
      IF ( TYPE .EQ. 'B1' ) THEN
           LN = 1
        ELSE IF ( TYPE .EQ. 'I2' ) THEN
           LN = 2
        ELSE IF ( TYPE .EQ. 'L2' ) THEN
           LN = 2
        ELSE IF ( TYPE .EQ. 'I4' ) THEN
           LN = 4
        ELSE IF ( TYPE .EQ. 'L4' ) THEN
           LN = 4
        ELSE IF ( TYPE .EQ. 'R4' ) THEN
           LN = 4
        ELSE IF ( TYPE .EQ. 'R8' ) THEN
           LN = 8
        ELSE IF ( TYPE .EQ. 'I8' ) THEN
           LN = 8
        ELSE
           CALL ERR_LOG ( 9041, IUER, 'RDBIN_ARRAY', 'Parameter TYP has '// &
     &         'unacceptable value: "'//TYP(1:I_LEN(TYP))// &
     &         '". Acceptable only B1, I2, L2, I4, L4, R4, R8, I8 ' )
           RETURN
      END IF
      CALL ERR_PASS (IUER, IER )
      CALL RDBIN_RECORD ( LUN, MEL*LN, ARRAY, LENBYTE, IER )
      IF ( IER .NE. 0 ) THEN
            CALL ERR_PASS ( IER, IUER )
            RETURN
      END IF
      NEL=LENBYTE/LN
      IF ( MOD(LENBYTE,LN) .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( LENBYTE, STR )
           CALL ERR_LOG ( 9042, IUER, 'RDBIN_ARRAY', 'Length of the '// &
     &         'record is not a multiple of the length of element array. '// &
     &         'record lenght = '//STR )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  RDBIN_ARRAY  #!#
