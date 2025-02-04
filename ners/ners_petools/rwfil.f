#ifdef GNU
#define GCC_VERSION (   __GNUC__ * 10000 \
                      + __GNUC_MINOR__ * 100 \
                      + __GNUC_PATCHLEVEL__)
#endif
      SUBROUTINE RD_TEXT ( FINAM, MBUF, BUF, LBUF, IUER )
! ************************************************************************
! *                                                                      *
! *  Subprogram  RD_TEXT  reads text file  FINAM and puts its content in *
! *  character array BUF.                                                *
! *                                                                      *
! *  ###  30-JUN-1991   RD_TEXT   v7.6  (C) Petrov L.  13-JUL-2021 ###   *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MBUF, LBUF, IUER
      CHARACTER  FINAM*(*), BUF(MBUF)*(*)
      CHARACTER  STR*32, STR1*32
      INTEGER*4  IOS, IS, J1, IER, LUN, UNIX_DATE
      INTEGER*8  OFFSET, POS1, POS2, LEN_STR, SIZE_I8
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, GET_UNIT, FILE_INFO
#ifdef GNU
      INTEGER*8, INTRINSIC :: FTELL
#else
      INTEGER*4, EXTERNAL  :: FTELL
#endif
      LOGICAL    L_EXIST
!
      IF ( LEN(FINAM) .EQ. 0 ) THEN
           CALL ERR_LOG ( 9002, IUER, 'RD_TEXT', 'Incorrect first argument:'// &
     &         ' length of the string = 0' )
           RETURN
      END IF
!
      IF ( MBUF .LE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( MBUF, STR )
           CALL ERR_LOG ( 9004, IUER, 'RD_TEXT', 'Incorrect second argument:'// &
     &         ' MBUF=<0   MBUF='//STR(1:I_LEN(STR)) )
           RETURN
      END IF
!
      IF ( LEN(FINAM) .EQ. 0 ) THEN
           CALL ERR_LOG ( 9006, IUER, 'RD_TEXT', 'Incorrect third argument:'// &
     &         ' length of the string = 0' )
           RETURN
      END IF
!
      INQUIRE ( FILE=FINAM, EXIST=L_EXIST )
      IF ( .NOT. L_EXIST ) THEN
           CALL ERR_LOG ( 9008, IUER, 'RD_TEXT', 'File "'// &
     &          FINAM(1:I_LEN(FINAM))//'" not found' )
           RETURN
      END IF
!
#if defined (INTEL) || defined (GNU)
!
! --- If we deal with Intel Fortran compiler 8.1 or older, we should get
! --- the file size
!
      IS = FILE_INFO ( FINAM(1:I_LEN(FINAM))//CHAR(0), UNIX_DATE, SIZE_I8 )
!$OMP CRITICAL(OPEN_FILE)
#endif
#ifdef GNU
#if    GCC_VERSION >= 40500
           OPEN ( NEWUNIT=LUN, FILE=FINAM, STATUS='OLD', IOSTAT=IOS )
#else 
           LUN = GET_UNIT () ! Get free Fortran I/O unit
           OPEN ( UNIT=LUN, FILE=FINAM, STATUS='OLD', IOSTAT=IOS )
#endif
#else
      LUN = GET_UNIT () ! Get free Fortran I/O unit
      OPEN ( UNIT=LUN, FILE=FINAM, STATUS='OLD', IOSTAT=IOS )
#endif
!$OMP END CRITICAL(OPEN_FILE)
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 9010, IUER, 'RD_TEXT', 'Error '//STR(1:I_LEN(STR))// &
     &         ' during openning file '//FINAM )
           RETURN
      END IF
!
      LBUF=0
      DO 410 J1=1,MBUF
         CALL CLRCH ( BUF(J1) )
#ifdef GNU
!
! ------ Gnu compiler does not support option Q...
!
         POS1 = FTELL ( LUN )
         READ ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) BUF(J1)
         POS2 = FTELL ( LUN )
         LEN_STR = POS2 - POS1 - 1
#else
         READ ( UNIT=LUN, FMT='(Q,A)', IOSTAT=IOS ) LEN_STR, BUF(J1)
#endif
#ifdef INTEL
!
! ------ Intel compiler 8.1 or older has a bug: if it gets the record which
! ------ of one byte with the decimal code 26 at the first character, 
! ------ it considers it as the end of file
!
         IF ( IOS .EQ. -1 ) THEN
              OFFSET = FTELL ( LUN )
              IF ( OFFSET .LT. SIZE_I8 ) IOS = 0
         END IF
#endif
         IF ( IOS .EQ. -1 ) GOTO 810
         IF ( IOS .NE.  0 ) THEN
              CALL CLRCH ( STR  )
              CALL INCH  ( IOS, STR )
              CALL CLRCH ( STR1 )
              CALL INCH  ( J1, STR1 )
              CALL ERR_LOG ( 9012, IUER, 'RD_TEXT', 'Error '// &
     &             STR(1:I_LEN(STR))//' during reading of the '// &
     &             STR1(1:I_LEN(STR1))//'-th line of the file '// &
     &             FINAM )
              CLOSE ( UNIT=LUN )
              RETURN
         END IF
         LBUF=LBUF+1
  410 CONTINUE
!
      OFFSET = FTELL ( LUN )
      IF ( OFFSET .LT. SIZE_I8 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( MBUF, STR )
           CALL ERR_PASS ( IUER, IER )
           IF ( IER .NE. 0 ) THEN
                WRITE ( 6, * ) ' POS1= ', POS1, ' POS2= ', POS2
                WRITE ( 6, * ) ' OFFSET = ', OFFSET, ' SIZE_I8 = ', SIZE_I8
           END IF
           CALL ERR_LOG ( 9014, IUER, 'RD_TEXT', 'File '// &
     &          FINAM(1:I_LEN(FINAM))//' has been read not up to the '// &
     &         'end: its lenght exceeded the value of parameter MBUF='// &
     &          STR(1:I_LEN(STR)) )
           CLOSE ( UNIT=LUN )
           RETURN
      END IF
!
 810  CONTINUE
      CLOSE ( UNIT=LUN )
      IF ( LBUF .EQ. 0 ) THEN
           CALL ERR_LOG ( 9016, IUER, 'RD_TEXT', 'File '// &
     &          FINAM(1:I_LEN(FINAM))//' appeared to be empty' )
           RETURN
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  RD_TEXT  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE WR_TEXT ( LBUF, BUF, FINAM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  WR_TEXT writes down  the text buffer BUF of LBUF lines    *
! *   in the file FINAM.                                                 *
! *                                                                      *
! *  ###  30-JAN-98     WR_TEXT    v2.1  (c)  L. Petrov 19-APR-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  LBUF, IUER
      CHARACTER  BUF(LBUF)*(*), FINAM*(*), STR*80, STR1*80
      INTEGER*4  IOS, J1, LUN
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GET_UNIT
!
! --- Test: whether the file exist?
!
      IF ( ILEN(FINAM) .EQ. 0 ) THEN
           CALL ERR_LOG ( 9501, IUER, 'WR_TEXT', 'Argument FINAM '// &
     &         '(file name to be opened) contains only blanks or '// &
     &         'binary zeroes' )
           RETURN
      END IF
!
      IF ( FINAM .EQ. '-' ) THEN
           LUN = 6
         ELSE
#ifdef GNU
#if    GCC_VERSION >= 40500
           OPEN ( NEWUNIT=LUN, FILE=FINAM, STATUS='UNKNOWN', IOSTAT=IOS )
#else 
           LUN = GET_UNIT () ! Get free Fortran I/O unit
           OPEN ( UNIT=LUN, FILE=FINAM, STATUS='UNKNOWN', IOSTAT=IOS )
#endif
#else
           LUN = GET_UNIT () ! Get free Fortran I/O unit
           OPEN ( UNIT=LUN, FILE=FINAM, STATUS='UNKNOWN', IOSTAT=IOS )
#endif
           IF ( IOS .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IOS, STR )
                CALL ERR_LOG ( 9504, IUER, 'WR_TEXT', 'Error during '// &
     &              'openning file "'//FINAM(1:I_LEN(FINAM))//'". IOSTAT='//STR )
                RETURN
           END IF
      END IF
!
      DO 410 J1=1,LBUF
         WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) BUF(J1)(1:I_LEN(BUF(J1)))
         IF ( IOS .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL CLRCH ( STR1 )
              CALL INCH  ( IOS, STR1 )
              CALL ERR_LOG ( 9505, IUER, 'WR_TEXT', 'Error during writing '// &
     &             STR(1:I_LEN(STR))//'-th record in the file "'// &
     &             FINAM(1:I_LEN(FINAM))//'". IOSTAT='//STR1 )
              CLOSE ( UNIT=LUN )
              RETURN
         END IF
 410  CONTINUE
      IF ( FINAM .NE. '-' ) THEN
           CLOSE ( UNIT=LUN )
      END IF
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  !#!  WR_TEXT  #!#

