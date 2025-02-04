      SUBROUTINE READ_HEB ( FILIN, HEB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine READ_HEB 
! *                                                                      *
! *  ### 22-FEB-2013    READ_HEB   v1.9 (c)  L. Petrov  13-JAN-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB
      CHARACTER  FILIN*(*)
      CHARACTER  TMP_DIR*128, INTERNET_HOSTNAME*64, SYSNAME*128, HARDWARE*128
      INTEGER*4  IUER
      CHARACTER  FILTMP*128, STR*128, COM*256
      LOGICAL*1  FL_BZIP2
      INTEGER*4  IS, IL, PID, NTHR, IER
      LOGICAL*4, EXTERNAL :: OMP_IN_PARALLEL
      INTEGER*4, EXTERNAL :: GETPID, SYSTEM, ILEN, I_LEN, OMP_GET_THREAD_NUM
!
      CALL GETINFO_HOST ( INTERNET_HOSTNAME )
      IF ( INTERNET_HOSTNAME == 'localhost' ) THEN
           CALL GETINFO_SYSTEM ( SYSNAME, INTERNET_HOSTNAME, HARDWARE )
      END IF
!
      IL = ILEN(FILIN)
      IF ( IL < 4 ) IL = 4
      IF ( FILIN(IL-3:IL) == '.bz2' ) THEN
           PID = GETPID()
           CALL INCH ( PID, FILTMP(1:8) )
           CALL CHASHR    ( FILTMP(1:8) )
           CALL BLANK_TO_ZERO ( FILTMP(1:8) )
           IF ( OMP_IN_PARALLEL() ) THEN
                FILTMP(9:9) = '_'
                CALL INCH ( OMP_GET_THREAD_NUM(), FILTMP(10:13) )
                CALL CHASHR    ( FILTMP(10:13) )
                CALL BLANK_TO_ZERO ( FILTMP(10:13) )
           END IF 
!          
           IF ( INTERNET_HOSTNAME(1:8)  == 'astrogeo'                   .OR. &
     &          INTERNET_HOSTNAME(1:13) == 'earthrotation'              .OR. &
     &          INTERNET_HOSTNAME(1:5)  == 'terra'                      .OR. &
     &          INTERNET_HOSTNAME(1:26) == 'gs61a-sagitta.ndc.nasa.gov' .OR. &
     &          INTERNET_HOSTNAME(1:24) == 'gs61a-crux.gsfc.nasa.gov'   .OR. &
     &          INTERNET_HOSTNAME(1:14) == 'gs61a-geodev-a'                  ) THEN
                TMP_DIR = '/dev/shm'
             ELSE 
                TMP_DIR = '/tmp'
           END IF
           FILTMP = TRIM(TMP_DIR)//'/'//FILTMP(1:I_LEN(FILTMP))//'.heb'
!
! -------- Honor environemnet variable OMP_NUM_THREADS.
! -------- We limit the number of threads for lbzip2
!
           CALL GETENVAR ( 'OMP_NUM_THREADS', STR )
           IF ( ILEN(STR) > 0 ) THEN
                CALL CHIN ( STR, NTHR )
                IF ( NTHR < 1 ) NTHR = 1
                CALL CLRCH ( STR ) 
                CALL INCH  ( NTHR, STR )
                STR = '-n '//STR
              ELSE
!
! ------------- ... or do not use any limit when the variable is not set up
!
                CALL CLRCH ( STR )
           END IF
!
           FL_BZIP2 = .TRUE.
           IF ( OMP_IN_PARALLEL() ) THEN
                COM = 'lbzip2 -n 1 -dfc '//FILIN(1:I_LEN(FILIN))//' > '//FILTMP
              ELSE 
                COM = 'lbzip2 '//STR(1:I_LEN(STR))//' -dsfc '//FILIN(1:I_LEN(FILIN))//' > '//FILTMP
           END IF
           IS = SYSTEM ( COM(1:I_LEN(COM))//CHAR(0) )
           IF ( IS .NE. 0 ) THEN
!
! ------------- lbzip2 may fail because of "Cannot allocate memory". As a desperate
! ------------- attempt we try once more with using pzip2 and with only one thread 
! ------------- and a small block size
!
                CALL UNLINK ( FILTMP(1:I_LEN(FILTMP))//CHAR(0) )
                COM = 'bzip2 -dfc '//FILIN(1:I_LEN(FILIN))//' > '//FILTMP
                IS = SYSTEM ( COM(1:I_LEN(COM))//CHAR(0) )
           END IF
           IF ( IS .NE. 0 ) THEN
                WRITE ( 6, * ) 'System: IS = ', IS
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 6241, IUER, 'READ_HEB', 'Failure to '// &
     &              'uncompress the input heb-file '// &
     &              FILIN(1:I_LEN(FILIN))//' using command '// &
     &              COM(1:I_LEN(COM))//' -- error: '//STR )
                IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
                RETURN 
           END IF
         ELSE
           FL_BZIP2 = .FALSE.
           FILTMP = FILIN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_HEB_HEADER ( FILTMP, HEB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6242, IUER, 'READ_HEB', 'Failure to parse '// &
     &         'the header of the input heb-file '//FILTMP )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN 
      END IF
      HEB%FILE_NAME = FILTMP
      HEB%VAL1 => NULL()
      HEB%VAL2 => NULL()
      HEB%VAL  => NULL()
      HEB%VAL8 => NULL()
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_HEB_DATA   ( FILTMP, HEB, IER )
      IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6243, IUER, 'READ_HEB', 'Failure to read '// &
     &         'the data section of the Input heb-file '//FILTMP )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN 
      END IF
      IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE READ_HEB  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE READ_HEB_HEADER ( FILIN, HEB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  READ_HEB_HEADER
! *                                                                      *
! * ### 29-JAN-2013  READ_HEB_HEADER  v3.2 (c) L. Petrov 13-JAN-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB
      CHARACTER  FILIN*(*)
      CHARACTER  HDR*(HEB__HDS)
      INTEGER*4  IUER
      CHARACTER  INTERNET_HOSTNAME*128, STR*128, TMP_DIR*128, FILTMP*128, COM*512
      INTEGER*4  LUN, IER, IS, IB, IE, PID, IL, J1
      LOGICAL*1  FL_BZIP2
      LOGICAL*4, EXTERNAL :: OMP_IN_PARALLEL
      INTEGER*4, EXTERNAL :: GETPID, ILEN, I_LEN, LIB$SKPC, OMP_GET_THREAD_NUM, READ
!
      CALL GETINFO_HOST ( INTERNET_HOSTNAME )
!
      IL = ILEN(FILIN)
      IF ( IL < 4 ) IL = 4
      IF ( FILIN(IL-3:IL) == '.bz2' ) THEN
           PID = GETPID()
           CALL INCH ( PID, FILTMP(1:8) )
           CALL CHASHR    ( FILTMP(1:8) )
           CALL BLANK_TO_ZERO ( FILTMP(1:8) )
           IF ( OMP_IN_PARALLEL() ) THEN
                FILTMP(9:9) = '_'
                CALL INCH ( OMP_GET_THREAD_NUM(), FILTMP(10:13) )
                CALL CHASHR    ( FILTMP(10:13) )
                CALL BLANK_TO_ZERO ( FILTMP(10:13) )
           END IF 
!          
           IF ( INTERNET_HOSTNAME(1:8)  == 'astrogeo'                   .OR. &
     &          INTERNET_HOSTNAME(1:13) == 'earthrotation'              .OR. &
     &          INTERNET_HOSTNAME(1:5)  == 'terra'                      .OR. &
     &          INTERNET_HOSTNAME(1:26) == 'gs61a-sagitta.ndc.nasa.gov' .OR. &
     &          INTERNET_HOSTNAME(1:24) == 'gs61a-crux.gsfc.nasa.gov'   .OR. &
     &          INTERNET_HOSTNAME(1:14) == 'gs61a-geodev-a'                  ) THEN
                TMP_DIR = '/dev/shm'
             ELSE 
                TMP_DIR = '/tmp'
           END IF
           FILTMP = TRIM(TMP_DIR)//'/'//FILTMP(1:I_LEN(FILTMP))//'.heb'
!
           FL_BZIP2 = .TRUE.
           CALL CLRCH ( STR )
           CALL INCH ( HEB__HDS, STR )
           COM = 'lbzip2 -n 1 -dfc '//FILIN(1:I_LEN(FILIN))// &
     &           ' | head -c '//STR(1:I_LEN(STR))//' > '//FILTMP
           IS = SYSTEM ( COM(1:I_LEN(COM))//CHAR(0) )
           IF ( IS .NE. 0 ) THEN
                WRITE ( 6, * ) 'System: IS = ', IS
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 6241, IUER, 'READ_HEB', 'Failure to '// &
     &              'uncompress the input heb-file '// &
     &              FILIN(1:I_LEN(FILIN))//' using command '// &
     &              COM(1:I_LEN(COM))//' -- error: '//STR )
                IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
                RETURN 
           END IF
         ELSE
           FL_BZIP2 = .FALSE.
           FILTMP = FILIN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL BINF_OPEN  ( FILTMP, 'OLD', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6251, IUER, 'READ_HEB_HEADER', 'Error in '// &
     &         'an attempt to open input file '//FILIN )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN 
      END IF
!
      IS = READ ( %VAL(LUN), %REF(HDR), %VAL(LEN(HDR)) )
      IF ( IS < 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6252, IUER, 'READ_HEB_HEADER', 'Error in '// &
     &         'an reading the header of the input file '// &
     &          FILIN(1:I_LEN(FILIN))//' '//STR )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN 
         ELSE IF ( IS < LEN(HDR) ) THEN
           CALL ERR_LOG ( 6253, IUER, 'READ_HEB_HEADER', 'Error in '// &
     &         'an reading the header of the input HEB file '// &
     &          FILIN(1:I_LEN(FILIN))//' -- it was not read to the end' )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN 
      END IF
!
      IF ( HDR(1:LEN(HEB__LABEL)) == HEB__LABEL ) THEN
           CONTINUE 
         ELSE IF ( HDR(1:LEN(HEB__LABEL_V1)) == HEB__LABEL_V1 ) THEN
           CONTINUE 
         ELSE 
           CALL CLRCH ( STR )
           CALL TRAN  ( 13, HDR(1:LEN(HEB__LABEL)), STR(1:LEN(HEB__LABEL)) )
           CALL ERR_LOG ( 6254, IUER, 'READ_HEB_HEADER', 'Wrong '// &
     &         'format of input file '//FILIN(1:I_LEN(FILIN))// &
     &         ' -- fitst line is '//STR(1:LEN(HEB__LABEL))// &
     &         ' while '//HEB__LABEL//' was expected' )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN 
      END IF
!
! --- Close the input file 
!
      CALL ERR_PASS   ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6255, IUER, 'READ_HEB_HEADER', 'Error in '// &
     &         'attempt to close file '//FILIN )
           IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
           RETURN 
      END IF
      IF ( FL_BZIP2 ) CALL UNLINK ( FILTMP(1:I_LEN(FILTMP)) )
!
      IB = INDEX ( HDR,  'SDS_name:' )
      IF ( IB > 0 ) THEN
           IB = INDEX    ( HDR(IB+1:),  ' '  ) + IB
           IB = LIB$SKPC ( ' ', HDR(IB+1:)   ) + IB
           IE = INDEX ( HDR(IB+1:), CHAR(10) ) + IB-1
           HEB%SDS_NAME  = HDR(IB:IE)
      END IF
!
      IB = INDEX ( HDR,  'Units:' )
      IF ( IB > 0 ) THEN
           IB = INDEX    ( HDR(IB+1:),  ' '  ) + IB
           IB = LIB$SKPC ( ' ', HDR(IB+1:)   ) + IB
           IE = INDEX ( HDR(IB+1:), CHAR(10) ) + IB-1
           HEB%UNITS = HDR(IB:IE)
      END IF
!
      IB = INDEX ( HDR,  'Prod_name:' )
      IF ( IB > 0 ) THEN
           IB = INDEX    ( HDR(IB+1:),  ' '  ) + IB
           IB = LIB$SKPC ( ' ', HDR(IB+1:)   ) + IB
           IE = INDEX ( HDR(IB+1:), CHAR(10) ) + IB-1
           HEB%PROD_NAME = HDR(IB:IE)
      END IF
!
      IB = INDEX ( HDR,  'File_name:' )
      IF ( IB > 0 ) THEN
           IB = INDEX    ( HDR(IB+1:),  ' '  ) + IB
           IB = LIB$SKPC ( ' ', HDR(IB+1:)   ) + IB
           IE = INDEX ( HDR(IB+1:), CHAR(10) ) + IB-1
           HEB%FILE_NAME = HDR(IB:IE)
      END IF
!
      IB = INDEX ( HDR,  'History:' )
      IF ( IB > 0 ) THEN
           IB = INDEX    ( HDR(IB+1:),  ' '  ) + IB
           IB = LIB$SKPC ( ' ', HDR(IB+1:)   ) + IB
           IE = INDEX ( HDR(IB+1:), CHAR(10) ) + IB-1
           HEB%HISTORY = HDR(IB:IE)
      END IF
!
      IB = INDEX ( HDR,  'Source:' )
      IF ( IB > 0 ) THEN
           IB = INDEX    ( HDR(IB+1:),  ' '  ) + IB
           IB = LIB$SKPC ( ' ', HDR(IB+1:)   ) + IB
           IE = INDEX ( HDR(IB+1:), CHAR(10) ) + IB-1
           HEB%SOURCE = HDR(IB:IE)
      END IF
!
      IB = INDEX ( HDR,  'Title:' )
      IF ( IB > 0 ) THEN
           IB = INDEX    ( HDR(IB+1:),  ' '  ) + IB
           IB = LIB$SKPC ( ' ', HDR(IB+1:)   ) + IB
           IE = INDEX ( HDR(IB+1:), CHAR(10) ) + IB-1
           HEB%TITLE = HDR(IB:IE)
      END IF
!
      IB = INDEX ( HDR,  'Institution:' )
      IF ( IB > 0 ) THEN
           IB = INDEX    ( HDR(IB+1:),  ' '  ) + IB
           IB = LIB$SKPC ( ' ', HDR(IB+1:)   ) + IB
           IE = INDEX ( HDR(IB+1:), CHAR(10) ) + IB-1
           HEB%INSTITUTION = HDR(IB:IE)
      END IF
!
      IB = INDEX ( HDR,  'References:' )
      IF ( IB > 0 ) THEN
           IB = INDEX    ( HDR(IB+1:),  ' '  ) + IB
           IB = LIB$SKPC ( ' ', HDR(IB+1:)   ) + IB
           IE = INDEX ( HDR(IB+1:), CHAR(10) ) + IB-1
           HEB%REFERENCES = HDR(IB:IE)
      END IF
!
      IB = INDEX ( HDR,  'Prod_date_time:' )
      IF ( IB > 0 ) THEN
           IB = INDEX    ( HDR(IB+1:),  ' '  ) + IB
           IB = LIB$SKPC ( ' ', HDR(IB+1:)   ) + IB
           IE = INDEX ( HDR(IB+1:), CHAR(10) ) + IB-1
           HEB%PROD_DATE_TIME = HDR(IB:IE)
      END IF
!
      IB = INDEX ( HDR,  'Version_ID:     ' )
      IF ( IB > 0 ) THEN
           IB = INDEX    ( HDR(IB+1:),  ' '  ) + IB
           IB = LIB$SKPC ( ' ', HDR(IB+1:)   ) + IB
           IE = INDEX ( HDR(IB+1:), CHAR(10) ) + IB-1
           HEB%VERSION_ID = HDR(IB:IE)
      END IF
!
      DO 410 J1=1,HEB__MC
         CALL CLRCH ( HEB%COMMENT(J1) )
         CALL INCH  ( J1, STR(1:1) )
         IB = INDEX ( HDR,  'Comment('//STR(1:1)//'):       ' )
         IF ( IB > 0 ) THEN
              IB = INDEX    ( HDR(IB+1:),  ' '  ) + IB
              IB = LIB$SKPC ( ' ', HDR(IB+1:)   ) + IB
              IE = INDEX ( HDR(IB+1:), CHAR(10) ) + IB-1
              HEB%COMMENT(J1) = HDR(IB:IE)
         END IF
 410  CONTINUE 
!
      IB = INDEX ( HDR,  'Dims:' )
      IF ( IB > 0 ) THEN
           IB = IB + 7
           IE = IB + 70
           IE = INDEX ( HDR(IB+1:), CHAR(10) ) + IB-1
           READ ( UNIT=HDR(IB:IE), FMT='(I18,1X,I18,1X,I18,1X,I18)' ) HEB%DIMS(1), &
     &                 HEB%DIMS(2), HEB%DIMS(3), HEB%DIMS(4)
      END IF
!
      IB = INDEX ( HDR,  'MJD, Time:' )
      IF ( IB > 0 ) THEN
           IB = INDEX    ( HDR(IB+5:),  ' '  ) + IB+4
           IB = LIB$SKPC ( ' ', HDR(IB+1:)   ) + IB
           IE = INDEX ( HDR(IB+1:), CHAR(10) ) + IB-1
           READ ( UNIT=HDR(IB:IE), FMT='(I5,1X,F7.1)' ) HEB%MJD, HEB%UTC
      END IF
!
      IB = INDEX ( HDR,  'MJD_Time:' )
      IF ( IB > 0 ) THEN
           IB = INDEX    ( HDR(IB+1:),  ' '  ) + IB
           IB = LIB$SKPC ( ' ', HDR(IB+1:)   ) + IB
           IE = INDEX ( HDR(IB+1:), CHAR(10) ) + IB-1
           READ ( UNIT=HDR(IB:IE), FMT='(I5,1X,F7.1)' ) HEB%MJD, HEB%UTC
      END IF
!
      IB = INDEX ( HDR,  'Endian:' )
      IF ( IB > 0 ) THEN
           IB = INDEX    ( HDR(IB+1:),  ' '  ) + IB
           IB = LIB$SKPC ( ' ', HDR(IB+1:)   ) + IB
           IE = INDEX ( HDR(IB+1:), CHAR(10) ) + IB-1
           HEB%ENDIAN = HDR(IB:IE)
      END IF
!
      IB = INDEX ( HDR,  'Data_Format:' )
      IF ( IB > 0 ) THEN
           IB = INDEX    ( HDR(IB+1:),  ' '  ) + IB
           IB = LIB$SKPC ( ' ', HDR(IB+1:)   ) + IB
           IE = INDEX ( HDR(IB+1:), CHAR(10) ) + IB-1
           HEB%DATA_FORMAT = HDR(IB:IE)
      END IF
!
      IB = INDEX ( HDR,  'Compression_Code:' )
      IF ( IB > 0 ) THEN
           IB = INDEX    ( HDR(IB+1:),  ' '  ) + IB
           IB = LIB$SKPC ( ' ', HDR(IB+1:)   ) + IB
           IE = INDEX ( HDR(IB+1:), CHAR(10) ) + IB-1
           HEB%DATA_COMPRESSION = HDR(IB:IE)
      END IF
!
      IB = INDEX ( HDR,  'Data_Offset:' )
      IF ( IB > 0 ) THEN
           IB = INDEX    ( HDR(IB+1:),  ' '  ) + IB
           IB = LIB$SKPC ( ' ', HDR(IB+1:)   ) + IB
           IE = INDEX ( HDR(IB+1:), CHAR(10) ) + IB-1
           READ ( UNIT=HDR(IB:IE), FMT='(I4)' ) HEB%DATA_OFFSET 
      END IF
!
      IB = INDEX ( HDR,  'Data_Length:' )
      IF ( IB > 0 ) THEN
           IB = INDEX    ( HDR(IB+1:),  ' '  ) + IB
           IB = LIB$SKPC ( ' ', HDR(IB+1:)   ) + IB
           IE = INDEX ( HDR(IB+1:), CHAR(10) ) + IB-1
           READ ( UNIT=HDR(IB:IE), FMT='(I18)', IOSTAT=IER ) HEB%DATA_LENGTH
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6256, IUER, 'READ_HEB', 'Error in read_heb '// &
     &                        'in reading HEB%DATA_LENGTH: '// &
     &                         HDR(IB:IE)//' file: '//TRIM(FILIN) )
                RETURN 
           END IF
      END IF
!
      IB = INDEX ( HDR,  'Data_Transform:' )
      IF ( IB > 0 ) THEN
           IB = INDEX    ( HDR(IB+1:),  ' '  ) + IB
           IB = LIB$SKPC ( ' ', HDR(IB+1:)   ) + IB
           IE = INDEX ( HDR(IB+1:), CHAR(10) ) + IB-1
           HEB%DATA_TRANSFORM  = HDR(IB:IE)
      END IF
!
      IB = INDEX ( HDR,  'Min_Value:' )
      IF ( IB > 0 ) THEN
           IB = INDEX    ( HDR(IB+1:),  ' '  ) + IB
           IB = LIB$SKPC ( ' ', HDR(IB+1:)   ) + IB
           IE = INDEX ( HDR(IB+1:), CHAR(10) ) + IB-1
           READ ( UNIT=HDR(IB:IE), FMT='(1PE14.7)' ) HEB%MIN_VALUE
      END IF
!
      IB = INDEX ( HDR,  'Max_Value:' )
      IF ( IB > 0 ) THEN
           IB = INDEX    ( HDR(IB+1:),  ' '  ) + IB
           IB = LIB$SKPC ( ' ', HDR(IB+1:)   ) + IB
           IE = INDEX ( HDR(IB+1:), CHAR(10) ) + IB-1
           READ ( UNIT=HDR(IB:IE), FMT='(1PE14.7)' ) HEB%MAX_VALUE
      END IF
!
      IB = INDEX ( HDR,  'Valid_Range:' )
      IF ( IB > 0 ) THEN
           IB = INDEX    ( HDR(IB+1:),  ' '  ) + IB
           IB = LIB$SKPC ( ' ', HDR(IB+1:)   ) + IB
           IE = INDEX ( HDR(IB+1:), CHAR(10) ) + IB-1
           READ ( UNIT=HDR(IB:IE), FMT='(1PE14.7,1X,1PE14.7)' ) HEB%VALID_RANGE
      END IF
!
      IB = INDEX ( HDR,  'Fill_Value:' )
      IF ( IB > 0 ) THEN
           IB = INDEX    ( HDR(IB+1:),  ' '  ) + IB
           IB = LIB$SKPC ( ' ', HDR(IB+1:)   ) + IB
           IE = INDEX ( HDR(IB+1:), CHAR(10) ) + IB-1
           READ ( UNIT=HDR(IB:IE), FMT='(1PE14.7)' ) HEB%FILL_VALUE
      END IF
!
      IB = INDEX ( HDR,  'Offset:         ' )
      IF ( IB > 0 ) THEN
           IB = INDEX    ( HDR(IB+1:),  ' '  ) + IB
           IB = LIB$SKPC ( ' ', HDR(IB+1:)   ) + IB
           IE = INDEX ( HDR(IB+1:), CHAR(10) ) + IB-1
           READ ( UNIT=HDR(IB:IE), FMT='(1PE14.7)' ) HEB%OFFSET
      END IF
!
      IB = INDEX ( HDR,  'Scale_Factor:' )
      IF ( IB > 0 ) THEN
           IB = INDEX    ( HDR(IB+1:),  ' '  ) + IB
           IB = LIB$SKPC ( ' ', HDR(IB+1:)   ) + IB
           IE = INDEX ( HDR(IB+1:), CHAR(10) ) + IB-1
           READ ( UNIT=HDR(IB:IE), FMT='(1PE14.7)' ) HEB%SCALE_FACTOR
      END IF
      IF ( ILEN(HEB%DATA_COMPRESSION) == 0 ) THEN
           HEB%DATA_COMPRESSION = HEB__NONE 
      END IF
!
      HEB%STATUS = HEB__HDLO
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  READ_HEB_HEADER  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE READ_HEB_DATA ( FILIN, HEB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  READ_HEB_DATA
! *                                                                      *
! * ### 29-JAN-2013   READ_HEB_DATA   v2.4 (c) L. Petrov 06-JUN-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB
      CHARACTER  FILIN*(*)
      CHARACTER  HDR*(HEB__HDS)
      INTEGER*1, ALLOCATABLE :: ARR_I1(:)
      INTEGER*2, ALLOCATABLE :: ARR_I2(:)
      INTEGER*4  IUER
      CHARACTER  STR*128
      INTEGER*4  LUN, IER, IS, SEEK_SET, ARG_LN
      INTEGER*8  J1, J2, J3, J4
      INTEGER*8  CHUNKS_TO_READ, BYTES_TO_READ, OFFS, &
     &           OFFSET_RET, LEN_ELEM, LEN_ALLOC
      INTEGER*8, EXTERNAL :: LSEEK
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, READ
!
      IF ( HEB%STATUS .NE. HEB__HDLO ) THEN
           CALL ERR_LOG ( 6261, IUER, 'READ_HEB_DATA', 'HEB Header '// &
     &         'has not been read. Please first run READ_HEB_HEADER' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_OPEN ( FILIN, 'OLD', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6262, IUER, 'READ_HEB_DATA', 'Error in '// &
     &         'an attempt to open input file '//FILIN )
           RETURN 
      END IF
!
      IF ( HEB%DATA_FORMAT == HEB__R8 ) THEN
           IF ( ASSOCIATED ( HEB%VAL8 ) ) THEN
                DEALLOCATE ( HEB%VAL8 )
           END IF
           LEN_ELEM = 8
           ALLOCATE ( HEB%VAL8(HEB%DIMS(1),HEB%DIMS(2),HEB%DIMS(3),HEB%DIMS(4)), STAT=IER )
        ELSE IF ( HEB%DATA_FORMAT    == HEB__I1   .AND. &
     &            HEB%DATA_TRANSFORM == HEB__NONE       ) THEN
           IF ( ASSOCIATED ( HEB%VAL1 ) ) THEN
                DEALLOCATE ( HEB%VAL1 )
           END IF
           LEN_ELEM = 1
           ALLOCATE ( HEB%VAL1(HEB%DIMS(1),HEB%DIMS(2),HEB%DIMS(3),HEB%DIMS(4)), STAT=IER )
        ELSE 
          IF ( ASSOCIATED ( HEB%VAL ) ) THEN
               DEALLOCATE ( HEB%VAL )
          END IF
          LEN_ELEM = 4
          ALLOCATE ( HEB%VAL(HEB%DIMS(1),HEB%DIMS(2),HEB%DIMS(3),HEB%DIMS(4)), STAT=IER )
      END IF
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH   ( STR )
           CALL IINCH8  ( LEN_ELEM*HEB%DIMS(1)*HEB%DIMS(2)*HEB%DIMS(3)*HEB%DIMS(4), STR )
           CALL ERR_LOG ( 6263, IUER, 'READ_HEB_DATA', 'Error in '// &
     &         'allocating '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for array HEB%VAL' )
           RETURN 
      END IF
      HEB%STATUS = HEB__ALLO
!
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_SET', SEEK_SET, ARG_LN )
      OFFSET_RET = LSEEK( %VAL(LUN), %VAL(HEB%DATA_OFFSET), %VAL(SEEK_SET) )
      IF ( OFFSET_RET .NE. HEB%DATA_OFFSET ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6264, IUER, 'READ_HEB_DATA', 'Failure in '// &
     &         'an attempt to seek for beginning the data section '// &
     &         'in the input file '//FILIN(1:I_LEN(FILIN))//' -- '//STR )
           RETURN 
      END IF
!
      IF ( HEB%DATA_FORMAT == HEB__I1 ) THEN
!
! -------- INTEGER*1 data. Allocate memory for data array
!
           LEN_ELEM = 1
           LEN_ALLOC = MIN ( HEB%DATA_LENGTH, HEB__DATA_CHUNK )
           ALLOCATE ( ARR_I1(LEN_ALLOC), STAT= IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH   ( STR )
                CALL IINCH8  ( LEN_ELEM*HEB%DIMS(1)*HEB%DIMS(2)*HEB%DIMS(3)*HEB%DIMS(4), STR )
                CALL ERR_LOG ( 6265, IUER, 'READ_HEB_DATA', 'Error in '// &
     &              'allocating '//STR(1:I_LEN(STR))//' bytes of '// &
     &              'dynamic memory for array ARR_I1' )
                RETURN 
           END IF
!
! -------- Read the data to an INTEGER*1 array
!
           CHUNKS_TO_READ = 1 + HEB%DATA_LENGTH/HEB__DATA_CHUNK
           OFFS = 0
           DO 410 J1=1,CHUNKS_TO_READ
              IF ( J1 == CHUNKS_TO_READ ) THEN
                   BYTES_TO_READ = HEB%DATA_LENGTH - (J1-1)*HEB__DATA_CHUNK
                   IF ( BYTES_TO_READ == 0 ) THEN
                        IS = 0
                        GOTO 810
                   END IF
                 ELSE 
                   BYTES_TO_READ= HEB__DATA_CHUNK
              END IF
              IS = READ ( %VAL(LUN), ARR_I1, %VAL(BYTES_TO_READ) )
              IF ( IS .NE. BYTES_TO_READ ) GOTO 810
!
! ----------- Copy there the data and apply the offset and scale factor
!
              IF ( HEB%DATA_TRANSFORM == HEB__LOG ) THEN
                   CALL HEB_DT_FROM_I1_TO_R4_LOG ( BYTES_TO_READ, HEB%SCALE_FACTOR, HEB%OFFSET, &
     &                  ARR_I1, %VAL(LOC(HEB%VAL) + 4*OFFS) )
                 ELSE IF ( HEB%DATA_TRANSFORM == HEB__SCOF ) THEN
                   CALL HEB_DT_FROM_I1_TO_R4_SCOF ( BYTES_TO_READ, HEB%SCALE_FACTOR, HEB%OFFSET, &
     &                  ARR_I1, %VAL(LOC(HEB%VAL) + 4*OFFS) )
                 ELSE
                   CALL MEMCPY ( %VAL(LOC(HEB%VAL1) + OFFS), ARR_I1, %VAL(BYTES_TO_READ) )
              END IF
              OFFS = OFFS + IS
 410       CONTINUE 
 810       CONTINUE 
         ELSE IF ( HEB%DATA_FORMAT == HEB__I2 ) THEN
!
! -------- INTEGER*2 data. Allocate memory for data array
!
           LEN_ELEM = 2
           LEN_ALLOC = MIN ( HEB%DATA_LENGTH/LEN_ELEM, HEB__DATA_CHUNK/LEN_ELEM )
           ALLOCATE ( ARR_I2(LEN_ALLOC), STAT= IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL IINCH  ( LEN_ALLOC, STR )
                CALL ERR_LOG ( 6266, IUER, 'READ_HEB_DATA', 'Error in '// &
     &              'allocating '//STR(1:I_LEN(STR))//' bytes of '// &
     &              'dynamic memory for array ARR_I2' )
                RETURN 
           END IF
!
! -------- Read the data to an INTEGER*2 array
!
           LEN_ELEM = 2
           CHUNKS_TO_READ = 1 + HEB%DATA_LENGTH/HEB__DATA_CHUNK
           OFFS = 0
           DO 420 J2=1,CHUNKS_TO_READ
              IF ( J2 == CHUNKS_TO_READ ) THEN
                   BYTES_TO_READ = HEB%DATA_LENGTH - (J2-1)*HEB__DATA_CHUNK
                   IF ( BYTES_TO_READ == 0 ) THEN
                        IS = 0
                        GOTO 820
                   END IF
                 ELSE 
                   BYTES_TO_READ= HEB__DATA_CHUNK
              END IF
              IS = READ ( %VAL(LUN), ARR_I2, %VAL(BYTES_TO_READ) )
              IF ( IS .NE. BYTES_TO_READ ) GOTO 820
!
! ----------- Copy there the data and apply the offset and scale factor
!
              IF ( HEB%DATA_TRANSFORM == HEB__LOG ) THEN
                   CALL HEB_DT_FROM_I2_TO_R4_LOG  ( BYTES_TO_READ/LEN_ELEM, HEB%SCALE_FACTOR, &
     &                                              HEB%OFFSET, ARR_I2, %VAL(LOC(HEB%VAL) + 2*OFFS) )
                 ELSE
                   CALL HEB_DT_FROM_I2_TO_R4_SCOF ( BYTES_TO_READ/LEN_ELEM, HEB%SCALE_FACTOR, &
     &                                              HEB%OFFSET, ARR_I2, %VAL(LOC(HEB%VAL) + 2*OFFS) )
              END IF
              OFFS = OFFS + IS
 420       CONTINUE 
 820       CONTINUE 
         ELSE IF ( HEB%DATA_FORMAT == HEB__R4 .OR. &
     &             HEB%DATA_FORMAT == HEB__I4      ) THEN
!
! -------- Read the data to an original array within any transformations
!
           LEN_ELEM = 4
           CHUNKS_TO_READ = 1 + HEB%DATA_LENGTH/HEB__DATA_CHUNK
           OFFS = 0
           DO 430 J3=1,CHUNKS_TO_READ
              IF ( J3 == CHUNKS_TO_READ ) THEN
                   BYTES_TO_READ = HEB%DATA_LENGTH - (J3-1)*HEB__DATA_CHUNK
                   IF ( BYTES_TO_READ == 0 ) THEN
                        IS = 0
                        GOTO 830
                   END IF
                 ELSE 
                   BYTES_TO_READ= HEB__DATA_CHUNK
              END IF
              IS = READ ( %VAL(LUN), %VAL(LOC(HEB%VAL) + OFFS), &
     &                    %VAL(BYTES_TO_READ) )
              IF ( IS .NE. BYTES_TO_READ ) GOTO 830
              OFFS = OFFS + IS
 430       CONTINUE 
 830       CONTINUE 
         ELSE IF ( HEB%DATA_FORMAT == HEB__R8      ) THEN
!
! -------- Read the data to an original array within any transformations
!
           LEN_ELEM = 8
           CHUNKS_TO_READ = 1 + HEB%DATA_LENGTH/HEB__DATA_CHUNK
           OFFS = 0
           DO 440 J4=1,CHUNKS_TO_READ
              IF ( J4 == CHUNKS_TO_READ ) THEN
                   BYTES_TO_READ = HEB%DATA_LENGTH - (J4-1)*HEB__DATA_CHUNK
                   IF ( BYTES_TO_READ == 0 ) THEN
                        IS = 0
                        GOTO 840
                   END IF
                 ELSE 
                   BYTES_TO_READ= HEB__DATA_CHUNK
              END IF
              IS = READ ( %VAL(LUN), %VAL(LOC(HEB%VAL8) + OFFS), &
     &                    %VAL(BYTES_TO_READ) )
              IF ( IS .NE. BYTES_TO_READ ) GOTO 840
              OFFS = OFFS + IS
 440       CONTINUE 
 840       CONTINUE 
      END IF
!
      IF ( IS < 0 ) THEN
!
! -------- Failure in reading
!
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6269, IUER, 'READ_HEB_DATA', 'Error in '// &
     &         'read the header of the output HEB file '// &
     &          FILIN (1:I_LEN(FILIN ))//' '//STR )
           RETURN 
         ELSE IF ( IS < BYTES_TO_READ ) THEN
!
! -------- Not all the data are read
!
           WRITE ( 6, * ) ' IS = ', IS, &
     &                    ' BYTES_TO_READ = ', BYTES_TO_READ, &
     &                    ' DL = ', LEN_ELEM*HEB%DIMS(1)*HEB%DIMS(2)*HEB%DIMS(3)*HEB%DIMS(4)
           CALL ERR_LOG ( 6270, IUER, 'READ_HEB_DATA', 'Error in '// &
     &         'reading the data section of the input heb file '// &
     &          FILIN (1:I_LEN(FILIN ))//' -- it was not read to the end' )
           RETURN 
      END IF
      IF ( HEB%DATA_FORMAT == HEB__I1 ) THEN
           DEALLOCATE ( ARR_I1 )
         ELSE IF ( HEB%DATA_FORMAT == HEB__I2 ) THEN
           DEALLOCATE ( ARR_I2 )
      END IF
!
! --- Close the output file 
!
      CALL ERR_PASS   ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6271, IUER, 'READ_HEB_DATA', 'Error in '// &
     &         'attempt to close file '//FILIN  )
           RETURN 
      END IF
      HEB%STATUS = HEB__LOAD
!      
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  READ_HEB_DATA  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE HEB_QUIT ( HEB )
! ************************************************************************
! *                                                                      *
! *   Routine HEB_QUIT deallocates memory allocatd by HEB and purges     *
! *   all the field of HEB object.                                       *
! *                                                                      *
! *  ### 09-MAR-2013    HEB_QUIT   v1.0 (c)  L. Petrov  09-MAR-2013 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB
      IF ( ASSOCIATED ( HEB%VAL ) ) DEALLOCATE ( HEB%VAL )
      CALL NOUT ( SIZEOF(HEB), HEB )
      HEB%VAL => NULL()
      RETURN
      END  SUBROUTINE  HEB_QUIT  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE HEB_DT_FROM_I1_TO_R4_LOG ( NEL, SCALE, OFFSET, ARR_I1, ARR_R4 )
      IMPLICIT   NONE 
      INTEGER*8  NEL
      REAL*4     SCALE, OFFSET
      INTEGER*1  ARR_I1(NEL)
      REAL*4     ARR_R4(NEL)
      INTEGER*4  J1
!
      DO 410 J1=1,NEL
         ARR_R4(J1) = EXP(SCALE*ARR_I1(J1) + OFFSET)
 410  CONTINUE 
      RETURN
      END  SUBROUTINE  HEB_DT_FROM_I1_TO_R4_LOG !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE HEB_DT_FROM_I1_TO_R4_SCOF ( NEL, SCALE, OFFSET, &
     &                                       ARR_I1, ARR_R4 )
      IMPLICIT   NONE 
      INTEGER*8  NEL
      REAL*4     SCALE, OFFSET
      INTEGER*1  ARR_I1(NEL)
      REAL*4     ARR_R4(NEL)
      INTEGER*4  J1, J2
!
      IF ( SCALE == 1.0 .AND. OFFSET == 0.0 ) THEN
           DO 410 J1=1,NEL
              ARR_R4(J1) = ARR_I1(J1)
 410       CONTINUE 
         ELSE 
           DO 420 J2=1,NEL
              ARR_R4(J2) = SCALE*ARR_I1(J2) + OFFSET
 420       CONTINUE 
      END IF
      RETURN
      END  SUBROUTINE  HEB_DT_FROM_I1_TO_R4_SCOF  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE HEB_DT_FROM_I2_TO_R4_LOG ( NEL, SCALE, OFFSET, &
     &                                      ARR_I2, ARR_R4 )
      IMPLICIT   NONE 
      INTEGER*8  NEL
      REAL*4     SCALE, OFFSET
      INTEGER*2  ARR_I2(NEL)
      REAL*4     ARR_R4(NEL)
      INTEGER*4  J1
!
      DO 410 J1=1,NEL
         ARR_R4(J1) = EXP(SCALE*ARR_I2(J1) + OFFSET)
 410  CONTINUE 
      RETURN
      END  SUBROUTINE  HEB_DT_FROM_I2_TO_R4_LOG  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE HEB_DT_FROM_I2_TO_R4_SCOF ( NEL, SCALE, OFFSET, &
     &                                       ARR_I2, ARR_R4 )
      IMPLICIT   NONE 
      INTEGER*8  NEL
      REAL*4     SCALE, OFFSET
      INTEGER*2  ARR_I2(NEL)
      REAL*4     ARR_R4(NEL)
      INTEGER*4  J1, J2
!
      IF ( SCALE == 1.0 .AND. OFFSET == 0.0 ) THEN
           DO 410 J1=1,NEL
              ARR_R4(J1) = ARR_I2(J1)
 410       CONTINUE 
        ELSE 
           DO 420 J2=1,NEL
              ARR_R4(J2) = SCALE*ARR_I2(J2) + OFFSET
 420       CONTINUE 
      END IF
      RETURN
      END  SUBROUTINE  HEB_DT_FROM_I2_TO_R4_SCOF  !#!#
