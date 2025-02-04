       PROGRAM    GTI_UPDATE_MAIN
       IMPLICIT   NONE 
       CHARACTER  STR*128
       INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
       PARAMETER  ( GB = 1024*1024*1024 )
       PARAMETER  ( STACK_SIZE_IN_BYTES = INT8(4) * GB )
       INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! ---- Set stacksize
!
       IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
       CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
       CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
       CALL GTI_UPDATE()
       END  PROGRAM  GTI_UPDATE_MAIN
!
! ------------------------------------------------------------------------
!
       SUBROUTINE GTI_UPDATE()
! ************************************************************************
! *                                                                      *
! *   Program  GTI_UPDATE  reads the directory tree with the output of   *
! *   GPS ionospere model produced from analysis of GPS observations in  *
! *   IONEX format, the binary file with the total electron contents     *
! *   (TEC) ionospheric model in vio format (if exists), checks the      *
! *   latest dates for which the the model is available, and if the      *
! *   directory tree conains the ionospheric model for dates later than  *
! *   the those that are in the binary file in vio format, then it reads *
! *   the TEC maps and updates the binary file for new data.             *
! *                                                                      *
! *   Usage:                                                             *
! *    gti_update dir_tree vio_file name date_beg date_end ivrb [create] *
! *                                                                      *
! *   where                                                              *
! *        dir_tree -- directory tree that contains files with GPS tec   *
! *                    in IONEX format. Three structures of the          *
! *                    directory tree are supported:                     *
! *                    a)   yyyy/NAME_ddd0000_01D_01H_GIM.INX.gz         *
! *                    b)   yyyy/NAMEddd0.yyI.Z                          *
! *                    c)   yyyy/ddd/nameddd0.yyI.Z                      *
! *        vio_file -- Name of the output binary file in VIONO format.   *
! *        name     -- 4-letter or 10-letters long model name or         *
! *                    analysis center name.                             *
! *        date_beg -- Start date of the interval. The data in the       *
! *                    input directory tree before that data are skipped.*
! *        date_end -- End date of the interval. The data in the input   *
! *                    directory tree after that data are skipped.       *
! *        ivrb     -- verbosity level. 0 -- silent, 1 -- moderate       *
! *                    verobosity, 2 -- testing mode.                    *
! *        create   -- optional argument. If present, then the output    *
! *                    file is created. If this argument is omitted,     *
! *                    the file is appended.                             *
! *                                                                      *
! *  Dates are supported in two formats:                                 *
! *   a) ISO8601:  yyyy.mm.dd  for instanfe:  2008.07.19                 *
! *   b) VEX:      YYYYyDDDd   for instance:  2010y109d where 109 is     *
! *                            the day of year.                          *
! *                                                                      *
! *  ### 07-MAY-2010   GTI_UPDATE  v1.2 (c)  L. Petrov  03-FEB-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'viono.i'
      LOGICAL*1  FL_CREATE
      ADDRESS__TYPE  DIR_DESC(64)
      CHARACTER  IONEX_TREE*128, VIO_FILE*128, MOD_NAM*8, STR*128, FILNAM*128
      CHARACTER  FILIN(M__ION)*128
      INTEGER*4  YEAR__MIN, YEAR__MAX, DOY__MIN, DOY__MAX
      PARAMETER  ( YEAR__MIN = 1990 )
      PARAMETER  ( YEAR__MAX = 2050 )
      PARAMETER  ( DOY__MIN  =    1 )
      PARAMETER  ( DOY__MAX  =  366 )
      INTEGER*4  J1, J2, J3, J4, J5, L_FIL, IS, IL, LEV, IVRB, YEAR, DOY, &
     &           IP1, IP2, IP3, MJD, MJD_BEG, MJD_END, MJD_ION_BEG, &
     &           NION, IDAY, IUER
      REAL*8     UTC, UTC_BEG, UTC_END, UTC_ION_BEG, TIM_DIF_DAYS 
      TYPE     ( IONO__TYPE ) :: VIO
      CHARACTER  IONEX_BUF(M__IOB)*128, DATE_BEG*32, DATE_END*32, YEAR_STR*4
      LOGICAL*1  FL_SOUTH_POLE, FL_NORTH_POLE
!
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, VEX_TO_DATE*19
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, GET_FILE_FROM_DIR, LINDEX 
!
      IF ( IARGC() < 6 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: gti_update dir_tree vio_file name date_beg date_end ivrb [create]'
           CALL EXIT ( 1 )
         ELSE 
!
! -------- Parse input arguments
!
           FL_CREATE = .FALSE.
           CALL GETARG ( 1, IONEX_TREE )
           CALL GETARG ( 2, VIO_FILE   )
           CALL GETARG ( 3, MOD_NAM    )
!
! -------- Get beginning date
!
           CALL GETARG ( 4, DATE_BEG   )
           IF ( INDEX ( DATE_BEG, 'y' ) > 0 ) THEN
!
! ------------- The date was in the VEX format ( f.e 2010y123d, or 2010d107d12h07m29s" )
!
                DATE_BEG = DATE_BEG(1:9)//'00h00m00s'
!
! ------------- Transform the date to ISO8601 format
!
                IUER = -1
                STR = VEX_TO_DATE ( DATE_BEG, IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 4301, -2, 'GTI_UPDATE', 'Failure in '// &
     &                   'parsing begin date: '//DATE_BEG )
                     CALL EXIT ( 1 )
                END IF
                DATE_BEG = STR
           END IF
!
! -------- Transform the begin date from the ISO8601 foramt to MJD/UTC pair
!
           IUER = -1
           CALL DATE_TO_TIME ( DATE_BEG, MJD_BEG, UTC_BEG, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 4302, -2, 'GTI_UPDATE', 'Failure in '// &
     &              'parsing begin date: '//DATE_BEG )
                CALL EXIT ( 1 )
           END IF
!
! -------- Get the end date
!
           CALL GETARG ( 5, DATE_END )
           IF ( INDEX ( DATE_END, 'y' ) > 0 ) THEN
!
! ------------- Transform the end date from VEX format to ISO8601 format
!
                DATE_END = DATE_END(1:9)//'00h00m00s'
                IUER = -1
                STR = VEX_TO_DATE ( DATE_END, IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 4303, -2, 'GTI_UPDATE', 'Failure in '// &
     &                   'parsing end date: '//DATE_END )
                     CALL EXIT ( 1 )
                END IF
                DATE_END = STR
           END IF
!
! -------- Transform the end date from ISO8601 format to MJD/UTC pair
!
           IUER = -1
           CALL DATE_TO_TIME ( DATE_END, MJD_END, UTC_END, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 4304, -2, 'GTI_UPDATE', 'Failure in '// &
     &              'parsing end date: '//DATE_END )
                CALL EXIT ( 1 )
           END IF
!
! -------- Get verbosity parameter
!
           CALL GETARG ( 6, STR        )
           CALL CHIN ( STR , IVRB )
           IF ( IVRB < 0 ) IVRB = 0
           IF ( IARGC() .GE. 7 ) THEN
!
! ------------- Parse optional argument
!
                CALL GETARG ( 7, STR )
                CALL TRAN ( 12, STR, STR )
                IF ( STR == 'create' ) THEN
                     FL_CREATE = .TRUE.
                   ELSE 
                     CALL ERR_LOG ( 4305, -2, 'GTI_UPDATE', 'Wrong value '// &
     &                   'of the 7-th argument: '//STR )
                     CALL EXIT ( 1 )
                END IF 
           END IF
      END IF
!
      IF ( IVRB > 0 ) THEN
           WRITE ( 6, * ) 'Get ionex files from the input directory tree'
      END IF
!
! --- Read the input directory tree and select files with data in 
! --- IONEX format that fit a certain pattern:
! --- 1) contans the model name sting
! --- 2) have extensions one of .Z, .z, or gz
! --- The (unsorted) list of files FILIN will be created
!
      LEV = 0
      L_FIL = 0
      DO 410 J1=1,1024*1024*1024
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, IONEX_TREE, FILNAM )
         IF ( LEV == 0 ) GOTO 810
         IF ( IS .NE. 0 ) THEN
              CALL ERR_LOG ( 4306, -2, 'GTI_UPDATE', 'Error in '// &
     &            'reading directory '//IONEX_TREE(1:I_LEN(IONEX_TREE))// &
     &            ' -- '//FILNAM )
              CALL EXIT ( 1 )
         END IF
         IF ( ILEN(FILNAM) < 5 ) GOTO 810
         IF ( ( FILNAM(ILEN(FILNAM)-3:ILEN(FILNAM)) .EQ. '.INX' .OR. &
     &          FILNAM(ILEN(FILNAM)-1:ILEN(FILNAM)) .EQ. '.Z'   .OR. &
     &          FILNAM(ILEN(FILNAM)-1:ILEN(FILNAM)) .EQ. '.z'   .OR. &
     &          FILNAM(ILEN(FILNAM)-2:ILEN(FILNAM)) .EQ. '.gz'       ) .AND. &
     &        INDEX ( FILNAM, MOD_NAM(1:I_LEN(MOD_NAM)) ) > 0 ) THEN
!
! ----------- If the file has a supported extension, let us consider it
!
              L_FIL = L_FIL + 1
              IF ( L_FIL > M__ION ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( M__ION, STR )
                   CALL ERR_LOG ( 4307, -2, 'GTI_UPDATE', 'Too many '// &
     &                 'ionex files were found in '// &
     &                  IONEX_TREE(1:I_LEN(IONEX_TREE))// &
     &                 ' directory tree: more than '//STR )
                   CALL EXIT ( 1 )
              END IF
              FILIN(L_FIL) = FILNAM 
         END IF
 410  CONTINUE 
 810  CONTINUE 
!
      IF ( L_FIL == 0 ) THEN
           CALL ERR_LOG ( 4308, -2, 'GTI_UPDATE', 'No ionex files with '// &
     &          MOD_NAM(1:I_LEN(MOD_NAM))//' model were '// &
     &         'found in '//IONEX_TREE(1:I_LEN(IONEX_TREE))//' directory' )
           CALL EXIT ( 1 )
      END IF
!
! --- Sort the data in alphabetic order (according to the naming convention,
! --- this equivalent to time order)
!
      CALL SORT_FAST_CH ( L_FIL, FILIN )
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, * ) L_FIL, ' files will be processed'
      END IF
!
! --- Initialization
!
      CALL NOUT ( SIZEOF(VIO%HEADER), VIO%HEADER )
      IF ( .NOT. FL_CREATE ) THEN
!
! -------- Read the header from  the existing binary ionosphere file
!
           IUER = -1
           CALL VIO_GET_HEADER ( VIO_FILE, VIO, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 4309, -2, 'GTI_UPDATE', 'Failure to read '// &
     &              'the header of existing binary inosphere file '// &
     &               VIO_FILE )
                IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
           END IF
!
! -------- How many full days the model the existing binario ionosphere file
! -------- contains
!
           TIM_DIF_DAYS = (VIO%HEADER%UTC_BEG + VIO%HEADER%NEPC*VIO%HEADER%TIM_STEP)/86400.0D0
           IDAY = IDINT ( TIM_DIF_DAYS + 100.0D0 )
!
! -------- Update the start MJD date: the next date after the last date with
! -------- data
!
           MJD_BEG = VIO%HEADER%MJD_BEG + IDAY
      END IF
!
! === Process the list of imput files
!
      DO 420 J2=1,L_FIL
         IL = I_LEN(FILIN(J2))
         IF ( IVRB == 1 ) THEN
              WRITE ( 6, '(2X, I6," ( ", I6, " )", A, 2X,A$)' ) &
     &                J2, L_FIL, ' File '//FILIN(J2)(1:I_LEN(FILIN(J2)))// &
     &                ' is being processed', CHAR(13) 
              CALL FLUSH ( 6 )
           ELSE IF ( IVRB .GE. 2 ) THEN
              WRITE ( 6, '(I6," ( ", I6, " )", A)' ) &
     &                J2, L_FIL, ' File '//FILIN(J2)(1:I_LEN(FILIN(J2)))// &
     &                ' is being processed' 
         END IF
!
! ------ Parse the file name. First search for slashes (at least three 
! ------ should be present)
!
         IP1 = LINDEX ( FILIN(J2), '/' )
         IF ( IP1 .LE. 0 ) THEN
              CALL ERR_LOG ( 4310, -2, 'GTI_UPDATE', 'Trap of internal '// &
     &            'control: malformed file name: '// &
     &            FILIN(J2)(1:I_LEN(FILIN(J2)))//' no slash in the name '// &
     &            ' was found' )
              CALL EXIT ( 1 )
         END IF 
         IP2 = LINDEX ( FILIN(J2)(1:IP1-1), '/' )
         IF ( IP2 .LE. 0 ) THEN
              CALL ERR_LOG ( 4311, -2, 'GTI_UPDATE', 'Trap of internal '// &
     &            'control: malformed file name: '// &
     &            FILIN(J2)(1:I_LEN(FILIN(J2)))//' only one slash in '// &
     &            'the name was found' )
              CALL EXIT ( 1 )
         END IF 
         IP3 = LINDEX ( FILIN(J2)(1:IP2-1), '/' )
         IF ( IP3 .LE. 0 ) THEN
              CALL ERR_LOG ( 4312, -2, 'GTI_UPDATE', 'Trap of internal '// &
     &            'control: malformed file name: '// &
     &            FILIN(J2)(1:I_LEN(FILIN(J2)))//' only two slashes in '// &
     &            'the name was found' )
              CALL EXIT ( 1 )
         END IF 
!
! ------ Parse the year as a part of the file name
!
         YEAR_STR = FILIN(J2)(IP3+1:IP2-1)
         CALL CHIN ( YEAR_STR, YEAR )
         IF ( YEAR < YEAR__MIN .OR. YEAR > YEAR__MAX ) THEN
              YEAR_STR = FILIN(J2)(IP2+1:IP1-1)
              CALL CHIN ( YEAR_STR, YEAR )
              IF ( YEAR < YEAR__MIN .OR. YEAR > YEAR__MAX ) THEN
                   CALL ERR_LOG ( 4313, -2, 'GTI_UPDATE', 'Trap of internal '// &
     &                 'control: malformed file name: '// &
     &                  FILIN(J2)(1:I_LEN(FILIN(J2)))//' -- wrong year '// &
     &                 'was found: '//YEAR_STR )
                   CALL EXIT ( 1 )
             END IF
         END IF
!
! ------ Parse the day of year
!
         IF ( INDEX ( FILIN(J2), 'COD0OPSFIN' ) > 0 .AND. IL > 25 ) THEN
              CALL CHIN ( FILIN(J2)(IL-25:IL-23), DOY )
           ELSE
              CALL CHIN ( FILIN(J2)(IL-9:IL-7), DOY )
         END IF
         IF ( DOY < DOY__MIN .OR. DOY > DOY__MAX ) THEN
              CALL ERR_LOG ( 4314, -2, 'GTI_UPDATE', 'Trap of internal '// &
     &            'control: malformed file name: '// &
     &             FILIN(J2)(1:I_LEN(FILIN(J2)))//' -- wrong day of year '// &
     &            'was found: '//FILIN(J2)(IL-9:IL-7) )
              CALL EXIT ( 1 )
         END IF
!
! ------ Transform the pair year/day_of_year into ISO8601 compatible pairs of 
! ------ dates MJD/SEC
!
         STR = YEAR_STR//'.01.01_00:00:00'
         IUER = -1
         CALL DATE_TO_TIME ( STR, MJD, UTC, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL ERR_LOG ( 4315, -2, 'GTI_UPDATE', 'Trap of internal '// &
     &            'control: cannot transform the date '//STR )
              CALL EXIT ( 1 )
         END IF
         MJD = MJD + DOY - 1
         IF ( J2 == 1 ) THEN
!
! ----------- If this is the first file, then store the date
!
              MJD_ION_BEG = MJD
              UTC_ION_BEG = UTC
         END IF
!
! ------ Bypass the files with dates our of range
!
         IF ( MJD < MJD_BEG ) GOTO 420
         IF ( MJD > MJD_END ) GOTO 820
!
! ------ Read the input file in the IONEX format, uncompress it, and put
! ------ its uncompressed contents in buffer IONEX_BUF
!
         IUER = -1
         CALL READ_IONEX_FILE ( FILIN(J2), IONEX_BUF, NION, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J2, STR )
              CALL ERR_LOG ( 4316, -2, 'GTI_UPDATE', 'Failure in '// &
     &            'an attempt to read the '//STR(1:I_LEN(STR))// &
     &            ' th IONEX file '//FILIN(J2) )
              CALL EXIT ( 1 )
         END IF
!
         IUER = -1
         CALL PARSE_IONEX_FILE ( NION, IONEX_BUF, VIO, FL_SOUTH_POLE, &
     &                           FL_NORTH_POLE, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J2, STR )
              CALL ERR_LOG ( 4317, -2, 'GTI_UPDATE', 'Failure in '// &
     &            'an attempt to read the '//STR(1:I_LEN(STR))// &
     &            ' th IONEX file '//FILIN(J2) )
              CALL EXIT ( 1 )
         END IF
!
         IF ( FL_CREATE ) THEN
!
! ----------- Create new output binary ionosphere file
!
              IUER = -1
              CALL GTI_CREATE ( VIO, VIO_FILE, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4318, -2, 'GTI_UPDATE', 'Failure in '// &
     &                 'an attempt to create the output binary '// &
     &                 'ionosphere file '//VIO_FILE )
                   CALL EXIT ( 1 )
              END IF
              FL_CREATE = .FALSE.
         END IF
!
! ------ Append contents of the input ionosphere TEC file in IONEX format
! ------ to the output ionosphere file in binary file and update the 
! ------ header
!
         IUER = -1
         CALL GTI_APPEND ( VIO, VIO_FILE, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J2, STR )
              CALL ERR_LOG ( 4319, -2, 'GTI_UPDATE', 'Failure in '// &
     &            'an attempt to append contents of the parsed IONEX '// &
     &            'file '//FILIN(J2)(1:I_LEN(FILIN(J2)))// &
     &            ' to the end of the output binary '// &
     &            'ionosphere file '//VIO_FILE )
              CALL EXIT ( 1 )
         END IF
 420  CONTINUE 
 820  CONTINUE 
      IF ( IVRB .GE. 1 ) THEN
           IF ( IVRB == 1 ) THEN
                CALL CLRCH ( STR )
                WRITE ( 6, '(A)' ) STR(1:79)
           END IF
           IF ( FL_CREATE ) THEN
                WRITE ( 6, '(A)' ) 'Ionosphere file '// &
     &                              VIO_FILE(1:I_LEN(VIO_FILE))// &
     &                             ' has been created'
              ELSE 
                WRITE ( 6, '(A)' ) 'Ionosphere file '// &
     &                              VIO_FILE(1:I_LEN(VIO_FILE))// &
     &                             ' has been updated'
           ENDIF 
      ENDIF 
!
      END  SUBROUTINE  GTI_UPDATE  !#!  
