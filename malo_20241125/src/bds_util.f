#include <mk5_preprocessor_directives.inc>
      SUBROUTINE BDS_EXTRACT ( FIL_BDS, DATE_EXTR, IUER )
! ************************************************************************
! *                                                                      *
! *   Program  BDS_EXTRACT extracts an two binary sectinos of an array   *
! *   of site displacemetns in the binary format. The first section      *
! *   is the header. The second section is site displacements starting   *
! *   with the specified epoch. Both sections are wtitten into the       *
! *   output file. The name of the output file has the                   *
! *   following format: IIIII_yyyymmdd_hhmmss__YYYYMMDD_HHMMSS.dat ,     *
! *   where                                                              *
! *   IIIII           -- input file name.                                *
! *   yyyymmdd_hhmmss -- is the date of the beginning of the extracted   *
! *                      section that corresponds to DATE_EXTR date;     *
! *   YYYYMMDD_HHMMSS -- is the date of the last epoch of the extracted  *
! *                      section.                                        *
! *   .dat            -- extension.                                      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * FIL_BDS   ( CHARACTER    ) -- Name of the input file with station    *
! *                               displacements in bindisp format.       *
! * DATE_EXTR ( CHARACTER    ) -- Date of the first epoch to be          *
! *                               extracted from the input file in       *
! *                               YYYY.MM.DD_hh:mm:ss format.            *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     IUER ( INTEGER*4, OPT ) -- Universal error handler.              *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
!! *  ### 28-NOV-2024  BDS_EXTRACT  v1.0 (c)  L. Petrov 28-NOV-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'bindisp.i'
      TYPE ( BINDISP_HEADER_4 ) ::  HDR4
      TYPE ( BINDISP_HEADER_8 ) ::  HDR8
      CHARACTER  FIL_BDS*(*), DATE_EXTR*(*)
      INTEGER*4  IUER
      CHARACTER  FIL_OUT*128, HEADER(M__HDR)*(LEN__HDR), STR*128, &
     &           STR_DAT_BEG*21, STR_DAT_END*21
      LOGICAL*1  LEX
      INTEGER*8  OFF_EXTR, LEN_EXTR
      REAL*8     EPS
      PARAMETER  ( EPS = 180.0D0 )
      REAL*8     TIM_INT, TAI_BEG, TAI_END, TAI_EXTR
      INTEGER*1, ALLOCATABLE :: DSP_BIN(:)
      INTEGER*4  IS, LUN_IN, LUN_OUT, L_EPC, MJD_BEG, MJD_END, &
     &           MJD_EXTR, NREC_EXTR, SEEK_SET, ARG_LEN, IDAY, IER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: GETPID, SYSTEM, RENAME
      ADDRESS__TYPE, EXTERNAL :: LSEEK, READ, WRITE
!
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_SET', SEEK_SET, ARG_LEN )
!
! --- Check whether the file in quetion exists
!
      INQUIRE ( FILE=FIL_BDS, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 5511, IUER, 'BDS_EXTRACT', 'File '// &
     &          TRIM(FIL_BDS)//' does not exist' )
           RETURN 
      END IF
!
! --- Tansform extraction date to MJD/TAI
!
      CALL ERR_PASS ( IUER, IER )
      CALL DATE_TO_TIME ( DATE_EXTR, MJD_EXTR, TAI_EXTR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5512, IUER, 'BDS_EXTRACT', 'Failure '// &
     &          'in parsing the extract date '//DATE_EXTR )
           RETURN 
      END IF
!
! --- Open the input file with site displacments 
!
      LUN_IN  = 10
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_OPEN ( FIL_BDS, 'OLD', LUN_IN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5513, IUER, 'BDS_EXTRACT', 'Error in opening '// &
     &         'file '//FIL_BDS )
           RETURN 
      END IF
!
! --- Read the header section
!
      IS = READ ( %VAL(LUN_IN), %REF(HEADER), %VAL(SIZEOF(HEADER)) )
      IF ( IS .NE. SIZEOF(HEADER) ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5514, IUER, 'BDS_EXTRACT', 'Error in '// &
     &          'an attempt to read the header section of the '// &
     &          'input file '//TRIM(FIL_BDS)// &
     &          ' : '//STR )
           RETURN 
      END IF
!
! --- Extract the number of epochs and the sampling interval
!
      CALL LIB$MOVC3 ( LEN__HDR, %REF(HEADER(4)), HDR4 )
      L_EPC = HDR4%NUM_REC
      TIM_INT = HDR4%SAMPLING_INTERVAL 
!
! --- Extract MJD and TAI for the first epoch
!
      CALL LIB$MOVC3 ( LEN__HDR, %REF(HEADER(8)), HDR8 )
      MJD_BEG = HDR8%MJD_FIRST 
      TAI_BEG = HDR8%TAI_FIRST 
!
      MJD_END = MJD_BEG
      TAI_END = TAI_BEG + (L_EPC-1)*TIM_INT
      IF ( TAI_END > 86400.0D0 - 1.D-6 ) THEN
           IDAY = (TAI_END + 2.D-6)/86400.0D0
           MJD_END = MJD_END + IDAY
           TAI_END = TAI_END - IDAY*86400.0D0
           IF ( TAI_END < 0.0D0 .AND. TAI_END > -3.D-6 ) TAI_END = 0.0D0
      ENDIF
!
! --- Determine how many data records have to be extracted
!
      NREC_EXTR = ( (MJD_END - MJD_EXTR)*86400 + &
     &              (TAI_END - TAI_EXTR)       + EPS )/TIM_INT + 1
!
! --- Determine the offset of the extracted data
!
      OFF_EXTR = SIZEOF(HEADER) + LEN__BDS*(L_EPC - NREC_EXTR)
      LEN_EXTR = LEN__BDS*NREC_EXTR
!
! --- Check whether the extraction request can be performed
!
      IF ( NREC_EXTR == 0 ) THEN
           WRITE ( 6, '(A)' ) 'Nothing to extract in file '//FIL_BDS
           CALL ERR_LOG ( 0, IUER )
           RETURN 
         ELSE IF ( NREC_EXTR < 0 ) THEN
!
! -------- The requested extraction date is later than the last date
!
           STR = MJDSEC_TO_DATE ( MJD_END, TAI_END, IER )
           STR = 'Last epoch: '//STR(1:21)//' Requested erxtraction: '//DATE_EXTR
           CALL ERR_LOG ( 5516, IUER, 'BDS_EXTRACT', 'Requested extraction '// &
     &         'date is too late: '//TRIM(STR)//' for file '//FIL_BDS )
           RETURN 
         ELSE IF ( OFF_EXTR .LE. 0 ) THEN
!
! -------- The requested extraction date is earlier than the first date
!
           STR = MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG, IER )
           STR = 'First epoch: '//STR(1:21)//' Requested extraction: '//DATE_EXTR
           CALL ERR_LOG ( 5517, IUER, 'BDS_EXTRACT', 'Requested extaction '// &
     &         'date is too early: '//TRIM(STR)//' for file '//FIL_BDS )
           RETURN 
      END IF
!
      STR_DAT_BEG = MJDSEC_TO_DATE ( MJD_EXTR, TAI_EXTR, IER )
      STR_DAT_END = MJDSEC_TO_DATE ( MJD_END, TAI_END, IER )
      FIL_OUT = TRIM(FIL_BDS)//'_'//STR_DAT_BEG(1:4)//STR_DAT_BEG(6:7)//STR_DAT_BEG(9:10)// &
     &                          '_'//STR_DAT_BEG(12:13)//STR_DAT_BEG(15:16)//STR_DAT_BEG(18:19)// &
     &                         '__'//STR_DAT_END(1:4)//STR_DAT_END(6:7)//STR_DAT_END(9:10)// &
     &                          '_'//STR_DAT_END(12:13)//STR_DAT_END(15:16)//STR_DAT_END(18:19)// &
     &                          '.dat'
!
! --- Seek the beginning of the relevant data record with displacements
!
      IS = LSEEK ( %VAL(LUN_IN), %VAL(OFF_EXTR), %VAL(SEEK_SET) )
      IF ( IS .NE. OFF_EXTR ) THEN
           CALL CLRCH  ( STR )
           CALL INCH8  ( OFF_EXTR, STR )
           CALL ERR_LOG ( 5518, IUER, 'BDS_EXTRACT', 'Error in '// &
     &          'an attempt to position the input file '// &
     &           TRIM(FIL_BDS)//' to offset '//TRIM(STR)// &
     &          ' for seaking the relevant delay section' )
           RETURN 
      END IF
!
! --- Allocate memory for the displacement buffer
!
      ALLOCATE ( DSP_BIN(LEN_EXTR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL INCH8  ( LEN_EXTR, STR )
           CALL ERR_LOG ( 5519, IUER, 'BDS_EXTRACT', 'Error in '// &
     &          'an attempt to allocate '//TRIM(STR)//' bytes of '// &
     &          'dynamic memory for the extracted displacement array' )
           RETURN 
      END IF
!
! --- Read the second section with displacement data 
!
      IS = READ ( %VAL(LUN_IN), DSP_BIN, %VAL(LEN_EXTR) )
      IF ( IS .NE. LEN_EXTR ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5520, IUER, 'BDS_EXTRACT', 'Error in '// &
     &          'an attempt to read the input file '//TRIM(FIL_BDS)// &
     &          ' : '//STR )
           RETURN 
      END IF
!
! --- Close the input file
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_CLOSE ( LUN_IN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5521, IUER, 'BDS_EXTRACT', 'Error in '// &
     &          'an attempt to close file '//FIL_OUT )
           RETURN 
      END IF
!
! --- Open the output file
!
      LUN_OUT = 11
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_OPEN ( FIL_OUT, 'UNKNOWN', LUN_OUT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5522, IUER, 'BDS_EXTRACT', 'Error in opening '// &
     &         'the output file '//FIL_OUT )
           RETURN 
      END IF
!
! --- Write the header section
!
      IS = WRITE ( %VAL(LUN_OUT), HEADER, %VAL(SIZEOF(HEADER)) )
      IF ( IS .NE. SIZEOF(HEADER) ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5523, IUER, 'BDS_EXTRACT', 'Error in '// &
     &          'an attempt to write into the output file '//FIL_OUT )
           RETURN 
      END IF
!
! --- Write displacement section
!
      IS = WRITE ( %VAL(LUN_OUT), DSP_BIN, %VAL(LEN_EXTR) )
      IF ( IS .NE. LEN_EXTR ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5524, IUER, 'BDS_EXTRACT', 'Error in '// &
     &          'an attempt to write into the output file '//FIL_OUT )
           RETURN 
      END IF
      DEALLOCATE ( DSP_BIN )
!
! --- Close the output file
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_CLOSE ( LUN_OUT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5525, IUER, 'BDS_EXTRACT', 'Error in '// &
     &          'an attempt to close file '//FIL_OUT )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  BDS_EXTRACT  !#!#
!
! ------------------------------------------------------------------------
!
     SUBROUTINE BDS_EXTEND ( FIL_BDS, DATA_EXTN_FIL, FL_INPLACE, IUER )
! ************************************************************************
! *                                                                      *
! *   Program  BDS_EXTEND  extends a given file with 3d dispaceents      *
! *   using the input displacement section. The first epoch of the       *
! *   displacement section file should just follow the last epoch of     *
! *   the input file with station displacments.                          *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * FIL_BDS       ( CHARACTER ) -- Name of the input file with station   *
! *                                displacements in bindisp format.      *
! * DATA_EXTN_FIL ( CHARACTER ) -- Name of a file with a section of      *
! *                                header and station discplacements     *
! *                                extracted from an external file.      *
! * FL_INPLACE    ( LOGICAL*1 ) -- If .TRUE., then the input file will   *
! *                                be updated in place.                  *
! *                                If .FALSE., then the output file will *
! *                                be written in the same directory as   *
! *                                DATA_EXTN_FIL.                        *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     IUER ( INTEGER*4, OPT ) -- Universal error handler.              *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  28-NOV-2024  BDS_EXTEND  v1.0 (c) L. Petrov  28-NOV-2024  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'bindisp.i'
      TYPE ( BINDISP_HEADER_4 ) ::  HDR4
      TYPE ( BINDISP_HEADER_8 ) ::  HDR8
      LOGICAL*1  FL_INPLACE
      CHARACTER  FIL_BDS*(*), DATA_EXTN_FIL*(*)
      INTEGER*4  IUER
      CHARACTER  FIL_TMP*128, FIL_OUT*128, STR_DAT_BEG*21, STR_DAT_END*21, &
     &           OLD_HEADER(M__HDR)*(LEN__HDR), NEW_HEADER(M__HDR)*(LEN__HDR), &
     &           STR*128
      LOGICAL*1  LEX
      INTEGER*8  OFF_EXTN, LEN_FIL, LEN_EXTN, LEN_DATA_EXTN
      REAL*8     EPS
      PARAMETER  ( EPS = 180.0D0 )
      REAL*8     TIM_INT, TAI_BEG, TAI_END, EXTN_TAI_BEG, EXTN_TAI_END
      INTEGER*1, ALLOCATABLE :: DSP_BIN(:)
      INTEGER*4  IS, LUN_IN, LUN_OUT, NREC_EXTN, EXTN_MJD_BEG, EXTN_MJD_END, &
     &           MJD_BEG, MJD_END, UNIX_DATE, SEEK_SET, ARG_LEN, &
     &           L_EPC, IDB, IDD, IL, IDAY, IER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: GETPID, ILEN, I_LEN, SYSTEM, RENAME
      ADDRESS__TYPE, EXTERNAL :: LINDEX, FILE_INFO, LSEEK, READ, WRITE
!
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_SET', SEEK_SET, ARG_LEN )
!
! --- Check whether the file with displacement records to be exended exists
!
      INQUIRE ( FILE=FIL_BDS, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 5531, IUER, 'BDS_EXTEND', 'File '// &
     &          TRIM(FIL_BDS)//' does not exist' )
           RETURN 
      END IF
!
! --- Check whether the file with displacement records exists
!
      INQUIRE ( FILE=DATA_EXTN_FIL, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 5532, IUER, 'BDS_EXTEND', 'Extension file '// &
     &          TRIM(DATA_EXTN_FIL)//' does not exist' )
           RETURN 
      END IF
!
! --- Open the input file with displacment records
!
      LUN_IN  = 10
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_OPEN ( FIL_BDS, 'OLD', LUN_IN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5533, IUER, 'BDS_EXTEND', 'Error in opening '// &
     &         'file '//FIL_BDS )
           RETURN 
      END IF
!
! --- Read the old header section
!
      IS = READ ( %VAL(LUN_IN), %REF(OLD_HEADER), %VAL(SIZEOF(OLD_HEADER)) )
      IF ( IS .NE. SIZEOF(OLD_HEADER) ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5534, IUER, 'BDS_EXTEND', 'Error in '// &
     &          'an attempt to read the header section of the '// &
     &          'input file '//TRIM(FIL_BDS)// &
     &          ' : '//STR )
           RETURN 
      END IF
!
! --- Extract the number of epochs and the sampling interval
!
      CALL LIB$MOVC3 ( LEN__HDR, %REF(OLD_HEADER(4)), HDR4 )
      L_EPC = HDR4%NUM_REC
      TIM_INT = HDR4%SAMPLING_INTERVAL 
!
! --- Extract MJD and TAI for the first epoch
!
      CALL LIB$MOVC3 ( LEN__HDR, %REF(OLD_HEADER(8)), HDR8 )
      MJD_BEG = HDR8%MJD_FIRST 
      TAI_BEG = HDR8%TAI_FIRST 
!
      MJD_END = MJD_BEG
      TAI_END = TAI_BEG + (L_EPC-1)*TIM_INT
      IF ( TAI_END > 86400.0D0 - 1.D-6 ) THEN
           IDAY = (TAI_END + 2.D-6)/86400.0D0
           MJD_END = MJD_END + IDAY
           TAI_END = TAI_END - IDAY*86400.0D0
           IF ( TAI_END < 0.0D0 .AND. TAI_END > -3.D-6 ) TAI_END = 0.0D0
      ENDIF
!
! --- Extract the first date of the displacement segment from the file name
!
      IL = ILEN(DATA_EXTN_FIL)
      STR_DAT_BEG = DATA_EXTN_FIL(IL-35:IL-32)//'.'//DATA_EXTN_FIL(IL-31:IL-30)//'.'// &
     &              DATA_EXTN_FIL(IL-29:IL-25)//':'//DATA_EXTN_FIL(IL-24:IL-23)//':'// &
     &              DATA_EXTN_FIL(IL-22:IL-21)
      CALL ERR_PASS ( IUER, IER )
      CALL DATE_TO_TIME ( STR_DAT_BEG, EXTN_MJD_BEG, EXTN_TAI_BEG, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5535, IUER, 'BDS_EXTEND', 'Malformed name of '// &
     &         'file '//DATA_EXTN_FIL )
           RETURN 
      END IF
!
! --- Extract the last date of the displacement segment from the file name
!
      IL = ILEN(DATA_EXTN_FIL)
      STR_DAT_END = DATA_EXTN_FIL(IL-18:IL-15)//'.'//DATA_EXTN_FIL(IL-14:IL-13)//'.'// &
     &              DATA_EXTN_FIL(IL-12:IL-8)//':'//DATA_EXTN_FIL(IL-7:IL-6)//':'// &
     &              DATA_EXTN_FIL(IL-5:IL-4)
      CALL ERR_PASS ( IUER, IER )
      CALL DATE_TO_TIME ( STR_DAT_END, EXTN_MJD_END, EXTN_TAI_END, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5536, IUER, 'BDS_EXTEND', 'Malformed name of '// &
     &         'file '//DATA_EXTN_FIL )
           RETURN 
      END IF
!
! --- Learn the length of the input file with displacment extension
!
      IS = FILE_INFO ( TRIM(DATA_EXTN_FIL)//CHAR(0), UNIX_DATE, LEN_EXTN )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 5537, IUER, 'BDS_EXTEND', 'Error in inquiring '// &
     &         'extention file '//DATA_EXTN_FIL )
           RETURN 
      END IF
!
! --- Determine how many displacement records have to be exended
!
      NREC_EXTN = ( (EXTN_MJD_END - MJD_END)*86400 + &
     &              (EXTN_TAI_END - TAI_END)       + EPS )/TIM_INT
      LEN_DATA_EXTN = LEN_EXTN - SIZEOF(OLD_HEADER) 
      IF ( NREC_EXTN*LEN__BDS + SIZEOF(OLD_HEADER) .NE. LEN_EXTN ) THEN
           WRITE ( 6, * ) 'EXTN_END: '//MJDSEC_TO_DATE ( EXTN_MJD_END, EXTN_TAI_END, IER )
           WRITE ( 6, * ) 'FIL_END:  '//MJDSEC_TO_DATE ( MJD_END, TAI_END, IER )
           WRITE ( 6, * ) 'LEN_EXTN= ', LEN_EXTN
           WRITE ( 6, * ) 'NREC_EXTN ', NREC_EXTN, ' LEN__BDS= ', LEN__BDS, &
     &                    ' SIZEOF(HEADER)= ', SIZEOF(OLD_HEADER)
           CALL ERR_LOG ( 5538, IUER, 'BDS_EXTEND', 'File length mismatch: '// &
     &         'the length of extension file '//TRIM(DATA_EXTN_FIL)// &
     &         ' does not correspond to the length comptued from its name' )
           RETURN 
      END IF
!
! --- Copy the file into a temporary 
!
      CALL CLRCH ( STR )
      WRITE ( UNIT=STR(1:8), FMT='(I8)') GETPID()
      CALL CHASHL ( STR )
      IF ( FL_INPLACE ) THEN
           FIL_TMP = TRIM(FIL_BDS)//'__'//TRIM(STR)
         ELSE
           IDB = LINDEX ( FIL_BDS,      '/' )
           IDD = LINDEX ( DATA_EXTN_FIL, '/' )
           IF ( IDD == 0 ) THEN
                FIL_TMP = TRIM(FIL_BDS(IDB+1:))//'__'//TRIM(STR)
              ELSE
                FIL_TMP = DATA_EXTN_FIL(1:IDD)//TRIM(FIL_BDS(IDB+1:))//'__'//TRIM(STR)
           END IF
      END IF
!
      IS = SYSTEM ( 'cp '//TRIM(FIL_BDS)//' '//TRIM(FIL_TMP)//CHAR(0) )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR(STR)
           CALL ERR_LOG ( 5539, IUER, 'BDS_EXTEND', 'Error in making '// &
     &         ' a temporary copy of file '//TRIM(FIL_BDS)//' : '//STR )
           RETURN 
      END IF
!
! --- Open the input file with displacement extension
!
      LUN_IN  = 10
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_OPEN ( DATA_EXTN_FIL, 'OLD', LUN_IN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5540, IUER, 'BDS_EXTEND', 'Error in opening '// &
     &         'temporary copy of file '//FIL_BDS )
           RETURN 
      END IF
!
! --- Allocate memory for the displacement buffer
!
      ALLOCATE ( DSP_BIN(LEN_DATA_EXTN), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL INCH8  ( LEN_EXTN, STR )
           CALL ERR_LOG ( 5541, IUER, 'BDS_EXTEND', 'Error in '// &
     &          'an attempt to allocate '//TRIM(STR)//' bytes of '// &
     &          'dynamic memory for the extracted displacement array' )
           RETURN 
      END IF
!
! --- Read the new header section
!
      IS = READ ( %VAL(LUN_IN), %REF(NEW_HEADER), %VAL(SIZEOF(NEW_HEADER)) )
      IF ( IS .NE. SIZEOF(NEW_HEADER) ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5542, IUER, 'BDS_EXTEND', 'Error in '// &
     &          'an attempt to read the header section from '// &
     &          'input file '//TRIM(FIL_BDS)// &
     &          ' : '//STR )
           RETURN 
      END IF
!
! --- Read the new data section
!
      IS = READ ( %VAL(LUN_IN), DSP_BIN, %VAL(LEN_DATA_EXTN) )
      IF ( IS .NE. LEN_DATA_EXTN ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5543, IUER, 'BDS_EXTEND', 'Error in '// &
     &          'an attempt to read the data section from input file '//TRIM(FIL_BDS)// &
     &          ' : '//STR )
           RETURN 
      END IF
!
! --- Close the input file
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_CLOSE ( LUN_IN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5544, IUER, 'BDS_EXTEND', 'Error in '// &
     &          'an attempt to close input file '//DATA_EXTN_FIL )
           RETURN 
      END IF
!
      IS = FILE_INFO ( TRIM(FIL_TMP)//CHAR(0), UNIX_DATE, LEN_FIL )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 5545, IUER, 'BDS_EXTEND', 'Error in inquiring '// &
     &         'extention file '//DATA_EXTN_FIL )
           RETURN 
      END IF
!
! --- Open the output file with displacements
!
      LUN_OUT  = 10
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_OPEN ( FIL_TMP, 'UNKNOWN', LUN_OUT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5546, IUER, 'BDS_EXTEND', 'Error in opening '// &
     &         'temporary copy of file '//FIL_TMP )
           RETURN 
      END IF
!
! --- Write there the new header section
!
      IS = WRITE ( %VAL(LUN_OUT), %REF(NEW_HEADER), %VAL(SIZEOF(NEW_HEADER)) )
      IF ( IS .NE. SIZEOF(NEW_HEADER) ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5547, IUER, 'BDS_EXTEND', 'Error in '// &
     &          'an attempt to write into output file '//FIL_TMP )
           RETURN 
      END IF
!
! --- Seek the end of the file
!
      IS = LSEEK ( %VAL(LUN_OUT), %VAL(LEN_FIL), %VAL(SEEK_SET) )
      IF ( IS .NE. LEN_FIL ) THEN
           CALL CLRCH  ( STR )
           CALL INCH8  ( LEN_FIL, STR )
           CALL ERR_LOG ( 5548, IUER, 'BDS_EXTEND', 'Error in '// &
     &          'seeking the end of the input file '//FIL_TMP )
           RETURN 
      END IF
!
! --- Write displacement section
!
      IS = WRITE ( %VAL(LUN_OUT), DSP_BIN, %VAL(LEN_DATA_EXTN) )
      IF ( IS .NE. LEN_DATA_EXTN ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5549, IUER, 'BDS_EXTEND', 'Error in '// &
     &          'an attempt to write into the output file '//FIL_TMP )
           RETURN 
      END IF
      DEALLOCATE ( DSP_BIN )
!
! --- Close the output file
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_CLOSE ( LUN_OUT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5550, IUER, 'BDS_EXTEND', 'Error in '// &
     &          'an attempt to close file '//FIL_TMP )
           RETURN 
      END IF
!
! --- Generate the name of the output file and rename the 
! --- temporary file
!
      IF ( FL_INPLACE ) THEN
           FIL_OUT = FIL_BDS
           IS = RENAME ( TRIM(FIL_TMP)//CHAR(0), TRIM(FIL_BDS)//CHAR(0) )
         ELSE
           IL = ILEN(FIL_TMP)
           FIL_OUT = FIL_TMP(1:IL-9)
           IS = RENAME ( TRIM(FIL_TMP)//CHAR(0), TRIM(FIL_OUT)//CHAR(0) )
      END IF
!
      IF ( IS .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5551, IUER, 'BDS_EXTEND', 'Error in '// &
     &          'an attempt to move the output file '//TRIM(FIL_TMP)// &
     &          ' to '//TRIM(FIL_OUT) )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  BDS_EXTEND !#!#
!
! ------------------------------------------------------------------------
!
     SUBROUTINE BDS_TRUNCATE ( FIL_BDS, DATE_TRUN, IUER )
! ************************************************************************
! *                                                                      *
! *   Program  BDS_TRUNCATE truncates file with site displacemts         *
! *   in bindisp format till the specified epoch. The specified epoch    *
! *   becomes the last epoch of the file.                                *
! *                                                                      *
! *  ### 28-NOV-2024  BDS_TRUNCATE  v1.0 (c)  L. Petrov  28-NOV-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'bindisp.i'
      TYPE ( BINDISP_HEADER_4 ) ::  HDR4
      TYPE ( BINDISP_HEADER_8 ) ::  HDR8
      CHARACTER  FIL_BDS*(*), DATE_TRUN*(*)
      INTEGER*4  IUER
      CHARACTER  FIL_OUT*128, FIL_TMP*128, HEADER(M__HDR)*(LEN__HDR), &
     &           STR_DAT_BEG*21, STR_DAT_END*21, STR*128
      LOGICAL*1  LEX
      INTEGER*8  OFF_TRUN, LEN_TRUN
      REAL*8     EPS
      PARAMETER  ( EPS = 180.0D0 )
      REAL*8     TIM_INT, TAI_BEG, TAI_END, TAI_TRUN
      INTEGER*1, ALLOCATABLE :: DSP_BIN(:)
      INTEGER*4  IS, LUN, L_EPC, MJD_BEG, MJD_END, &
     &           MJD_TRUN, NREC_TRUN, SEEK_SET, ARG_LEN, IDAY, IER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: GETPID, SYSTEM, RENAME
      ADDRESS__TYPE, EXTERNAL :: LSEEK, READ, WRITE, FTRUNCATE
!
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_SET', SEEK_SET, ARG_LEN )
!
! --- Check whether the file in quetion exists
!
      INQUIRE ( FILE=FIL_BDS, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 5561, IUER, 'BDS_TRUNCATE', 'File '// &
     &          TRIM(FIL_BDS)//' does not exist' )
           RETURN 
      END IF
!
! --- Transform extraction date to MJD/TAI
!
      CALL ERR_PASS ( IUER, IER )
      CALL DATE_TO_TIME ( DATE_TRUN, MJD_TRUN, TAI_TRUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5562, IUER, 'BDS_TRUNCATE', 'Failure '// &
     &          'in parsing the extract date '//DATE_TRUN )
           RETURN 
      END IF
!
! --- Open the input file with site displacements
!
      LUN = 10
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_OPEN ( FIL_BDS, 'OLD', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5563, IUER, 'BDS_TRUNCATE', 'Error in opening '// &
     &         'file '//FIL_BDS )
           RETURN 
      END IF
!
! --- Read the header section
!
      IS = READ ( %VAL(LUN), %REF(HEADER), %VAL(SIZEOF(HEADER)) )
      IF ( IS .NE. SIZEOF(HEADER) ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5564, IUER, 'BDS_TRUNCATE', 'Error in '// &
     &          'an attempt to read the header section of the '// &
     &          'input file '//TRIM(FIL_BDS)// &
     &          ' : '//STR )
           RETURN 
      END IF
!
! --- Close the input file
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5565, IUER, 'BDS_TRUNCATE', 'Error in '// &
     &          'an attempt to close file '//FIL_BDS )
           RETURN 
      END IF
!
! --- Extract the number of epochs and the sampling interval
!
      CALL LIB$MOVC3 ( LEN__HDR, %REF(HEADER(4)), HDR4 )
      L_EPC = HDR4%NUM_REC
      TIM_INT = HDR4%SAMPLING_INTERVAL 
!
! --- Extract MJD and TAI for the first epoch
!
      CALL LIB$MOVC3 ( LEN__HDR, %REF(HEADER(8)), HDR8 )
      MJD_BEG = HDR8%MJD_FIRST 
      TAI_BEG = HDR8%TAI_FIRST 
!
      MJD_END = MJD_BEG
      TAI_END = TAI_BEG + (L_EPC-1)*TIM_INT
      IF ( TAI_END > 86400.0D0 - 1.D-6 ) THEN
           IDAY = (TAI_END + 2.D-6)/86400.0D0
           MJD_END = MJD_END + IDAY
           TAI_END = TAI_END - IDAY*86400.0D0
           IF ( TAI_END < 0.0D0 .AND. TAI_END > -3.D-6 ) TAI_END = 0.0D0
      ENDIF
!
! --- Determine how many data records will be truncated
!
      NREC_TRUN = ( (MJD_END - MJD_TRUN)*86400 + &
     &              (TAI_END - TAI_TRUN)       + EPS )/TIM_INT
!
! --- Determine the length of the truncated file
!
      OFF_TRUN = SIZEOF(HEADER) + LEN__BDS*(L_EPC - NREC_TRUN)
!
! --- Check whether the extraction request can be performed
!
      IF ( NREC_TRUN == 0 ) THEN
           WRITE ( 6, '(A)' ) 'Nothing to extract in file '//FIL_BDS
           CALL ERR_LOG ( 0, IUER )
           RETURN 
          ELSE IF ( NREC_TRUN < 0 ) THEN
!
! -------- The requested extraction date is later than the last date
!
           STR = MJDSEC_TO_DATE ( MJD_END, TAI_END, IER )
           STR = 'Last epoch: '//STR(1:21)//' Requested erxtraction: '//DATE_TRUN
           CALL ERR_LOG ( 5567, IUER, 'BDS_TRUNCATE', 'Requested extraction '// &
     &         'date is too late: '//TRIM(STR)//' for file '//FIL_BDS )
           RETURN 
         ELSE IF ( OFF_TRUN .LE. 0 ) THEN
!
! -------- The requested extraction date is earlier than the first date
!
           STR = MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG, IER )
           STR = 'First epoch: '//STR(1:21)//' Requested extraction: '//DATE_TRUN
           CALL ERR_LOG ( 5568, IUER, 'BDS_TRUNCATE', 'Requested extaction '// &
     &         'date is too early: '//TRIM(STR)//' for file '//FIL_BDS )
           RETURN 
      END IF
!
! --- Copy the file into a temporary 
!
      CALL CLRCH ( STR )
      WRITE ( UNIT=STR(1:8), FMT='(I8)') GETPID()
      CALL CHASHL ( STR )
      FIL_TMP = TRIM(FIL_BDS)//'__'//TRIM(STR)
      IS = SYSTEM ( 'cp '//TRIM(FIL_BDS)//' '//TRIM(FIL_TMP)//CHAR(0) )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR(STR)
           CALL ERR_LOG ( 5569, IUER, 'BDS_TRUNCATE', 'Error in making '// &
     &         ' a temporary copy of file '//TRIM(FIL_BDS)//' : '//STR )
           RETURN 
      END IF
!
! --- Update the header
!
      HDR4%NUM_REC = L_EPC - NREC_TRUN
      HDR4%SAMPLING_INTERVAL = TIM_INT 
      CALL LIB$MOVC3 ( LEN__HDR, HDR4, %REF(HEADER(4)) )
!
! --- Open the output file with site displacements
!
      LUN = 10
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_OPEN ( FIL_TMP, 'UNKNOWN', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5570, IUER, 'BDS_TRUNCATE', 'Error in opening '// &
     &         'file '//FIL_BDS )
           RETURN 
      END IF
!
! --- Seek for the file beginning
!
      IS = LSEEK ( %VAL(LUN), %VAL(0), %VAL(SEEK_SET) )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5571, IUER, 'BDS_TRUNCATE', 'Error in '// &
     &          'an attempt to position the temporary file '//TRIM(FIL_TMP)// &
     &          ' to the beginning' )
           RETURN 
      END IF
!
! --- Write the updated header
!
      IS = WRITE ( %VAL(LUN), HEADER, %VAL(SIZEOF(HEADER)) )
      IF ( IS .NE. SIZEOF(HEADER) ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5572, IUER, 'BDS_TRUNCATE', 'Error in '// &
     &          'an attempt to write the updated header record into '// &
     &          'the output file '//FIL_TMP )
           RETURN 
      END IF
!
! --- Truncate the temporary file FIL_TMP
!
      IS = FTRUNCATE ( %VAL(LUN), %VAL(OFF_TRUN) )
      IF ( IS .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5573, IUER, 'BDS_TRUNCATE', 'Error in '// &
     &          'an attempt to truncate the output file '// &
     &          TRIM(FIL_TMP)//' : '//STR  )
           RETURN 
      END IF
!
! --- Close the output temporary file
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5574, IUER, 'BDS_TRUNCATE', 'Error in '// &
     &          'an attempt to close file '//FIL_TMP )
           RETURN 
      END IF
!
! --- Rename the truncated file back
!
      IS = RENAME ( TRIM(FIL_TMP)//CHAR(0), TRIM(FIL_BDS)//CHAR(0) )
      IF ( IS .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5575, IUER, 'BDS_TRUNCATE', 'Error in '// &
     &          'an attempt to move the output file '//TRIM(FIL_TMP)// &
     &          ' to '//TRIM(FIL_BDS) )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  BDS_TRUNCATE  !#!#
!
! ------------------------------------------------------------------------
!
     SUBROUTINE BDS_DIR_TRUNCATE ( BDS_DIR, DATE_TRUN, IUER )
! ************************************************************************
! *                                                                      *
! *   Program  BDS_DIR_TRUNCATE truncates all files in the directory     *
! *   with station displacements in bindisp form till the specified      *
! *   epoch and updates the summary file. The specified epoch becomes    *
! *   the last epoch of the file.                                        *
! *                                                                      *
! *  ### 28-NOV-2024 BDS_DIR_TRUNCATE v1.0 (c) L. Petrov 28-NOV-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'bindisp.i'
      TYPE     ( BINDISP_HEADER_4 ) ::  HDR4
      TYPE     ( BINDISP_HEADER_8 ) ::  HDR8
      CHARACTER  BDS_DIR*(*), DATE_TRUN*(*)
      INTEGER*4  IUER
      INTEGER*4  M_STA
      PARAMETER  ( M_STA = 8192 )
      CHARACTER  FILSUM*128, BUF(M_STA)*128, FIL_BDS(M_STA)*128, &
     &           HEADER(M__HDR)*(LEN__HDR), STA_NAM_HEADER*8, STA_NAM_SUM*8, &
     &           SUM_BEG_STR*21, SUM_END_STR*21, STR*128
      LOGICAL*1  LEX
      REAL*8     EPS
      PARAMETER  ( EPS = 180.0D0 )
      INTEGER*4  MIND
      PARAMETER  ( MIND = 64 )
      INTEGER*8  LEN_BDS, LEN_BDS_1ST, COO_CFS(3)
      INTEGER*4  IS, L_BDS, L_STA, LUN_IN, UNIX_DATE, MJD_BEG, MJD_END, &
     &           MJD_BEG_1ST, MJD_END_1ST, &
     &           SUM_MJD_BEG, SUM_MJD_END, SUM_NEPC, NB, IDAY, LIND, &
     &           IND(2,MIND), NEPC, NEPC_1ST, L_EPC, J1, J2, J3, IER
      REAL*8     TAI_TRUN, TIM_INT, TAI_BEG, TAI_END, TAI_BEG_1ST, TAI_END_1ST, &
     &           SUM_TAI_BEG, SUM_TAI_END, SUM_TIM_BEG, SUM_TIM_END, SUM_TIM_STEP
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
      INTEGER*4, EXTERNAL :: FILE_INFO, GETPID, READ, RENAME, SYSTEM
!
      FILSUM = TRIM(BDS_DIR)//'/bds_summary.txt'
      INQUIRE ( FILE=FILSUM, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 5611, IUER, 'BDS_DIR_TRUNCATE', 'Summary file '// &
     &          TRIM(FILSUM)//' was not found' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILSUM, M_STA, BUF, NB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5612, IUER, 'BDS_DIR_TRUNCATE', 'Error in reading '// &
     &          'summary file '//FILSUM )
           RETURN
      END IF
!
      IF ( BUF(1)(1:LEN(BINDISP_SUMMARY__LABEL)) .NE. BINDISP_SUMMARY__LABEL ) THEN
           CALL ERR_LOG ( 5613, IUER, 'BDS_DIR_TRUNCATE', 'Wrong format '// &
     &         'of file '//TRIM(FILSUM)//' -- its first line is '// &
     &          TRIM(BUF(1))//' while '//BINDISP_SUMMARY__LABEL//' was expected' )
           RETURN
      END IF
!
      L_BDS = 0
      DO 410 J1=1,NB
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IER )
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'MIN_EPOCH:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I5)'    ) SUM_MJD_BEG
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT='(F10.5)' ) SUM_TAI_BEG
              SUM_BEG_STR = BUF(J1)(IND(1,4):IND(2,4))
              SUM_TIM_BEG = (SUM_MJD_BEG - J2000__MJD)*86400.D0 + SUM_TAI_BEG
         END IF
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'MAX_EPOCH:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I5)'    ) SUM_MJD_END
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT='(F10.5)' ) SUM_TAI_END
              SUM_END_STR = BUF(J1)(IND(1,4):IND(2,4))
              SUM_TIM_END = (SUM_MJD_END - J2000__MJD)*86400.D0 + SUM_TAI_END
         END IF
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'L_EPC:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I6)'    ) SUM_NEPC
         END IF
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'L_STA:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I6)'    ) L_STA
         END IF
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'SMP_INTRV:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F10.5)' ) SUM_TIM_STEP
         END IF
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'STA:' ) THEN
              STA_NAM_SUM = BUF(J1)(IND(1,3):IND(2,3))
              L_BDS = L_BDS + 1
              FIL_BDS(L_BDS) = TRIM(BDS_DIR)//'/'//TRIM(STA_NAM_SUM)//'.bds'
              INQUIRE ( FILE=FIL_BDS(L_BDS), EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 5614, IUER, 'BDS_DIR_TRUNCATE', 'Cannot '// &
     &                 'find file with site deisplacments '//FIL_BDS(L_BDS) )
                   RETURN 
              END IF
!
! ----------- Learn the length of the input file with site displacement
!
              IS = FILE_INFO ( TRIM(FIL_BDS(L_BDS))//CHAR(0), UNIX_DATE, LEN_BDS )
              IF ( IS .NE. 0 ) THEN
                   CALL GERROR ( STR )
                   CALL ERR_LOG ( 5615, IUER, 'BDS_DIR_TRUNCATE', 'Error in '// &
     &                 'checking the size of station displacement file '//FIL_BDS(L_BDS) )
                   RETURN 
              END IF
!
! ----------- Open the input file with site displacments 
!
              LUN_IN  = 10
              CALL ERR_PASS ( IUER, IER )
              CALL BINF_OPEN ( FIL_BDS(L_BDS), 'OLD', LUN_IN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5616, IUER, 'BDS_DIR_TRUNCATE', 'Error in opening '// &
     &                 'file '//FIL_BDS(L_BDS) )
                   RETURN 
              END IF
!        
! ----------- Read the header section
!
              IS = READ ( %VAL(LUN_IN), %REF(HEADER), %VAL(SIZEOF(HEADER)) )
              IF ( IS .NE. SIZEOF(HEADER) ) THEN
                   CALL CLRCH  ( STR )
                   CALL GERROR(STR)
                   CALL ERR_LOG ( 5617, IUER, 'BDS_DIR_TRUNCATE', 'Error in '// &
     &                  'an attempt to read the header section of the '// &
     &                  'input file '//TRIM(FIL_BDS(L_BDS))//' : '//STR )
                   RETURN 
              END IF
!
! ----------- Close the input file
!
              CALL ERR_PASS ( IUER, IER )
              CALL BINF_CLOSE ( LUN_IN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH  ( STR )
                   CALL GERROR(STR)
                   CALL ERR_LOG ( 5618, IUER, 'BDS_DIR_TRUNCATE', 'Error in '// &
     &                  'an attempt to close file '//FIL_BDS(L_BDS) )
                   RETURN 
              END IF
!
! ----------- Extract the number of epochs and the sampling interval
!
              CALL LIB$MOVC3 ( LEN__HDR, %REF(HEADER(4)), HDR4 )
              L_EPC = HDR4%NUM_REC
              TIM_INT = HDR4%SAMPLING_INTERVAL 
!
! ----------- Extract MJD and TAI for the first epoch
!
              CALL LIB$MOVC3 ( LEN__HDR, %REF(HEADER(8)), HDR8 )
              MJD_BEG = HDR8%MJD_FIRST 
              TAI_BEG = HDR8%TAI_FIRST 
!
              MJD_END = MJD_BEG
              TAI_END = TAI_BEG + (L_EPC-1)*TIM_INT
              IF ( TAI_END > 86400.0D0 - 1.D-6 ) THEN
                   IDAY = (TAI_END + 2.D-6)/86400.0D0
                   MJD_END = MJD_END + IDAY
                   TAI_END = TAI_END - IDAY*86400.0D0
                   IF ( TAI_END < 0.0D0 .AND. TAI_END > -3.D-6 ) TAI_END = 0.0D0
              END IF
              CALL LIB$MOVC3 ( LEN__HDR, %REF(HEADER(3)), %REF(STA_NAM_HEADER) )
              CALL LIB$MOVC3 ( LEN__HDR, %REF(HEADER(5)), COO_CFS(1) )
              CALL LIB$MOVC3 ( LEN__HDR, %REF(HEADER(6)), COO_CFS(2) )
              CALL LIB$MOVC3 ( LEN__HDR, %REF(HEADER(7)), COO_CFS(3) )
!
! ----------- Perform checks
!
              IF ( L_BDS == 1 ) THEN
                   NEPC_1ST    = L_EPC
                   MJD_BEG_1ST = MJD_BEG
                   TAI_BEG_1ST = TAI_BEG
                   MJD_END_1ST = MJD_END
                   TAI_END_1ST = TAI_END
                   IF ( NEPC_1ST .NE. SUM_NEPC ) THEN
                        CALL ERR_LOG ( 5619, IUER, 'BDS_DIR_TRUNCATE', 'Mismatch '// &
     &                       'in the number of epochs in the summary file '// &
     &                       TRIM(FILSUM)//' and in the station displacement file '// &
     &                       FIL_BDS(L_BDS) )
                        RETURN 
                   END IF
!
                   IF ( DABS( ((MJD_BEG_1ST - J2000__MJD)*86400.0D0 + TAI_BEG_1ST) - &
     &                        ((SUM_MJD_BEG - J2000__MJD)*86400.0D0 + SUM_TAI_BEG) ) > &
     &                  EPS ) THEN
                        CALL ERR_LOG ( 5620, IUER, 'BDS_DIR_TRUNCATE', 'Mismatch '// &
     &                       'in the start epoch in the summary file '// &
     &                       TRIM(FILSUM)//' and in the station displacement file '// &
     &                       FIL_BDS(L_BDS) )
                        RETURN 
                   END IF
!
                   IF ( DABS( ((MJD_END_1ST - J2000__MJD)*86400.0D0 + TAI_END_1ST) - &
     &                        ((SUM_MJD_END - J2000__MJD)*86400.0D0 + SUM_TAI_END) ) > &
     &                  EPS ) THEN
                        CALL ERR_LOG ( 5621, IUER, 'BDS_DIR_TRUNCATE', 'Mismatch '// &
     &                       'in the end epoch in the summary file '// &
     &                       TRIM(FILSUM)//' and in the station displacement file'// &
     &                       FIL_BDS(L_BDS) )
                        RETURN 
                   END IF
                 ELSE
                   NEPC    = L_EPC
                   IF ( NEPC_1ST .NE. NEPC ) THEN
                        CALL ERR_LOG ( 5622, IUER, 'BDS_DIR_TRUNCATE', 'Mismatch '// &
     &                       'in the number of displacements in the summary file '// &
     &                       TRIM(FILSUM)//' and in the station displacement file'// &
     &                       FIL_BDS(L_BDS) )
                        RETURN 
                   END IF
!
                   IF ( DABS( ((MJD_BEG     - J2000__MJD)*86400.0D0 + TAI_BEG) - &
     &                        ((SUM_MJD_BEG - J2000__MJD)*86400.0D0 + SUM_TAI_BEG) ) > &
     &                  EPS ) THEN
                        CALL ERR_LOG ( 5623, IUER, 'BDS_DIR_TRUNCATE', 'Mismatch '// &
     &                       'in the start epoch in the summary file '// &
     &                       TRIM(FILSUM)//' and in the station displacement file'// &
     &                       FIL_BDS(L_BDS) )
                        RETURN 
                   END IF
!
                   IF ( DABS( ((MJD_END     - J2000__MJD)*86400.0D0 + TAI_END    ) - &
     &                        ((SUM_MJD_END - J2000__MJD)*86400.0D0 + SUM_TAI_END) ) < &
     &                  -EPS ) THEN
                        WRITE ( 6, * ) 'MJD_END=     ', MJD_END,     ' TAI_END=     ', TAI_END 
                        WRITE ( 6, * ) 'SUM_MJD_END= ', SUM_MJD_END, ' SUM_TAI_END= ', SUM_TAI_END 
                        CALL ERR_LOG ( 5624, IUER, 'BDS_DIR_TRUNCATE', 'Mismatch '// &
     &                       'in the end epoch in the summary file '// &
     &                       TRIM(FILSUM)//' and in the station displacement file'// &
     &                       FIL_BDS(L_BDS) )
                        RETURN 
                   END IF
              END IF
         END IF
 410  CONTINUE 
!
! --- Now truncate files
!
      DO 420 J2=1,L_BDS
         CALL ERR_PASS ( IUER, IER )
         CALL BDS_TRUNCATE ( FIL_BDS(J2), DATE_TRUN, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 5625, IUER, 'BDS_DIR_TRUNCATE', 'Failure in '// &
     &            'an attempt to truncate station displacement file in bindisp '//&
     &            'format '//FIL_BDS(J2) )
              RETURN
         END IF
 420  CONTINUE 
!
! --- Set new last epoch and new number of epochs
!
      CALL DATE_TO_TIME ( DATE_TRUN, MJD_END, TAI_END, IER )
      L_EPC = NINT( ((MJD_END - MJD_BEG)*86400.0D0 + (TAI_END - TAI_BEG))/TIM_INT )  + 1
!
      DO 430 J3=1,NB
         CALL EXWORD ( BUF(J3), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IER )
         IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'LAST_UPDATE:' ) THEN
              WRITE ( BUF(J3), '(A)' ) 'LAST_UPDATE: '//GET_CDATE()
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'MAX_EPOCH:' ) THEN
              WRITE ( BUF(J3), 110 ) 'MAX_EPOCH: ', MJD_END, TAI_END, &
     &                                MJDSEC_TO_DATE ( MJD_END, TAI_END, IER )
 110          FORMAT ( A, I5,' ',F7.1,' ',A )
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'L_EPC:' ) THEN
              WRITE ( BUF(J3), '(A,I9)'  ) 'L_EPC: ', L_EPC
            ELSE IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'STA:' ) THEN
              STR = MJDSEC_TO_DATE ( MJD_END, TAI_END, IER )
              BUF(J3)(42:60) = STR(1:19)
              WRITE ( UNIT=BUF(J3)(65:70), FMT='(I6)' ) L_EPC
         END IF
 430  CONTINUE 
!
      CALL ERR_PASS ( IUER, IER )
      CALL WR_TEXT ( NB, BUF, FILSUM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5625, IUER, 'BDS_DIR_TRUNCATE', 'Failure in '// &
     &         'an attempt to re-write simmary file '//FILSUM )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  BDS_DIR_TRUNCATE  !#!#
!
! ------------------------------------------------------------------------
!
     SUBROUTINE BDS_CHECK ( BDS_DIR, IUER )
! ************************************************************************
! *                                                                      *
! *   Program  BDS_CHECK 
! *                                                                      *
! *  ### 28-NOV-2024    BDS_CHECK    v1.0 (c) L. Petrov 28-NOV-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'bindisp.i'
      TYPE     ( BINDISP_HEADER_4 ) ::  HDR4
      TYPE     ( BINDISP_HEADER_8 ) ::  HDR8
      CHARACTER  BDS_DIR*(*)
      INTEGER*4  IUER
      INTEGER*4  M_STA
      PARAMETER  ( M_STA = 8192 )
      CHARACTER  FILSUM*128, BUF(M_STA)*128, FIL_BDS(M_STA)*128, &
     &           HEADER(M__HDR)*(LEN__HDR), STA_NAM_HEADER*8, STA_NAM_SUM*8, &
     &           SUM_BEG_STR*21, SUM_END_STR*21, STR*128, STR1*128, STR2*128, &
     &           STATUS_LINE*256, STA_MIN_BEG_STR*21, STA_MAX_BEG_STR*21, &
     &           STA_MIN_END_STR*21, STA_MAX_END_STR*21, NUM_EPC_STR*9, &
     &           NUM_EPC_MIN_STR*9, NUM_EPC_MAX_STR*9, C_ERR(M_STA)*128
      LOGICAL*1  LEX, FL_WRONG_SIZE_LEN
      REAL*8     EPS
      PARAMETER  ( EPS = 180.0D0 )
      INTEGER*4  MIND
      PARAMETER  ( MIND = 64 )
      INTEGER*8  LEN_BDS, LEN_BDS_1ST, COO_CFS(3)
      INTEGER*4  IS, L_BDS, L_STA, LUN_IN, UNIX_DATE, MJD_BEG, MJD_END, &
     &           MJD_BEG_1ST, MJD_END_1ST, &
     &           SUM_MJD_BEG, SUM_MJD_END, STA_MJD_BEG, STA_MJD_END, &
     &           SUM_NEPC, NB, IDAY, LIND, &
     &           IND(2,MIND), NEPC, NEPC_1ST, L_EPC, N_ERR, N_NFD, &
     &           NUM_EPC, NUM_EPC_MIN, NUM_EPC_MAX, J1, J2, J3, IER
      REAL*8     TAI_TRUN, TAI_BEG, TAI_END, TAI_BEG_1ST, TAI_END_1ST, &
     &           SUM_TAI_BEG, SUM_TAI_END, SUM_TIM_BEG, SUM_TIM_END, SUM_TIM_STEP, &
     &           STA_TIM_BEG, STA_TIM_END, MIN_STA_TIM_BEG, MAX_STA_TIM_BEG, &
     &           MIN_STA_TIM_END, MAX_STA_TIM_END, TIM_STEP
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19, TIM_TO_DATE*23
      INTEGER*4, EXTERNAL :: FILE_INFO, GETPID, READ, RENAME, SYSTEM
!
      FILSUM = TRIM(BDS_DIR)//'/bds_summary.txt'
      INQUIRE ( FILE=FILSUM, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 5631, IUER, 'BDS_CHECK', 'Summary file '// &
     &          TRIM(FILSUM)//' was not found' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILSUM, M_STA, BUF, NB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5632, IUER, 'BDS_CHECK', 'Error in reading '// &
     &          'summary file '//FILSUM )
           RETURN
      END IF
!
      IF ( BUF(1)(1:LEN(BINDISP_SUMMARY__LABEL)) .NE. BINDISP_SUMMARY__LABEL ) THEN
           CALL ERR_LOG ( 5633, IUER, 'BDS_CHECK', 'Wrong format '// &
     &         'of file '//TRIM(FILSUM)//' -- its first line is '// &
     &          TRIM(BUF(1))//' while '//BINDISP_SUMMARY__LABEL//' was expected' )
           RETURN
      END IF
!
      L_BDS = 0
      STA_MJD_BEG = -999999
      FL_WRONG_SIZE_LEN = .FALSE.
      DO 410 J1=1,NB
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IER )
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'MIN_EPOCH:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I5)'    ) SUM_MJD_BEG
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT='(F10.5)' ) SUM_TAI_BEG
              SUM_BEG_STR = BUF(J1)(IND(1,4):IND(2,4))
              SUM_TIM_BEG = (SUM_MJD_BEG - J2000__MJD)*86400.D0 + SUM_TAI_BEG
         END IF
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'MAX_EPOCH:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I5)'    ) SUM_MJD_END
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT='(F10.5)' ) SUM_TAI_END
              SUM_END_STR = BUF(J1)(IND(1,4):IND(2,4))
              SUM_TIM_END = (SUM_MJD_END - J2000__MJD)*86400.D0 + SUM_TAI_END
         END IF
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'L_EPC:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I6)'    ) SUM_NEPC
         END IF
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'L_STA:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I6)'    ) L_STA
         END IF
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'SMP_INTRV:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F10.5)' ) SUM_TIM_STEP
         END IF
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'STA:' ) THEN
              L_BDS = L_BDS + 1
              STA_NAM_SUM = BUF(J1)(IND(1,3):IND(2,3))
              FIL_BDS(L_BDS) = TRIM(BDS_DIR)//'/'//TRIM(STA_NAM_SUM)//'.bds'
              INQUIRE ( FILE=FIL_BDS(L_BDS), EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 5634, IUER, 'BDS_CHECK', 'Cannot '// &
     &                 'find file with site deisplacments '//FIL_BDS(L_BDS) )
                   RETURN 
              END IF
!
! ----------- Learn the length of the input file with site displacement
!
              IS = FILE_INFO ( TRIM(FIL_BDS(L_BDS))//CHAR(0), UNIX_DATE, LEN_BDS )
              IF ( IS .NE. 0 ) THEN
                   CALL GERROR ( STR )
                   CALL ERR_LOG ( 5635, IUER, 'BDS_CHECK', 'Error in '// &
     &                 'checking the size of station displacement file '//FIL_BDS(L_BDS) )
                   RETURN 
              END IF
!
! ----------- Open the input file with site displacments 
!
              LUN_IN  = 10
              CALL ERR_PASS ( IUER, IER )
              CALL BINF_OPEN ( FIL_BDS(L_BDS), 'OLD', LUN_IN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5636, IUER, 'BDS_CHECK', 'Error in opening '// &
     &                 'file '//FIL_BDS(L_BDS) )
                   RETURN 
              END IF
!        
! ----------- Read the header section
!
              IS = READ ( %VAL(LUN_IN), %REF(HEADER), %VAL(SIZEOF(HEADER)) )
              IF ( IS .NE. SIZEOF(HEADER) ) THEN
                   CALL CLRCH  ( STR )
                   CALL GERROR(STR)
                   CALL ERR_LOG ( 5637, IUER, 'BDS_CHECK', 'Error in '// &
     &                  'an attempt to read the header section of the '// &
     &                  'input file '//TRIM(FIL_BDS(L_BDS))//' : '//STR )
                   RETURN 
              END IF
!
! ----------- Close the input file
!
              CALL ERR_PASS ( IUER, IER )
              CALL BINF_CLOSE ( LUN_IN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH  ( STR )
                   CALL GERROR(STR)
                   CALL ERR_LOG ( 5638, IUER, 'BDS_CHECK', 'Error in '// &
     &                  'an attempt to close file '//FIL_BDS(L_BDS) )
                   RETURN 
              END IF
!
! ----------- Extract the number of epochs and the sampling interval
!
              CALL LIB$MOVC3 ( LEN__HDR, %REF(HEADER(4)), HDR4 )
              L_EPC = HDR4%NUM_REC
              TIM_STEP = HDR4%SAMPLING_INTERVAL 
!
! ----------- Extract MJD and TAI for the first epoch
!
              CALL LIB$MOVC3 ( LEN__HDR, %REF(HEADER(8)), HDR8 )
              MJD_BEG = HDR8%MJD_FIRST 
              TAI_BEG = HDR8%TAI_FIRST 
!
              MJD_END = MJD_BEG
              TAI_END = TAI_BEG + (L_EPC-1)*TIM_STEP
              IF ( TAI_END > 86400.0D0 - 1.D-6 ) THEN
                   IDAY = (TAI_END + 2.D-6)/86400.0D0
                   MJD_END = MJD_END + IDAY
                   TAI_END = TAI_END - IDAY*86400.0D0
                   IF ( TAI_END < 0.0D0 .AND. TAI_END > -3.D-6 ) TAI_END = 0.0D0
              END IF
              CALL LIB$MOVC3 ( LEN__HDR, %REF(HEADER(3)), %REF(STA_NAM_HEADER) )
              CALL LIB$MOVC3 ( LEN__HDR, %REF(HEADER(5)), COO_CFS(1) )
              CALL LIB$MOVC3 ( LEN__HDR, %REF(HEADER(6)), COO_CFS(2) )
              CALL LIB$MOVC3 ( LEN__HDR, %REF(HEADER(7)), COO_CFS(3) )
              STA_TIM_BEG = (MJD_BEG - J2000__MJD)*86400.D0 + TAI_BEG
              STA_TIM_END = (MJD_END - J2000__MJD)*86400.D0 + TAI_END
!
              IF (  L_EPC .NE. (LEN_BDS - M__HDR*LEN__HDR)/LEN__BDS ) THEN
                    CALL CLRCH ( STR1 )
                    CALL CLRCH ( STR2 )
                    CALL INCH  ( L_EPC, STR1 )
                    CALL INCH8 ( (LEN_BDS - M__HDR*LEN__HDR)/LEN__BDS, STR2 )
                    CALL ERR_LOG ( 5639, IUER, 'BDS_CHECK', 'Mismatch '// &
     &                  'in the number of epochs in the header '//TRIM(STR1)// &
     &                  ' and computed from the file length '//TRIM(STR2)// &
     &                  ' in the station displacement file '//FIL_BDS(L_BDS) )
                    RETURN 
              END IF
!
              IF ( STA_MJD_BEG == -999999 ) THEN
                   MIN_STA_TIM_BEG = STA_TIM_BEG
                   MAX_STA_TIM_BEG = STA_TIM_BEG
                   MIN_STA_TIM_END = STA_TIM_END 
                   MAX_STA_TIM_END = STA_TIM_END
                   STA_MJD_BEG     = MJD_BEG 
                 ELSE
                   IF ( (STA_TIM_BEG - MIN_STA_TIM_BEG) < -EPS ) THEN
                        MIN_STA_TIM_BEG = STA_TIM_BEG
                   END IF
                   IF ( (STA_TIM_BEG - MAX_STA_TIM_BEG) >  EPS ) THEN
                        MAX_STA_TIM_BEG = STA_TIM_BEG
                   END IF
                   IF ( (STA_TIM_END - MIN_STA_TIM_END) < -EPS ) THEN
                        MIN_STA_TIM_END = STA_TIM_END
                        N_ERR = N_ERR + 1
                        NUM_EPC = NINT( (SUM_TIM_END - MIN_STA_TIM_END)/SUM_TIM_STEP )
                        CALL CLRCH (          NUM_EPC_STR )
                        CALL INCH  ( NUM_EPC, NUM_EPC_STR )
                        C_ERR(N_ERR) = 'Min epoch is too early for station '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                                 ' at '//TRIM(NUM_EPC_STR)//' epochs' 
                   END IF
                   IF ( (STA_TIM_END - MAX_STA_TIM_END) >  EPS ) THEN
                        MAX_STA_TIM_END = STA_TIM_END
                        N_ERR = N_ERR + 1
                        C_ERR(N_ERR) = 'Max epoch is too late for station '//BUF(J1)(IND(1,2):IND(2,2))
                   END IF
              END IF
!
! ----------- Perform checks
!
              IF ( L_BDS == 1 ) THEN
                   NEPC_1ST    = L_EPC
                   MJD_BEG_1ST = MJD_BEG
                   TAI_BEG_1ST = TAI_BEG
                   MJD_END_1ST = MJD_END
                   TAI_END_1ST = TAI_END
                   IF ( NEPC_1ST .NE. SUM_NEPC ) THEN
                        WRITE ( 6, * ) 'SUM_NEPC= ', SUM_NEPC, ' NEPC_1ST= ', NEPC_1ST
                        CALL ERR_LOG ( 5640, IUER, 'BDS_CHECK', 'Mismatch '// &
     &                       'in the number of epochs in the summary file '// &
     &                       TRIM(FILSUM)//' and in the station displacement file '// &
     &                       FIL_BDS(L_BDS) )
                        RETURN 
                   END IF
!
                   IF ( DABS( ((MJD_BEG_1ST - J2000__MJD)*86400.0D0 + TAI_BEG_1ST) - &
     &                        ((SUM_MJD_BEG - J2000__MJD)*86400.0D0 + SUM_TAI_BEG) ) > &
     &                  EPS ) THEN
                        CALL ERR_LOG ( 5641, IUER, 'BDS_CHECK', 'Mismatch '// &
     &                       'in the start epoch in the summary file '// &
     &                       TRIM(FILSUM)//' and in the station displacement file '// &
     &                       FIL_BDS(L_BDS) )
                        RETURN 
                   END IF
!
                   IF ( DABS( ((MJD_END_1ST - J2000__MJD)*86400.0D0 + TAI_END_1ST) - &
     &                        ((SUM_MJD_END - J2000__MJD)*86400.0D0 + SUM_TAI_END) ) > &
     &                  EPS ) THEN
                        STR1 = MJDSEC_TO_DATE ( SUM_MJD_END, SUM_TAI_END, IER )
                        STR2 = MJDSEC_TO_DATE ( MJD_END_1ST, TAI_END_1ST, IER )
                        CALL ERR_LOG ( 5642, IUER, 'BDS_CHECK', 'Mismatch '// &
     &                       'in the end epoch in the summary file '// &
     &                       TRIM(FILSUM)//' -- '//STR1(1:19)//' and in the '// &
     &                       'station displacement file '//TRIM(FIL_BDS(L_BDS))// &
     &                       ' -- '//STR2(1:19) )
                        RETURN 
                   END IF
                 ELSE
                   NEPC    = L_EPC
                   MJD_BEG = MJD_BEG
                   TAI_BEG = TAI_BEG
                   MJD_END = MJD_END
                   TAI_END = TAI_END
                   IF ( NEPC_1ST .NE. NEPC ) THEN
                        CALL ERR_LOG ( 5643, IUER, 'BDS_CHECK', 'Mismatch '// &
     &                       'in the number of displacements in the summary file '// &
     &                       TRIM(FILSUM)//' and in the station displacement file'// &
     &                       FIL_BDS(L_BDS) )
                        RETURN 
                   END IF
!
                   IF ( DABS( ((MJD_BEG     - J2000__MJD)*86400.0D0 + TAI_BEG) - &
     &                        ((SUM_MJD_BEG - J2000__MJD)*86400.0D0 + SUM_TAI_BEG) ) > &
     &                  EPS ) THEN
                        CALL ERR_LOG ( 5644, IUER, 'BDS_CHECK', 'Mismatch '// &
     &                       'in the start epoch in the summary file '// &
     &                       TRIM(FILSUM)//' and in the station displacement file'// &
     &                       FIL_BDS(L_BDS) )
                        RETURN 
                   END IF
!
                   IF ( DABS( ((MJD_END     - J2000__MJD)*86400.0D0 + TAI_END    ) - &
     &                        ((SUM_MJD_END - J2000__MJD)*86400.0D0 + SUM_TAI_END) ) < &
     &                  -EPS ) THEN
                        WRITE ( 6, * ) 'MJD_END=     ', MJD_END,     ' TAI_END=     ', TAI_END 
                        WRITE ( 6, * ) 'SUM_MJD_END= ', SUM_MJD_END, ' SUM_TAI_END= ', SUM_TAI_END 
                        CALL ERR_LOG ( 5645, IUER, 'BDS_CHECK', 'Mismatch '// &
     &                       'in the end epoch in the summary file '// &
     &                       TRIM(FILSUM)//' and in the station displacement file'// &
     &                       FIL_BDS(L_BDS) )
                        RETURN 
                   END IF
              END IF
         END IF
 410  CONTINUE 
      STATUS_LINE = 'OK'
      IF ( DABS(MIN_STA_TIM_BEG - SUM_TIM_BEG) > EPS .OR. &
     &     DABS(MAX_STA_TIM_BEG - SUM_TIM_BEG) > EPS      ) THEN
           STATUS_LINE = 'Mismatch in the start date'
      END IF
!
      IF ( L_BDS .NE. L_STA ) THEN
           STATUS_LINE = 'Mismatch in the number of stations'
      END IF
!
      IF ( (TIM_STEP - SUM_TIM_STEP) > 1.D-6) THEN
           STATUS_LINE = 'Mismatch in time interval'
      END IF
!
      IF ( DABS(MIN_STA_TIM_END - SUM_TIM_END) > EPS .OR. &
     &     DABS(MAX_STA_TIM_END - SUM_TIM_END) > EPS      ) THEN
           NUM_EPC = NINT( (SUM_TIM_END - MIN_STA_TIM_END)/SUM_TIM_STEP )
           STA_MJD_END = MIN_STA_TIM_END/86400.0 + J2000__MJD
           STA_TIM_END = MIN_STA_TIM_END - (STA_MJD_END - J2000__MJD)*86400.0D0
           WRITE  ( UNIT=STATUS_LINE, FMT=220 ) NUM_EPC, NINT(MIN_STA_TIM_END/SUM_TIM_STEP), &
     &                                          TIM_TO_DATE ( MIN_STA_TIM_END, IER ), &
     &                                          STA_MJD_END, STA_TIM_END 
 220       FORMAT ( 'Mismatch in the end date at ', I9, ' epochs. ', &
     &              'Number of common epochs: ', I9, &
     &              ' Min epoch: ', A, 2X, I5, 2X, F7.1 )
      END IF
      STA_MIN_BEG_STR = TIM_TO_DATE ( MIN_STA_TIM_BEG, IER )
      STA_MAX_BEG_STR = TIM_TO_DATE ( MAX_STA_TIM_BEG, IER )
      STA_MIN_END_STR = TIM_TO_DATE ( MIN_STA_TIM_END, IER )
      STA_MAX_END_STR = TIM_TO_DATE ( MAX_STA_TIM_END, IER )
      NUM_EPC_MIN = NINT( (SUM_TIM_END - MIN_STA_TIM_END)/SUM_TIM_STEP )
      NUM_EPC_MAX = NINT( (SUM_TIM_END - MAX_STA_TIM_END)/SUM_TIM_STEP )
      IF ( N_NFD > 0 ) STATUS_LINE = 'Not all files with path were found'
      IF ( FL_WRONG_SIZE_LEN ) STATUS_LINE = 'Wrong file size'
!
      WRITE ( 6, 110   ) TRIM(STATUS_LINE)
      WRITE ( 6, 120   ) SUM_NEPC, SUM_TIM_STEP
      WRITE ( 6, 130   ) SUM_BEG_STR, STA_MIN_BEG_STR, STA_MAX_BEG_STR
      WRITE ( 6, 140   ) SUM_END_STR, STA_MIN_END_STR, STA_MAX_END_STR, &
     &                   NUM_EPC_MIN, NUM_EPC_MAX
 110  FORMAT ( 'Status: ', A )
 120  FORMAT ( 'BDS_Summary Num_epochs: ', I6, ' Time_step: ', F8.1 ) 
 130  FORMAT ( 'BDS_Summary Beg epoch:  ', A, &
     &         ' BDS Beg_min epoch: ', A, ' BDS Beg_max epoch: ', A )
 140  FORMAT ( 'BDS_Summary End epoch:  ', A, &
     &         ' BDS End_min epoch: ', A, ' BDS End_max epoch: ', A, &
     &         ' Num_min_epochs: ', I9, ' Num_max_epochs: ', I9 )
      IF ( N_ERR > 0 ) THEN
!
! -------- Print error messages
!
           DO 420 J2=1,N_ERR
              WRITE ( 6, '(A)' ) TRIM(C_ERR(J2))
 420       CONTINUE 
      END IF
!
      IF ( STATUS_LINE == 'OK' ) THEN
           CALL ERR_LOG ( 0, IUER )
         ELSE
           CALL ERR_PASS ( 1, IUER )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  BDS_CHECK  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE BDS_INFO ( FIL_BDS, STR_MODE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  BDS_INFO
! *                                                                      *
! *  ### 28-NOV-2024   BDS_INFO   v1.0  (c)  L. Petrov  28-NOV-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'bindisp.i'
      TYPE ( BINDISP_HEADER_4 ) ::  HDR4
      TYPE ( BINDISP_HEADER_8 ) ::  HDR8
      CHARACTER  FIL_BDS*(*), STR_MODE*(*)
      INTEGER*4  IUER
      CHARACTER  FIL_OUT*128, HEADER(M__HDR)*(LEN__HDR), STR*128, &
     &           STA_NAM*8, STR_DAT_BEG*21, STR_DAT_END*21
      LOGICAL*1  LEX
      INTEGER*8  SIZE_I8 
      REAL*8     EPS
      PARAMETER  ( EPS = 180.0D0 )
      REAL*8     TIM_INT, TAI_BEG, TAI_END, COO_CFS(3)
      INTEGER*1, ALLOCATABLE :: DSP_BIN(:)
      INTEGER*4  IS, LUN_IN, LUN_OUT, L_EPC, MJD_BEG, MJD_END, &
     &           SEEK_SET, ARG_LEN, IDAY, UNIX_DATE, IP, J1, IER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: FILE_INFO, GETPID, SYSTEM, RENAME
      ADDRESS__TYPE, EXTERNAL :: LSEEK, READ, WRITE
!
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_SET', SEEK_SET, ARG_LEN )
!
! --- Check whether the file in quetion exists
!
      INQUIRE ( FILE=FIL_BDS, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 5581, IUER, 'BDS_INFO', 'File '// &
     &          TRIM(FIL_BDS)//' does not exist' )
           RETURN 
      END IF
!
! --- Open the input file with site displacments 
!
      LUN_IN  = 10
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_OPEN ( FIL_BDS, 'OLD', LUN_IN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5583, IUER, 'BDS_INFO', 'Error in opening '// &
     &         'file '//FIL_BDS )
           RETURN 
      END IF
!
! --- Read the header section
!
      IS = READ ( %VAL(LUN_IN), %REF(HEADER), %VAL(SIZEOF(HEADER)) )
      IF ( IS .NE. SIZEOF(HEADER) ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5584, IUER, 'BDS_INFO', 'Error in '// &
     &          'an attempt to read the header section of the '// &
     &          'input file '//TRIM(FIL_BDS)// &
     &          ' : '//STR )
           RETURN 
      END IF
!
! --- Close the input file
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_CLOSE ( LUN_IN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5585, IUER, 'BDS_INFO', 'Error in '// &
     &          'an attempt to close file '//FIL_OUT )
           RETURN 
      END IF
!
! --- Extract the number of epochs and the sampling interval
!
      CALL LIB$MOVC3 ( LEN__HDR, %REF(HEADER(4)), HDR4 )
      L_EPC = HDR4%NUM_REC
      TIM_INT = HDR4%SAMPLING_INTERVAL 
!
! --- Extract MJD and TAI for the first epoch
!
      CALL LIB$MOVC3 ( LEN__HDR, %REF(HEADER(8)), HDR8 )
      MJD_BEG = HDR8%MJD_FIRST 
      TAI_BEG = HDR8%TAI_FIRST 
!
      MJD_END = MJD_BEG
      TAI_END = TAI_BEG + (L_EPC-1)*TIM_INT
      IF ( TAI_END > 86400.0D0 - 1.D-6 ) THEN
           IDAY = (TAI_END + 2.D-6)/86400.0D0
           MJD_END = MJD_END + IDAY
           TAI_END = TAI_END - IDAY*86400.0D0
           IF ( TAI_END < 0.0D0 .AND. TAI_END > -3.D-6 ) TAI_END = 0.0D0
      END IF
!
      IF ( STR_MODE == '-d' .OR. STR_MODE == '--date' ) THEN
           STR_DAT_BEG = MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG, IER ) 
           STR_DAT_END = MJDSEC_TO_DATE ( MJD_END, TAI_END, IER ) 
           IS = FILE_INFO ( TRIM(FIL_BDS)//CHAR(0), UNIX_DATE, SIZE_I8 )
           WRITE ( 6, 110 ) MJD_BEG,  TAI_BEG, STR_DAT_BEG, &
     &                      MJD_END,  TAI_END, STR_DAT_END, &
     &                      TIM_INT, L_EPC, (SIZE_I8 - M__HDR*LEN__HDR)/LEN__BDS, SIZE_I8
 110       FORMAT ( 'MJD_BEG: ', I5, ' TAI_BEG: ', F8.1, ' BEG_DATE: ', A, &
     &                ' MJD_END: ', I5, ' TAI_END: ', F8.1, ' END_DATE: ', A, &
     &                ' TIM_STEP: ', F8.1, ' NUM_EPOCHS(header): ', I6, &
     &                ' NUM_EPOCHS(file_size): ', I6, ' FILE_SIZE: ', I9  )
        ELSE IF ( STR_MODE == '-nd' .OR. STR_MODE == '--num_displacements' ) THEN
           WRITE ( 6, 120 ) L_EPC
 120       FORMAT ( 'N_DEL: ', I6 )
        ELSE IF ( STR_MODE == '-s' .OR. STR_MODE == '--station' ) THEN
           CALL LIB$MOVC3 ( LEN__HDR, %REF(HEADER(3)), %REF(STA_NAM) )
           CALL LIB$MOVC3 ( LEN__HDR, %REF(HEADER(5)), COO_CFS(1) )
           CALL LIB$MOVC3 ( LEN__HDR, %REF(HEADER(6)), COO_CFS(2) )
           CALL LIB$MOVC3 ( LEN__HDR, %REF(HEADER(7)), COO_CFS(3) )
           WRITE ( 6, 130 ) HEADER(3), COO_CFS(1:3)
 130       FORMAT ( 'STA: ',A, ' COO: ', 3(F14.3,1X) )
        ELSE IF ( STR_MODE == '-m' .OR. STR_MODE == '--model' ) THEN
           IP = 1
           DO 410 J1=M__HDR1+1,M__HDR
              WRITE ( 6, 140 ) HEADER(J1)
 140          FORMAT ( A )
 410       CONTINUE 
        ELSE 
           CALL ERR_LOG ( 5586, IUER, 'BDS_INFO', 'Unsupported mode: '// &
     &          TRIM(STR_MODE)//' Supported modes: -d, --date, -nd, --num_displacements, '// &
     &          '-s, --station, -m, --model' )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  BDS_INFO !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE BDS_GEN_SUMMARY ( BDS_DIR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine BDS_GEN_SUMMARY generages an ascii file with the summary   *
! *   of site displacements in the bindisp format that are in directory  *
! *   BDS_DIR and have extension .bds .                                  *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * FIL_BDS   ( CHARACTER    ) -- Name of the input file with station    *
! *                               displacements in bindisp format.       *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     IUER ( INTEGER*4, OPT ) -- Universal error handler.              *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 28-NOV-2024  BDS_GEN_SUMMARY v1.0 (c)  L. Petrov 28-NOV-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'bindisp.i'
      CHARACTER  BDS_DIR*(*)
      TYPE ( BINDISP_HEADER_4 ) ::  HDR4
      TYPE ( BINDISP_HEADER_8 ) ::  HDR8
      INTEGER*4  IUER 
      INTEGER*4  M_BDS, M_LEV
      REAL*8     EPS
      PARAMETER  ( M_BDS = 64*1024 )
      PARAMETER  ( M_LEV = 16      )
      PARAMETER  ( EPS   = 1.0D-6  )
      CHARACTER  HEADER(M__HDR)*(LEN__HDR), STR_BEG*21, STR_END*21, &
     &           FILOUT*128, FILNAM*128, C_STA(M_BDS)*8, &
     &           FIL_BDS(M_BDS)*128, FILTMP*128, TIM_BEG_STR*19, &
     &           TIM_END_STR*19, STR*128, STR1*128, STR2*128, STR3*128, &
     &           BUF(M_BDS)*256, FILSUM*128
      INTEGER*8  DIR_DESC(M_LEV )
      INTEGER*4  J1, J2, J3, IL, NO, IS, IDAY, LEV, LUN, L_BDS, L_EPC, &
     &           MJD_BEG, MJD_END, MJD_BEG_1ST, MJD_END_1ST, L_EPC_1ST, IER
      REAL*8     TAI_BEG, TAI_END, TIM_INT, TAI_BEG_1ST, TAI_END_1ST, TIM_INT_1ST, &
     &           COO_CFS(3,M_BDS)
      CHARACTER, EXTERNAL :: GET_CDATE*19, MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: GETPID, GET_FILE_FROM_DIR, ILEN, I_LEN, LINDEX, READ, RENAME
!
      LEV = 0
      L_BDS = 0
      DO 410 J1=1,M_BDS
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, BDS_DIR, FILNAM )
         IF ( LEV == 0 ) GOTO 810 ! End of work
         IL = ILEN(FILNAM)
         IF ( IL < 5 ) GOTO 410
         IF ( FILNAM(IL-3:IL) == '.bds' ) THEN
              L_BDS = L_BDS + 1
              IF ( L_BDS > M_BDS ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( M_BDS, STR )
                   CALL ERR_LOG ( 5631, IUER, 'BDS_GEN_SUMMARY', 'Too many files '// &
     &                 'with extension .bds in in the input directory '//BDS_DIR )
                   RETURN 
              END IF
              FIL_BDS(L_BDS) = FILNAM
         END IF
 410  CONTINUE 
 810  CONTINUE 
      IF ( L_BDS == 0 ) THEN
           CALL ERR_LOG ( 5632, IUER, 'BDS_GEN_SUMMARY', 'Did not find files '// &
     &         'with extension .bds in in the input directory '//BDS_DIR )
           RETURN 
      END IF
!
! --- Sort file names
!
      CALL SORT_FAST_CH ( L_BDS, FIL_BDS )
!
      DO 420 J2=1,L_BDS
!
! ------ Open the input file with site displacments 
!
         LUN = 10
         CALL ERR_PASS ( IUER, IER )
         CALL BINF_OPEN ( FIL_BDS(J2), 'OLD', LUN, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 5633, IUER, 'BDS_GEN_SUMMARY', 'Error in opening '// &
     &            'file '//FIL_BDS(J2) )
              RETURN 
         END IF
!
! ------ Read the header section
!
         IS = READ ( %VAL(LUN), %REF(HEADER), %VAL(SIZEOF(HEADER)) )
         IF ( IS .NE. SIZEOF(HEADER) ) THEN
              CALL CLRCH  ( STR )
              CALL GERROR(STR)
              CALL ERR_LOG ( 5634, IUER, 'BDS_GEN_SUMMARY', 'Error in '// &
     &             'an attempt to read the header section of the '// &
     &             'input file '//TRIM(FIL_BDS(J2))// &
     &             ' : '//STR )
              RETURN 
         END IF
!
! ------ Extract the number of epochs and the sampling interval
!
         CALL LIB$MOVC3 ( LEN__HDR, %REF(HEADER(4)), HDR4 )
         L_EPC = HDR4%NUM_REC
         TIM_INT = HDR4%SAMPLING_INTERVAL 
!
! ------ Extract MJD and TAI for the first epoch
!
         CALL LIB$MOVC3 ( LEN__HDR, %REF(HEADER(8)), HDR8 )
         MJD_BEG = HDR8%MJD_FIRST 
         TAI_BEG = HDR8%TAI_FIRST 
!
         MJD_END = MJD_BEG
         TAI_END = TAI_BEG + (L_EPC-1)*TIM_INT
         IF ( TAI_END > 86400.0D0 - 1.D-6 ) THEN
              IDAY = (TAI_END + 2.D-6)/86400.0D0
              MJD_END = MJD_END + IDAY
              TAI_END = TAI_END - IDAY*86400.0D0
              IF ( TAI_END < 0.0D0 .AND. TAI_END > -3.D-6 ) TAI_END = 0.0D0
         END IF
         CALL LIB$MOVC3 ( LEN__HDR, %REF(HEADER(3)), %REF(C_STA(J2)) )
         CALL LIB$MOVC3 ( LEN__HDR, %REF(HEADER(5)), COO_CFS(1,J2) )
         CALL LIB$MOVC3 ( LEN__HDR, %REF(HEADER(6)), COO_CFS(2,J2) )
         CALL LIB$MOVC3 ( LEN__HDR, %REF(HEADER(7)), COO_CFS(3,J2) )
         IF ( J2 == 1 ) THEN
              L_EPC_1ST = L_EPC
              MJD_BEG_1ST = MJD_BEG
              MJD_END_1ST = MJD_END
              TAI_BEG_1ST = TAI_BEG
              TAI_END_1ST = TAI_END
              TIM_INT_1ST = TIM_INT
            ELSE
              IF ( L_EPC .NE. L_EPC_1ST ) THEN
                   WRITE ( 6, * ) 'L_EPC= ', L_EPC, ' L_EPS_1ST= ', L_EPC_1ST
                   CALL ERR_LOG ( 5635, IUER, 'BDS_GEN_SUMMARY', 'Mismatch '// &
     &                 'in the number of epochs between files '// &
     &                 TRIM(FIL_BDS(J2))//' and '//FIL_BDS(1) )
                   RETURN 
              END IF
              IF ( DABS( (MJD_BEG - MJD_BEG_1ST)*86400.D0 + (TAI_BEG - TAI_BEG_1ST) ) > EPS ) THEN
                   STR  = MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG, IER ) 
                   STR1 = MJDSEC_TO_DATE ( MJD_BEG_1ST, TAI_BEG_1ST, IER ) 
                   WRITE ( 6, * ) 'TIM_BEG= ', STR(1:19), ' TIM_BEG_1ST = ', STR1(1:19)
                   CALL ERR_LOG ( 5636, IUER, 'BDS_GEN_SUMMARY', 'Mismatch '// &
     &                 'in start time in files '//TRIM(FIL_BDS(J2))//' and '//FIL_BDS(1) )
                   RETURN 
              END IF
              IF ( DABS( (MJD_END - MJD_END_1ST)*86400.D0 + (TAI_END - TAI_END_1ST) ) > EPS ) THEN
                   STR  = MJDSEC_TO_DATE ( MJD_END, TAI_END, IER ) 
                   STR1 = MJDSEC_TO_DATE ( MJD_END_1ST, TAI_END_1ST, IER ) 
                   WRITE ( 6, * ) 'TIM_END= ', STR(1:19), ' TIM_END_1ST = ', STR1(1:19)
                   CALL ERR_LOG ( 5637, IUER, 'BDS_GEN_SUMMARY', 'Mismatch '// &
     &                 'in end time in files '//TRIM(FIL_BDS(J2))//' and '//FIL_BDS(1) )
                   RETURN 
              END IF
              IF ( DABS(TIM_INT - TIM_INT_1ST) > EPS ) THEN 
                   WRITE ( 6, * ) 'TIM_INT= ', TIM_INT, ' TIM_INT_1ST= ', TIM_INT_1ST
                   CALL ERR_LOG ( 5638, IUER, 'BDS_GEN_SUMMARY', 'Mismatch '// &
     &                 'in samling itme in files '//TRIM(FIL_BDS(J2))//' and '//FIL_BDS(1) )
                   RETURN 
              END IF
         END IF
 420  CONTINUE 
!
      NO = 0
      NO = NO + 1
      BUF(NO) = BINDISP_SUMMARY__LABEL
      NO = NO + 1
      WRITE ( BUF(NO), '(A)' ) 'LAST_UPDATE: '//GET_CDATE()
      NO = NO + 1
      WRITE ( BUF(NO), 110 ) 'MIN_EPOCH: ', MJD_BEG, TAI_BEG, &
     &                    MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG, IER )
      NO = NO + 1
      WRITE ( BUF(NO), 110 ) 'MAX_EPOCH: ', MJD_END, TAI_END, &
     &                    MJDSEC_TO_DATE ( MJD_END, TAI_END, IER )
 110  FORMAT ( A, I5,' ',F7.1,' ',A )
      NO = NO + 1
      WRITE ( BUF(NO), '(A,I9)'  ) 'L_EPC: ', L_EPC
      NO = NO + 1
      WRITE ( BUF(NO), '(A,I9)'  ) 'L_STA: ', L_BDS
      NO = NO + 1
      WRITE ( BUF(NO), '(A,I15)' ) 'L_DSP: ', L_EPC*L_BDS
      NO = NO + 1
      WRITE ( BUF(NO), '(A,2X,F11.5,2X,F11.5,2X,"sec")' ) 'SMP_INTRV:', TIM_INT, TIM_INT
      NO = NO + 1
      WRITE ( BUF(NO), '(A,1X,F14.6)' ) 'RD_AREA:', 3000.0
      NO = NO + 1
      WRITE ( BUF(NO), '(A)' )  '#'
      NO = NO + 1
      WRITE ( BUF(NO), '(A,I2)' )  'L_MOD: ', 1
      NO = NO + 1
      CALL LIB$MOVC3 ( LEN__HDR, %REF(HEADER(9)),  %REF(STR1(1:8)) )
      CALL LIB$MOVC3 ( LEN__HDR, %REF(HEADER(10)), %REF(STR2(1:8)) )
      CALL LIB$MOVC3 ( LEN__HDR, %REF(HEADER(11)), %REF(STR3(1:8)) )
      WRITE ( BUF(NO), '(A,I2,1X,A,1X,A,1X,A)' )  'MODEL: ', 1, &
     &        STR1(1:8), STR2(1:8), STR3(1:8) 
      NO = NO + 1
      WRITE ( BUF(NO), '(A)' )  '#'
      NO = NO + 1
      WRITE ( BUF(NO), '(A)' )  '#     Num Site ID  Dates begin         / Date '// &
     &                      'end             N points  smp_intrv  '// &
     &                      'X-coordinate  Y coordinate  Z coordinate'
      NO = NO + 1
      WRITE ( BUF(NO), '(A)' )  '#'
      DO 430 J3=1,L_BDS
         NO = NO + 1
         TIM_BEG_STR = MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG, IER )
         TIM_END_STR = MJDSEC_TO_DATE ( MJD_END, TAI_END, IER )
         WRITE ( BUF(NO), 120 ) J3, C_STA(J3), TIM_BEG_STR, TIM_END_STR, L_EPC, &
     &                          TIM_INT/86400.0D0, COO_CFS(1:3,J3)
 120     FORMAT ( 'STA: ', I4, 1X, A8, 1X, A19, ' / ', A19, 4X, I6, 1X, &
     &            F16.11, 3(1X, F13.4), ' LI' )
 430  CONTINUE 
!
! --- Write the summary in the output file
!
      FILSUM = TRIM(BDS_DIR)//'/bds_summary.txt'
      CALL ERR_PASS ( IUER, IER )
      CALL WR_TEXT ( NO, BUF, FILSUM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5639, IUER, 'BDS_GEN_SUMMARY', 'Failure in '// &
     &         'an attempt to re-write simmary file '//FILSUM )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER ) 
      RETURN
      END  SUBROUTINE  BDS_GEN_SUMMARY  !#!#
