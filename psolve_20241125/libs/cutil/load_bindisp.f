      SUBROUTINE LOAD_BINDISP ( I_PSV, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  LOAD_BINDISP reads a summary of the set of the files of   *
! *   site position variations time series in BINDISP format specified   *
! *   as the I_PSV -th model of the POSITION_VARIAONS keyword of         *
! *   in the $MAPPING section of a batch control file. It allocates      *
! *   dynamic memory for data structures used for binary displacements.  *
! *   Routine  LOAD_BINDISP  puts there results of parsing the summary   *
! *   file. These results will be later used in order to spead up        *
! *   reading files with site position variations time series in BINDISP *
! *   format.                                                            *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! * I_PSV ( INTEGER*4 ) -- Index of the position variation file in the   *
! *                        array POSVAR_FIL.                             *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  IUER ( INTEGER*4, OPT ) -- Universal error handler.                 *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 16-DEC-2002  LOAD_BINDISP  v3.0 (c) L. Petrov  18-OCT-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'bindisp.i'
      CHARACTER  FILSUM*128
      INTEGER*4  I_PSV, IUER
      LOGICAL*4  LEX
      CHARACTER  DIRNAM*128, BUF(M__BDSLEN)*256, FILE_IO_LOCK*128, &
     &           FILE_READ_LOCK*128, FILE_WRITE_LOCK*128, FMT_VERSION*10, STR*256
      INTEGER*4  DATE_NOW, DATE_WRT_LCK, ID, IS, LUN, STAT_BLOCK(16), IOS, &
     &           N_SUM, MEM_LEN, MEM_ADR, FD_READ_LOCK, FD_WRITE_LOCK, J1, J2, IER
      CHARACTER  IO_LOCK_NAME*12, READ_LOCK_NAME*12, WRITE_LOCK_NAME*13
      REAL*8     TIM_REM
      REAL*8     LOCK_TIMEOUT
      PARAMETER  ( IO_LOCK_NAME    = 'dir_io.lck'    )
      PARAMETER  ( READ_LOCK_NAME  = 'dir_read.lck'  )
      PARAMETER  ( WRITE_LOCK_NAME = 'dir_write.lck' )
      PARAMETER  ( LOCK_TIMEOUT    = 8.0D0           )
      INTEGER*4, EXTERNAL :: GETPID, GET_UNIT, I_LEN, ILEN, FOR_STAT, TIME
      CHARACTER  GET_CDATE*19
!
! --- Check argumetns
!
      IF ( I_PSV .LE. 0  .OR.  I_PSV .GT. M__POSVAR ) THEN
           CALL CLRCH ( STR )
           CALL INCH   ( I_PSV, STR )
           CALL ERR_LOG ( 2761, IUER, 'LOAD_BINDISP', 'Wrong parameter '// &
     &         'I_PSV: '//STR )
           RETURN
      END IF
!
      IF ( ILEN(POSVAR_FIL(I_PSV)) .EQ. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH   ( I_PSV, STR )
           CALL ERR_LOG ( 2762, IUER, 'LOAD_BINDISP', 'Empty directory name '// &
     &         'off the '//STR(1:I_LEN(STR))//'-th position variation file' )
           RETURN
      END IF
!
! --- Build directory name where the displacement files and the summary
! --- files are located
!
      DIRNAM = POSVAR_FIL(I_PSV)
      ID = ILEN(DIRNAM)
      IF ( DIRNAM(ID:ID) .NE. '/' ) THEN
           ID = ID + 1
           DIRNAM(ID:ID) = '/'
      END IF
!
! --- Build the name of the summary file
!
      FILSUM = POSVAR_FIL(I_PSV)(1:I_LEN(POSVAR_FIL(I_PSV)))//SUMMARY_BDS_FILE
!
! --- Check: whether the file exists?
!
      INQUIRE ( FILE=FILSUM, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 2763, IUER, 'LOAD_BINDISP', 'BINDISP summary file '// &
     &          FILSUM(1:I_LEN(FILSUM))//' was not found' )
           RETURN
      END IF
!
! --- Build the names of lock files
!
      FILE_IO_LOCK    = DIRNAM(1:ID)//IO_LOCK_NAME
      FILE_READ_LOCK  = DIRNAM(1:ID)//READ_LOCK_NAME
      FILE_WRITE_LOCK = DIRNAM(1:ID)//WRITE_LOCK_NAME
!
      CALL ERR_PASS ( IUER, IER )
      CALL SET_WRITE_LOCK ( FILE_IO_LOCK, FILE_READ_LOCK, FILE_WRITE_LOCK, &
     &                      LOCK_TIMEOUT, FD_READ_LOCK, FD_WRITE_LOCK, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2764, IUER, 'LOAD_BINDISP', 'Error in setting '// &
     &         'up write lock while reading loading' )
           RETURN
      END IF
!
! --- Learn information about the summary file, including date of last
! --- modification
!
      IS = FOR_STAT ( FILSUM, STAT_BLOCK )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 2766, IUER, 'LOAD_BINDISP', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to get information about '// &
     &         'the summary file '//FILSUM )
           CALL LIFT_READ_WRITE_LOCKS ( FD_READ_LOCK, FD_WRITE_LOCK )
           RETURN
      END IF
!
! --- Save the data of the last modification of summary file
!
      TIM_PSVFIL(I_PSV) = STAT_BLOCK(10)
!
! --- Read the summary file of the set of BINDISP files.
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILSUM, M__BDSLEN, BUF, N_SUM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2767, IUER, 'LOAD_BINDISP', 'Error in reading '// &
     &         'summary file '//FILSUM )
           CALL LIFT_READ_WRITE_LOCKS ( FD_READ_LOCK, FD_WRITE_LOCK )
           RETURN
      END IF
!
! --- Check whether the first line contains the label of the format
!
      IF ( BUF(1)(1:LEN(BINDISP_SUMMARY__LABEL)) == BINDISP_SUMMARY__LABEL ) THEN
           FMT_VERSION = '2005.03.28'
         ELSE IF ( BUF(1)(1:LEN(BINDISP_SUMMARY__LABEL)) == BINDISP_SUMMARY__LABEL_1 ) THEN
           FMT_VERSION = '2002.12.12'
         ELSE 
           CALL ERR_LOG ( 2768, IUER, 'LOAD_BINDISP', 'Format violation for '// &
     &         'the summary file '//FILSUM(1:I_LEN(FILSUM))//' the first '// &
     &         'line does not have the signature which was expected: '// &
     &          BINDISP_SUMMARY__LABEL )
           CALL LIFT_READ_WRITE_LOCKS ( FD_READ_LOCK, FD_WRITE_LOCK )
           RETURN
      END IF
!
! --- Read summary. We need to extract the number of sites
!
      DO 420 J2=1,N_SUM
         IF ( BUF(J2)(1:6) .EQ. 'L_STA:' ) THEN
              READ ( UNIT=BUF(J2)(7:16), FMT='(I10)' ) N_PSVSTA(I_PSV)
         END IF
 420  CONTINUE
!
      IF ( N_PSVSTA(I_PSV) .EQ. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( I_PSV, STR )
           CALL ERR_LOG ( 2769, IUER, 'LOAD_BINDISP', 'No sites were '// &
     &         'specified in the position variation file '//FILSUM )
           CALL LIFT_READ_WRITE_LOCKS ( FD_READ_LOCK, FD_WRITE_LOCK )
           RETURN
      END IF
!
      LEN_NAMSIT(I_PSV) =   8*N_PSVSTA(I_PSV)*1
      LEN_STACOO(I_PSV) =   3*N_PSVSTA(I_PSV)*8
      LEN_BDSFIL(I_PSV) = 128*N_PSVSTA(I_PSV)*1
      LEN_BDSSAM(I_PSV) =     N_PSVSTA(I_PSV)*8
      LEN_BDSFMJ(I_PSV) =     N_PSVSTA(I_PSV)*4
      LEN_BDSFSC(I_PSV) =     N_PSVSTA(I_PSV)*8
      LEN_BDSLMJ(I_PSV) =     N_PSVSTA(I_PSV)*4
      LEN_BDSLSC(I_PSV) =     N_PSVSTA(I_PSV)*8
      LEN_BDSNSA(I_PSV) =     N_PSVSTA(I_PSV)*4
!
! --- Get dynamic memory for arrays whcih will held results of parsing
!
      CALL ERR_PASS ( IUER, IER )
      CALL GRAB_MEM ( IER, MEM_LEN,           MEM_ADR,           9, &
     &                     LEN_NAMSIT(I_PSV), ADR_NAMSIT(I_PSV), &
     &                     LEN_STACOO(I_PSV), ADR_STACOO(I_PSV), &
     &                     LEN_BDSFIL(I_PSV), ADR_BDSFIL(I_PSV), &
     &                     LEN_BDSSAM(I_PSV), ADR_BDSSAM(I_PSV), &
     &                     LEN_BDSFMJ(I_PSV), ADR_BDSFMJ(I_PSV), &
     &                     LEN_BDSFSC(I_PSV), ADR_BDSFSC(I_PSV), &
     &                     LEN_BDSLMJ(I_PSV), ADR_BDSLMJ(I_PSV), &
     &                     LEN_BDSLSC(I_PSV), ADR_BDSLSC(I_PSV), &
     &                     LEN_BDSNSA(I_PSV), ADR_BDSNSA(I_PSV)   )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( MEM_LEN, STR )
           CALL ERR_LOG ( 2770, IUER, 'LOAD_BINDISP', 'Error in an attempt '// &
     &         'to grab '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           CALL LIFT_READ_WRITE_LOCKS ( FD_READ_LOCK, FD_WRITE_LOCK )
           RETURN
      END IF
!
! --- Set status: allocated
!
      STS_NAMSIT(I_PSV) = PSV__ALC
      STS_NAMSIT(I_PSV) = PSV__ALC
      STS_STACOO(I_PSV) = PSV__ALC
      STS_BDSFIL(I_PSV) = PSV__ALC
      STS_BDSSAM(I_PSV) = PSV__ALC
      STS_BDSFMJ(I_PSV) = PSV__ALC
      STS_BDSFSC(I_PSV) = PSV__ALC
      STS_BDSLMJ(I_PSV) = PSV__ALC
      STS_BDSLSC(I_PSV) = PSV__ALC
      STS_BDSNSA(I_PSV) = PSV__ALC
!
! --- Parse the summary file
!
      POSVAR_RD_AREA(I_PSV) = -1.0D0
      CALL ERR_PASS ( IUER, IER )
      CALL BINDISP_SUMMARY_READ ( N_SUM, BUF, DIRNAM, N_PSVSTA(I_PSV), &
     &                            POSVAR_INT(I_PSV), 8, 128,           &
     &                            POSVAR_RD_AREA(I_PSV),               &
     &             %VAL(ADR_NAMSIT(I_PSV)), %VAL(ADR_STACOO(I_PSV)),   &
     &             %VAL(ADR_BDSFIL(I_PSV)), %VAL(ADR_BDSSAM(I_PSV)),   &
     &             %VAL(ADR_BDSFMJ(I_PSV)), %VAL(ADR_BDSFSC(I_PSV)),   &
     &             %VAL(ADR_BDSLMJ(I_PSV)), %VAL(ADR_BDSLSC(I_PSV)),   &
     &             %VAL(ADR_BDSNSA(I_PSV)), BDS_ENDIAN(I_PSV),         &
     &             BDS_FLOAT(I_PSV), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2771, IUER, 'LOAD_BINDISP', 'Error in parsing '// &
     &         'summary file '//FILSUM )
           CALL UNLINK ( FILE_WRITE_LOCK(1:I_LEN(FILE_WRITE_LOCK))//CHAR(0) )
           CALL FREE_MEM ( STS_NAMSIT(I_PSV) )
!
           STS_NAMSIT(I_PSV) = 0
           STS_NAMSIT(I_PSV) = 0
           STS_STACOO(I_PSV) = 0
           STS_BDSFIL(I_PSV) = 0
           STS_BDSSAM(I_PSV) = 0
           STS_BDSFMJ(I_PSV) = 0
           STS_BDSFSC(I_PSV) = 0
           STS_BDSLMJ(I_PSV) = 0
           STS_BDSLSC(I_PSV) = 0
           STS_BDSNSA(I_PSV) = 0
      END IF
!
! --- Check POSVAR_RD_AREA  
!
      IF ( FMT_VERSION == '2002.12.12' ) THEN
!
! -------- BINDISP summary file format of 2002.02.12 did not define 
! -------- POSVAR_RD_AREA. Use default defiend in NEA__PSV
!
           POSVAR_RD_AREA(I_PSV) = NEA__PSV
         ELSE 
           IF ( POSVAR_RD_AREA(I_PSV) < 0.0D0 ) THEN
                CALL ERR_LOG ( 2772, IUER, 'LOAD_BINDISP', 'Error in '// &
     &              'parsing summary file '//FILSUM(1:I_LEN(FILSUM))// &
     &              ' -- no A-record was found' )
                CALL LIFT_READ_WRITE_LOCKS ( FD_READ_LOCK, FD_WRITE_LOCK )
                RETURN
           END IF
      END IF
!
! --- Lift the write lock
!
      CALL LIFT_READ_WRITE_LOCKS ( FD_READ_LOCK, FD_WRITE_LOCK )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  LOAD_BINDISP  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE BINDISP_SUMMARY_READ ( N_BUF, BUF, DIRNAM, N_STA, &
     &           POSVAR_INT, L_NAMSIT, L_BDSFIL, RD_AREA, NAMSIT, &
     &           STACOO, BDSFIL, BDSSAM, BDSFMJ, BDSFSC, BDSLMJ, BDSLSC, &
     &           BDSNSA, BDS_ENDIAN, BDS_FLOAT, &
     &           IUER )
! ************************************************************************
! *                                                                      *
! *   Auxiliary Routine BINDISP_SUMMARY_READ  parses station records of  *
! *   the summary of the set of site displacement timer series files in  *
! *   BINDISP format. It is assumd that the files have already been read *
! *   into the test buffer BUF and some information, the number of       *
! *   sites, is already extracted from there.                            *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *      N_BUF ( INTEGER*4 ) -- The number of lines in the buffer with   *
! *                             the summary of the site position         *
! *                             variations time series.                  *
! *        BUF ( CHARACTER ) -- Character array which keeps the image of *
! *                             the summary file of site position        *
! *                             variations time series. Dimension: N_BUF *
! *     DIRNAM ( CHARACTER ) -- The name of the directory whether the    *
! *                             time series of site position variation   *
! *                             files and the summary file are located.  *
! *      N_STA ( INTEGER*4 ) -- The number of sites in summary file.     *
! * POSVAR_INT ( INTEGER*4 ) -- Specifier of interpolation mode. It can  *
! *                             be one of PSV__LIN or PSV__SPL.          *
! *   L_NAMSIT ( INTEGER*4 ) -- The length of the site name string in    *
! *                             bytes.                                   *
! *   L_BDSFIL ( INTEGER*4 ) -- The length of the file name string in    *
! *                             bytes.                                   *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *    RD_AREA ( REAL*8    ) -- The radius of the area for which         *
! *                             displacements are applicable. Files in   *
! *                             BINDISP Summary format of 2002.12.20     *
! *                             did not define the radius of the area.   *
! *                             The output value of RD_AREA is undefined *
! *                             in this case. Files in BINDISP Summary   *
! *                             format of 2005.03.28 define the radius   *
! *                             of this area.                            *
! *     NAMSIT ( CHARACTER ) -- Array of site names. Each site name has  *
! *                             length of L_NAMSIT characters.           *
! *                             Dimension: N_SIT.                        *
! *     STACOO ( REAL*8    ) -- Arrays of site coordinates in a crust    *
! *                             reference frame. Dimension: (3,N_STA).   *
! *     BDSFIL ( CHARACTER ) -- Array of full names including path of    *
! *                             the files with site position variations  *
! *                             time seriesin BINDISP format.            *
! *                             Dimension: N_STA.                        *
! *     BDSSAM ( REAL*8    ) -- Array of sampling intervals for each     *
! *                             site position variations time series     *
! *                             files. Dimension: N_STA.                 *
! *     BDSFMJ ( INTEGER*4 ) -- Array of the MJD at the midnight of the  *
! *                             first epoch of the site displacement in  *
! *                             the file of site position variations.    *
! *                             Dimension: N_STA.                        *
! *     BDSFSC ( REAL*8    ) -- Array of time elaped from the midnight   *
! *                             in seconds of the first epoch of the     *
! *                             site displacement in the file of site    *
! *                             position variations. Dimension: N_STA.   *
! *     BDSLMJ ( INTEGER*4 ) -- Array of the MJD at the midnight of the  *
! *                             last epoch of the site displacement in   *
! *                             the file of site position variations.    *
! *                             Dimension: N_STA.                        *
! *     BDSLSC ( REAL*8    ) -- Array of time elaped from the midnight   *
! *                             in seconds of the first epoch of the     *
! *                             site displacement in the file of site    *
! *                             position variations. Dimension: N_STA.   *
! *     BDSNSA ( INTEGER*4 ) -- Array of the number of samples in each   *
! *                             file of site displacements time series.  *
! *                             Dimension: N_STA.                        *
! * BDS_ENDIAN ( CHARACTER ) -- Flag of the endian binary format         *
! *                             specifier of the binary files of site    *
! *                             position variations: B for Big-Endian,   *
! *                             L for Little-endian. It is assumed       *
! *                             that all files have the same flags.      *
! *  BDS_FLOAT ( CHARACTER ) -- Flag of the float numbers format         *
! *                             specifier site position variations:      *
! *                             I for IEE 754/854 format, D for DEC      *
! *                             format. It is assumed that all files     *
! *                             have the same flags.                     *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  IUER ( INTEGER*4, OPT ) -- Universal error handler.                 *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * # 16-DEC-2002 BINDISP_SUMMARY_READ v2.0 (c) L. Petrov 28-MAR-2005 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'bindisp.i'
      TYPE ( BDSSUM_STAREC ) ::  BDSUM
      INTEGER*4  N_BUF, N_STA, BDSFMJ(N_STA), BDSLMJ(N_STA), BDSNSA(N_STA), &
     &           POSVAR_INT, L_NAMSIT, L_BDSFIL, IUER
      CHARACTER  BUF(N_BUF)*(*), DIRNAM*(*), NAMSIT(N_STA)*(L_NAMSIT), &
     &           BDSFIL(N_STA)*(L_BDSFIL), BDS_ENDIAN*1, BDS_FLOAT*1
      REAL*8     RD_AREA, STACOO(3,N_STA), BDSSAM(N_STA), BDSFSC(N_STA), &
     &           BDSLSC(N_STA)
      CHARACTER  STR*256
      LOGICAL*4  LEX
      INTEGER*4  I_STA, IOS, J1, IER
      INTEGER*4, EXTERNAL :: I_LEN
!
      BDS_ENDIAN = CHAR(0)
      BDS_FLOAT  = CHAR(0)
      I_STA = 0
!
      DO 410 J1=1,N_BUF
         IF ( BUF(J1)(1:8) .EQ. 'RD_AREA:' ) THEN
              READ ( UNIT=BUF(J1)(10:22), FMT='(F13.5)', IOSTAT=IOS ) RD_AREA
              IF ( IOS .NE. 0 ) THEN
                   CALL ERR_LOG ( 2781, IUER, 'BINDISP_SUMMARY_READ', &
     &                 'Error in parsing the value of the RD_AREA keyword' )
                   RETURN
              END IF
           ELSE IF ( BUF(J1)(1:4) .EQ. 'STA:' ) THEN
!
! ----------- Aga, this is the STA-record
!
              I_STA = I_STA + 1
              IF ( I_STA .GT. N_STA ) THEN
                   WRITE ( 6, * ) 'I_STA = ', I_STA,' N_STA = ',N_STA
                   CALL ERR_LOG ( 2782, IUER, 'BINDISP_SUMMARY_READ', &
     &                 'Trap of internal control: I_STA is too large' )
                   RETURN
              END IF
!
! ----------- Put the line from the summary file into the record BDSUM
!
              CALL LIB$MOVC3 ( LEN__BDSUM, %REF(BUF(J1)), BDSUM )
!
! ----------- Extract the site ID
!
              NAMSIT(I_STA) = BDSUM%SITE_ID
!
! ----------- Extract the MJD, TDT time of the first epoch for this station
!
              CALL ERR_PASS     ( IUER, IER )
              CALL DATE_TO_TIME ( BDSUM%DATE_BEG, BDSFMJ(I_STA), BDSFSC(I_STA), &
     &                            IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IER, STR )
                   CALL ERR_LOG ( 2783, IUER, 'BINDISP_SUMMARY_READ', &
     &                 'Error in date transformation of the date '// &
     &                  BDSUM%DATE_BEG//' at the '//STR(1:I_LEN(STR))//'-th '// &
     &                 'line of the summary file' )
                   RETURN
              END IF
!
! ----------- Extract the MJD, TDT time of the last epoch for this station
!
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( BDSUM%DATE_END, BDSLMJ(I_STA), BDSLSC(I_STA), &
     &                            IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IER, STR )
                   CALL ERR_LOG ( 2784, IUER, 'BINDISP_SUMMARY_READ', &
     &                 'Error in date transformation of the date '// &
     &                  BDSUM%DATE_END//' at the '//STR(1:I_LEN(STR))//'-th '// &
     &                 'line of the summary file' )
                   RETURN
              END IF
!
! ----------- Extract the number of samples for this station
!
              READ ( UNIT=BDSUM%NUM_PTS, FMT='(I6)', IOSTAT=IOS ) BDSNSA(I_STA)
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IER, STR )
                   CALL ERR_LOG ( 2785, IUER, 'BINDISP_SUMMARY_READ', &
     &                 'Error in reading the number of samples '// &
     &                  BDSUM%NUM_PTS//' at the '//STR(1:I_LEN(STR))//'-th '// &
     &                 'line of the summary file' )
                   RETURN
              END IF
!
! ----------- Check whether the number of samples is OK
!
              IF ( BDSNSA(I_STA) .LT. 2  .AND.  POSVAR_INT .EQ. PSV__LIN ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IER, STR )
                   CALL ERR_LOG ( 2786, IUER, 'BINDISP_SUMMARY_READ', &
     &                 'Too few points for station '//BDSUM%SITE_ID//' -- '// &
     &                  BDSUM%NUM_PTS//' -- it is not enough for linear '// &
     &                 'interpolation ' )
                   RETURN
              END IF
!
              IF ( BDSNSA(I_STA) .LT. 4  .AND.  POSVAR_INT .EQ. PSV__SPL ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IER, STR )
                   CALL ERR_LOG ( 2787, IUER, 'BINDISP_SUMMARY_READ', &
     &                 'Too few points for station '//BDSUM%SITE_ID//' -- '// &
     &                  BDSUM%NUM_PTS//' -- it is not enough for spline '// &
     &                 'interpolation ' )
                   RETURN
              END IF
!
! ----------- Estract the lenght of the sampling interval in days
!
              READ ( UNIT=BDSUM%SAMPLE_INT, FMT='(F10.5)', IOSTAT=IOS ) &
     &               BDSSAM(I_STA)
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IER, STR )
                   CALL ERR_LOG ( 2788, IUER, 'BINDISP_SUMMARY_READ', &
     &                 'Error in reading sample length '// &
     &                  BDSUM%SAMPLE_INT//' at the '//STR(1:I_LEN(STR))// &
     &                  '-th line of the summary file' )
                   RETURN
              END IF
!
! ----------- Check whether the samping interval length is OK
!
              IF ( BDSSAM(I_STA) .LT. 1.0D0/86400.0D0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IER, STR )
                   CALL ERR_LOG ( 2789, IUER, 'BINDISP_SUMMARY_READ', &
     &                 'Wrong sample interval length '// &
     &                  BDSUM%SAMPLE_INT//' at the '//STR(1:I_LEN(STR))// &
     &                  '-th line of the summary file' )
                   RETURN
              END IF
!
! ----------- Extract the X-coordinate of the J1-th site
!
              READ ( UNIT=BDSUM%X_COORD, FMT='(F10.5)', IOSTAT=IOS ) STACOO(1,I_STA)
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IER, STR )
                   CALL ERR_LOG ( 2790, IUER, 'BINDISP_SUMMARY_READ', &
     &                 'Error in reading X coordinate of the site: '// &
     &                  BDSUM%X_COORD//' at the '//STR(1:I_LEN(STR))// &
     &                  '-th line of the summary file' )
                   RETURN
              END IF
!
! ----------- Extract the Y-coordinate of the J1-th site
!
              READ ( UNIT=BDSUM%Y_COORD, FMT='(F10.5)', IOSTAT=IOS ) STACOO(2,I_STA)
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IER, STR )
                   CALL ERR_LOG ( 2791, IUER, 'BINDISP_SUMMARY_READ', &
     &                 'Error in reading Y coordinate of the site: '// &
     &                  BDSUM%Y_COORD//' at the '//STR(1:I_LEN(STR))// &
     &                  '-th line of the summary file' )
                   RETURN
              END IF
!
! ----------- Extract the Z-coordinate of the J1-th site
!
              READ ( UNIT=BDSUM%Z_COORD, FMT='(F10.5)', IOSTAT=IOS ) STACOO(3,I_STA)
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IER, STR )
                   CALL ERR_LOG ( 2792, IUER, 'BINDISP_SUMMARY_READ', &
     &                 'Error in reading Z coordinate of the site: '// &
     &                  BDSUM%Z_COORD//' at the '//STR(1:I_LEN(STR))// &
     &                  '-th line of the summary file' )
                   RETURN
              END IF
!
! ----------- Check ENDIAN flag
!
              IF ( BDS_ENDIAN .NE. CHAR(0) ) THEN
                   IF ( BDSUM%ENDIAN_FMT .NE. BDS_ENDIAN ) THEN
                        CALL ERR_LOG ( 2793, IUER, 'BINDISP_SUMMARY_READ', &
     &                      'File '//BDSFIL(I_STA)(1:I_LEN(BDSFIL(I_STA)))// &
     &                      ' has different ENDIAN flag than the previous '// &
     &                      'files' )
                        RETURN
                   END IF
              END IF
!
! ----------- Extract ENDIAN format descriptor
!
              BDS_ENDIAN = BDSUM%ENDIAN_FMT
!
! ----------- Check FLOAT flag
!
              IF ( BDS_FLOAT .NE. CHAR(0) ) THEN
                   IF ( BDSUM%FLOAT_FMT .NE. BDS_FLOAT ) THEN
                        CALL ERR_LOG ( 2794, IUER, 'BINDISP_SUMMARY_READ', &
     &                      'File '//BDSFIL(I_STA)(1:I_LEN(BDSFIL(I_STA)))// &
     &                      ' has different FLOAT flag than the previous '// &
     &                      'files' )
                        RETURN
                   END IF
              END IF
!
! ----------- Extract ENDIAN format descriptor
!
              BDS_ENDIAN = BDSUM%ENDIAN_FMT
!
! ----------- Biuld the the name of the file in BINDISP format for
! ----------- the J1-th site
!
              BDSFIL(I_STA) = DIRNAM(1:I_LEN(DIRNAM))// &
     &                        NAMSIT(I_STA)(1:I_LEN(NAMSIT(I_STA)))//'.bds'
              INQUIRE ( FILE=BDSFIL(I_STA), EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   CALL ERR_LOG ( 2795, IUER, 'BINDISP_SUMMARY_READ', &
     &                 'File '//BDSFIL(I_STA)(1:I_LEN(BDSFIL(I_STA)))// &
     &                 ' was not found, although it was mentioned in the '// &
     &                 'summary file' )
                   RETURN
              END IF
         END IF
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  BINDISP_SUMMARY_READ  #!#
