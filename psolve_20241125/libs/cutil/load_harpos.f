      SUBROUTINE LOAD_HARPOS ( I_PSV, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  LOAD_HARPOS  loads the file with harmonic site position   *
! *   variations in HARPOS format into the dynamic memory area. The      *
! *   file name is defined in glbc4 as the I_PSV-th element of the array *
! *   POSVAR_FIL. The routine reads the file, checks its format, learns  *
! *   the number of harmonics and sites, allocates dynamic memory for    *
! *   these arrays and for the array with displacement. Then it reads    *
! *   phase, frequency and acceleration of harmonics, reads site names   *
! *   and coordinates and eventually reads entire table of               *
! *   displacements. It also stores the date of last modification of the *
! *   HARPOS file.                                                       *
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
! *  ### 16-DEC-2002  LOAD_HARPOS  v2.1 (c)  L. Petrov  10-OCT-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'bindisp.i'
      INCLUDE   'harpos.i'
      INTEGER*4  I_PSV, IUER
      CHARACTER  BUF(M__HPSLEN)*80, FMT_VERSION*10, STR*128
      INTEGER*4  N_HPSLEN, STAT_BLOCK(16), LEN_NAMHAR, IS, &
     &           J1, IER
      ADDRESS__TYPE :: ADR_NAMHAR, MEM_LEN, MEM_ADR
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, FOR_STAT
!
      IF ( I_PSV .LE. 0  .OR.  I_PSV .GT. M__POSVAR ) THEN
           CALL CLRCH ( STR )
           CALL INCH   ( I_PSV, STR )
           CALL ERR_LOG ( 2731, IUER, 'LOAD_HARPOS', 'Wrong parameter I_PSV: '// &
     &          STR )
           RETURN
      END IF
!
      IF ( ILEN(POSVAR_FIL(I_PSV)) .EQ. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH   ( I_PSV, STR )
           CALL ERR_LOG ( 2732, IUER, 'LOAD_HARPOS', 'Empty filename with '// &
     &          'the '//STR(1:I_LEN(STR))//'-th position variation file' )
           RETURN
      END IF
!
! --- Get information about the file FINAM
!
      IS = FOR_STAT ( POSVAR_FIL(I_PSV), STAT_BLOCK )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 2733, IUER, 'LOAD_HARPOS', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to learn date of '// &
     &         'modification of the input file '//POSVAR_FIL(I_PSV) )
           RETURN
      END IF
!
! --- Save the data of modification
!
      TIM_PSVFIL(I_PSV) = STAT_BLOCK(10)
!
! --- Read the file into the buffer BUF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( POSVAR_FIL(I_PSV), M__HPSLEN, BUF, N_HPSLEN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2734, IUER, 'LOAD_HARPOS', 'Error in an attempt '// &
     &         'to read file '//POSVAR_FIL(I_PSV)(1:I_LEN(POSVAR_FIL(I_PSV))) )
           RETURN
      END IF
!
! --- Check the first line: it should have the HARPOS label
!
      IF ( BUF(1)(1:LEN(HARPOS__LABEL)) .EQ. HARPOS__LABEL ) THEN
           FMT_VERSION = '2005.03.28'
         ELSE IF ( BUF(1)(1:LEN(HARPOS__LABEL)) .EQ. HARPOS__LABEL_1 ) THEN
           FMT_VERSION = '2002.12.12'
         ELSE 
           CALL ERR_LOG ( 2735, IUER, 'LOAD_HARPOS', 'Input file '// &
     &          POSVAR_FIL(I_PSV)(1:I_LEN(POSVAR_FIL(I_PSV)))//' is not in '// &
     &         'HARPOS format' )
           RETURN
      END IF
!
! --- Check the last line: it should have the HARPOS label
!
      IF ( BUF(N_HPSLEN)(1:LEN(HARPOS__LABEL)) .NE. BUF(1)(1:LEN(HARPOS__LABEL)) ) THEN
           CALL ERR_LOG ( 2736, IUER, 'LOAD_HARPOS', 'Input file '// &
     &          POSVAR_FIL(I_PSV)(1:I_LEN(POSVAR_FIL(I_PSV)))//' was not '// &
     &         'read up to the end. Presumably, the file end of the file '// &
     &         'is corrupted' )
           RETURN
      END IF
!
! --- Learn the number of H-records and the number of S-records
!
      N_PSVHAR(I_PSV) = 0
      N_PSVSTA(I_PSV) = 0
      DO 410 J1=2,N_HPSLEN
         IF ( BUF(J1)(1:2) .EQ. 'H ' ) THEN
              N_PSVHAR(I_PSV) = N_PSVHAR(I_PSV) + 1
         END IF
         IF ( BUF(J1)(1:2) .EQ. 'S ' ) THEN
              N_PSVSTA(I_PSV) = N_PSVSTA(I_PSV) + 1
         END IF
         IF ( BUF(J1)(1:2) .EQ. 'D ' ) GOTO 810
 410  CONTINUE
 810  CONTINUE
!
! --- Check whether the number of records is OK
!
      IF ( N_PSVHAR(I_PSV) .EQ. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( I_PSV, STR )
           CALL ERR_LOG ( 2737, IUER, 'LOAD_HARPOS', 'No harmonics were '// &
     &         'specified in the position variation file '// &
     &          POSVAR_FIL(I_PSV)(1:I_LEN(POSVAR_FIL(I_PSV))) )
           RETURN
      END IF
!
      IF ( N_PSVSTA(I_PSV) .EQ. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( I_PSV, STR )
           CALL ERR_LOG ( 2738, IUER, 'LOAD_HARPOS', 'No sites were '// &
     &         'specified in the position variation file '//POSVAR_FIL(I_PSV) )
           RETURN
      END IF
!
      LEN_NAMSIT(I_PSV) =   8*N_PSVSTA(I_PSV)*1
      LEN_HARVAL(I_PSV) =   3*N_PSVHAR(I_PSV)*8
      LEN_HARDSP(I_PSV) = 3*2*N_PSVHAR(I_PSV)*N_PSVSTA(I_PSV)*8
      LEN_STACOO(I_PSV) =   3*N_PSVSTA(I_PSV)*8
      LEN_NAMHAR = N_PSVHAR(I_PSV)*8
!
! --- Get dynamic memory for arrays which will hold results of parsing
!
      CALL ERR_PASS ( IUER, IER )
      CALL GRAB_MEM ( IER, MEM_LEN,           MEM_ADR,           5, &
     &                     LEN_NAMSIT(I_PSV), ADR_NAMSIT(I_PSV), &
     &                     LEN_HARVAL(I_PSV), ADR_HARVAL(I_PSV), &
     &                     LEN_HARDSP(I_PSV), ADR_HARDSP(I_PSV), &
     &                     LEN_STACOO(I_PSV), ADR_STACOO(I_PSV), &
     &                     LEN_NAMHAR,        ADR_NAMHAR          )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( MEM_LEN, STR )
           CALL ERR_LOG ( 2739, IUER, 'LOAD_HARPOS', 'Error in an attempt '// &
     &         'to grab '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN
      END IF
!
      STS_NAMSIT(I_PSV) = PSV__ALC
      STS_HARVAL(I_PSV) = PSV__ALC
      STS_HARDSP(I_PSV) = PSV__ALC
      STS_STACOO(I_PSV) = PSV__ALC
!
! --- Parse the file and put results of parsing in the dynamic arrays
! --- ADR_NAMSIT, ADR_HARVAL, ADR_STACOO, ADR_HDRDSP and ADR_NAMHAR
!
      POSVAR_RD_AREA(I_PSV) = -1.0D0
      CALL ERR_PASS ( IUER, IER )
      CALL HARPOS_FILE_READ ( POSVAR_FIL(I_PSV), N_HPSLEN, BUF, &
     &                        N_PSVHAR(I_PSV), N_PSVSTA(I_PSV), 8, &
     &                        %VAL(ADR_NAMHAR), POSVAR_RD_AREA(I_PSV), &
     &                        %VAL(ADR_NAMSIT(I_PSV)), %VAL(ADR_HARVAL(I_PSV)), &
     &                        %VAL(ADR_STACOO(I_PSV)), %VAL(ADR_HARDSP(I_PSV)), &
     &                        IER )
!
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2740, IUER, 'LOAD_HARPOS', 'Error in parsing '// &
     &         'input file '//POSVAR_FIL(I_PSV) )
           CALL FREE_MEM ( ADR_NAMSIT(I_PSV) )
           STS_NAMSIT(I_PSV) = 0
           STS_HARVAL(I_PSV) = 0
           STS_HARDSP(I_PSV) = 0
           STS_STACOO(I_PSV) = 0
           RETURN
      END IF
!
! --- Check POSVAR_RD_AREA  
!
      IF ( FMT_VERSION == '2002.12.12' ) THEN
!
! -------- HARPOS file format of 2002.02.12 did not define POSVAR_RD_AREA.
! -------- Use default defiend in NEA__PSV
!
           POSVAR_RD_AREA(I_PSV) = NEA__PSV
         ELSE 
           IF ( POSVAR_RD_AREA(I_PSV) < 0.0D0 ) THEN
                CALL ERR_LOG ( 2741, IUER, 'LOAD_HARPOS', 'Error in parsing '// &
     &              'position variation file '// &
     &               POSVAR_FIL(I_PSV)(1:I_LEN(POSVAR_FIL(I_PSV)))// &
     &              ' -- no A-record was found' )
                RETURN
           END IF
      END IF
!
      STS_NAMSIT(I_PSV) = PSV__REA
      STS_HARVAL(I_PSV) = PSV__REA
      STS_HARDSP(I_PSV) = PSV__REA
      STS_STACOO(I_PSV) = PSV__REA
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  LOAD_HARPOS  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE HARPOS_FILE_READ ( FINAM, N_BUF, BUF, N_HAR, N_STA, L_NAM, &
     &                              NAMHAR, RD_AREA, NAMSIT, HARVAL, STACOO, &
     &                              HARDSP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine HARPOS_FILE_READ parses harmonic records, station records  *
! *   and displacement records of the file of site displacements in      *
! *   HARPOS format. It is assumd that the files have already been read  *
! *   into the text buffer BUF and some information, the number of       *
! *   harmonics, the number of sites, is already extracted from there.   *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  FINAM ( CHARACTER ) -- The name of the file with the harmonic site  *
! *                         position variations model. Used for          *
! *                         generating error messages only.              *
! *  N_BUF ( INTEGER*4 ) -- The number of lines in the buffer with       *
! *                         contents of the file with definition of the  *
! *                         harminic model of site position variations.  *
! *    BUF ( CHARACTER ) -- Character array which keeps the image of the *
! *                         file with the harmonic model of site         *
! *                         position variations. Dimension: N_BUF        *
! *  N_HAR ( INTEGER*4 ) -- The number of harmonics in the model of site *
! *                         position variations.                         *
! *  N_STA ( INTEGER*4 ) -- The number of sites in the harmonic model of *
! *                         site position  variations.                   *
! *  L_NAM ( INTEGER*4 ) -- The length of the site name string in bytes. *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *  NAMHAR ( CHARACTER ) -- Arrays of hamonic names. Dimension: N_HAR.  *
! * RD_AREA ( REAL*8    ) -- The radius of the area for which            *
! *                          displacements are applicable. Files in      *
! *                          HARPOS format of 2002.12.20 did not define  *
! *                          the radius of the area. The output value    *
! *                          is undefined in this case. Files in HARPOS  *
! *                          format of 2005.03.28 define the radius of   *
! *                          this area in the A-record.                  *
! *  NAMSIT ( CHARACTER ) -- Array of site names. Each site name has     *
! *                          length of L_NAM characters. Dimension:      *
! *                          N_STA.                                      *
! *  STACOO ( REAL*8    ) -- Arrays of site coordinates in a crust       *
! *                          reference frame. Dimension: (3,N_STA).      *
! *  HARVAL ( REAL*8    ) -- Two-dimensional array of phases,            *
! *                          frequencies and accelerations for each      *
! *                          harmonis. Dimension: (3,N_HAR).             *
! *  HARDSP ( REAL*8    ) -- Four-dimensional array of site position     *
! *                          displacements. The first index rnus through *
! *                          Up, East, North component of a displacement *
! *                          vector. The secon index runs through cosine *
! *                          (1) and sine (2) mode, the third index runs *
! *                          through harmonics, and the fourth index     *
! *                          runs through stations. Dimension:           *
! *                          3,2,N_HAR,N_STA                             *
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
! * ### 16-DEC-2002 HARPOS_FILE_READ  v2.0 (c) L. Petrov 28-MAR-2005 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'bindisp.i'
      INCLUDE   'harpos.i'
      TYPE ( HARPOS__H_RECORD ) :: HREC
      TYPE ( HARPOS__A_RECORD ) :: AREC
      TYPE ( HARPOS__S_RECORD ) :: SREC
      TYPE ( HARPOS__D_RECORD ) :: DREC
      INTEGER*4  N_BUF, N_HAR, N_STA, L_NAM, IUER
      CHARACTER  FINAM*(*), BUF(N_BUF)*(*), NAMSIT(N_STA)*(L_NAM), &
     &           NAMHAR(N_HAR)*(L_NAM)
      REAL*8     RD_AREA, HARVAL(3,N_HAR), STACOO(3,N_STA), &
     &           HARDSP(3,2,N_HAR,N_STA)
      CHARACTER  STR*128
      INTEGER*4  K_HAR, K_STA, I_WAV, I_STA, IOS, IP, J1
      INTEGER*4  LTM_DIF, I_LEN
!
      K_HAR = 0
      K_STA = 0
!
! --- Read the buffer, except the very first and the very last lines which
! --- should be labels of the format identifer
!
      DO 410 J1=2,N_BUF-1
         IF ( BUF(J1)(1:1) .EQ. '#' ) THEN
!
! ----------- This is a comment line
!
              CONTINUE
            ELSE IF ( BUF(J1)(1:2) .EQ. 'A' ) THEN
              CALL LIB$MOVC3 ( LEN__A_REC, %REF(BUF(J1)), AREC )
              READ ( UNIT=AREC%AREA_RD, FMT='(F14.6)', IOSTAT=IOS ) RD_AREA 
              IF ( IOS .NE. 0 ) THEN
                   CALL ERR_LOG ( 2751, IUER, 'HARPOS_FILE_READ', 'Error '// &
     &                 'in an attempt to decode the A-record' )
                   RETURN
              END  IF
            ELSE IF ( BUF(J1)(1:2) .EQ. 'H ' ) THEN
!
! ----------- Aga, this is the H-record
!
              K_HAR = K_HAR + 1
              IF ( K_HAR .GT. N_HAR ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( N_HAR, STR )
                   CALL ERR_LOG ( 2752, IUER, 'HARPOS_FILE_READ', 'Too many '// &
     &                 'harmonics: more than N_HAR='//STR )
                   RETURN
              END IF
!
! ----------- Put the line from the butter into the record BDSUM
!
              CALL LIB$MOVC3 ( LEN__H_REC, %REF(BUF(J1)), HREC )
!
! ----------- Extract the harmonic ID
!
              NAMHAR(K_HAR) = HREC%WAVE_ID
              IF ( K_HAR .GT. 1 ) THEN
                   IP = LTM_DIF ( 1, K_HAR-1, NAMHAR, NAMHAR(K_HAR) )
                   IF ( IP .GT. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( N_HAR, STR )
                        CALL ERR_LOG ( 2753, IUER, 'HARPOS_FILE_READ', &
     &                      'Harmonic '//NAMHAR(K_HAR)//' was determined '// &
     &                      'more than once in HARPOS file '//FINAM )
                        RETURN
                   END IF
              END IF
!
! ----------- Read phase, frequency and acceleration of the K_HAR -th harmonic
!
              READ ( UNIT=HREC%PHASE, FMT='(F13.1)' ) HARVAL(1,K_HAR)
              READ ( UNIT=HREC%FREQ,  FMT='(F19.1)' ) HARVAL(2,K_HAR)
              READ ( UNIT=HREC%ACCEL, FMT='(F10.1)' ) HARVAL(3,K_HAR)
            ELSE IF ( BUF(J1)(1:2) .EQ. 'S ' ) THEN
!
! ----------- U-u-u! It is the S-record
!
              K_STA = K_STA + 1
              IF ( K_STA .GT. N_STA ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( N_STA, STR )
                   CALL ERR_LOG ( 2754, IUER, 'HARPOS_FILE_READ', 'Too many '// &
     &                 'stations: more than N_STA='//STR )
                   RETURN
              END IF
!
! ----------- Put the line from the summary file into the S-record
!
              CALL LIB$MOVC3 ( LEN__S_REC, %REF(BUF(J1)), SREC )
              NAMSIT(K_STA) = SREC%SITE_ID
              IF ( K_HAR .GT. 1 ) THEN
                   IP = LTM_DIF ( 1, K_STA-1, NAMSIT, NAMSIT(K_STA) )
                   IF ( IP .GT. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( N_HAR, STR )
                        CALL ERR_LOG ( 2755, IUER, 'HARPOS_FILE_READ', &
     &                      'Station '//NAMSIT(K_STA)//' was determined '// &
     &                      'more than once in HARPOS file '//FINAM )
                        RETURN
                   END IF
              END IF
!
! ----------- Extract station coordinates
!
              READ ( UNIT=SREC%X_COORD, FMT='(F13.1)' ) STACOO(1,K_STA)
              READ ( UNIT=SREC%Y_COORD, FMT='(F13.1)' ) STACOO(2,K_STA)
              READ ( UNIT=SREC%Z_COORD, FMT='(F13.1)' ) STACOO(3,K_STA)
            ELSE IF ( BUF(J1)(1:2) .EQ. 'D ' ) THEN
!
! ----------- Oooh! It is the D-record
!
              CALL LIB$MOVC3 ( LEN__D_REC, %REF(BUF(J1)), DREC )
!
! ----------- Search for the name of the harmonic in the D-record
!
              I_WAV = LTM_DIF ( 1, N_HAR, NAMHAR, DREC%WAVE_ID )
              IF ( I_WAV .LE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2756, IUER, 'HARPOS_FILE_READ', &
     &                 'Error in processing the '//STR(1:I_LEN(STR))//'-th '// &
     &                 'line of the input file '//FINAM(1:I_LEN(FINAM))// &
     &                 ' -- the harmonic "'//DREC%WAVE_ID//'" was not '// &
     &                 'defined in the preceeding H-records')
                   RETURN
              END IF
!
! ----------- Search for the site name in the D-record
!
              I_STA = LTM_DIF ( 1, K_STA, NAMSIT, DREC%SITE_ID )
              IF ( I_STA .LE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2757, IUER, 'HARPOS_FILE_READ', &
     &                 'Error in processing the '//STR(1:I_LEN(STR))//'-th '// &
     &                 'line of the input file '//FINAM(1:I_LEN(FINAM))// &
     &                 ' -- the site "'//DREC%SITE_ID//'" was not '// &
     &                 'defined in the preceeding S-records' )
                   RETURN
              END IF
!
! ----------- Read cosine amplitudes of the displacements
!
              READ ( UNIT=DREC%UP_COS,    FMT='(F8.1)' ) HARDSP(1,1,I_WAV,I_STA)
              READ ( UNIT=DREC%EAST_COS,  FMT='(F8.1)' ) HARDSP(2,1,I_WAV,I_STA)
              READ ( UNIT=DREC%NORTH_COS, FMT='(F8.1)' ) HARDSP(3,1,I_WAV,I_STA)
!
! ----------- Read sine amplitudes of the displacements
!
              READ ( UNIT=DREC%UP_SIN,    FMT='(F8.1)' ) HARDSP(1,2,I_WAV,I_STA)
              READ ( UNIT=DREC%EAST_SIN,  FMT='(F8.1)' ) HARDSP(2,2,I_WAV,I_STA)
              READ ( UNIT=DREC%NORTH_SIN, FMT='(F8.1)' ) HARDSP(3,2,I_WAV,I_STA)
            ELSE
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 2758, IUER, 'HARPOS_FILE_READ', 'Error in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &            'input file '//FINAM(1:I_LEN(FINAM))//' -- unknown record '// &
     &            'type was encountered' )
              RETURN
         END IF
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  HARPOS_FILE_READ  #!#
