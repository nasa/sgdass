      SUBROUTINE VTD_LOAD_HARPOS ( VTD, I_PSV, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_LOAD_HARPOS  loads the file with harmonic site        *
! *   position variations in HARPOS format into the dynamic memory area. *
! *   The file name is defined in VTD as the I_PSV-th element of the     *
! *   array VTD%CONF%POSVAR_FIL. The routine reads the file, checks its  *
! *   format, learns  the number of harmonics and sites, allocates       *
! *   dynamic memory for these arrays and for the array with             *
! *   displacement. Then it reads phase, frequency and acceleration      *
! *   of harmonics, reads site names and coordinates and eventually      *
! *   reads entire table of displacements. It also stores the date       *
! *   of last modification of the HARPOS file.                           *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! * I_PSV ( INTEGER*4 ) -- Index of the position variation file in the   *
! *                        array VTD%CONF%POSVAR_FIL.                    *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       VTD ( RECORD    ) -- Object which keeps configuration and data *
! *                            related to VLBI Theoretical Delay (VTD)   *
! *                            package.                                  *
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
! * ### 16-DEC-2002  VTD_LOAD_HARPOS  v2.2 (c) L. Petrov 06-AUG-2014 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'vtd.i'
      TYPE ( VTD__TYPE ) :: VTD
      INTEGER*4  I_PSV, IUER
      CHARACTER  BUF(M__HPSLEN)*80, STR*128, FMT_VERSION*10
      ADDRESS__TYPE :: LEN_NAMHAR, ADR_NAMHAR
      REAL*8     VAL_R8
      INTEGER*4  N_HPSLEN, STAT_BLOCK(16), IS, J1, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, FOR_STAT
!
      IF ( I_PSV .LE. 0  .OR.  I_PSV .GT. VTD__M_PSF  ) THEN
           CALL CLRCH ( STR )
           CALL INCH   ( I_PSV, STR )
           CALL ERR_LOG ( 2731, IUER, 'VTD_LOAD_HARPOS', 'Wrong parameter '// &
     &         'I_PSV: '//STR )
           RETURN
      END IF
!
      IF ( ILEN(VTD%CONF%POSVAR_FIL(I_PSV)) .EQ. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH   ( I_PSV, STR )
           CALL ERR_LOG ( 2732, IUER, 'VTD_LOAD_HARPOS', 'Empty filename '// &
     &          'with the '//STR(1:I_LEN(STR))//'-th position variation file' )
           RETURN
      END IF
!
! --- Get information about the file VTD%CONF%POSVAR_FIL(I_PSV)
!
      IS = FOR_STAT ( VTD%CONF%POSVAR_FIL(I_PSV), STAT_BLOCK )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 2733, IUER, 'VTD_LOAD_HARPOS', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to learn date of '// &
     &         'modification of the input file '//VTD%CONF%POSVAR_FIL(I_PSV) )
           RETURN
      END IF
!
! --- Save the data of modification
!
      VTD%POSVAR(I_PSV)%TIM_PSVFIL = STAT_BLOCK(10)
!
! --- Read the file into the buffer BUF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( VTD%CONF%POSVAR_FIL(I_PSV), M__HPSLEN, BUF, &
     &                N_HPSLEN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2734, IUER, 'VTD_LOAD_HARPOS', 'Error in an attempt '// &
     &         'to read file '//VTD%CONF%POSVAR_FIL(I_PSV) )
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
           CALL ERR_LOG ( 2735, IUER, 'VTD_LOAD_HARPOS', 'Input file '// &
     &          VTD%CONF%POSVAR_FIL(I_PSV)(1:I_LEN(VTD%CONF%POSVAR_FIL(I_PSV)))// &
     &         ' is not in HARPOS format' )
           RETURN
      END IF
!
! --- Check the last line: it should have the HARPOS label
!
      IF ( BUF(N_HPSLEN)(1:LEN(HARPOS__LABEL)) .NE. BUF(1)(1:LEN(HARPOS__LABEL)) ) THEN
           CALL ERR_LOG ( 2736, IUER, 'VTD_LOAD_HARPOS', 'Input file '// &
     &       VTD%CONF%POSVAR_FIL(I_PSV)(1:I_LEN(VTD%CONF%POSVAR_FIL(I_PSV)))// &
     &         ' was not read up to the end. Presumably, the file end of '// &
     &         'the file is corrupted' )
           RETURN
      END IF
!
! --- Learn the number of H-records and the number of S-records
!
      VTD%POSVAR(I_PSV)%N_PSVHAR = 0
      VTD%POSVAR(I_PSV)%N_PSVSTA = 0
      DO 410 J1=2,N_HPSLEN
         IF ( BUF(J1)(1:2) .EQ. 'H ' ) THEN
              VTD%POSVAR(I_PSV)%N_PSVHAR = VTD%POSVAR(I_PSV)%N_PSVHAR + 1
         END IF
         IF ( BUF(J1)(1:2) .EQ. 'S ' ) THEN
              VTD%POSVAR(I_PSV)%N_PSVSTA = VTD%POSVAR(I_PSV)%N_PSVSTA + 1
         END IF
         IF ( BUF(J1)(1:2) .EQ. 'D ' ) GOTO 810
 410  CONTINUE
 810  CONTINUE
!
! --- Check whether the number of records is OK
!
      IF ( VTD%POSVAR(I_PSV)%N_PSVSTA  .EQ. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( I_PSV, STR )
           CALL ERR_LOG ( 2737, IUER, 'VTD_LOAD_HARPOS', 'No harmonics '// &
     &         'were specified in the position variation file '// &
     &          VTD%CONF%POSVAR_FIL(I_PSV) )
           RETURN
      END IF
!
      IF ( VTD%POSVAR(I_PSV)%N_PSVSTA .EQ. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( I_PSV, STR )
           CALL ERR_LOG ( 2738, IUER, 'VTD_LOAD_HARPOS', 'No sites were '// &
     &         'specified in the position variation file '// &
     &          VTD%CONF%POSVAR_FIL(I_PSV) )
           RETURN
      END IF
!
      VTD%POSVAR(I_PSV)%LEN_NAMSIT  = 8*VTD%POSVAR(I_PSV)%N_PSVSTA*1
      VTD%POSVAR(I_PSV)%LEN_HARVAL  = 3*VTD%POSVAR(I_PSV)%N_PSVHAR*8
      VTD%POSVAR(I_PSV)%LEN_HARDSP  = 3*2*VTD%POSVAR(I_PSV)%N_PSVHAR * &
     &                                   VTD%POSVAR(I_PSV)%N_PSVSTA*8
      VTD%POSVAR(I_PSV)%LEN_STACOO  = 3*VTD%POSVAR(I_PSV)%N_PSVSTA*8
      LEN_NAMHAR                    = VTD%POSVAR(I_PSV)%N_PSVHAR*8
!
! --- Get dynamic memory for arrays which will hold results of parsing
!
      VTD%MEM_CHN = VTD%MEM_CHN + 1
      IF ( VTD%MEM_CHN > VTD__MEM ) THEN
           CALL ERR_LOG ( 2739, IUER, 'VTD_LOAD_HARPOS', 'Too many chunks '// &
     &         'of memory allocated. Please check your program for memory '// &
     &         'leakage' )
           RETURN
      END IF
      CALL ERR_PASS ( IUER, IER )
      CALL GRAB_MEM ( IER, VTD%POSVAR(I_PSV)%MEM_LEN,    &
     &                     VTD%POSVAR(I_PSV)%MEM_ADR, 5, &
     &     VTD%POSVAR(I_PSV)%LEN_NAMSIT,  VTD%POSVAR(I_PSV)%ADR_NAMSIT,  &
     &     VTD%POSVAR(I_PSV)%LEN_HARVAL,  VTD%POSVAR(I_PSV)%ADR_HARVAL,  &
     &     VTD%POSVAR(I_PSV)%LEN_HARDSP,  VTD%POSVAR(I_PSV)%ADR_HARDSP,  &
     &     VTD%POSVAR(I_PSV)%LEN_STACOO,  VTD%POSVAR(I_PSV)%ADR_STACOO,  &
     &     LEN_NAMHAR,                    ADR_NAMHAR                     )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
#ifdef ADR_32BIT
           CALL IINCH  ( VTD%POSVAR(I_PSV)%MEM_LEN, STR )
#else
           CALL IINCH8 ( VTD%POSVAR(I_PSV)%MEM_LEN, STR )
#endif
           CALL ERR_LOG ( 2739, IUER, 'VTD_LOAD_HARPOS', 'Error in an '// &
     &         'attempt to grab '//STR(1:I_LEN(STR))//  &
     &         ' bytes of dynamic memory' )
           RETURN
      END IF
!
! --- Memory initialization. That matters. In partucular, if some D-records
! --- are undefined, zero displacements will be used
!
      CALL NOUT ( VTD%POSVAR(I_PSV)%MEM_LEN, %VAL(VTD%POSVAR(I_PSV)%MEM_ADR) )
!
      VTD%POSVAR(I_PSV)%STS_NAMSIT  = PSV__ALC
      VTD%POSVAR(I_PSV)%STS_HARVAL  = PSV__ALC
      VTD%POSVAR(I_PSV)%STS_HARDSP  = PSV__ALC
      VTD%POSVAR(I_PSV)%STS_STACOO  = PSV__ALC
!
! --- Parse the file and put results of parsing in the dynamic arrays
! --- ADR_NAMSIT, ADR_HARVAL, ADR_STACOO, ADR_HDRDSP and ADR_NAMHAR
!
      VTD%POSVAR(I_PSV)%RD_AREA = -1.0D0
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_HARPOS_FILE_READ ( VTD%CONF%POSVAR_FIL(I_PSV), N_HPSLEN, BUF, &
     &     VTD%POSVAR(I_PSV)%N_PSVHAR, VTD%POSVAR(I_PSV)%N_PSVSTA, 8, &
     &     %VAL(ADR_NAMHAR), VTD%POSVAR(I_PSV)%RD_AREA, &
     &     %VAL(VTD%POSVAR(I_PSV)%ADR_NAMSIT), &
     &     %VAL(VTD%POSVAR(I_PSV)%ADR_HARVAL), &
     &     %VAL(VTD%POSVAR(I_PSV)%ADR_STACOO), &
     &     %VAL(VTD%POSVAR(I_PSV)%ADR_HARDSP), IER )
!
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2740, IUER, 'VTD_LOAD_HARPOS', 'Error in parsing '// &
     &         'input file '//VTD%CONF%POSVAR_FIL(I_PSV) )
           CALL FREE_MEM ( VTD%POSVAR(I_PSV)%MEM_ADR )
           VTD%POSVAR(I_PSV)%STS_NAMSIT = 0
           VTD%POSVAR(I_PSV)%STS_HARVAL = 0
           VTD%POSVAR(I_PSV)%STS_HARDSP = 0
           VTD%POSVAR(I_PSV)%STS_STACOO = 0
           RETURN
      END IF
!
! --- Check VTD%POSVAR(I_PSV)%RD_AREA
!
      IF ( FMT_VERSION == '2002.12.12' ) THEN
!
! -------- HARPOS file format of 2002.12.12 did not define POSVAR_RD_AREA.
! -------- Use default defiend in NEA__PSV
!
           VTD%POSVAR(I_PSV)%RD_AREA = NEA__PSV
         ELSE
           IF ( VTD%POSVAR(I_PSV)%RD_AREA < 0.0D0 ) THEN
                CALL ERR_LOG ( 2741, IUER, 'VTD_LOAD_HARPOS', 'Error in parsing '// &
     &              'position variation file '// &
     &               VTD%CONF%POSVAR_FIL(I_PSV)(1:I_LEN(VTD%CONF%POSVAR_FIL(I_PSV)))// &
     &              ' -- no A-record was found' )
                RETURN
           END IF
      END IF
!
      VTD%POSVAR(I_PSV)%STS_NAMSIT = PSV__REA
      VTD%POSVAR(I_PSV)%STS_HARVAL = PSV__REA
      VTD%POSVAR(I_PSV)%STS_HARDSP = PSV__REA
      VTD%POSVAR(I_PSV)%STS_STACOO = PSV__REA
      VTD%POSVAR(I_PSV)%STATUS     = VTD__LOAD
      VTD%POSVAR(I_PSV)%PSV_TYPE   = PSV__HMD
      VTD%POSVAR(I_PSV)%STS_BSPPOS = VTD__UNDF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_LOAD_HARPOS  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE VTD_HARPOS_FILE_READ ( FINAM, N_BUF, BUF, N_HAR, N_STA, &
     &                                  L_NAM, NAMHAR, RD_AREA, NAMSIT, &
     &                                  HARVAL,   STACOO, HARDSP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_HARPOS_FILE_READ parses harmonic records, station      *
! *   records and displacement records of the file of site displacements *
! *   in HARPOS format. It is assumd that the files have already been    *
! *   read into the text buffer BUF and some information, the number of  *
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
! *  N_HAR ( INTEGER*4 ) -- The maximal number of harmonics in the model *
! *                         of site position variations.                 *
! *  N_STA ( INTEGER*4 ) -- The maximal number of sites in the harmonic  *
! *                         model of site position  variations.          *
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
! * NAMSIT ( CHARACTER ) -- Array of site names. Each site name has      *
! *                         length of L_NAM characters. Dimension:       *
! *                         N_STA.                                       *
! * STACOO ( REAL*8    ) -- Arrays of site coordinates in a crust        *
! *                         reference frame. Dimension: (3,N_STA).       *
! * HARVAL ( REAL*8    ) -- Two-dimensional array of phases, frequencies *
! *                         and accelerations for each harmonis.         *
! *                         Dimension: (3,N_HAR).                        *
! * HARDSP ( REAL*8    ) -- Four-dimensional array of site position      *
! *                         displacements. The first index rnus through  *
! *                         Up, East, North component of a displacement  *
! *                         vector. The secon index runs through cosine  *
! *                         (1) and sine (2) mode, the third index runs  *
! *                         through harmonics, and the fourth index runs *
! *                         through stations. Dimension: 3,2,N_HAR,N_STA *
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
! * # 16-DEC-2002 VTD_HARPOS_FILE_READ  v2.0 (c) L. Petrov 28-MAR-2005 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
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
      INTEGER*4  K_HAR, K_STA, I_WAV, I_STA, IP, IOS, J1
      INTEGER*4, EXTERNAL :: LTM_DIF, I_LEN, LOC__SUN$$_STR
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
            ELSE IF ( BUF(J1)(1:2) .EQ. 'H ' ) THEN
!
! ----------- Aga, this is the H-record
!
              K_HAR = K_HAR + 1
              IF ( K_HAR .GT. N_HAR ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( N_HAR, STR )
                   CALL ERR_LOG ( 2751, IUER, 'VTD_HARPOS_FILE_READ', 'Too '// &
     &                 'many harmonics: more than N_HAR='//STR )
                   RETURN
              END IF
!
! ----------- Put the line from the butter into the record BDSUM
!
#ifdef SUN
              CALL LIB$MOVC3 ( LEN__H_REC, %VAL(LOC__SUN$$_STR(BUF(J1))), HREC )
#else
              CALL LIB$MOVC3 ( LEN__H_REC, %REF(BUF(J1)), HREC )
#endif
!
! ----------- Extract the harmonic ID
!
              NAMHAR(K_HAR) = HREC%WAVE_ID
              IF ( K_HAR .GT. 1 ) THEN
                   IP = LTM_DIF ( 1, K_HAR-1, NAMHAR, NAMHAR(K_HAR) )
                   IF ( IP .GT. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( N_HAR, STR )
                        CALL ERR_LOG ( 2752, IUER, 'VTD_HARPOS_FILE_READ', &
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
            ELSE IF ( BUF(J1)(1:2) .EQ. 'A ' ) THEN
#ifdef SUN
              CALL LIB$MOVC3 ( LEN__A_REC, %VAL(LOC__SUN$$_STR(BUF(J1))), AREC )
#else
              CALL LIB$MOVC3 ( LEN__A_REC, %REF(BUF(J1)), AREC )
#endif
              READ ( UNIT=AREC%AREA_RD, FMT='(F14.6)', IOSTAT=IOS ) RD_AREA
              IF ( IOS .NE. 0 ) THEN
                   CALL ERR_LOG ( 2753, IUER, 'VTD_HARPOS_FILE_READ', &
     &                 'Error in an attempt to decode the A-record' )
                   RETURN
              END  IF
            ELSE IF ( BUF(J1)(1:2) .EQ. 'S ' ) THEN
!
! ----------- U-u-u! It is the S-record
!
              K_STA = K_STA + 1
              IF ( K_STA .GT. N_STA ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( N_STA, STR )
                   CALL ERR_LOG ( 2754, IUER, 'VTD_HARPOS_FILE_READ', 'Too '// &
     &                 'many stations: more than N_STA='//STR )
                   RETURN
              END IF
!
! ----------- Put the line from the summary file into the S-record
!
#ifdef SUN
              CALL LIB$MOVC3 ( LEN__S_REC, %VAL(LOC__SUN$$_STR(BUF(J1))), SREC )
#else
              CALL LIB$MOVC3 ( LEN__S_REC, %REF(BUF(J1)), SREC )
#endif
              NAMSIT(K_STA) = SREC%SITE_ID
              IF ( K_HAR .GT. 1 ) THEN
                   IP = LTM_DIF ( 1, K_STA-1, NAMSIT, NAMSIT(K_STA) )
                   IF ( IP .GT. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( N_HAR, STR )
                        CALL ERR_LOG ( 2755, IUER, 'VTD_HARPOS_FILE_READ', &
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
#ifdef SUN
              CALL LIB$MOVC3 ( LEN__D_REC, %VAL(LOC__SUN$$_STR(BUF(J1))), DREC )
#else
              CALL LIB$MOVC3 ( LEN__D_REC, %REF(BUF(J1)), DREC )
#endif
!
! ----------- Search for the name of the harmonic in the D-record
!
              I_WAV = LTM_DIF ( 1, N_HAR, NAMHAR, DREC%WAVE_ID )
              IF ( I_WAV .LE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2756, IUER, 'VTD_HARPOS_FILE_READ', &
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
                   CALL ERR_LOG ( 2757, IUER, 'VTD_HARPOS_FILE_READ', &
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
              CALL ERR_LOG ( 2758, IUER, 'VTD_HARPOS_FILE_READ', 'Error in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &            'input file '//FINAM(1:I_LEN(FINAM))//' -- unknown record '// &
     &            'type was encountered' )
              RETURN
         END IF
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_HARPOS_FILE_READ  !#!#
