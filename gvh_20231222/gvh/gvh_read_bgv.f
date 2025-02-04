      SUBROUTINE GVH_READ_BGV ( GVH, ISEG, FILENAME, REMAINED_BYTES, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GVH_READ_BGV  reads the contents of one or more           *
! *   segments of the input file in binary GVH format. If ISEG=0,        *
! *   then it reads from the file segments. If ISEG is not equal to      *
! *   zero, then the ISEG-th segment is read. NB: argument ISEG          *
! *   corresponds to thte segment counter counted from the beginning of  *
! *   the file starting from 1. This may not correspond to the segments  *
! *   counter of GVH! GVH_READ_BGV increments segment counter before     *
! *   reading each new segment, except the case when no segments have    *
! *   been loaded in GVH, i.e. it was  just initialized. In that case    *
! *   GVH_READ_BGV will put the first segment from the input file        *
! *   to the first segment of GVH.                                       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     ISEG ( INTEGER*4 ) -- Segment index. If ISEG = 0, then all       *
! *                           segments are read from the input file      *
! *                           which may contain one segment or several   *
! *                           concatenated segments.                     *
! *                           If ISEG > 0  then the ISEG-th segment is   *
! *                           read from the input file.                  *
! * FILENAME ( CHARACTER ) -- Name of the file from which the database   *
! *                           will be read.                              *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * REMAINED_BYTES ( INTEGER*4 ) -- The number of bytes which remained   *
! *                                 unread in the file. If the file      *
! *                                 contain only one segment or ISEG=0,  *
! *                                 then this number is 0. Non-zero      *
! *                                 number indicates that there is at    *
! *                                 least one segment after the one      *
! *                                 which has been read.                 *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     GVH ( GVH__STRU      ) -- Data structure which keeps internal    *
! *                               information related to the database of *
! *                               an astro/geo VLBI experiment.          *
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
! * ### 25-NOV-2001  GVH_READ_BGV  v2.2 (c)  L. Petrov  12-FEB-2022  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      TYPE      ( GVH__STRU      ) :: GVH
      TYPE      ( GVH_PREF__STRU ) :: GVH_PREF
      CHARACTER    FILENAME*(*)
      CHARACTER    KEYWORD_DEL, CHAPTER_DEL*1, RECORD_DEL*1, LABEL*128
      CHARACTER    STR*32, STR1*32
      PARAMETER  ( KEYWORD_DEL = CHAR(10) )
      PARAMETER  ( CHAPTER_DEL = CHAR(26) )
      PARAMETER  ( RECORD_DEL  = CHAR(10) )
      INTEGER*4  ISEG, REMAINED_BYTES, IUER
      LOGICAL*4  LEX 
      CHARACTER  KEYWORD_NAME*32, KEYWORD_VALUE*128
      INTEGER*4  GVH_FILDES, STAT_BLOCK(24), J1, J2, J3, LEN_FIL, &
     &           LEN_MEM, ILN, IS, IP, KSEG, READ_BYTES, IER
      LOGICAL*4  FL_B1
      ADDRESS__TYPE IAD, ADR_MEM
      INTEGER*1  TAB(0:255), TAB_VAL
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LIB$SCANC, FOR_STAT
!
      IF ( GVH%STATUS .NE. GVH__INITIALIZED ) THEN
           CALL ERR_LOG ( 4171, IUER, 'GVH_READ_BGV', 'The GVH data '// &
     &         'structure was not initialized. Please, use gvh_init first' )
           RETURN
      END IF
!
      IF ( ISEG .LT.  0 ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( ISEG, STR )
           CALL ERR_LOG ( 4172, IUER, 'GVH_READ_BGV', 'Wrong parameter '// &
     &         'ISEG: '//STR )
           RETURN
      END IF
!
      IF ( ISEG .GT. GVH__MSEG ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( ISEG, STR )
           CALL CLRCH ( STR1 ) 
           CALL INCH  ( GVH__MSEG, STR1 )
           CALL ERR_LOG ( 4173, IUER, 'GVH_READ_BGV', 'Wrong parameter '// &
     &         'ISEG: '//STR(1:I_LEN(STR))//' -- it exceeds the '// &
     &         'maximal number of segments GVH%NSEG: '//STR1 )
           RETURN
      END IF
!
      IF ( ISEG .GT. GVH%NSEG+1 ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( ISEG, STR )
           CALL CLRCH ( STR1 ) 
           CALL INCH  ( GVH%NSEG, STR1 )
           CALL ERR_LOG ( 4174, IUER, 'GVH_READ_BGV', 'Wrong parameter '// &
     &         'ISEG: '//STR(1:I_LEN(STR))//' -- it exceeds the total '// &
     &         'numer of segments GVH%NSEG by more than 1: '//STR1 )
           RETURN
      END IF
!
      IF ( GVH%NSEG == 1 ) THEN
!
! -------- Do not advance current section counter in the case if the current
! -------- section is 1 and no file has been loaded
!
           IF ( ILEN(GVH%FILENAME(1)) > 0 ) GVH%NSEG = GVH%NSEG + 1
         ELSE 
           GVH%NSEG = GVH%NSEG + 1
      END IF
      KSEG = GVH%NSEG 
!
      INQUIRE ( FILE=FILENAME, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 4175, IUER, 'GVH_READ_BGV', 'File '// &
     &                    FILENAME(1:I_LEN(FILENAME))//' not found' )
           RETURN
      END IF
!
! --- Inqure status of the input file
!
      IS = FOR_STAT ( FILENAME, STAT_BLOCK )
      LEN_FIL = STAT_BLOCK(8)
!
      IER = IUER
      CALL BINF_OPEN ( FILENAME, 'OLD', GVH_FILDES, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4176, IUER, 'GVH_READ_BGV', 'Error '// &
     &         ' in an attempt to open the input file '//FILENAME )
           RETURN
      END IF
!
! --- Read the label
!
      IER = IUER
      CALL GVH_READ_BIN ( GVH_FILDES, LABEL, LEN(GVH__BGV_LABEL_BIG_ENDIAN), &
     &                     IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4177, IUER, 'GVH_READ_BGV', 'Error during '// &
     &         'reading the first record of the input file '//FILENAME )
           RETURN
      END IF
!
! --- Parse the label. In particularly the label should tell, whether the 
! --- database has been written in big endian or little endian mode.
!
      FL_B1 = .FALSE.
      IF ( LABEL .EQ. GVH__BGV_LABEL_BIG_ENDIAN ) THEN
#ifdef LITTLE_ENDIAN
           GVH%ENDIAN_SWAP = .TRUE.
#else
           GVH%ENDIAN_SWAP = .FALSE.
#endif
        ELSE IF ( LABEL .EQ. GVH__BGV_LABEL_LITTLE_ENDIAN ) THEN
#ifdef BIG_ENDIAN
           GVH%ENDIAN_SWAP = .TRUE.
#else
           GVH%ENDIAN_SWAP = .FALSE.
#endif
        ELSE IF ( LABEL .EQ. GVH__BGV_LABEL_20050114 ) THEN
           FL_B1 = .TRUE.
#ifdef BIG_ENDIAN
           GVH%ENDIAN_SWAP = .TRUE.
#else
           GVH%ENDIAN_SWAP = .FALSE.
#endif
        ELSE 
           CALL ERR_LOG ( 4178, IUER, 'GVH_READ_BGV', 'File '// &
     &          FILENAME(1:I_LEN(FILENAME))//' is not in Geo VLBI format '// &
     &         'Label: '//LABEL )
           RETURN
      END IF
!
      READ_BYTES = LEN(GVH__BGV_LABEL_BIG_ENDIAN)
      DO 410 J1=1,GVH__MSEG 
!
! ------ Extract prefix of PREA section
!
         IER = IUER
         CALL GVH_READ_BIN ( GVH_FILDES, GVH_PREF, SIZEOF(GVH_PREF), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4179, IUER, 'GVH_READ_BGV', 'Error during '// &
     &            'reading prefix of PREA section of the input file '// &
     &             FILENAME )
              RETURN
         END IF
         IF ( GVH_PREF%NAME .NE. GVH__PREA ) THEN
              WRITE ( 6, * ) ' GVH_PREF%NAME = ', GVH_PREF%NAME
              CALL ERR_LOG ( 4180, IUER, 'GVH_READ_BGV', 'Trap of '// &
     &            'internal control: prefix of PREA section was '// &
     &            'not found in the input file '//FILENAME )
              RETURN
         END IF
!
         IF ( GVH%ENDIAN_SWAP ) CALL ENDIAN_CNV_I4 ( GVH_PREF%LEN )
         READ_BYTES = READ_BYTES + SIZEOF(GVH_PREF)
!
         IF ( ISEG == 0  .OR.  ISEG == J1 ) THEN
!
! =========== Parse PREA section
!
              CALL CLRCH ( GVH%FILENAME(KSEG) )
              GVH%FILENAME(KSEG)     = FILENAME
              GVH%OLD_FILENAME(KSEG) = FILENAME
!
              IER = IUER
              GVH%PREA(KSEG)%LEN = GVH_PREF%LEN 
              IF ( GVH%PREA(KSEG)%LEN > 0 ) THEN
                   CALL GVH_ALLOCATE ( GVH, GVH%PREA(KSEG)%LEN, &
     &                                 GVH%PREA(KSEG)%ADR, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL IINCH ( GVH%PREA(KSEG)%LEN, STR )
                        CALL ERR_LOG ( 4181, IUER, 'GVH_READ_BGV', 'Error '// &
     &                      'in allocating '//STR(1:I_LEN(STR))//' bytes '// &
     &                      'of memory for PREA section of the image '// &
     &                     'of input file '//FILENAME )
                       RETURN
                   END IF
                 ELSE 
                   GVH%PREA(KSEG)%ADR = 0
              END IF
!
              IER = IUER
              CALL GVH_READ_BIN ( GVH_FILDES, %VAL(GVH%PREA(KSEG)%ADR), &
     &                                             GVH%PREA(KSEG)%LEN,  IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4182, IUER, 'GVH_READ_BGV', 'Error during '// &
     &                 'reading PREA section of the input file '//FILENAME )
                   RETURN
              END IF
              READ_BYTES = READ_BYTES + GVH_PREF%LEN 
!
              IAD = GVH%PREA(KSEG)%ADR
              ILN = GVH%PREA(KSEG)%LEN
!
              CALL NOUT ( 256, TAB )
              TAB_VAL = 1
              TAB(10) = TAB_VAL ! Carriage return -- record delimiter
              TAB(26) = TAB_VAL ! chapter delimiter
!
              GVH%PREA(KSEG)%NKWD = 0
              DO 420 J2=1,1024*1024
                 GVH%PREA(KSEG)%NKWD = GVH%PREA(KSEG)%NKWD + 1
                 IF ( GVH%PREA(KSEG)%NKWD  .GT.  GVH__MKWD ) THEN
                      CALL CLRCH ( STR1 )
                      CALL CLRCH ( STR  )
                      CALL INCH  ( GVH%PREA(KSEG)%NKWD, STR )
                      CALL ERR_LOG ( 4183, IUER, 'GVH_READ_BGV', &
     &                    'The number of keywords in preamble section '// &
     &                    'exceeded the current limit GVH__MKWD: '//STR )
                      RETURN
                 END IF
!
                 GVH%PREA(KSEG)%KWD_ADR(J2) = IAD
                 IP = LIB$SCANC ( %VAL(IAD), TAB, TAB_VAL, %VAL(ILN) )
                 GVH%PREA(KSEG)%KWD_LEN(J2) = IP
                 CALL CLRCH  ( KEYWORD_NAME )
                 CALL MEMCPY ( KEYWORD_NAME, %VAL(GVH%PREA(KSEG)%KWD_ADR(J2)), &
     &                         %VAL(MIN(GVH%PREA(KSEG)%KWD_LEN(J2),LEN(KEYWORD_NAME))) )
                 IAD = IAD + IP
                 ILN = ILN - IP
!
                 GVH%PREA(KSEG)%VAL_ADR(J2) = IAD
                 IP = LIB$SCANC ( %VAL(IAD), TAB, TAB_VAL, %VAL(ILN) )
                 GVH%PREA(KSEG)%VAL_LEN(J2) = IP
                 CALL CLRCH  ( KEYWORD_VALUE )
                 CALL MEMCPY ( KEYWORD_VALUE, %VAL(GVH%PREA(KSEG)%VAL_ADR(J2)), &
     &                         %VAL(MIN(GVH%PREA(KSEG)%VAL_LEN(J2),LEN(KEYWORD_VALUE))) )
                 IF ( KEYWORD_NAME(1:LEN('GENERATOR:')) == 'GENERATOR:' ) THEN
                      GVH%GENERATOR = KEYWORD_VALUE
                 END IF
                 IAD = IAD + IP
                 ILN = ILN - IP
                 IF ( IP .EQ. 0 ) THEN
                      GVH%PREA(KSEG)%NKWD = GVH%PREA(KSEG)%NKWD - 1
                      GOTO 820
                 END IF
!
                 IF ( IAD .GE. GVH%PREA(KSEG)%ADR + GVH%PREA(KSEG)%LEN ) GOTO 820
 420          CONTINUE
 820          CONTINUE
            ELSE
              IER = IUER
              CALL GVH_SKIP_BIN ( GVH_FILDES, GVH_PREF%LEN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4184, IUER, 'GVH_READ_BGV', 'Error during '// &
     &                 'skipping PREA section of the input file '//FILENAME )
                   RETURN
              END IF
              READ_BYTES = READ_BYTES + GVH_PREF%LEN 
         END IF
!
! ------ Extract prefix of TEXT section
!
         IER = IUER
         CALL GVH_READ_BIN ( GVH_FILDES, GVH_PREF, SIZEOF(GVH_PREF), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4185, IUER, 'GVH_READ_BGV', 'Error during '// &
     &            'reading prefix of TEXT section of the input file '// &
     &             FILENAME )
              RETURN
         END IF
!
         IF ( GVH_PREF%NAME .NE. GVH__TEXT ) THEN
              write ( 6, * ) ' GVH_PREF%NAME = ', GVH_PREF%NAME
              CALL ERR_LOG ( 4186, IUER, 'GVH_READ_BGV', 'Trap of '// &
     &            'internal control: prefix  of TEXT section was '// &
     &            'not found in the input file '//FILENAME )
              RETURN
         END IF
         IF ( GVH%ENDIAN_SWAP ) CALL ENDIAN_CNV_I4 ( GVH_PREF%LEN )
         READ_BYTES = READ_BYTES + SIZEOF(GVH_PREF)
!
         IF ( ISEG == 0  .OR.  ISEG == J1 ) THEN
!
! =========== TEXT section
!
              IER = IUER
              GVH%TEXT(KSEG)%LEN = GVH_PREF%LEN 
              IF ( GVH%TEXT(KSEG)%LEN > 0 ) THEN
                   CALL GVH_ALLOCATE ( GVH, GVH%TEXT(KSEG)%LEN, &
     &                                 GVH%TEXT(KSEG)%ADR, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL IINCH ( GVH%TEXT(KSEG)%LEN, STR )
                        CALL ERR_LOG ( 4187, IUER, 'GVH_READ_BGV', &
     &                      'Error in allocating '// &
     &                       STR(1:I_LEN(STR))//' bytes of '// &
     &                      'memory for TEXT section of the image '// &
     &                      'of input file '//FILENAME )
                        RETURN
                   END IF
                 ELSE
                   GVH%TEXT(KSEG)%ADR = 0
              END IF
!
! ----------- Read the portion of file with text section
!
              IER = IUER
              CALL GVH_READ_BIN ( GVH_FILDES, %VAL(GVH%TEXT(KSEG)%ADR), &
     &                                             GVH%TEXT(KSEG)%LEN,  IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4188, IUER, 'GVH_READ_BGV', 'Error during '// &
     &                 'reading TEXT section of the input file '//FILENAME )
                   RETURN
              END IF
              READ_BYTES = READ_BYTES + GVH_PREF%LEN 
!
              ILN = GVH_PREF%LEN
              IAD = GVH%TEXT(KSEG)%ADR
!
! ----------- Set the search table
!
              CALL NOUT ( 256, TAB )
              TAB_VAL = 1
              TAB(26) = TAB_VAL ! chapter delimiter
!
              GVH%TEXT(KSEG)%NTIT = 0
              DO 430 J3=1,1024*1024
                 GVH%TEXT(KSEG)%NTIT = GVH%TEXT(KSEG)%NTIT + 1
                 IF ( GVH%TEXT(KSEG)%NTIT  .GT.  GVH__MTIT ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( GVH__MTIT, STR )
                      CALL ERR_LOG ( 4189, IUER, 'GVH_READ_BGV', &
     &                    'The number of keywords in text section exceeded '// &
     &                    'the current limit GVH__MTIT: '//STR )
                      RETURN
                 END IF
!
                 GVH%TEXT(KSEG)%TITLE_ADR(J3) = IAD
!
! -------------- Search for delimiter
!
                 IP = LIB$SCANC ( %VAL(IAD), TAB, TAB_VAL, %VAL(ILN) )
                 GVH%TEXT(KSEG)%TITLE_LEN(J3) = IP
                 IAD = IAD + IP
                 ILN = ILN - IP
!
                 GVH%TEXT(KSEG)%BODY_ADR(J3) = IAD
                 IP = LIB$SCANC ( %VAL(IAD), TAB, TAB_VAL, %VAL(ILN) )
                 GVH%TEXT(KSEG)%BODY_LEN(J3) = IP
                 IAD = IAD + IP
                 ILN = ILN - IP
                 IF ( IP .EQ. 0 ) THEN
                      GVH%TEXT(KSEG)%NTIT = GVH%TEXT(KSEG)%NTIT - 1
                      GOTO 830
                 END IF
                 IF ( IAD .GE. GVH%TEXT(KSEG)%ADR + GVH%TEXT(KSEG)%LEN ) GOTO 830
 430          CONTINUE
 830          CONTINUE
            ELSE
              IER = IUER
              CALL GVH_SKIP_BIN ( GVH_FILDES, GVH_PREF%LEN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4190, IUER, 'GVH_READ_BGV', 'Error during '// &
     &                 'skipping TEXT section of the input file '//FILENAME )
                   RETURN
              END IF
              READ_BYTES = READ_BYTES + GVH_PREF%LEN 
         END IF
!
! ------ Extract the prefix of TOCS section
!
         IER = IUER
         CALL GVH_READ_BIN ( GVH_FILDES, GVH_PREF, SIZEOF(GVH_PREF), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4191, IUER, 'GVH_READ_BGV', 'Error during '// &
     &            'reading DATA section of the input file '//FILENAME )
              RETURN
         END IF
!
         IF ( GVH_PREF%NAME .NE. GVH__TOCS ) THEN
              CALL ERR_LOG ( 4192, IUER, 'GVH_READ_BGV', 'Trap of '// &
     &            'internal control: prefix  of TOCS section was '// &
     &            'not found in the input file '//FILENAME )
              RETURN
         END IF
         IF ( GVH%ENDIAN_SWAP ) CALL ENDIAN_CNV_I4 ( GVH_PREF%LEN )
         READ_BYTES = READ_BYTES + SIZEOF(GVH_PREF)
!
         IF ( ISEG == 0  .OR.  ISEG == J1 ) THEN
!
! =========== TOCS section
!
              IER = IUER
!
              IF ( FL_B1 ) THEN
                   GVH%TOCS(KSEG)%LEN = (GVH_PREF%LEN/GVH__LCODE1_LEN__20050114 + 1)*&
     &                                   GVH__LCODE1_LEN__64BIT
                 ELSE 
                   GVH%TOCS(KSEG)%LEN = GVH__MTOC*GVH__LCODE1_LEN
              END IF
!
              CALL GVH_ALLOCATE ( GVH, GVH%TOCS(KSEG)%LEN, &
     &                                 GVH%TOCS(KSEG)%ADR, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( GVH%TOCS(KSEG)%LEN, STR )
                   CALL ERR_LOG ( 4193, IUER, 'GVH_READ_BGV', 'Error in '// &
     &                 'allocating '//STR(1:I_LEN(STR))//' bytes of '// &
     &                 'memory for TOCS section of the image '// &
     &                 'of input file '//FILENAME )
                   RETURN
              END IF
!
              IER = IUER
              CALL GVH_READ_BIN ( GVH_FILDES, %VAL(GVH%TOCS(KSEG)%ADR), &
     &                                             GVH_PREF%LEN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4194, IUER, 'GVH_READ_BGV', 'Error during '// &
     &                 'reading TOCS section of the input file '//FILENAME )
                   RETURN
              END IF
              READ_BYTES = READ_BYTES + GVH_PREF%LEN 
              GVH%TOCS(KSEG)%STATUS = GVH__INITIALIZED
#ifdef ADR_32BIT
              IF ( FL_B1 ) THEN
                   GVH%TOCS(KSEG)%NTOC = GVH_PREF%LEN/GVH__LCODE1_LEN__20050114
                   CALL GVH_TOCS_COMPAT ( TOCS_2005__32BIT, &
     &                                    GVH%TOCS(KSEG)%NTOC, &
     &                                    %VAL(GVH%TOCS(KSEG)%ADR) )
                 ELSE 
                   GVH%TOCS(KSEG)%NTOC = GVH_PREF%LEN /GVH__LCODE1_LEN__64BIT
                   CALL GVH_TOCS_COMPAT ( TOCS_64BIT__32BIT, &
     &                                    GVH%TOCS(KSEG)%NTOC, &
     &                                    %VAL(GVH%TOCS(KSEG)%ADR) )
              END IF
#else
              IF ( FL_B1 ) THEN
                   GVH%TOCS(KSEG)%NTOC = GVH_PREF%LEN /GVH__LCODE1_LEN__20050114
                   CALL GVH_TOCS_COMPAT ( TOCS_2005__64BIT, &
     &                                    GVH%TOCS(KSEG)%NTOC, &
     &                                    %VAL(GVH%TOCS(KSEG)%ADR) )
                 ELSE
                   GVH%TOCS(KSEG)%NTOC = GVH_PREF%LEN /GVH__LCODE1_LEN
              END IF
#endif
            ELSE 
              IER = IUER
              CALL GVH_SKIP_BIN ( GVH_FILDES, GVH_PREF%LEN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4195, IUER, 'GVH_READ_BGV', 'Error during '// &
     &                 'skipping TOCS section of the input file '//FILENAME )
                   RETURN
              END IF
              READ_BYTES = READ_BYTES + GVH_PREF%LEN 
         END IF
!
! ------ Extract the prefix of DATA section
!
         IER = IUER
         CALL GVH_READ_BIN ( GVH_FILDES, GVH_PREF, SIZEOF(GVH_PREF), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4196, IUER, 'GVH_READ_BGV', 'Error during '// &
     &            'reading DATA section of the input file '//FILENAME )
              RETURN
         END IF
!
         IF ( GVH_PREF%NAME .NE. GVH__DATA ) THEN
              CALL ERR_LOG ( 4197, IUER, 'GVH_READ_BGV', 'Trap of '// &
     &            'internal control: prefix  of DATA section was '// &
     &            'not found in the input file '//FILENAME )
              RETURN
         END IF
         IF ( GVH%ENDIAN_SWAP ) CALL ENDIAN_CNV_I4 ( GVH_PREF%LEN )
         READ_BYTES = READ_BYTES + SIZEOF(GVH_PREF)
!
         IF ( ISEG == 0  .OR.  ISEG == J1 ) THEN
!
! =========== DATA section
!
              IER = IUER
              GVH%DATA(KSEG)%LEN = GVH_PREF%LEN
              CALL GVH_ALLOCATE ( GVH, GVH%DATA(KSEG)%LEN, &
     &                                 GVH%DATA(KSEG)%ADR, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( GVH%DATA(KSEG)%LEN, STR )
                   CALL ERR_LOG ( 4198, IUER, 'GVH_READ_BGV', 'Error in '// &
     &                 'allocating '//STR(1:I_LEN(STR))//' bytes of '// &
     &                 'memory for DATA section of the image '// &
     &                 'of input file '//FILENAME )
                   RETURN
              END IF
!
              IER = IUER
              CALL GVH_READ_BIN ( GVH_FILDES, %VAL(GVH%DATA(KSEG)%ADR), &
     &                                             GVH%DATA(KSEG)%LEN,  IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4199, IUER, 'GVH_READ_BGV', 'Error during '// &
     &                 'reading DATA section of the input file '//FILENAME )
                   RETURN
              END IF
              READ_BYTES = READ_BYTES + GVH_PREF%LEN 
            ELSE 
              IER = IUER
              CALL GVH_SKIP_BIN ( GVH_FILDES, GVH_PREF%LEN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4200, IUER, 'GVH_READ_BGV', 'Error during '// &
     &                 'skipping DATA section of the input file '//FILENAME )
                   RETURN
              END IF
              READ_BYTES = READ_BYTES + GVH_PREF%LEN 
         END IF
!
! ------ Extract the prefix of HEAP section
!
         IER = IUER
         CALL GVH_READ_BIN ( GVH_FILDES, GVH_PREF, SIZEOF(GVH_PREF), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4201, IUER, 'GVH_READ_BGV', 'Error during '// &
     &            'reading prefix of HEAP section of the input file '// &
     &             FILENAME )
              RETURN
         END IF
!
         IF ( GVH_PREF%NAME .NE. GVH__HEAP ) THEN
              CALL ERR_LOG ( 4202, IUER, 'GVH_READ_BGV', 'Trap of '// &
     &            'internal control: prefix  of HEAP section was '// &
     &            'not found in the input file '//FILENAME )
              RETURN
         END IF
         IF ( GVH%ENDIAN_SWAP ) CALL ENDIAN_CNV_I4 ( GVH_PREF%LEN )
         READ_BYTES = READ_BYTES + SIZEOF(GVH_PREF)
!
         IF ( ISEG == 0  .OR.  ISEG == J1 ) THEN
!
! =========== HEAP section
!
              IER = IUER
              GVH%HEAP(KSEG)%LEN = GVH_PREF%LEN
              IF ( GVH%HEAP(KSEG)%LEN > 0 ) THEN
                   CALL GVH_ALLOCATE ( GVH, GVH%HEAP(KSEG)%LEN, &
     &                                 GVH%HEAP(KSEG)%ADR, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL IINCH ( GVH%HEAP(KSEG)%LEN, STR )
                        CALL ERR_LOG ( 4203, IUER, 'GVH_READ_BGV', 'Error '// &
          &                 'in allocating '//STR(1:I_LEN(STR))//' bytes '// &
     &                      'of memory for HEAP section of the image '// &
          &                 'of input file '//FILENAME )
                        RETURN
                   END IF
                 ELSE
                   GVH%HEAP(KSEG)%ADR = 0
              END IF
!
              IER = IUER
              CALL GVH_READ_BIN ( GVH_FILDES, %VAL(GVH%HEAP(KSEG)%ADR), &
     &                                             GVH%HEAP(KSEG)%LEN,  IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4204, IUER, 'GVH_READ_BGV', 'Error during '// &
     &                 'reading HEAP section of the input file '//FILENAME )
                   RETURN
              END IF
              READ_BYTES = READ_BYTES + GVH_PREF%LEN 
              IF ( ISEG == J1 ) THEN
                   GOTO 810
                ELSE IF ( ISEG == 0  .AND. LEN_FIL - READ_BYTES .LE. 0 ) THEN
                   GOTO 810
              END IF
            ELSE IF ( ( ISEG == J1 .OR. ISEG == 0)  .AND.  GVH_PREF%LEN > 0 ) THEN
              IF ( ISEG == 1 ) THEN
                   GOTO 810
                ELSE IF ( ISEG == 0  .AND. LEN_FIL - READ_BYTES .LE. 0 ) THEN
                   GOTO 810
              END IF
            ELSE 
              IER = IUER
              CALL GVH_SKIP_BIN ( GVH_FILDES, GVH_PREF%LEN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4205, IUER, 'GVH_READ_BGV', 'Error during '// &
     &                 'skipping HEAP section of the input file '//FILENAME )
                   RETURN
              END IF
              READ_BYTES = READ_BYTES + GVH_PREF%LEN 
         END IF
!
         IF ( ISEG == 0 ) THEN
              KSEG = KSEG + 1
              GVH%NSEG = GVH%NSEG + 1
         END IF
 410  CONTINUE 
 810  CONTINUE 
!
! --- Close the file
!
      IER = IUER
      CALL BINF_CLOSE ( GVH_FILDES, IER )
!
      REMAINED_BYTES = LEN_FIL - READ_BYTES
!
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END  SUBROUTINE  GVH_READ_BGV  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GVH_TOCS_COMPAT ( GVH_OP, NTOC, TOCS_ARR )
! ************************************************************************
! *                                                                      *
! *   Routine GVH_TOCS_COMPAT 
! *                                                                      *
! * ### 22-JAN-2011  GVH_TOCS_COMPAT  v1.0 (c) L. Petrov 22-JAN-2011 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      INTEGER*4  GVH_OP, NTOC
      INTEGER*4  ML
      PARAMETER  ( ML = GVH__MTOC*GVH__LCODE1_LEN*2 )
      INTEGER*1  TOCS_ARR(ML)
      TYPE ( GVH_LCODE1__STRU    ) :: PTOC
      TYPE ( GVH_PTOC_2005__STRU ) :: PTOC_2005
      TYPE ( GVH_PTOC_32__STRU   ) :: PTOC_32BIT
      TYPE ( GVH_PTOC_64__STRU   ) :: PTOC_64BIT
      INTEGER*4  J1, J2, J3, J4, OFFS_IN, OFFS_OUT
!
      IF ( GVH_OP == TOCS_2005__32BIT ) THEN
           OFFS_IN  = (NTOC-1)*GVH__LCODE1_LEN__20050114 + 1
           OFFS_OUT = (NTOC-1)*GVH__LCODE1_LEN__32BIT + 1
           DO 410 J1=NTOC,1,-1
              CALL MEMCPY ( PTOC_2005, TOCS_ARR(OFFS_IN), &
     &                      %VAL(GVH__LCODE1_LEN__20050114) )
              PTOC_32BIT%LCODE    = PTOC_2005%LCODE
              PTOC_32BIT%DESCR    = PTOC_2005%DESCR
              PTOC_32BIT%CLASS    = PTOC_2005%CLASS
              PTOC_32BIT%TYP      = PTOC_2005%TYP
              PTOC_32BIT%DIMS     = PTOC_2005%DIMS 
              PTOC_32BIT%ADR_DATA = PTOC_2005%ADR_DATA
              PTOC_32BIT%LEN_DATA = PTOC_2005%LEN_DATA
              PTOC_32BIT%FILLER   = 0
              CALL MEMCPY ( TOCS_ARR(OFFS_OUT), PTOC_32BIT, &
     &                      %VAL(GVH__LCODE1_LEN__32BIT) )
              OFFS_IN  = OFFS_IN  - GVH__LCODE1_LEN__20050114
              OFFS_OUT = OFFS_OUT - GVH__LCODE1_LEN__32BIT
 410       CONTINUE 
        ELSE IF ( GVH_OP == TOCS_2005__64BIT ) THEN
           OFFS_IN  = (NTOC-1)*GVH__LCODE1_LEN__20050114 + 1
           OFFS_OUT = (NTOC-1)*GVH__LCODE1_LEN__64BIT + 1
           DO 420 J2=NTOC,1,-1
              CALL MEMCPY ( PTOC_2005, TOCS_ARR(OFFS_IN), &
     &                      %VAL(GVH__LCODE1_LEN__20050114) )
              PTOC_64BIT%LCODE    = PTOC_2005%LCODE
              PTOC_64BIT%DESCR    = PTOC_2005%DESCR
              PTOC_64BIT%CLASS    = PTOC_2005%CLASS
              PTOC_64BIT%TYP      = PTOC_2005%TYP
              PTOC_64BIT%DIMS     = PTOC_2005%DIMS
              PTOC_64BIT%ADR_DATA = PTOC_2005%ADR_DATA
              PTOC_64BIT%LEN_DATA = PTOC_2005%LEN_DATA
              PTOC_64BIT%FILLER   = 0
              CALL MEMCPY ( TOCS_ARR(OFFS_OUT), PTOC_64BIT, &
     &                      %VAL(GVH__LCODE1_LEN__64BIT) )
              OFFS_IN  = OFFS_IN  - GVH__LCODE1_LEN__20050114
              OFFS_OUT = OFFS_OUT - GVH__LCODE1_LEN__64BIT
 420       CONTINUE 
        ELSE IF ( GVH_OP == TOCS_64BIT__32BIT  ) THEN
           OFFS_IN  = 1
           OFFS_OUT = 1
           DO 430 J3=1,NTOC
              CALL MEMCPY ( PTOC_64BIT, TOCS_ARR(OFFS_IN), %VAL(GVH__LCODE1_LEN__64BIT ) )
              PTOC_32BIT%LCODE    = PTOC_64BIT%LCODE
              PTOC_32BIT%DESCR    = PTOC_64BIT%DESCR
              PTOC_32BIT%CLASS    = PTOC_64BIT%CLASS
              PTOC_32BIT%TYP      = PTOC_64BIT%TYP
              PTOC_32BIT%DIMS     = PTOC_64BIT%DIMS
              PTOC_32BIT%ADR_DATA = PTOC_64BIT%ADR_DATA
              PTOC_32BIT%LEN_DATA = PTOC_64BIT%LEN_DATA
              PTOC_32BIT%FILLER   = 0
              CALL MEMCPY ( TOCS_ARR(OFFS_OUT), PTOC_32BIT, &
     &                      %VAL(GVH__LCODE1_LEN__32BIT) )
              OFFS_IN  = OFFS_IN  + GVH__LCODE1_LEN__64BIT
              OFFS_OUT = OFFS_OUT + GVH__LCODE1_LEN__32BIT
 430       CONTINUE 
        ELSE IF ( GVH_OP == TOCS_32BIT__64BIT  ) THEN
           OFFS_IN  = (NTOC-1)*GVH__LCODE1_LEN__32BIT + 1
           OFFS_OUT = (NTOC-1)*GVH__LCODE1_LEN__64BIT + 1
           DO 440 J3=NTOC,1,-1
              CALL MEMCPY ( PTOC_32BIT, TOCS_ARR(OFFS_IN), %VAL(GVH__LCODE1_LEN__32BIT ) )
              PTOC_64BIT%LCODE    = PTOC_32BIT%LCODE
              PTOC_64BIT%DESCR    = PTOC_32BIT%DESCR
              PTOC_64BIT%CLASS    = PTOC_32BIT%CLASS
              PTOC_64BIT%TYP      = PTOC_32BIT%TYP
              PTOC_64BIT%DIMS     = PTOC_32BIT%DIMS
              PTOC_64BIT%ADR_DATA = PTOC_32BIT%ADR_DATA
              PTOC_64BIT%LEN_DATA = PTOC_32BIT%LEN_DATA
              PTOC_64BIT%FILLER   = 0
              CALL MEMCPY ( TOCS_ARR(OFFS_OUT), PTOC_64BIT, &
     &                      %VAL(GVH__LCODE1_LEN__64BIT) )
              OFFS_IN  = OFFS_IN  - GVH__LCODE1_LEN__32BIT
              OFFS_OUT = OFFS_OUT - GVH__LCODE1_LEN__64BIT
 440       CONTINUE 
      END IF
      RETURN
      END  SUBROUTINE  GVH_TOCS_COMPAT  !#!#
