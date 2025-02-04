      SUBROUTINE GVH_READ_AGV ( GVH, ISEG, FILENAME, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GVH_READ_AGV  reads the contents of one or more           *
! *   segments of the input file in ASCII GVH format. If ISEG=0,         *
! *   then it reads all segments from the input file. If ISEG is not     *
! *   equal to zero, then the ISEG-th segment is read. NB: argument ISEG *
! *   corresponds to the segment counter counted from the beginning of   *
! *   the file starting from 1. This may not correspond to the segments  *
! *   counter of GVH! GVH_READ_AGV increments segment counter before     *
! *   reading each new segment, except the case when no segments have    *
! *   been loaded in GVH, i.e. it was  just initialized. In that case    *
! *   GVH_READ_AGV will put the first segment from the input file        *
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
! * ###  07-NOV-2005  GVH_READ_AGV   v1.3 (c) L. Petrov  06-JAN-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      TYPE      ( GVH__STRU      ) :: GVH
      TYPE      ( GVH_PREF__STRU ) :: GVH_PREF
      CHARACTER  FILENAME*(*)
      INTEGER*4  ISEG, IUER
      CHARACTER  STR*256, STR1*32, STR2*32, PREF*4, LCODE*8, &
     &           C_MND(GVH__NMLCODE)*8, C_STA(GVH__MSTA)*8, &
     &           C_LCD(GVH__MTOC)*8, CH_VAL*(GVH__L_TEXT)
      INTEGER*4  MIND
      PARAMETER  ( MIND = 64 )
      LOGICAL*4  LEX 
      INTEGER*4  LUN, J1, J2, J3, J4, J5, J6, LEN_FIL, LIND, IND(2,MIND), &
     &           K_TEXT, K_TOCS, K_PREA, L_PREA, LTIT, L_TITS, L_TOCS, &
     &           L_DATA, LEN_MEM, ILN, IAD, IS, IL, IP, IM, KSEG, &
     &           KTIT, DIMS(2), L_TIT, IND_TYP, IND_CLS, NTOCS, &
     &           K_MND, NUM_OBS, NUM_SCA, NUM_STA, NOBS_STA(GVH__MSTA), &
     &           TYP_LCD(GVH__MTOC), DIM_LCD(2,GVH__MTOC), CLS_LCD(GVH__MTOC), &
     &           IND_VAL, IND_OBS, IND_STA, IND_STA_BAS, I4_VAL, L_LCD, &
     &           OFFSET, KSTA, IND_SCA, MAX_DIMS(2), ADIMS(2), IER
      INTEGER*1  TAB(0:255), TAB_VAL
      INTEGER*2  I2_VAL
      REAL*4     R4_VAL
      REAL*8     R8_VAL
      CHARACTER, ALLOCATABLE :: BUF_PREA(:)*(GVH__L_TEXT), &
     &                          BUF_TEXT(:)*(GVH__L_TEXT), &
     &                          BUF_TITS(:)*(GVH__L_TEXT), &
     &                          BUF_TOCS(:)*(GVH__L_TEXT)
      INTEGER*1, ALLOCATABLE :: ARR_I1(:)
      INTEGER*2, ALLOCATABLE :: ARR_I2(:)
      INTEGER*4, ALLOCATABLE :: ARR_I4(:), OBS_TAB(:,:)
      REAL*4,    ALLOCATABLE :: ARR_R4(:)
      REAL*8,    ALLOCATABLE :: ARR_R8(:)
      LOGICAL*4  FL_MND
      INTEGER*4, EXTERNAL :: GET_UNIT, ILEN, I_LEN, LIB$SCANC, LTM_DIF
!
      IF ( GVH%STATUS .NE. GVH__INITIALIZED ) THEN
           CALL ERR_LOG ( 4411, IUER, 'GVH_READ_AGV', 'The GVH data '// &
     &         'structure was not initialized. Please, use gvh_init first' )
           RETURN
      END IF
!
      IF ( ISEG .LT.  0 ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( ISEG, STR )
           CALL ERR_LOG ( 4412, IUER, 'GVH_READ_AGV', 'Wrong parameter '// &
     &         'ISEG '//STR )
           RETURN
      END IF
!
      IF ( ISEG .GT. GVH__MSEG ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( ISEG, STR )
           CALL CLRCH ( STR1 ) 
           CALL INCH  ( GVH__MSEG, STR1 )
           CALL ERR_LOG ( 4413, IUER, 'GVH_READ_AGV', 'Wrong parameter '// &
     &         'ISEG: '//STR(1:I_LEN(STR))//' -- it exceeds the '// &
     &         'maximal number of segments GVH__MSEG: '//STR1 )
           RETURN
      END IF
!
      IF ( ISEG .GT. GVH%NSEG+1 ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( ISEG, STR )
           CALL CLRCH ( STR1 ) 
           CALL INCH  ( GVH%NSEG, STR1 )
           CALL ERR_LOG ( 4414, IUER, 'GVH_READ_AGV', 'Wrong parameter '// &
     &         'ISEG: '//STR(1:I_LEN(STR))//' -- it exceeds the total '// &
     &         'numer of segments GVH%NSEG by more than 1: '//STR1 )
           RETURN
      END IF
!
      IF ( GVH%NSEG == 1 ) THEN
!
! -------- Do not advance current section counter in the case if the current
! -------- section is 1 and not file has been loaded
!
           IF ( ILEN(GVH%FILENAME(1)) > 0 ) GVH%NSEG = GVH%NSEG + 1
         ELSE
           GVH%NSEG = GVH%NSEG + 1
      END IF
      KSEG = GVH%NSEG 
!
      INQUIRE ( FILE=FILENAME, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 4415, IUER, 'GVH_READ_AGV', 'File '// &
     &                    FILENAME(1:I_LEN(FILENAME))//' not found' )
           RETURN
      END IF
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=FILENAME, STATUS='OLD', IOSTAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4416, IUER, 'GVH_READ_AGV', 'Error '// &
     &         ' in an attempt to open the input file '//FILENAME )
           RETURN
      END IF
!
! --- Read the label
!
      READ ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) STR
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4417, IUER, 'GVH_READ_AGV', 'Error during '// &
     &         'reading the first record of the input file '//FILENAME )
           RETURN
      END IF
!
! --- Parse the label. In particularly the label should tell, whether the 
! --- database has been written in big endian or little endian mode.
!
      IF ( STR(1:LEN(GVH__AGV_LABEL)) == GVH__AGV_V1_LABEL ) THEN
           STR = GVH__AGV_LABEL 
      END IF
      IF ( STR(1:LEN(GVH__AGV_LABEL)) == GVH__AGV_LABEL ) THEN
           CONTINUE 
        ELSE 
           CALL ERR_LOG ( 4418, IUER, 'GVH_READ_AGV', 'File '// &
     &          FILENAME(1:I_LEN(FILENAME))//' is not in Geo VLBI format '// &
     &         'Label: '//GVH__AGV_LABEL  )
           RETURN
      END IF
      GVH%DB_VERS = 0
!
      ALLOCATE ( BUF_PREA(GVH__A_MPREA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( GVH__A_MPREA*GVH__L_TEXT, STR )
           CALL ERR_LOG ( 4419, IUER, 'GVH_READ_AGV', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for preamble buffer' )
           RETURN
      END IF
!
      ALLOCATE ( BUF_TEXT(GVH__A_MTEXT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( GVH__A_MTEXT*GVH__L_TEXT, STR )
           CALL ERR_LOG ( 4420, IUER, 'GVH_READ_AGV', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for text buffer' )
           RETURN
      END IF
!
      ALLOCATE ( BUF_TITS(GVH__MTIT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( GVH__A_MTEXT*GVH__L_TEXT, STR )
           CALL ERR_LOG ( 4421, IUER, 'GVH_READ_AGV', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for text title buffer' )
           RETURN
      END IF
!
      ALLOCATE ( BUF_TOCS(GVH__MTOC), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( GVH__MTOC*GVH__L_TEXT, STR )
           CALL ERR_LOG ( 4422, IUER, 'GVH_READ_AGV', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for the table of contents buffer' )
           RETURN
      END IF
!
      ALLOCATE ( ARR_I1(GVH__MDIM), STAT=IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( GVH__MDIM, STR )
           CALL ERR_LOG ( 4423, IUER, 'GVH_READ_AGV', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for the integer*1 buffer' )
           RETURN
      END IF
!
      ALLOCATE ( ARR_I2(GVH__MDIM), STAT=IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 2*GVH__MDIM, STR )
           CALL ERR_LOG ( 4424, IUER, 'GVH_READ_AGV', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for the integer*2 buffer' )
           RETURN
      END IF
!
      ALLOCATE ( ARR_I4(GVH__MDIM), STAT=IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*GVH__MDIM, STR )
           CALL ERR_LOG ( 4425, IUER, 'GVH_READ_AGV', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for the integer*4 buffer' )
           RETURN
      END IF
!
      ALLOCATE ( OBS_TAB(3,GVH__MOBS), STAT=IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*3*GVH__MOBS, STR )
           CALL ERR_LOG ( 4426, IUER, 'GVH_READ_AGV', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for OBS_TAB' )
           RETURN
      END IF
!
      ALLOCATE ( ARR_R4(GVH__MDIM), STAT=IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*GVH__MDIM, STR )
           CALL ERR_LOG ( 4427, IUER, 'GVH_READ_AGV', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for the real*4 buffer' )
           RETURN
      END IF
!
      ALLOCATE ( ARR_R8(GVH__MDIM), STAT=IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*GVH__MDIM, STR )
           CALL ERR_LOG ( 4428, IUER, 'GVH_READ_AGV', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for the real*8 buffer' )
           RETURN
      END IF
!
      K_TEXT = 0
      K_TOCS = 0
      L_PREA = 0
      LTIT   = 0
      L_TITS = 0
      L_TOCS = 0
      L_DATA = 0
      IF ( GVH%CACHE%LCODE_STATUS .EQ. GVH__POPULATED ) THEN
           FL_MND = .FALSE. ! Do not insert mandatory lcodes
         ELSE 
           FL_MND = .TRUE.  ! First insert mandatory lcodes
      END IF
      L_LCD = 0
      K_MND = 0
!
      CALL NOUT ( 256, TAB )
      TAB_VAL = 1
      TAB(10) = TAB_VAL ! Carriage return -- record delimiter
      TAB(26) = TAB_VAL ! keyword delimiter
!
      DO 410 J1=2,1024*1024*1024
!Q         READ ( UNIT=LUN, FMT='(Q,A)', IOSTAT=IER ) IL, STR
         READ ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) STR
         IL = ILEN(STR)
         IF ( IER == -1 ) THEN
              GOTO   810
            ELSE IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL CLRCH ( STR1 )
              CALL INCH  ( IER, STR1 )
              CALL ERR_LOG ( 4429, IUER, 'GVH_READ_AGV', 'Error '// &
     &             STR1(1:I_LEN(STR1))//' in an attempt to read line '// &
     &             STR(1:I_LEN(STR))//' of the input file '//FILENAME )
              RETURN
         END IF
         IF ( STR(1:LEN(GVH__AGV_LABEL)) == GVH__AGV_V1_LABEL ) THEN
              STR = GVH__AGV_LABEL 
         END IF
         IF ( STR(1:LEN(GVH__AGV_LABEL)) == GVH__AGV_LABEL ) THEN
              GVH%NSEG = GVH%NSEG + 1
              GOTO 410
         END IF
         IP = LIB$SCANC ( STR(1:IL), TAB, TAB_VAL )
         IF ( IP > 0 ) IL = IP
!
         IER = 0
         CALL EXWORD ( STR(1:IL), MIND, LIND, IND, CHAR(32)//CHAR(0), IER )
         IF ( LIND < 2  .AND. STR(1:4) .NE. 'TEXT' ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL CLRCH ( STR1 )
              CALL INCH  ( IER, STR1 )
              CALL ERR_LOG ( 4430, IUER, 'GVH_READ_AGV', 'Error '// &
     &            'in an attempt to process line "'//STR(1:I_LEN(STR))// &
     &            '" of the input file '//FILENAME(1:I_LEN(FILENAME))// &
     &            ' -- too few words' )
              RETURN
         END IF
!
         PREF = STR(1:4)
         CALL CHIN  ( STR(6:IND(2,1)), KSEG )
         IF ( KSEG < 1  .OR.  KSEG > GVH__MSEG ) THEN
              CALL CLRCH ( STR1 )
              CALL INCH  ( J1, STR1 )
              CALL CLRCH ( STR2 )
              CALL INCH  ( KSEG, STR2 )
              CALL ERR_LOG ( 4431, IUER, 'GVH_READ_AGV', 'Wrong segment '// &
     &             'counter in parsing line '//STR1(1:I_LEN(STR1))// &
     &             ' of input file '//FILENAME(1:I_LEN(FILENAME))// &
     &             ' -- "'//STR(1:I_LEN(STR))// &
     &             '" -- either wrong format or the counter exceeds GVH__MSEG' )
              RETURN
         END IF
!
         IF ( PREF == 'FILE'  .AND. ( ISEG == 0  .OR.  ISEG == KSEG ) .AND. &
     &        INDEX ( STR, '@section_length:' ) < 1 ) THEN
              GVH%OLD_FILENAME(KSEG) = STR(8:)
         END IF
         IF ( PREF == 'PREA'  .AND. ( ISEG == 0  .OR.  ISEG == KSEG ) ) THEN
              IF ( LIND .GE. 3 ) THEN
                   IF ( STR(IND(1,2):IND(2,2)) == 'GENERATOR:'    ) THEN
                        GVH%GENERATOR = STR(IND(1,3):IND(2,3)) 
                   END IF
                   IF ( STR(IND(1,2):IND(2,2)) == 'GVH_VERSION:'  ) THEN
                        GVH%VERSION = STR(IND(1,3):IND(2,3)) 
                   END IF
              END IF
              CALL CLRCH ( GVH%FILENAME(KSEG) )
              GVH%FILENAME(KSEG) = FILENAME
!
              IF ( INDEX ( STR(IND(1,2):IND(2,2)), '@section_length' ) > 0 ) THEN
                   CALL CHIN ( STR(IND(1,3):IND(2,3)), L_PREA ) 
                   IF ( L_PREA .LT. 0  .OR.  L_PREA > GVH__MTOC ) THEN
                        CALL CLRCH ( STR1 ) 
                        CALL INCH  ( J1, STR1 )
                        CALL ERR_LOG ( 4432, IUER, 'GVH_READ_AGV', 'Wrong '// &
     &                      'keywords counter in parsing the '// &
     &                       STR1(1:I_LEN(STR1))//'-th line '// &
     &                      'of input file '//FILENAME(1:I_LEN(FILENAME))// &
     &                      ' -- "'//STR )
                        RETURN
                   END IF
                   K_PREA = 0
                 ELSE 
                   IF ( L_PREA == 0 ) THEN
                        CALL CLRCH ( STR1 ) 
                        CALL INCH  ( J1, STR1 )
                        CALL ERR_LOG ( 4433, IUER, 'GVH_READ_AGV', 'Trap '// &
     &                      'of internal control: the line with the number '// &
     &                      'of keywords of preamble was not found before '// &
     &                      'the first line with preamble '//STR )
                        RETURN
                   END IF
                   K_PREA = K_PREA + 1
                   IF ( LIND == 1 ) THEN
                        BUF_PREA(K_PREA) = ' '
                     ELSE
!
! --------------------- Search for CHAR(10). If not found, then it is the line
! --------------------- delimeter. Otherwise, entire line is taken
!
                        BUF_PREA(K_PREA) = STR(IND(1,2):IND(2,LIND))
                   END IF
              END IF
              IF ( L_PREA > 0  .AND. K_PREA == L_PREA ) THEN
                  DO 420 J2=1,L_PREA
                     IP = INDEX ( BUF_PREA(J2), ':' ) 
                     IF ( IP .LT. 1 ) THEN
                          CALL ERR_LOG ( 4434, IUER, 'GVH_READ_AGV', &
     &                        'No colon was found in preamlbe line '// &
     &                         BUF_TEXT(J2) )
                          RETURN 
                     END IF
!
! ------------------ Put the keyword and the value in the preamble
!
                     IER = -1
                     CALL GVH_PPREA ( GVH, GVH%NSEG, BUF_PREA(J2)(1:IP), &
     &                                BUF_PREA(J2)(IP+2:I_LEN(BUF_PREA(J2))), &
     &                                IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL ERR_LOG ( 4435, IUER, 'GVH_READ_AGV', &
     &                        'Error in attempt to insert pramble line '// &
     &                         BUF_PREA(J2) )
                          RETURN 
                     END IF
 420              CONTINUE
              END IF
         END IF
!
         IF ( PREF == 'TEXT'  .AND. ( ISEG == 0  .OR.  ISEG == KSEG ) ) THEN
              IF ( INDEX ( STR, '@section_length' ) > 0 ) THEN
                   IF ( LIND .LT. 3 ) THEN
                        CALL ERR_LOG ( 4436, IUER, 'GVH_READ_AGV', &
     &                      'Trap of internal control: the first line of '// &
     &                      'TEXT section has less than 3 words: '// &
     &                       STR )
                       RETURN 
                   END IF
                   CALL CHIN ( STR(IND(1,3):IND(2,3)), LTIT )
                   IF ( LTIT .LT. 0  .OR.  LTIT > GVH__MTIT ) THEN
                        CALL ERR_LOG ( 4437, IUER, 'GVH_READ_AGV', &
     &                      'The number of chapters in the text section '// &
     &                      'either wrongly formated or exceeded the '// &
     &                      'current limit GVH__MTXT: '//STR )
                       RETURN 
                  END IF
                  KTIT = 0
                  K_TEXT = 0
                ELSE IF ( INDEX ( STR, '@@chapter' ) > 0 ) THEN
                  IF ( LIND .LT. 3 ) THEN
                       CALL ERR_LOG ( 4438, IUER, 'GVH_READ_AGV', 'Trap '// &
     &                     'of internal control: the chapter title of '// &
     &                     'the TEXT section has less than 3 words: '//STR )
                       RETURN 
                  END IF
!
                  IF ( KTIT > 0 ) THEN
!
! -------------------- Put the old chapter to GVH
!
                       IER = IUER
                       CALL GVH_PTEXT_CHP ( GVH, GVH%NSEG, &
     &                      BUF_TITS(KTIT)(1:I_LEN(BUF_TITS(KTIT))), K_TEXT, &
     &                      BUF_TEXT, IER )
                       IF ( IER .NE. 0 ) THEN
                            CALL ERR_LOG ( 4439, IUER, 'GVH_READ_AGV', &
     &                          'Error in an attempt to put text chapeter '// &
     &                          'in GVH internal data structure' )
                            RETURN 
                       END IF
                  END IF
!
                  CALL CHIN ( STR(IND(1,3):IND(2,3)), KTIT )
                  IF ( KTIT .LE. 0  .OR.  KTIT > LTIT ) THEN
                       CALL CLRCH ( STR1 )
                       CALL INCH ( KTIT, STR1 )
                       CALL CLRCH ( STR2 )
                       CALL IINCH ( LTIT, STR2 )
                       CALL ERR_LOG ( 4440, IUER, 'GVH_READ_AGV', 'Trap '// &
     &                     'of internal control: chapter counter '// &
     &                     STR1(1:I_LEN(STR1))//' is '// &
     &                     ' out of range [1, '//STR2(1:I_LEN(STR2))// &
     &                     '] when the line "'//STR(1:I_LEN(STR))// &
     &                     '" was being parsed' )
                       RETURN 
                  END IF
                  CALL CLRCH ( BUF_TITS(KTIT) )
                  BUF_TITS(KTIT) = STR(IND(1,MIN(8,LIND)):IND(2,LIND))
                  K_TEXT = 0
                ELSE
                  K_TEXT = K_TEXT + 1
                  IF ( K_TEXT > GVH__A_MTEXT ) THEN
                       CALL CLRCH ( STR  )
                       CALL CLRCH ( STR1 )
                       CALL INCH  ( J1, STR  )
                       CALL INCH  ( GVH__A_MTEXT, STR1  )
                       CALL ERR_LOG ( 4441, IUER, 'GVH_READ_AGV', 'Error in '// &
     &                     'parsing of the '//TRIM(STR)//'-th line of VDA '// &
     &                     'database file '//TRIM(FILENAME)//' -- the number of '// &
     &                     'TEXT records exceeded the limit '//STR1 )
                       RETURN
                  END IF
                  IF ( LIND == 1 ) THEN
                       BUF_TEXT(K_TEXT) = ' '
                     ELSE
                       BUF_TEXT(K_TEXT) = STR(IND(2,1)+2:IL)
                  END IF
              END IF
         END IF
         IF ( PREF == 'TOCS'  .AND. ( ISEG == 0  .OR.  ISEG == KSEG ) ) THEN
              IF ( KTIT > 0 ) THEN
!
! ---------------- Put the old chapter of text to GVH
!
                   IER = IUER
                   CALL GVH_PTEXT_CHP ( GVH, GVH%NSEG, &
     &                      BUF_TITS(KTIT)(1:I_LEN(BUF_TITS(KTIT))), K_TEXT, &
     &                      BUF_TEXT, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 4442, IUER, 'GVH_READ_AGV', &
     &                      'Error in an attempt to put text chapeter '// &
     &                      'in GVH internal data structure' )
                        RETURN 
                   END IF
                   KTIT = 0
              END IF
!
              IF ( INDEX ( STR(IND(1,2):IND(2,2)), '@section_length' ) > 0 ) THEN
                   CALL CHIN ( STR(IND(1,3):IND(2,3)), NTOCS )
                   IF ( NTOCS .LT. 1  .OR.  NTOCS > GVH__MTOC ) THEN
                        CALL ERR_LOG ( 4443, IUER, 'GVH_READ_AGV', 'Wrong '// &
     &                      'number of entries in the table of contents '// &
     &                      'when line '//STR(1:I_LEN(STR))//' was being '// &
     &                      'parsed' )
                        RETURN 
                   END IF
                 ELSE
                   IF ( LIND < 7 ) THEN
                        CALL ERR_LOG ( 4444, IUER, 'GVH_READ_AGV', 'Trap '// &
     &                      'of internal control: the table of contents '// &
     &                      'line has less than 7 words: '//STR )
                        RETURN 
                   END IF 
!
                   LCODE = STR(IND(1,2):IND(2,2))
                   IND_CLS = LTM_DIF ( 1, GVH__MCLASS, GVH__CLASS_CHR, &
     &                                    STR(IND(1,3):IND(2,3)) )
                   IF ( IND_CLS .LE. 0  .OR.  IND_CLS > GVH__MCLASS ) THEN
                        CALL ERR_LOG ( 4445, IUER, 'GVH_READ_AGV', 'Trap '// &
     &                      'of internal control: insupported class was '// &
     &                      'found when the table of contents line "'// &
     &                       STR(1:I_LEN(STR))//'" was parsed' )
                        RETURN 
                   END IF 
                   IND_TYP   = LTM_DIF ( 1, GVH__MTYPE, GVH__TYPE_CHR, &
     &                                   STR(IND(1,4):IND(2,4)) )
                   IF ( IND_TYP .LE. 0  .OR.  IND_TYP > GVH__MTYPE ) THEN
                        CALL ERR_LOG ( 4446, IUER, 'GVH_READ_AGV', 'Trap '// &
     &                      'of internal control: insupported type was '// &
     &                      'found when the table of contents line "'// &
     &                       STR(1:I_LEN(STR))//'" was parsed' )
                        RETURN 
                   END IF 
                   CALL CHIN ( STR(IND(1,5):IND(2,5)), DIMS(1) )
                   CALL CHIN ( STR(IND(1,6):IND(2,6)), DIMS(2) )
                   IF ( IABS(DIMS(1)) == 0  .OR. IABS(DIMS(1)) > GVH__MDIM ) THEN
                        CALL ERR_LOG ( 4447, IUER, 'GVH_READ_AGV', 'Trap '// &
     &                      'of internal control: wrong first dimension was '// &
     &                      'found when the table of contents line "'// &
     &                       STR(1:I_LEN(STR))//'" was parsed' )
                        RETURN 
                   END IF 
                   IF ( IABS(DIMS(2)) == 0  .OR. IABS(DIMS(2)) > GVH__MDIM ) THEN
                        CALL ERR_LOG ( 4448, IUER, 'GVH_READ_AGV', 'Trap '// &
     &                      'of internal control: wrong second dimension '// &
     &                      'was found when the table of contents line "'// &
     &                       STR(1:I_LEN(STR))//'" was parsed' )
                        RETURN 
                   END IF 
!
                   L_LCD = L_LCD + 1
                   C_LCD(L_LCD) = LCODE
                   TYP_LCD(L_LCD)   = IND_TYP
                   CLS_LCD(L_LCD)   = IND_CLS
                   DIM_LCD(1,L_LCD) = DIMS(1)
                   DIM_LCD(2,L_LCD) = DIMS(2)
!
                   IP = LTM_DIF ( 1, GVH__NMLCODE, GVH__MLCODE, LCODE )
                   IF ( IP .LE. 0 ) THEN
                        IER = IUER
                        CALL GVH_PTOC ( GVH, LCODE, GVH__TYPE_INT(IND_TYP), &
     &                                  GVH__CLASS_INT(IND_CLS), DIMS(1), &
     &                                  DIMS(2), STR(IND(1,7):IL), KSEG, IER )
                        IF ( IER .NE. 0 ) THEN
                             CALL ERR_LOG ( 4449, IUER, 'GVH_READ_AGV', &
     &                           'Error in an attempt to insert the lcode '// &
     &                            LCODE//' when the table of contents line "'// &
     &                            STR(1:I_LEN(STR))//'" was parsed' )
                             RETURN 
                        END IF
                   END IF
              END IF
         END IF
!
         IF ( PREF == 'DATA' .AND.  ( ISEG == 0  .OR. ISEG == KSEG ) ) THEN
              IF ( INDEX ( STR(IND(1,2):IND(2,2)), '@section_length' ) > 0 ) THEN
                   IF ( .NOT. FL_MND ) THEN
                        IER = IUER
                        CALL GVH_GLCODE ( GVH, 'NUMB_OBS', 0, 0, 4, &
     &                                    ADIMS(1), ADIMS(2), NUM_OBS, IER )
                        IER = IUER
                        CALL GVH_GLCODE ( GVH, 'NUMB_SCA', 0, 0, 4, &
     &                                    ADIMS(1), ADIMS(2), NUM_SCA, IER )
                        IER = IUER
                        CALL GVH_GLCODE ( GVH, 'NUMB_STA', 0, 0, 4, &
     &                                    ADIMS(1), ADIMS(2), NUM_STA, IER )
                        IER = IUER
                        CALL GVH_GLCODE ( GVH, 'NOBS_STA', 0, 0, NUM_STA*4, &
     &                                    ADIMS(1), ADIMS(2), NOBS_STA, IER )
                        IER = IUER
                        CALL GVH_GLCODE ( GVH, 'SITNAMES', 0, 0, &
     &                                    SIZEOF(C_STA(1))*GVH__MSTA, &
     &                                    ADIMS(1), ADIMS(2), %REF(C_STA), IER )
                        IER = IUER
                        CALL GVH_GLCODE ( GVH, 'OBS_TAB ', 0, 0, &
     &                                    SIZEOF(OBS_TAB), &
     &                                    ADIMS(1), ADIMS(2), OBS_TAB, IER )
                        CALL GVH_PREPUT ( GVH, NUM_OBS, NUM_SCA, NUM_STA, &
     &                                    NOBS_STA, C_STA, OBS_TAB, IER )
                        IF ( IER .NE. 0 ) THEN
                             CALL ERR_LOG ( 4450, IUER, 'GVH_READ_AGV', &
     &                           'Error in initialization cache table' )
                             RETURN 
                        END IF
                   END IF
                 ELSE 
                   LCODE = STR(IND(1,2):IND(2,2))
                   IF ( LIND == 6 ) THEN
                        LIND = 7
                        IND(1,7) = IND(2,6)+2
                        IND(2,7) = IND(2,6)+2
                   END IF
                   IF ( LIND < 7 ) THEN
                        CALL ERR_LOG ( 4451, IUER, 'GVH_READ_AGV', 'Trap '// &
     &                      'of internal control: the DATA lines '// &
     &                      'has less than 7 words: '//STR )
                        RETURN 
                   END IF
!
                   CALL CHIN ( STR(IND(1,3):IND(2,3)), IND_VAL )
                   IF ( IND_VAL < 0  .OR.  IND_VAL > GVH__MOBS ) THEN
                        CALL ERR_LOG ( 4452, IUER, 'GVH_READ_AGV', &
     &                      'Observation index is out of range '// &
     &                      'in porcessing line '//STR )
                        RETURN 
                   END IF
!
                   CALL CHIN ( STR(IND(1,4):IND(2,4)), IND_STA )
                   IF ( IND_STA < 0  .OR.  IND_STA > GVH__MSTA ) THEN
                        CALL ERR_LOG ( 4453, IUER, 'GVH_READ_AGV', &
     &                      'Station index is out of range '// &
     &                      'in processing line '//STR )
                        RETURN 
                   END IF
!
                   CALL CHIN ( STR(IND(1,5):IND(2,5)), DIMS(1) )
                   IF ( DIMS(1) < 1  .OR.  DIMS(1) > GVH__MDIM ) THEN
                        CALL ERR_LOG ( 4454, IUER, 'GVH_READ_AGV', &
     &                      'First dimension is out of range '// &
     &                      'in processing line '//STR )
                        RETURN 
                   END IF
!
                   CALL CHIN ( STR(IND(1,6):IND(2,6)), DIMS(2) )
                   IF ( DIMS(2) < 1  .OR.  DIMS(2) > GVH__MDIM ) THEN
                        CALL ERR_LOG ( 4455, IUER, 'GVH_READ_AGV', &
     &                      'Second dimension is out of range '// &
     &                      'in processing line '//STR )
                        RETURN 
                   END IF
!
                   IM = LTM_DIF ( 1, L_LCD, C_LCD, LCODE )
                   IF ( IM .LE. 0 ) THEN
                        CALL ERR_LOG ( 4456, IUER, 'GVH_READ_AGV', &
     &                      'Trap of internal in processing line :'// &
     &                       STR(1:I_LEN(STR))//': --  LCODE '//LCODE// &
     &                      ' was not defined in the table of contents' )
                        RETURN 
                   END IF
                   IF ( TYP_LCD(IM) == GVH__C1 ) THEN
                        CH_VAL = STR(IND(2,6)+2:IND(2,LIND))
                        IF ( LTM_DIF ( 0, GVH__NL_UNDS, GVH__LCODE_UNDS, LCODE ) > 0 ) THEN
!
! -------------------------- Replaced underscores with blanks
!
                             CALL UNDERSCORE_TO_BLANK ( CH_VAL ) 
                        END IF
                     ELSE IF ( TYP_LCD(IM) == GVH__I2 ) THEN
                        READ ( UNIT=STR(IND(1,7):IND(2,7)), FMT='(I6)', &
     &                         IOSTAT=IER ) I2_VAL 
                     ELSE IF ( TYP_LCD(IM) == GVH__I4 ) THEN
                        READ ( UNIT=STR(IND(1,7):IND(2,7)), FMT='(I11)', &
     &                         IOSTAT=IER ) I4_VAL 
                     ELSE IF ( TYP_LCD(IM) == GVH__R4 ) THEN
                        READ ( UNIT=STR(IND(1,7):IND(2,7)), FMT='(E15.7)', &
     &                         IOSTAT=IER ) R4_VAL 
                     ELSE IF ( TYP_LCD(IM) == GVH__R8 ) THEN
                        READ ( UNIT=STR(IND(1,7):IND(2,7)), FMT='(D22.15)', &
     &                         IOSTAT=IER ) R8_VAL 
                   END IF
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 4457, IUER, 'GVH_READ_AGV', &
     &                      'Trap of internal in processing line :'// &
     &                       STR(1:I_LEN(STR))//': --  Error in decoding '// &
     &                      'th value' )
                        RETURN 
                   END IF
!
                   IF ( FL_MND ) THEN
!
! --------------------- Mandatory lcode
!
                        IP = LTM_DIF ( 1, GVH__NMLCODE, GVH__MLCODE, LCODE )
                        IF ( IP .LE. 0  .AND. K_MND == GVH__NMLCODE ) THEN
                             IER = IUER
                             CALL GVH_PREPUT ( GVH, NUM_OBS, NUM_SCA, NUM_STA, &
     &                                         NOBS_STA, C_STA, OBS_TAB, IER )
                             IF ( IER .NE. 0 ) THEN
                                  CALL ERR_LOG ( 4458, IUER, 'GVH_READ_AGV', &
     &                                'Error in initialization cache table' )
                                  RETURN 
                             END IF
                             FL_MND = .FALSE.
                           ELSE IF ( IP .LE. 0 ) THEN
                             CALL ERR_LOG ( 4459, IUER, 'GVH_READ_AGV', &
     &                           'Trap of internal control when parsing '// &
     &                           'line '//STR(1:I_LEN(STR))//' -- not all '// &
     &                           'mandatory lcodes were read before '// &
     &                           'processing a non-mandatory lcode' )
                             RETURN 
                           ELSE 
                             IM = LTM_DIF ( 1, K_MND, C_MND, LCODE )
                             IF ( IM .LE. 0 ) THEN
                                  K_MND = K_MND + 1
                                  C_MND(K_MND) = LCODE
                             END IF
!
                             IF ( LCODE == 'NOBS_STA' ) THEN
                                  NOBS_STA(DIMS(1)) = I4_VAL
                               ELSE IF ( LCODE == 'NUMB_OBS' ) THEN
                                  NUM_OBS = I4_VAL
                               ELSE IF ( LCODE == 'NUMB_SCA' ) THEN
                                  NUM_SCA = I4_VAL
                               ELSE IF ( LCODE == 'NUMB_STA' ) THEN
                                  NUM_STA = I4_VAL
                               ELSE IF ( LCODE == 'SITNAMES' ) THEN
                                  C_STA(DIMS(2)) = CH_VAL(1:8)
                               ELSE IF ( LCODE == 'OBS_TAB ' ) THEN
                                  OBS_TAB(DIMS(1),DIMS(2)) = I4_VAL
                             END IF
                        END IF
                   END IF 
!
                   IF ( .NOT. FL_MND ) THEN
                      IF ( DIM_LCD(1,IM) .GE. 0  .AND.  &
     &                     DIM_LCD(2,IM) .GE. 0         ) THEN
!
! ---------------------- Non-mandatory lcode
!
                         OFFSET = DIMS(1) + (DIMS(2)-1)*DIM_LCD(1,IM)
                         IF ( TYP_LCD(IM) == GVH__C1 ) THEN
                              CALL LIB$MOVC3 ( GVH__TYPE_LEN(GVH__C1)*DIM_LCD(1,IM), &
     &                                         %REF(CH_VAL), ARR_I1(OFFSET) )
                           ELSE IF ( TYP_LCD(IM) == GVH__I2 ) THEN
                              OFFSET = DIMS(1) + (DIMS(2)-1)*DIM_LCD(1,IM)
                              CALL LIB$MOVC3 ( GVH__TYPE_LEN(GVH__I2), &
     &                                         I2_VAL, ARR_I2(OFFSET) )
                           ELSE IF ( TYP_LCD(IM) == GVH__I4 ) THEN
                              CALL LIB$MOVC3 ( GVH__TYPE_LEN(GVH__I4), &
     &                                         I4_VAL, ARR_I4(OFFSET) )
                           ELSE IF ( TYP_LCD(IM) == GVH__R4 ) THEN
                              CALL LIB$MOVC3 ( GVH__TYPE_LEN(GVH__R4), &
     &                                         R4_VAL, ARR_R4(OFFSET) )
                           ELSE IF ( TYP_LCD(IM) == GVH__R8 ) THEN
                              CALL LIB$MOVC3 ( GVH__TYPE_LEN(GVH__R8), &
     &                                        R8_VAL, ARR_R8(OFFSET) )
                        END IF
!
                        IF ( GVH__CLASS_INT(CLS_LCD(IM)) == GVH__SES ) THEN
                             IND_OBS = 1
                           ELSE IF ( GVH__CLASS_INT(CLS_LCD(IM)) == GVH__BAS ) THEN
                             IND_OBS = IND_VAL
                           ELSE IF ( GVH__CLASS_INT(CLS_LCD(IM)) == GVH__SCA ) THEN
                             IND_OBS = -1
                             DO 430 J3=1,NUM_OBS
                                IF ( OBS_TAB(1,J3) == IND_VAL ) THEN
                                     IND_OBS = J3
                                     GOTO 830
                                END IF
 430                         CONTINUE 
 830                         CONTINUE 
                           ELSE IF ( GVH__CLASS_INT(CLS_LCD(IM)) == GVH__STA ) THEN
                             IND_OBS = -1
                             KSTA = 0
                             IND_SCA = 0 
                             DO 440 J4=1,NUM_OBS
                                IF ( OBS_TAB(1,J4) .NE. IND_SCA  .AND. &
     &                               OBS_TAB(2,J4) == IND_STA          ) THEN
                                     KSTA = KSTA + 1
                                     IF ( KSTA == IND_VAL ) THEN
                                          IND_STA_BAS = 1
                                          IND_OBS = J4
                                          GOTO 840
                                     END IF
                                     IND_SCA  = OBS_TAB(1,J4) 
                                END IF 
!
                                IF ( OBS_TAB(1,J4) .NE. IND_SCA  .AND. &
     &                               OBS_TAB(3,J4) == IND_STA          ) THEN
                                     KSTA = KSTA + 1
                                     IF ( KSTA == IND_VAL ) THEN
                                          IND_STA_BAS = 2
                                          IND_OBS = J4
                                          GOTO 840
                                     END IF
                                     IND_SCA  = OBS_TAB(1,J4) 
                                END IF 
 440                         CONTINUE 
 840                         CONTINUE 
                        END IF
!
                       IF ( IND_OBS .GE. 0 ) THEN
                          IER = IUER
                          IF ( DIMS(1) == DIM_LCD(1,IM)  .AND.  &
     &                         DIMS(2) == DIM_LCD(2,IM)         ) THEN
                               IF ( TYP_LCD(IM) == GVH__I2 ) THEN
                                    CALL GVH_PLCODE ( GVH, LCODE, IND_OBS, &
     &                                                IND_STA_BAS, ARR_I2, IER )
                                 ELSE IF ( TYP_LCD(IM) == GVH__I4 ) THEN
                                    CALL GVH_PLCODE ( GVH, LCODE, IND_OBS, &
     &                                                IND_STA_BAS, ARR_I4, IER )
                                 ELSE IF ( TYP_LCD(IM) == GVH__R4 ) THEN
                                    CALL GVH_PLCODE ( GVH, LCODE, IND_OBS, &
     &                                                IND_STA_BAS, ARR_R4, IER )
                                 ELSE IF ( TYP_LCD(IM) == GVH__R8 ) THEN
                                    CALL GVH_PLCODE ( GVH, LCODE, IND_OBS, &
     &                                                IND_STA_BAS, ARR_R8, IER )
                               END IF
!
                               IF ( TYP_LCD(IM) == GVH__I2 .OR. &
     &                              TYP_LCD(IM) == GVH__I4 .OR. &
     &                              TYP_LCD(IM) == GVH__R4 .OR. &
     &                              TYP_LCD(IM) == GVH__R8      ) THEN
                               
                                    IF ( IER .NE. 0 ) THEN
                                         CALL ERR_LOG ( 4460, IUER, &
     &                                       'GVH_READ_AGV', 'Error in an '// &
     &                                       'attempt to put lcode '//LCODE )
                                         RETURN 
                                    END IF
                               END IF
                          END IF
                          IF ( TYP_LCD(IM) == GVH__C1   .AND.  &
     &                         DIMS(2) == DIM_LCD(2,IM)        ) THEN
                               CALL GVH_PLCODE ( GVH, LCODE, IND_OBS, &
     &                                           IND_STA, ARR_I1, IER )
                               IF ( IER .NE. 0 ) THEN
                                    CALL ERR_LOG ( 4461, IUER, 'GVH_READ_AGV', &
     &                                  'Error in an attempt to put lcode '// &
     &                                   LCODE )
                                    RETURN 
                               END IF
                          END IF
                        END IF
                     END IF ! DIMS
                   END IF ! .NOT. FL_MND
              END IF
         END IF
!
         IF ( PREF == 'HEAP' .AND.  ( ISEG == 0  .OR. ISEG == KSEG ) ) THEN
              IF ( INDEX ( STR(IND(1,2):IND(2,2)), '@section_length' ) > 0 ) THEN
                   CONTINUE 
                 ELSE
                   LCODE = STR(IND(1,2):IND(2,2))
                   IF ( LIND < 9 ) THEN
                        CALL ERR_LOG ( 4462, IUER, 'GVH_READ_AGV', 'Trap '// &
     &                      'of internal control: the DATA lines '// &
     &                      'has less than 7 words: '//STR )
                        RETURN 
                   END IF
!
                   CALL CHIN ( STR(IND(1,3):IND(2,3)), IND_VAL )
                   IF ( IND_VAL < 0  .OR.  IND_VAL > GVH__MOBS ) THEN
                        CALL ERR_LOG ( 4463, IUER, 'GVH_READ_AGV', &
     &                      'Observation index is out of range '// &
     &                      'in porcessing line '//STR )
                        RETURN 
                   END IF
!
                   CALL CHIN ( STR(IND(1,4):IND(2,4)), IND_STA )
                   IF ( IND_STA < 0  .OR.  IND_STA > GVH__MSTA ) THEN
                        CALL ERR_LOG ( 4464, IUER, 'GVH_READ_AGV', &
     &                      'Station index is out of range '// &
     &                      'in processing line '//STR )
                        RETURN 
                   END IF
!
                   CALL CHIN ( STR(IND(1,5):IND(2,5)), MAX_DIMS(1) )
                   MAX_DIMS(1) = -MAX_DIMS(1)
                   IF ( MAX_DIMS(1) < 1  .OR.  MAX_DIMS(1) > GVH__MDIM ) THEN
                        CALL ERR_LOG ( 4465, IUER, 'GVH_READ_AGV', &
     &                      'First maximum dimension is out of range '// &
     &                      'in processing line '//STR )
                        RETURN 
                   END IF
!
                   CALL CHIN ( STR(IND(1,6):IND(2,6)), MAX_DIMS(2) )
                   MAX_DIMS(2) = -MAX_DIMS(2)
                   IF ( MAX_DIMS(2) < 1  .OR.  MAX_DIMS(2) > GVH__MDIM ) THEN
                        CALL ERR_LOG ( 4466, IUER, 'GVH_READ_AGV', &
     &                      'Second maximum dimension is out of range '// &
     &                      'in processing line '//STR )
                        RETURN 
                   END IF
!
                   CALL CHIN ( STR(IND(1,7):IND(2,7)), DIMS(1) )
                   IF ( DIMS(1) < 1  .OR.  DIMS(1) > GVH__MDIM ) THEN
                        CALL ERR_LOG ( 4467, IUER, 'GVH_READ_AGV', &
     &                      'First maximum dimension is out of range '// &
     &                      'in processing line '//STR )
                        RETURN 
                   END IF
!
                   CALL CHIN ( STR(IND(1,8):IND(2,8)), DIMS(2) )
                   IF ( DIMS(2) < 1  .OR.  DIMS(2) > GVH__MDIM ) THEN
                        CALL ERR_LOG ( 4468, IUER, 'GVH_READ_AGV', &
     &                      'Second maximum dimension is out of range '// &
     &                      'in processing line '//STR )
                        RETURN 
                   END IF
!
                   IM = LTM_DIF ( 1, L_LCD, C_LCD, LCODE )
                   IF ( IM .LE. 0 ) THEN
                        CALL ERR_LOG ( 4469, IUER, 'GVH_READ_AGV', &
     &                      'Trap of internal in processing line "'// &
     &                       STR(1:I_LEN(STR))//'" --  LCODE '//LCODE// &
     &                      ' was not defined in the table of contents' )
                        RETURN 
                   END IF
                   IF ( TYP_LCD(IM) == GVH__C1 ) THEN
                        CH_VAL = STR(IND(1,9):IND(2,LIND))
                        IF ( LTM_DIF ( 0, GVH__NL_UNDS, GVH__LCODE_UNDS, LCODE ) > 0 ) THEN
!
! -------------------------- Replaced underscores with blanks
!
                             CALL UNDERSCORE_TO_BLANK ( CH_VAL ) 
                        END IF
                     ELSE IF ( TYP_LCD(IM) == GVH__I2 ) THEN
                        READ ( UNIT=STR(IND(1,9):IND(2,9)), FMT='(I6)', &
     &                         IOSTAT=IER ) I2_VAL 
                     ELSE IF ( TYP_LCD(IM) == GVH__I4 ) THEN
                        READ ( UNIT=STR(IND(1,9):IND(2,9)), FMT='(I11)', &
     &                         IOSTAT=IER ) I4_VAL 
                     ELSE IF ( TYP_LCD(IM) == GVH__R4 ) THEN
                        READ ( UNIT=STR(IND(1,9):IND(2,9)), FMT='(E15.7)', &
     &                         IOSTAT=IER ) R4_VAL 
                     ELSE IF ( TYP_LCD(IM) == GVH__R8 ) THEN
                        READ ( UNIT=STR(IND(1,9):IND(2,9)), FMT='(D22.15)', &
     &                         IOSTAT=IER ) R8_VAL 
                   END IF
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 4470, IUER, 'GVH_READ_AGV', &
     &                      'Trap of internal in processing line :'// &
     &                       STR(1:I_LEN(STR))//': --  Error in decoding '// &
     &                      'th value' )
                        RETURN 
                   END IF
!
                   OFFSET = DIMS(1) + (DIMS(2)-1)*MAX_DIMS(1)
                   IF ( TYP_LCD(IM) == GVH__C1 ) THEN
                        CALL LIB$MOVC3 ( GVH__TYPE_LEN(GVH__C1)*MAX_DIMS(1), &
     &                                   %REF(CH_VAL), ARR_I1(OFFSET) )
                      ELSE IF ( TYP_LCD(IM) == GVH__I2 ) THEN
                        OFFSET = DIMS(1) + (DIMS(2)-1)*MAX_DIMS(1)
                        CALL LIB$MOVC3 ( GVH__TYPE_LEN(GVH__I2), I2_VAL, &
     &                                   ARR_I2(OFFSET) )
                      ELSE IF ( TYP_LCD(IM) == GVH__I4 ) THEN
                        CALL LIB$MOVC3 ( GVH__TYPE_LEN(GVH__I4), I4_VAL, &
     &                                   ARR_I4(OFFSET) )
                      ELSE IF ( TYP_LCD(IM) == GVH__R4 ) THEN
                        CALL LIB$MOVC3 ( GVH__TYPE_LEN(GVH__R4), R4_VAL, &
     &                                   ARR_R4(OFFSET) )
                      ELSE IF ( TYP_LCD(IM) == GVH__R8 ) THEN
                        CALL LIB$MOVC3 ( GVH__TYPE_LEN(GVH__R8), R8_VAL, &
     &                                   ARR_R8(OFFSET) )
                   END IF
!
                   IF ( GVH__CLASS_INT(CLS_LCD(IM)) == GVH__SES ) THEN
                        IND_OBS = 1
                      ELSE IF ( GVH__CLASS_INT(CLS_LCD(IM)) == GVH__BAS ) THEN
                        IND_OBS = IND_VAL
                      ELSE IF ( GVH__CLASS_INT(CLS_LCD(IM)) == GVH__SCA ) THEN
                        IND_OBS = -1
                        DO 450 J5=1,NUM_OBS
                           IF ( OBS_TAB(1,J5) == IND_VAL ) THEN
                                IND_OBS = J5
                                GOTO 850
                           END IF
 450                    CONTINUE 
 850                    CONTINUE 
                      ELSE IF ( GVH__CLASS_INT(CLS_LCD(IM)) == GVH__STA ) THEN
                        IND_OBS = -1
                        KSTA = 0
                        IND_SCA = 0 
                        DO 460 J6=1,NUM_OBS
                           IF ( OBS_TAB(1,J6) .NE. IND_SCA  .AND. &
     &                          OBS_TAB(2,J6) == IND_STA          ) THEN
                                KSTA = KSTA + 1
                                IF ( KSTA == IND_VAL ) THEN
                                     IND_STA_BAS = 1
                                     IND_OBS = J6
                                     GOTO 860
                                END IF
                                IND_SCA  = OBS_TAB(1,J6) 
                           END IF 
!
                           IF ( OBS_TAB(1,J6) .NE. IND_SCA  .AND. &
     &                          OBS_TAB(3,J6) == IND_STA          ) THEN
                                KSTA = KSTA + 1
                                IF ( KSTA == IND_VAL ) THEN
                                     IND_STA_BAS = 2
                                     IND_OBS = J6
                                     GOTO 860
                                END IF
                                IND_SCA  = OBS_TAB(1,J6) 
                           END IF 
 460                    CONTINUE 
 860                    CONTINUE 
                   END IF
!
                   IF ( IND_OBS .GE. 0 ) THEN
                        IER = IUER
                        IF ( DIMS(1) == MAX_DIMS(1)  .AND.  &
     &                       DIMS(2) == MAX_DIMS(2)         ) THEN
                             IF ( TYP_LCD(IM) == GVH__I2 ) THEN
                                  CALL GVH_PHLCODE ( GVH, LCODE, IND_OBS, &
     &                                               IND_STA_BAS, MAX_DIMS(1), &
     &                                               MAX_DIMS(2), ARR_I2, IER )
                                ELSE IF ( TYP_LCD(IM) == GVH__I4 ) THEN
                                  CALL GVH_PHLCODE ( GVH, LCODE, IND_OBS, &
     &                                               IND_STA_BAS, MAX_DIMS(1), &
     &                                               MAX_DIMS(2), ARR_I4, IER )
                                ELSE IF ( TYP_LCD(IM) == GVH__R4 ) THEN
                                  CALL GVH_PHLCODE ( GVH, LCODE, IND_OBS, &
     &                                               IND_STA_BAS, MAX_DIMS(1), &
     &                                               MAX_DIMS(2), ARR_R4, IER )
                                ELSE IF ( TYP_LCD(IM) == GVH__R8 ) THEN
                                  CALL GVH_PHLCODE ( GVH, LCODE, IND_OBS, &
     &                                               IND_STA_BAS, MAX_DIMS(1), &
     &                                               MAX_DIMS(2), ARR_R8, IER )
                             END IF
                             IF ( IER .NE. 0 ) THEN
                                  CALL ERR_LOG ( 4471, IUER, 'GVH_READ_AGV', &
     &                                'Error in an attempt to put lcode '// &
     &                                 LCODE )
                                  RETURN 
                             END IF
                        END IF
                        IF ( TYP_LCD(IM) == GVH__C1  .AND.  &
     &                       DIMS(2) == MAX_DIMS(2)         ) THEN
                             CALL GVH_PLCODE ( GVH, LCODE, IND_OBS, &
     &                                         IND_STA, MAX_DIMS(1), &
     &                                         MAX_DIMS(2), ARR_I1, IER )
                             IF ( IER .NE. 0 ) THEN
                                  CALL ERR_LOG ( 4472, IUER, 'GVH_READ_AGV', &
     &                                'Error in an attempt to put lcode '//LCODE )
                                  RETURN 
                             END IF
                        END IF
                   END IF
              END IF
         END IF
         IF ( STR(1:25) == 'DATA.4 DB_VERS  0 0  1  1' ) THEN
              READ ( UNIT=STR(27:31), FMT='(I5)' ) GVH%DB_VERS
         END IF
 410  CONTINUE 
 810  CONTINUE 
!
! --- Close the file
!
      CLOSE ( UNIT=LUN )
!
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END  SUBROUTINE  GVH_READ_AGV  !#!#
