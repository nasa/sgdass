      SUBROUTINE GVH_WRITE_AGV ( GVH, ISEG, OPCODE, FILENAME, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GVH_WRITE_AGV  writes the contents of one or more         *
! *   segments into the output file in ASCII GVH format. If ISEG=0,      *
! *   then all segments are written into the file. If ISEG is not equal  *
! *   to zero, then the ISEG-th segment is written. If OPCODE = GVH__CRT,*
! *   then the new file is created. If OPCODE = GVH__APP, then the       *
! *   contents is appended to the existing file.                         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     ISEG ( INTEGER*4 ) -- Segment index. If ISEG = 0, then all       *
! *                           segments are written into the output file. *
! *                           In that case the output will contains      *
! *                           several concatenated segments.             *
! *                           If ISEG > 0  then the ISEG-th segment is   *
! *                           written into the output file.              *
! *   OPCODE ( INTEGER*4 ) -- Operation code: two codes are supported:   *
! *                           GVH__CRT -- create new file;               *
! *                           GVH__APP -- append output to existing file.*
! * FILENAME ( CHARACTER ) -- Name of the file where the database will   *
! *                           be written,                                &
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
! * ### 25-NOV-2001  GVH_WRITE_AGV  v2.6 (c)  L. Petrov  31-OCT-2019 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      TYPE     ( GVH__STRU      ) :: GVH
      TYPE     ( GVH_DESC__STRU ) :: GVH_DESC
      TYPE     ( GVH_LCD__STRU  ) :: LCD(GVH__MTOC)
      CHARACTER  FILENAME*(*)
      INTEGER*4  ISEG, OPCODE, IUER
      INTEGER*4  GVH_UNIT
      CHARACTER  STR*512, STR1*512, SEG_LABEL*10, KEYWORD*256, VALUE*256, &
     &           TITLE*256, LINE*1024, LCODE*8, DESCR*80
      CHARACTER  CURRENT_DATE*26, USER_NAME*128, USER_REALNAME*128, &
     &           USER_E_ADDRESS*128, WHOAMI*128, KEYWORD_STR*128, &
     &           VALUE_STR*128
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           ISEG0, ISEG1, ISEG2, ISEG3, ISEG4, ISEG5, &
     &           IOS, LSEG, IP, ISL, NREC, &
     &           ILN, ILN_BEG, IND_TOC, ILN_CHA, ICL, ITP, &
     &           NUM_DATA_ELEM, NUM_HEAP_ELEM, SEG_IND, NUM_FIELDS, &
     &           DIMS_DUMMY(2), ILN_MAX, KREC, IER
      INTEGER*4  CLASS, TYP, DIMS(2), LEN_REC, LEN_DATA
      ADDRESS__TYPE  ADR_DATA, ADR_CONV, IAD, IAD_END, IAD_CHA, IAD_DUMMY
      INTEGER*1  TAB(0:255), TAB_VAL
      INTEGER*8  LCODE_I8
      LOGICAL*4  FL_CREAT_ALL, FL_CREAT_PRE, FL_UND
      CHARACTER, EXTERNAL :: GET_TZ_CDATE*26
#ifdef GNU
      INTEGER*4, EXTERNAL :: GVH_COMPAR_LCD  
#else
      INTEGER*2, EXTERNAL :: GVH_COMPAR_LCD  
#endif
      INTEGER*4, EXTERNAL :: LIB$SCANC, LTM_DIF, IFIND_PL, GET_UNIT, I_LEN, ILEN
!
! --- intialization of the delimiters table
!
      CALL NOUT ( 256, TAB )
      TAB_VAL = 1
      TAB(10) = TAB_VAL ! Carriage return -- record delimiter
      TAB(26) = TAB_VAL ! chapter delimiter
!
      IF ( GVH%STATUS .NE. GVH__INITIALIZED ) THEN
           CALL ERR_LOG ( 4131, IUER, 'GVH_WRITE_AGV', 'The GVH data '// &
     &         'structure was not initialized. Please, use gvh_init first' )
           RETURN
      END IF
!
      IF ( GVH%CACHE%OBS_STATUS  .NE.  GVH__POPULATED ) THEN
           CALL ERR_LOG ( 4132, IUER, 'GVH_WRITE_AGV', 'The GVH '// &
     &         'observations cache table has not been populated' )
           RETURN
      END IF
!
      IF ( GVH%CACHE%LCODE_STATUS  .NE.  GVH__POPULATED ) THEN
           CALL ERR_LOG ( 4133, IUER, 'GVH_WRITE_AGV', 'The GVH '// &
     &         'lcodes cache table has not been populated' )
           RETURN
      END IF
!
      IF ( ISEG < 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( ISEG, STR )
           CALL ERR_LOG ( 4134, IUER, 'GVH_WRITE_AGV', 'Wrong parameter '// &
     &         'ISEG: '//STR )
           RETURN
      END IF
!
      IF ( ISEG .GT. GVH%NSEG ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( ISEG, STR )
           CALL CLRCH ( STR1 ) 
           CALL INCH  ( GVH%NSEG, STR1 )
           CALL ERR_LOG ( 4135, IUER, 'GVH_WRITE_AGV', 'Wrong parameter '// &
     &         'ISEG: '//STR(1:I_LEN(STR))//' -- it exceeds the total '// &
     &         'numer of segments GVH%NSEG: '//STR1 )
           RETURN
      END IF
!
      IF ( OPCODE .NE. GVH__CRT  .AND.  OPCODE .NE. GVH__APP ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( OPCODE, STR )
           CALL ERR_LOG ( 4136, IUER, 'GVH_WRITE_AGV', 'Wrong parameter '// &
     &         'OPCODE: '//STR(1:I_LEN(OPCODE))//' only GVH__CRT and '// &
     &         'GVH__APP are supported' )
           RETURN
      END IF
!
      IF ( ISEG == 0  .OR.  ISEG == 1 ) THEN
!
! -------- Update CREATED_AT, CREATED_BY labels in the preamble section
! -------- of the first segment
!
! -------- Check whether the label was defined
!
           FL_CREAT_PRE = .FALSE.
           FL_CREAT_ALL = .FALSE.
           IF ( GVH%PREA(GVH%SEG)%NKWD .GE. 1 ) THEN
                IER = 0
                CALL GVH_GPREA ( GVH, 1, GVH%PREA(GVH%SEG)%NKWD, &
     &                           KEYWORD_STR, VALUE_STR, IER )
                IF ( INDEX( KEYWORD_STR, 'CREATED_BY:' ) > 0 ) THEN
                     FL_CREAT_PRE = .TRUE. ! yes it was
                END IF
!
                DO 410 J1=1,GVH%DMA
                   IF ( GVH%MEMADR(J1) == &
     &                  GVH%PREA(1)%KWD_ADR(GVH%PREA(1)%NKWD) ) THEN
                        FL_CREAT_ALL = .TRUE.
                   END IF
 410            CONTINUE 
           END IF
!
           IF ( FL_CREAT_PRE  .AND.  FL_CREAT_ALL ) THEN 
!
! ------------- If was defined remove two last preamble records
!
                IER = IUER
                CALL GVH_FREE ( GVH, GVH%PREA(1)%KWD_ADR(GVH%PREA(1)%NKWD), IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 4137, IUER, 'GVH_WRITE_AGV', &
     &                   'Trap of internal control: cannot free memory '// &
     &                   'of the last by one preamble record' )
                     RETURN 
                END IF
                CALL GVH_FREE ( GVH, GVH%PREA(1)%VAL_ADR(GVH%PREA(1)%NKWD), &
     &                          IER )
                CALL GVH_FREE ( GVH, GVH%PREA(1)%KWD_ADR(GVH%PREA(1)%NKWD-1), IER )
                CALL GVH_FREE ( GVH, GVH%PREA(1)%VAL_ADR(GVH%PREA(1)%NKWD-1), IER )
                GVH%PREA(GVH%SEG)%NKWD = GVH%PREA(GVH%SEG)%NKWD - 2
           END IF
!
           IF ( FL_CREAT_ALL ) THEN
!
! ------------- Get the current date and the user name
!
                CURRENT_DATE = GET_TZ_CDATE()
                CALL GETINFO_USER ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
                WHOAMI = USER_REALNAME(1:I_LEN(USER_REALNAME))//' ( '// &
     &                   USER_E_ADDRESS(1:I_LEN(USER_E_ADDRESS))//' )'
!
! ------------- Add preamble record CREATED_AT
!
                IER = IUER
                CALL GVH_PPREA ( GVH, 1, 'CREATED_AT:', CURRENT_DATE, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 4138, IUER, 'GVH_WRITE_AGV', 'Error in '// &
     &                   'an attempt to update preamble record and to put '// &
     &                   'modification date stamp' )
                     RETURN 
                END IF
!
! ------------- Add preamble record CREATED_BY
!
                IER = IUER
                CALL GVH_PPREA ( GVH, 1, 'CREATED_BY:', WHOAMI, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 4139, IUER, 'GVH_WRITE_AGV', 'Error in '// &
     &                   'an attempt to update preamble record and to put '// &
     &                   'the person name how has modified the file' )
                     RETURN 
                END IF
           END IF
      END IF
!
! --- Open the output file
!
      GVH_UNIT = GET_UNIT () ! Get free Fortran i/o unit
      IF ( OPCODE .EQ. GVH__CRT ) THEN
           OPEN ( UNIT=GVH_UNIT, FILE=FILENAME, STATUS='UNKNOWN', IOSTAT=IOS )
           LSEG = 1
         ELSE IF ( OPCODE .EQ. GVH__APP ) THEN
           OPEN ( UNIT=GVH_UNIT, FILE=FILENAME, STATUS='OLD', &
     &            ACCESS='APPEND', IOSTAT=IOS )
           LSEG = ISEG
      END IF
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 4140, IUER, 'GVH_WRITE_AGV', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to open the output file '// &
     &          FILENAME )
           RETURN
      END IF
!
! --- Write the first line: format label
!
      WRITE ( GVH_UNIT, '(A)', IOSTAT=IER ) GVH__AGV_LABEL
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 4141, IUER, 'GVH_WRITE_AGV', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to write the first '// &
     &         'record in the outpuf file '//FILENAME )
           RETURN
      END IF
      NREC = 1 ! unitialize records counter
      WRITE ( GVH_UNIT, '(A,I1,A)' ) 'FILE.', ISEG, ' @section_length: 1 file'
      NREC = NREC + 1
      WRITE ( GVH_UNIT, '(A,I1,1X,A)' ) 'FILE.', ISEG, &
     &                    GVH%FILENAME(ISEG)(1:I_LEN(GVH%FILENAME(ISEG)))
!
! --- Create the section label
!
      SEG_LABEL = 'PREA.     '
      CALL INCH ( LSEG, SEG_LABEL(6:9) )
      ISL = ILEN(SEG_LABEL) + 1
!
! --- Write preamble header
!
      WRITE ( GVH_UNIT, '(A,I6,A)' ) SEG_LABEL(1:ISL)//'@section_length: ', &
     &        GVH%PREA(ISEG)%NKWD, ' keywords'
      NREC = NREC + 1
!
! --- Printing contents of the preamble section
!
      DO 420 J2=1,GVH%PREA(ISEG)%NKWD
         CALL MEMCPY ( %REF(KEYWORD), %VAL(GVH%PREA(ISEG)%KWD_ADR(J2)), &
     &                                %VAL(GVH%PREA(ISEG)%KWD_LEN(J2)) )
         CALL MEMCPY ( %REF(VALUE),   %VAL(GVH%PREA(ISEG)%VAL_ADR(J2)), &
     &                                %VAL(GVH%PREA(ISEG)%VAL_LEN(J2)) )
         WRITE ( GVH_UNIT, '(A,A,1X,A)' ) SEG_LABEL(1:ISL), &
     &           KEYWORD(1:GVH%PREA(ISEG)%KWD_LEN(J2)-1), &
     &           VALUE(1:GVH%PREA(ISEG)%VAL_LEN(J2)-1)
         NREC = NREC + 1
 420  CONTINUE
!
! --- Text section
!
      SEG_LABEL(1:4) = 'TEXT'
!
! --- Write the header of the text section
!
      WRITE ( GVH_UNIT, '(A,I6,A)' ) SEG_LABEL(1:ISL)//'@section_length: ', &
     &        GVH%TEXT(ISEG)%NTIT, ' chapters'
      NREC = NREC + 1
      DO 430 J3=1,GVH%TEXT(ISEG)%NTIT
         CALL CLRCH ( STR )
         CALL INCH  ( J3, STR )
!
! ------ Extract chapter's title
!
         CALL MEMCPY ( %REF(TITLE), %VAL(GVH%TEXT(ISEG)%TITLE_ADR(J3)), &
     &                              %VAL(GVH%TEXT(ISEG)%TITLE_LEN(J3)-1) )
         NREC = NREC + 1
         IAD_CHA = GVH%TEXT(ISEG)%BODY_ADR(J3)
         ILN_CHA = GVH%TEXT(ISEG)%BODY_LEN(J3)
         IAD_END = GVH%TEXT(ISEG)%BODY_ADR(J3) + ILN_CHA-1
!
! ------ Cound lines and get the longest line
!
         ILN_MAX  = 1
         KREC = 0
         IAD  = IAD_CHA
         DO 440 J4=1,1024*1024*1024
!
! --------- Search for a delimiter
!
            IP = LIB$SCANC ( %VAL(IAD), TAB, TAB_VAL, %VAL(ILN_CHA) )
            IF ( IP .EQ. 0 ) THEN
                 CALL ERR_LOG ( 4142, IUER, 'GVH_WRITE_AGV', 'Trap of '// &
     &               'internal control: text section is corrupted' )
                 RETURN
            END IF
!
            ILN = MIN ( IP-1, ILN_CHA, LEN(LINE) )
            IF ( ILN > 0 ) THEN
                 ILN_MAX = MAX ( ILN, ILN_MAX )
            END IF
            KREC = KREC + 1
            IAD = IAD + IP
            ILN_CHA = ILN_CHA - IP
            IF ( IAD .GE. IAD_END ) GOTO 840
 440     CONTINUE
 840     CONTINUE
!
         CALL CLRCH ( STR1 )
         STR1(1:GVH%TEXT(ISEG)%TITLE_LEN(J3)-1) = TITLE(1:GVH%TEXT(ISEG)%TITLE_LEN(J3)-1)
         IF ( STR1(1:len('characters')) == 'characters' ) THEN
              STR1 = STR1(11:)
              CALL CHASHL ( STR1 )
         END IF
         IF ( STR1(1:len('characters')) == 'characters' ) THEN
              STR1 = STR1(11:)
              CALL CHASHL ( STR1 )
         END IF
         IF ( STR1(1:len('charaters')) == 'charaters' ) THEN
!
! ----------- This is is compatibility with pre-OCT2019 bug
!
              STR1 = STR1(10:)
              CALL CHASHL ( STR1 )
         END IF
         IF ( STR1(1:len('charaters')) == 'charaters' ) THEN
!
! ----------- This is is compatibility with pre-OCT2019 bug
!
              STR1 = STR1(10:)
              CALL CHASHL ( STR1 )
         END IF
         WRITE ( GVH_UNIT, '(A,A,1X,I6,1X,A,1X,I6,1X,A,1X,A)' ) &
     &           SEG_LABEL(1:ISL)//'  @@chapter: ', &
     &           STR(1:I_LEN(STR)), KREC, ' records, max_len: ', ILN_MAX, &
     &           'characters', TRIM(STR1)
!
         IAD_CHA = GVH%TEXT(ISEG)%BODY_ADR(J3)
         ILN_CHA = GVH%TEXT(ISEG)%BODY_LEN(J3)
         IAD_END = GVH%TEXT(ISEG)%BODY_ADR(J3) + ILN_CHA-1
         IAD = IAD_CHA
!
! ------ Cycle on records in the chapter
!
         DO 450 J5=1,1024*1024*1024
!
! --------- Seach for delimiter
!
            IP = LIB$SCANC ( %VAL(IAD), TAB, TAB_VAL, %VAL(ILN_CHA) )
!
            ILN = MIN ( IP-1, ILN_CHA, LEN(LINE) )
            IF ( ILN .LE. 0 ) THEN
                 WRITE ( GVH_UNIT, '(A)' ) SEG_LABEL(1:ISL)
                 NREC = NREC + 1
              ELSE
!
! -------------- Write the next line of the chapter's body
!
                 CALL MEMCPY ( %REF(LINE), %VAL(IAD), %VAL(ILN) )
                 WRITE ( GVH_UNIT, '(A,A)' ) SEG_LABEL(1:ISL), LINE(1:ILN)
                 NREC = NREC + 1
            END IF
            IAD = IAD + IP
            ILN_CHA = ILN_CHA - IP
            IF ( IAD .GE. IAD_END ) GOTO 850
 450     CONTINUE
 850     CONTINUE
 430  CONTINUE
!
! --- Get the table of lcode names for this segment
!
      DO 460 J6=1,GVH%TOCS(ISEG)%NTOC
         CALL GVH_EXCH_LCODE1 ( .FALSE., %VAL(GVH%TOCS(ISEG)%ADR), J6, &
     &                          GVH__GET, LCODE, DESCR, CLASS, TYP,  &
     &                          DIMS, LEN_DATA, ADR_DATA, IER )
         LCD(J6)%LCODE   = LCODE
         LCD(J6)%IND_TOC = J6
 460  CONTINUE 
!
! --- ...and sort it
!
      CALL FOR_QSORT ( LCD, GVH%TOCS(ISEG)%NTOC, SIZEOF(LCD(1)), GVH_COMPAR_LCD )
!
! --- TOCS section
!
      SEG_LABEL(1:4) = 'TOCS'
      WRITE ( GVH_UNIT, '(A,I6,A)' ) SEG_LABEL(1:ISL)//'@section_length: ', &
     &        GVH%TOCS(ISEG)%NTOC, ' lcodes'
      NREC = NREC + 1
!
! --- Cycle over records of the table of content
!
      NUM_DATA_ELEM = 0
      DO 470 J7=1,GVH%TOCS(ISEG)%NTOC
         IND_TOC = LCD(J7)%IND_TOC 
!
! ------ Retrieve contents of the definition of the J7-th lcode from the
! ------ table of conents
!
         IER = IUER
         CALL GVH_EXCH_LCODE1 ( .FALSE., %VAL(GVH%TOCS(ISEG)%ADR), IND_TOC, &
     &                          GVH__GET, LCODE, DESCR, CLASS, TYP,  &
     &                          DIMS, LEN_DATA, ADR_DATA, IER )
!
! ------ Retrieve contents of the defintion of the J7-th lcode from the
! ------ cache table of contents
!
         IER = IUER
         CALL MEMCPY ( LCODE_I8, LCODE )
         CALL GVH_LCODE_TAB_INQ ( %VAL(GVH%CACHE%LCODE_ADR), &
     &                            GVH%CACHE%NUM_LCODE, LCODE, LCODE_I8, &
     &                            GVH%LCODE_CACHE_I8, GVH%IND_LCODE_CACHE, &
     &                            DESCR, CLASS, TYP, DIMS, LEN_REC, LEN_DATA, &
     &                            SEG_IND, NUM_FIELDS, ADR_DATA, ADR_CONV, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4143, IUER, 'GVH_WRITE_BGV', &
     &            'Error in GVH_LCODE_TAB_INQ ' )
              RETURN 
         END IF
!
! ------ Format the output record
!
         LINE = SEG_LABEL(1:ISL)//LCODE
         ILN = LEN(SEG_LABEL)+8
!
         ICL = IFIND_PL ( GVH__MCLASS, GVH__CLASS_INT, CLASS )
         LINE(ILN+1:ILN+3) = GVH__CLASS_CHR(ICL)
!
         ILN = ILEN(LINE)
         ITP = IFIND_PL ( GVH__MTYPE, GVH__TYPE_INT, TYP )
         LINE(ILN+3:ILN+4) = GVH__TYPE_CHR(ITP)
!
         ILN = ILEN(LINE)
         CALL INCH ( DIMS(1), STR )
         IF ( ILEN(STR) .LE. 3 ) CALL CHASHR ( STR(1:3) )
         LINE(ILN+2:) = STR
!
         ILN = ILEN(LINE)
         CALL INCH ( DIMS(2), STR )
         IF ( ILEN(STR) .LE. 3 ) CALL CHASHR ( STR(1:3) )
         LINE(ILN+2:) = STR
!
         ILN = ILEN(LINE)
         LINE(ILN+3:) = DESCR
!
! ------ Now we write the tocs line down
!
         WRITE ( GVH_UNIT, '(A)' ) LINE(1:ILEN(LINE))
         NREC = NREC + 1
!
! ------ Count the number of elements in the data sections
!
         IF ( TYP .EQ. GVH__C1 ) THEN
!
! -------------- Character elements. The first dimension means the character
! -------------- length (except the element in HEAP section)
!
                 IF ( DIMS(2) .GT. 0 ) THEN
                      NUM_DATA_ELEM = NUM_DATA_ELEM + NUM_FIELDS*DIMS(2)
                    ELSE
!
! ------------------- It means that it is the element in heap section
!
                      NUM_DATA_ELEM = NUM_DATA_ELEM + 3
                 END IF
               ELSE
!
! -------------- Non-character elements
!
                 IF ( DIMS(2) .GT. 0 ) THEN
                      NUM_DATA_ELEM = NUM_DATA_ELEM + &
     &                                NUM_FIELDS*DIMS(1)*DIMS(2)
                    ELSE
!
! ------------------- It means that it is the element in heap section
!
                      NUM_DATA_ELEM = NUM_DATA_ELEM + 3
                 END IF
            END IF
 470  CONTINUE
!
! --- Now time came to write down contents of the DATA section.
! --- We write them down lcode by lcode.
!
      SEG_LABEL(1:4) = 'DATA'
      WRITE ( GVH_UNIT, '(A,I10,A)' ) SEG_LABEL(1:ISL)//'@section_length: ', &
     &        NUM_DATA_ELEM, ' records'
      NREC = NREC + 1
      NUM_HEAP_ELEM = 0
      DO 480 J8=1,GVH%TOCS(ISEG)%NTOC ! Cycle over lcodes
         IND_TOC = LCD(J8)%IND_TOC 
!
! ------ Extract information from the tocs cache
!
         IER = IUER
         CALL GVH_EXCH_LCODE1 ( .FALSE., %VAL(GVH%TOCS(ISEG)%ADR), IND_TOC, &
     &                           GVH__GET, LCODE, DESCR, CLASS, TYP,  &
     &                           DIMS, LEN_DATA, ADR_DATA, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4144, IUER, 'GVH_WRITE_AGV', 'Trap of '// &
     &            'internal control: cache section is corrupted' )
              RETURN
         END IF
         IF ( LTM_DIF ( 1, GVH__NL_UNDS, GVH__LCODE_UNDS, LCODE ) > 0 ) THEN
              FL_UND = .TRUE.
            ELSE
              FL_UND = .FALSE.
         END IF
!
         LINE = SEG_LABEL(1:ISL)//LCODE
         ILN  = ISL+8
         IAD  = ADR_DATA
!
! ------ Now we write down all elements of this lcode. Rules how to do it
! ------ depends on the lcode's class
!
         IF ( CLASS .EQ. GVH__SES ) THEN
!
! ----------- Session class. Create a dummy primary and secondary indices
!
              LINE(ILN+2:ILN+4) = '0 0'
              ILN_BEG = ILEN(LINE)
!
! ----------- Write the single element
!
              IF ( DIMS(1) .GT. 0 ) THEN
                   CALL GVH_PUT_INLINE ( GVH_UNIT, DIMS, TYP, IAD, LINE, &
     &                                   ILN_BEG, FL_UND, NREC )
                 ELSE
                   CALL MEMCPY ( GVH_DESC, %VAL(IAD), %VAL(SIZEOF(GVH_DESC)) )
                   NUM_HEAP_ELEM = NUM_HEAP_ELEM + GVH_DESC%DIMS(1)* &
     &                                             GVH_DESC%DIMS(2)
                   DIMS_DUMMY(1) = 2
                   DIMS_DUMMY(2) = 1
                   IAD_DUMMY = LOC(GVH_DESC%DIMS(1))
                   CALL GVH_PUT_INLINE ( GVH_UNIT, DIMS_DUMMY, GVH__I4, &
     &                                   IAD_DUMMY, LINE, ILN_BEG, FL_UND, NREC )
                   IAD = IAD + SIZEOF(GVH_DESC) ! skip the address field
              END IF
            ELSE IF ( CLASS .EQ. GVH__SCA ) THEN
!
! ----------- Scan class
!
              DO 490 J9=1,GVH%CACHE%NUM_SCA
!
! -------------- Put the primary index
!
                 ILN = ISL+8
                 CALL CLRCH ( LINE(ILN+1:) )
                 CALL INCH ( J9, LINE(ILN+2:) )
!
                 ILN = ILEN(LINE)
                 LINE(ILN+2:) = '0'  ! .. and dummy secondary index
                 ILN_BEG = ILEN(LINE)
!
                 IF ( DIMS(1) .GT. 0 ) THEN
                      CALL GVH_PUT_INLINE ( GVH_UNIT, DIMS, TYP, IAD, LINE, &
     &                                      ILN_BEG, FL_UND, NREC )
                    ELSE
                      CALL MEMCPY ( GVH_DESC, %VAL(IAD), &
     &                                        %VAL(SIZEOF(GVH_DESC)) )
                      NUM_HEAP_ELEM = NUM_HEAP_ELEM + GVH_DESC%DIMS(1)* &
     &                                                GVH_DESC%DIMS(2)
                      DIMS_DUMMY(1) = 2
                      DIMS_DUMMY(2) = 1
                      IAD_DUMMY = LOC(GVH_DESC%DIMS(1))
                      CALL GVH_PUT_INLINE ( GVH_UNIT, DIMS_DUMMY, GVH__I4, &
     &                                      IAD_DUMMY, LINE, ILN_BEG, FL_UND, NREC )
                      IAD = IAD + SIZEOF(GVH_DESC) ! skip the address field
                 END IF
 490          CONTINUE
            ELSE IF ( CLASS .EQ. GVH__STA ) THEN
!
! ----------- Station class
!
              DO 4100 J10=1,GVH%CACHE%NUM_STA
                 DO 4110 J11=1,GVH%CACHE%NOBS_STA(J10)
!
! ----------------- Create primary and secondary indices
!
                    ILN = ISL+8
                    CALL CLRCH ( LINE(ILN+1:) )
                    CALL INCH ( J11, LINE(ILN+2:) )
!
                    ILN = ILEN(LINE)
                    CALL INCH ( J10, LINE(ILN+2:) )
                    ILN_BEG = ILEN(LINE)
!
                    IF ( DIMS(1) .GT. 0 ) THEN
!
! ---------------------- Putting normal lcode
!
                         CALL GVH_PUT_INLINE ( GVH_UNIT, DIMS, TYP, IAD, &
     &                                         LINE, ILN_BEG, FL_UND, NREC )
                       ELSE
                         CALL MEMCPY ( GVH_DESC, %VAL(IAD), &
     &                                           %VAL(SIZEOF(GVH_DESC)) )
                         NUM_HEAP_ELEM = NUM_HEAP_ELEM + GVH_DESC%DIMS(1)* &
     &                                                   GVH_DESC%DIMS(2)
                         DIMS_DUMMY(1) = 2
                         DIMS_DUMMY(2) = 1
                         IAD_DUMMY = LOC(GVH_DESC%DIMS(1))
                         CALL GVH_PUT_INLINE ( GVH_UNIT, DIMS_DUMMY, &
     &                                         GVH__I4, IAD_DUMMY, LINE, &
     &                                         ILN_BEG, FL_UND, NREC )
                         IAD = IAD + SIZEOF(GVH_DESC) ! skip the address field
                    END IF
 4110            CONTINUE
 4100          CONTINUE
            ELSE IF ( CLASS .EQ. GVH__BAS ) THEN
              DO 4120 ISEG0=1,GVH%CACHE%NUM_OBS
!
! -------------- Baseline class. Create the primary index
!
                 ILN = ISL+8
                 CALL CLRCH ( LINE(ILN+1:) )
                 CALL INCH ( ISEG0, LINE(ILN+2:) )
!
                 ILN = ILEN(LINE)
                 LINE(ILN+2:) = '0' ! and dummy secondary index
                 ILN_BEG = ILEN(LINE)
!
                 IF ( DIMS(1) .GT. 0 ) THEN
                      CALL GVH_PUT_INLINE ( GVH_UNIT, DIMS, TYP, IAD, LINE, &
     &                                      ILN_BEG, FL_UND, NREC )
                    ELSE
                      CALL MEMCPY ( GVH_DESC, %VAL(IAD), &
     &                                        %VAL(SIZEOF(GVH_DESC)) )
                      NUM_HEAP_ELEM = NUM_HEAP_ELEM + GVH_DESC%DIMS(1)* &
     &                                                GVH_DESC%DIMS(2)
                      DIMS_DUMMY(1) = 2
                      DIMS_DUMMY(2) = 1
                      IAD_DUMMY = LOC(GVH_DESC%DIMS(1))
                      CALL GVH_PUT_INLINE ( GVH_UNIT, DIMS_DUMMY, GVH__I4, &
     &                                      IAD_DUMMY, LINE, &
     &                                      ILN_BEG, FL_UND, NREC )
                      IAD = IAD + SIZEOF(GVH_DESC) ! skip the address field
                 END IF
 4120         CONTINUE
         END IF
 480  CONTINUE
!
! --- Now time came to write down contents of the HEAP section.
! --- We write them down lcode by lcode.
!
      SEG_LABEL(1:4) = 'HEAP'
      WRITE ( GVH_UNIT, '(A,I10,A)' ) SEG_LABEL(1:ISL)//'@section_length: ', &
     &        NUM_HEAP_ELEM, ' records'
      NREC = NREC + 1
      IF ( NUM_HEAP_ELEM > 0 ) THEN
           DO 4130 ISEG1=1,GVH%TOCS(ISEG)%NTOC ! Cycle over lcodes
              IND_TOC = LCD(ISEG1)%IND_TOC 
!
! ----------- Extract information from the tocs cache
!
              IER = IUER
              CALL GVH_EXCH_LCODE1 ( .FALSE., %VAL(GVH%TOCS(ISEG)%ADR), ISEG1, &
     &                                GVH__GET, LCODE, DESCR, CLASS, TYP,   &
     &                                DIMS, LEN_DATA, ADR_DATA, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4145, IUER, 'GVH_WRITE_AGV', 'Trap of '// &
     &                 'internal control: cache section is corrupted' )
                   RETURN
              END IF
              IF ( LTM_DIF ( 1, GVH__NL_UNDS, GVH__LCODE_UNDS, LCODE ) > 0 ) THEN
                   FL_UND = .TRUE.
                 ELSE
                   FL_UND = .FALSE.
              END IF
!
              IF ( DIMS(1) .GT. 0 ) GOTO 4130
              LINE = SEG_LABEL(1:ISL)//LCODE
              ILN = ISL+8
!
! ----------- Now we write down all elements of this lcode. Rules how to do it
! ----------- depends on the lcode's class
!
              IF ( CLASS .EQ. GVH__SES ) THEN
!
! ---------------- Session class. Create a dummy primary and secondary indices
!
                   LINE(ILN+2:ILN+4) = '0 0'
                   ILN_BEG = ILEN(LINE)
!
                   CALL MEMCPY ( GVH_DESC, %VAL(ADR_DATA), %VAL(SIZEOF(GVH_DESC)) )
                   IAD = GVH_DESC%ADR
!
! ---------------- Write actual dimension of the array
!
                   CALL CLRCH ( LINE(ILN_BEG+1:) )
                   ILN = ILEN(LINE)
                   CALL INCH ( -GVH_DESC%DIMS(1), LINE(ILN+2:) )
!
                   ILN = ILEN(LINE)
                   CALL INCH ( -GVH_DESC%DIMS(2), LINE(ILN+2:) )
                   ILN_BEG = ILEN(LINE)
!
! ---------------- Write the single element
!
                   CALL GVH_PUT_INLINE ( GVH_UNIT, GVH_DESC%DIMS, TYP, IAD, &
     &                                   LINE, ILN_BEG, FL_UND, NREC )
                   NREC = NREC + 1
                 ELSE IF ( CLASS .EQ. GVH__SCA ) THEN
!
! ---------------- Scan class
!
                   DO 4140 ISEG2=1,GVH%CACHE%NUM_SCA
!
! ------------------- Put the primary index
!
                      ILN = ISL+8
                      CALL CLRCH ( LINE(ILN+1:) )
                      CALL INCH ( ISEG2, LINE(ILN+2:) )
!
                      ILN = ILEN(LINE)
                      LINE(ILN+2:) = '0'  ! ... and dummy secondary index
                      ILN_BEG = ILEN(LINE)
!
                      CALL MEMCPY ( GVH_DESC, %VAL(ADR_DATA), &
     &                                        %VAL(SIZEOF(GVH_DESC)) )
                      ADR_DATA = ADR_DATA + SIZEOF(GVH_DESC)
                      IAD = GVH_DESC%ADR
!
! ------------------- Write actual dimension of the array
!
                      CALL CLRCH ( LINE(ILN_BEG+1:) )
                      ILN = ILEN(LINE)
                      CALL INCH ( -GVH_DESC%DIMS(1), LINE(ILN+2:) )
!
                      ILN = ILEN(LINE)
                      CALL INCH ( -GVH_DESC%DIMS(2), LINE(ILN+2:) )
                      ILN_BEG = ILEN(LINE)
!
! ------------------- Write the element
!
                      CALL GVH_PUT_INLINE ( GVH_UNIT, GVH_DESC%DIMS, TYP, IAD, &
     &                                      LINE, ILN_BEG, FL_UND, NREC )
                      NREC = NREC + 1
 4140              CONTINUE
                 ELSE IF ( CLASS .EQ. GVH__STA ) THEN
                   DO 4150 ISEG3=1,GVH%CACHE%NUM_STA
                      DO 4160 ISEG4=1,GVH%CACHE%NOBS_STA(ISEG3)
!
! ---------------------- Create primary and secondary indices
!
                         ILN = ISL+8
                         CALL CLRCH ( LINE(ILN+1:) )
                         CALL INCH ( ISEG4, LINE(ILN+2:) )
!
                         ILN = ILEN(LINE)
                         CALL INCH ( ISEG3, LINE(ILN+2:) )
                         ILN_BEG = ILEN(LINE)
!
                         CALL MEMCPY ( GVH_DESC, %VAL(ADR_DATA), &
     &                                           %VAL(SIZEOF(GVH_DESC)) )
                         ADR_DATA = ADR_DATA + SIZEOF(GVH_DESC)
                         IAD = GVH_DESC%ADR
!
! ---------------------- Write actual dimension of the array
!
                         CALL CLRCH ( LINE(ILN_BEG+1:) )
                         ILN = ILEN(LINE)
                         CALL INCH ( -GVH_DESC%DIMS(1), LINE(ILN+2:) )
!
                         ILN = ILEN(LINE)
                         CALL INCH ( -GVH_DESC%DIMS(2), LINE(ILN+2:) )
                         ILN_BEG = ILEN(LINE)
!
! ---------------------- Write the element
!
                         CALL GVH_PUT_INLINE ( GVH_UNIT, GVH_DESC%DIMS, TYP, &
     &                                         IAD, LINE, ILN_BEG, FL_UND, NREC )
                         NREC = NREC + 1
 4160                 CONTINUE
 4150              CONTINUE
                 ELSE IF ( CLASS .EQ. GVH__BAS ) THEN
                   DO 4170 ISEG5=1,GVH%CACHE%NUM_OBS
!
! ------------------- Baseline class. Create the primary index
!
                      ILN = ISL+8
                      CALL CLRCH ( LINE(ILN+1:) )
                      CALL INCH ( ISEG5, LINE(ILN+2:) )
!
                      ILN = ILEN(LINE)
                      LINE(ILN+2:) = '0' ! and dummy secondary index
                      ILN_BEG = ILEN(LINE)
!
                      CALL MEMCPY ( GVH_DESC, %VAL(ADR_DATA), &
     &                                        %VAL(SIZEOF(GVH_DESC)) )
                      ADR_DATA = ADR_DATA + SIZEOF(GVH_DESC)
                      IAD = GVH_DESC%ADR
!
! ------------------- Write actual dimension of the array
!
                      CALL CLRCH ( LINE(ILN_BEG+1:) )
                      ILN = ILEN(LINE)
                      CALL INCH ( -GVH_DESC%DIMS(1), LINE(ILN+2:) )
!
                      ILN = ILEN(LINE)
                      CALL INCH ( -GVH_DESC%DIMS(2), LINE(ILN+2:) )
                      ILN_BEG = ILEN(LINE)
!
! ------------------- Write the element
!
                      CALL GVH_PUT_INLINE ( GVH_UNIT, GVH_DESC%DIMS, TYP, &
     &                                      IAD, LINE, ILN_BEG, FL_UND, NREC )
                      NREC = NREC + 1
 4170              CONTINUE
              END IF
 4130      CONTINUE
      END IF 
!
! --- That's it. Write the total records counter
!
      CALL CLRCH ( LINE )
      LINE(1:5) = 'CHUN.'
      CALL INCH ( LSEG, LINE(6:) )
      LINE(ILEN(LINE)+2:) = '@chunk_size:'
      CALL INCH ( NREC+1, LINE(ILEN(LINE)+2:) )
      LINE(ILEN(LINE)+2:) = 'records'
      WRITE ( GVH_UNIT, '(A)' ) LINE(1:ILEN(LINE))
!
      CLOSE ( UNIT=GVH_UNIT )
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END  !#!  GVH_WRITE_AGV  #!#
