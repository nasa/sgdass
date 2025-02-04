      SUBROUTINE GVH_WRITE_BGV ( GVH, ISEG, OPCODE, FILENAME, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GVH_WRITE_BGV  writes the contents of one or more         *
! *   segments into the output file in binary GVH format. If ISEG=0,     *
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
! *                           be written,                                *
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
! * ### 25-NOV-2001  GVH_WRITE_BGV  v2.7 (c)  L. Petrov  12-FEB-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      TYPE     ( GVH__STRU      ) :: GVH
      TYPE     ( GVH_PREF__STRU ) :: GVH_PREF
      TYPE     ( GVH_DESC__STRU ) :: GVH_DESC
      TYPE     ( GVH_LCD__STRU  ) :: LCD(GVH__MTOC)
      CHARACTER  FILENAME*(*)
      CHARACTER    KEYWORD_DEL, CHAPTER_DEL*1, RECORD_DEL*1
      CHARACTER    LCODE*8, DESCR*80
      PARAMETER  ( KEYWORD_DEL = CHAR(10) )
      PARAMETER  ( CHAPTER_DEL = CHAR(26) )
      PARAMETER  ( RECORD_DEL  = CHAR(10) )
      INTEGER*4  ISEG, OPCODE, IUER
      INTEGER*4  CLASS, TYP, DIMS(2), LEN_REC, NUM_FIELDS, &
     &           LEN_LCODE_DATA, LEN_DATA
      ADDRESS__TYPE  ADR_CONV, ADR_LCODE_DATA, ADR_DATA, &
     &               ADR_ARRAY(GVH__MTOC)
      INTEGER*4  GVH_FILDES, J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, &
     &           ILN, IS, ILN_TOT, IND_TOC, LEN_TOC, LEN_HEAP, &
     &           SEG_IND, VAL_CNST, LEN_CNST, BYTES_WRITTEN, &
     &           LEN_TOC_SAVE, IER
      ADDRESS__TYPE  IAD, IAD_HEAP, ADR_TEMP_COPY, ADR_DATA_ORIG, &
     &               ADR_TOCS_SAVE, ADR_PRE_SAVE,  ADR_TEXT_SAVE
      CHARACTER  CURRENT_DATE*26, USER_NAME*128, USER_REALNAME*128, &
     &           USER_E_ADDRESS*128, WHOAMI*128, KEYWORD_STR*128, &
     &           VALUE_STR*128, STR*32, STR1*32, STR2*80, STR3*80
      INTEGER*8  LCODE_I8
      LOGICAL*4  LEX, FL_CREAT_PRE 
      INTEGER*4    ALGN
      PARAMETER  ( ALGN = 16 )
      INTEGER*1  PADDING(ALGN)
      CHARACTER, EXTERNAL :: GET_TZ_CDATE*26
#ifdef GNU
      INTEGER*4, EXTERNAL :: GVH_COMPAR_LCD  
#else
      INTEGER*2, EXTERNAL :: GVH_COMPAR_LCD  
#endif
      INTEGER*4, EXTERNAL :: INT4_ALIGN, I_LEN, LSEEK, UNLINK
!
      IF ( GVH%STATUS .NE. GVH__INITIALIZED ) THEN
           CALL ERR_LOG ( 4511, IUER, 'GVH_WRITE_BGV', 'The GVH data '// &
     &         'structure was not initialized. Please, use gvh_init first' )
           RETURN
      END IF
!
      IF ( GVH%CACHE%OBS_STATUS  .NE.  GVH__POPULATED ) THEN
           CALL ERR_LOG ( 4512, IUER, 'GVH_WRITE_BGV', 'The GVH '// &
     &         'observations cache table has not been populated' )
           RETURN
      END IF
!
      IF ( GVH%CACHE%LCODE_STATUS  .NE.  GVH__POPULATED ) THEN
           CALL ERR_LOG ( 4513, IUER, 'GVH_WRITE_BGV', 'The GVH '// &
     &         'observations cache table has not been populated' )
           RETURN
      END IF
      CALL NOUT ( ALGN, PADDING )
!
      IF ( ISEG .LE.  0 ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( ISEG, STR )
           CALL ERR_LOG ( 4514, IUER, 'GVH_WRITE_BGV', 'Wrong parameter '// &
     &         'ISEG: '//STR )
           RETURN
      END IF
!
      IF ( ISEG .GT. GVH%NSEG ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( ISEG, STR )
           CALL CLRCH ( STR1 ) 
           CALL INCH  ( GVH%NSEG, STR1 )
           CALL ERR_LOG ( 4515, IUER, 'GVH_WRITE_BGV', 'Wrong parameter '// &
     &         'ISEG: '//STR(1:I_LEN(STR))//' -- it exceeds the total '// &
     &         'numer of segments GVH%NSEG: '//STR1 )
           RETURN
      END IF
!
      IF ( OPCODE .NE. GVH__CRT  .AND.  OPCODE .NE. GVH__APP ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( OPCODE, STR )
           CALL ERR_LOG ( 4516, IUER, 'GVH_WRITE_BGV', 'Wrong parameter '// &
     &         'OPCODE: '//STR(1:I_LEN(OPCODE))//' only GVH__CRT and '// &
     &         'GVH__APP are supported' )
           RETURN
      END IF
!
      IF ( OPCODE .EQ. GVH__CRT  ) THEN
           INQUIRE ( FILE=FILENAME, EXIST=LEX )
           IF ( LEX ) THEN
                IS = UNLINK ( FILENAME(1:I_LEN(FILENAME))//CHAR(0) )
                IF ( IS .NE. 0 ) THEN
                     CALL GERROR ( STR )
                     CALL ERR_LOG ( 4517, IUER, 'GVH_WRITE_BGV', 'Error "'// &
     &                    STR(1:I_LEN(STR))//'" in an attempt to remove '// &
     &                   'the old database file '//FILENAME )
                     RETURN
                END IF
           END IF
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
           IF ( GVH%PREA(GVH%SEG)%NKWD .GE. 1 ) THEN
                IER = 0
                CALL GVH_GPREA ( GVH, 1, GVH%PREA(GVH%SEG)%NKWD, &
     &                           KEYWORD_STR, VALUE_STR, IER )
                IF ( INDEX( KEYWORD_STR, 'CREATED_BY:' ) > 0 ) THEN
                     FL_CREAT_PRE = .TRUE. ! yes it was
                END IF
           END IF
!
           IF ( FL_CREAT_PRE ) THEN 
!@!
!@! ------------- If was defined, remove two last preamble records
!@!
!@                IER = IUER
!@                CALL GVH_FREE ( GVH, GVH%PREA(1)%KWD_ADR(GVH%PREA(1)%NKWD), IER )
!@                IF ( IER .NE. 0 ) THEN
!@                     WRITE ( 6, * ) 'GVH%PREA(1)%NKWD= ', GVH%PREA(1)%NKWD ! %%%
!@                     WRITE ( 6, * ) 'GVH_ADR= ', GVH%PREA(1)%KWD_ADR(GVH%PREA(1)%NKWD) ! %%%
!@                     CALL ERR_LOG ( 4518, IUER, 'GVH_WRITE_BGV', 'Trap of '// &
!@     &                   'internal control: cannot free memory of the last '// &
!@     &                   'by one preamble record' )
!@                     RETURN 
!@                END IF
!@                CALL GVH_FREE ( GVH, GVH%PREA(1)%VAL_ADR(GVH%PREA(1)%NKWD), IER )
!@                CALL GVH_FREE ( GVH, GVH%PREA(1)%KWD_ADR(GVH%PREA(1)%NKWD-1), IER )
!@                CALL GVH_FREE ( GVH, GVH%PREA(1)%VAL_ADR(GVH%PREA(1)%NKWD-1), IER )
                GVH%PREA(GVH%SEG)%NKWD = GVH%PREA(GVH%SEG)%NKWD - 2
           END IF
!
! -------- Get the current date and the user name
!
           CURRENT_DATE = GET_TZ_CDATE()
           CALL GETINFO_USER ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
           WHOAMI = USER_REALNAME(1:I_LEN(USER_REALNAME))//' ( '// &
     &              USER_E_ADDRESS(1:I_LEN(USER_E_ADDRESS))//' )'
!
! -------- Add preamble record CREATED_AT
!
           IER = IUER
           CALL GVH_PPREA ( GVH, 1, 'CREATED_AT:', CURRENT_DATE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4519, IUER, 'GVH_WRITE_BGV', 'Error in '// &
     &              'an attempt to update preamble record and to put '// &
     &              'modification date stamp' )
                RETURN 
           END IF
!
! -------- Add preamble record CREATED_BY
!
           IER = IUER
           CALL GVH_PPREA ( GVH, 1, 'CREATED_BY:', WHOAMI, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4520, IUER, 'GVH_WRITE_BGV', 'Error in '// &
     &              'an attempt to update preamble record and to put '// &
     &              'the person name how has modified the file' )
                RETURN 
           END IF
!
!%           CALL ERR_PASS  ( IUER, IER )
!%           CALL GVH_PPREA ( GVH, 1, 'FILENAME:', FILENAME, IER )
!%           IF ( IER .NE. 0 ) THEN
!%                CALL ERR_LOG ( 4521, IUER, 'GVH_WRITE_BGV', 'Error in '// &
!%     &              'an attempt to update preamble record and to put '// &
!%     &              'there the file name' )
!%                RETURN 
!%           END IF
      END IF
!
      IER = IUER
      IF ( OPCODE .EQ. GVH__CRT  ) THEN
           CALL BINF_OPEN ( FILENAME, 'NEW', GVH_FILDES, IER )
         ELSE IF ( OPCODE .EQ. GVH__APP ) THEN
           CALL BINF_OPEN ( FILENAME, 'UNKNOWN', GVH_FILDES, IER )
           CALL GET_SYSTEM_CONSTANT ( 'SEEK_END', VAL_CNST, LEN_CNST ) 
           IS = LSEEK ( %VAL(GVH_FILDES), %VAL(0), %VAL(VAL_CNST) )
           IF ( IS .LE. 0 )  THEN
                CALL ERR_LOG ( 4522, IUER, 'GVH_WRITE_BGV', 'Error '// &
     &              'in repositioning the offset of the output file '// &
     &              FILENAME(1:I_LEN(FILENAME))//' to the end of file' )
                RETURN
           END IF
      END IF
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4523, IUER, 'GVH_WRITE_BGV', 'Error '// &
     &         ' in an attempt to open the output file '//FILENAME )
           RETURN
      END IF
!
! --- Write the label
!
      IF ( OPCODE .EQ. GVH__CRT  ) THEN
           IER = IUER
#ifdef BIG_ENDIAN
           CALL GVH_WRITE_BIN ( GVH_FILDES, GVH__BGV_LABEL_BIG_ENDIAN, &
     &                          LEN(GVH__BGV_LABEL_BIG_ENDIAN), IER )
#else
           CALL GVH_WRITE_BIN ( GVH_FILDES, GVH__BGV_LABEL_LITTLE_ENDIAN, &
     &                          LEN(GVH__BGV_LABEL_LITTLE_ENDIAN), IER )
#endif
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4524, IUER, 'GVH_WRITE_BGV', 'Error during '// &
     &              'writing the first record with the format label into '// &
     &              'the output file '//FILENAME )
                RETURN
           END IF
      END IF
!
! --- Consolidation of preamble section.
! --- We do it in two passes. First to learn the section length, then
! --- put all records of the section in one block by adding delimeters.
!
      ILN = 0
      DO 420 J2=1,GVH%PREA(ISEG)%NKWD
         ILN = ILN + GVH%PREA(ISEG)%KWD_LEN(J2)
         ILN = ILN + GVH%PREA(ISEG)%VAL_LEN(J2)
 420  CONTINUE
      GVH%PREA(ISEG)%LEN = INT4_ALIGN( ILN, ALGN )
!
      IF ( GVH%PREA(ISEG)%LEN > 0 ) THEN
           IER = IUER
           CALL GVH_ALLOCATE ( GVH, GVH%PREA(ISEG)%LEN, &
     &                         ADR_PRE_SAVE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4525, IUER, 'GVH_WRITE_BGV', 'The error in '// &
     &              'an attempt to allocate memory for consolidated preamble '// &
     &              'section' )
                RETURN
           END IF
      END IF
!
      IAD = ADR_PRE_SAVE
      ILN_TOT = 0
      DO 430 J3=1,GVH%PREA(ISEG)%NKWD
!
! ------ Copy the keyword
!
         ILN = GVH%PREA(ISEG)%KWD_LEN(J3)
         IF ( ILN .GT. 0 ) CALL MEMCPY ( %VAL(IAD), &
     &                     %VAL(GVH%PREA(ISEG)%KWD_ADR(J3)) , %VAL(ILN) )
         IAD = IAD + ILN
         ILN_TOT = ILN_TOT + ILN
!
! ------ Copy the value
!
         ILN = GVH%PREA(ISEG)%VAL_LEN(J3)
         IF ( ILN .GT. 0 ) CALL MEMCPY ( %VAL(IAD), &
     &                     %VAL(GVH%PREA(ISEG)%VAL_ADR(J3)), %VAL(ILN) )
         IAD = IAD + ILN
         ILN_TOT = ILN_TOT + ILN
 430  CONTINUE
      IF ( ILN_TOT .LT. GVH%PREA(ISEG)%LEN ) THEN
           CALL NOUT ( GVH%PREA(ISEG)%LEN-ILN_TOT, %VAL(IAD) )
      END IF
!
      IER = 0
      CALL GVH_FREE ( GVH, GVH%PREA(ISEG)%ADR, IER )
      IF ( GVH%PREA(ISEG)%LEN > 0 ) THEN
           GVH%PREA(ISEG)%ADR = ADR_PRE_SAVE
         ELSE
           GVH%PREA(ISEG)%ADR = 0
      END IF
      GVH_PREF%NAME = GVH__PREA
      GVH_PREF%LEN  = GVH%PREA(ISEG)%LEN
!
! --- Write the prefix of preamble section
!
      IER = IUER
      CALL GVH_WRITE_BIN ( GVH_FILDES, GVH_PREF%NAME, SIZEOF(GVH_PREF), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4526, IUER, 'GVH_WRITE_BGV', 'Error during '// &
     &         'writing the prefix of the preamble section into the file '// &
     &          FILENAME )
           RETURN
      END IF
!
! --- Write the preamble section
!
      IER = IUER
      CALL GVH_WRITE_BIN ( GVH_FILDES, %VAL(GVH%PREA(ISEG)%ADR), &
     &                     GVH%PREA(ISEG)%LEN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4527, IUER, 'GVH_WRITE_BGV', 'Error during '// &
     &         'writing consolidated preamble section into the file '// &
     &          FILENAME )
           RETURN
      END IF
!
! --- No we do the same with text section: coonsoldation
! --- We do it in two passes. First to learn the section length, then
! --- put all records of the section by one block
! --- (we don't add delimieters since title and body already have them)
!
      ILN = 0
      DO 440 J4=1,GVH%TEXT(ISEG)%NTIT
         ILN = ILN + GVH%TEXT(ISEG)%TITLE_LEN(J4)
         ILN = ILN + GVH%TEXT(ISEG)%BODY_LEN(J4)
 440  CONTINUE
      GVH%TEXT(ISEG)%LEN = INT4_ALIGN( ILN, ALGN )
!
      IF ( GVH%TEXT(ISEG)%LEN > 0 ) THEN
           IER = IUER
           CALL GVH_ALLOCATE ( GVH, GVH%TEXT(ISEG)%LEN, &
     &                         ADR_TEXT_SAVE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4528, IUER, 'GVH_WRITE_BGV', 'The error in '// &
     &              'an attempt to allocate memory for consolidated '// &
     &              'text section' )
                RETURN
           END IF
         ELSE
           GVH%TEXT(ISEG)%ADR = 0
      END IF
!
      IAD = ADR_TEXT_SAVE
      ILN_TOT = 0
      DO 450 J5=1,GVH%TEXT(ISEG)%NTIT
!
! ------ Copy the title
!
         ILN = GVH%TEXT(ISEG)%TITLE_LEN(J5)
         IF ( ILN .GT. 0 ) CALL MEMCPY ( %VAL(IAD), &
     &                     %VAL(GVH%TEXT(ISEG)%TITLE_ADR(J5)), %VAL(ILN) )
         IAD = IAD + ILN
         ILN_TOT = ILN_TOT + ILN
!
! ------ Copy the value
!
         ILN = GVH%TEXT(ISEG)%BODY_LEN(J5)
         IF ( ILN .GT. 0 ) CALL MEMCPY ( %VAL(IAD), &
     &                     %VAL(GVH%TEXT(ISEG)%BODY_ADR(J5)), %VAL(ILN) )
         IAD = IAD + ILN
         ILN_TOT = ILN_TOT + ILN
 450  CONTINUE
      IF ( ILN_TOT .LT. GVH%TEXT(ISEG)%LEN ) THEN
           CALL NOUT ( GVH%TEXT(ISEG)%LEN-ILN_TOT, %VAL(IAD) )
      END IF
!
      IER = 0
      CALL GVH_FREE ( GVH, GVH%TEXT(ISEG)%ADR, IER )
      IF ( GVH%TEXT(ISEG)%LEN > 0 ) THEN
           GVH%TEXT(ISEG)%ADR = ADR_TEXT_SAVE
         ELSE
           GVH%TEXT(ISEG)%ADR = 0
      END IF
!
      GVH_PREF%NAME = GVH__TEXT
      GVH_PREF%LEN  = GVH%TEXT(ISEG)%LEN
!
! --- Write the prefix of text section
!
      IER = IUER
      CALL GVH_WRITE_BIN ( GVH_FILDES, GVH_PREF%NAME, SIZEOF(GVH_PREF), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4529, IUER, 'GVH_WRITE_BGV', 'Error during '// &
     &         'writing the prefix of the TEXT section into the file '// &
     &          FILENAME )
           RETURN
      END IF
!
! --- Write the text section
!
      IER = IUER
      CALL GVH_WRITE_BIN ( GVH_FILDES, %VAL(GVH%TEXT(ISEG)%ADR), &
     &                     GVH%TEXT(ISEG)%LEN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4530, IUER, 'GVH_WRITE_BGV', 'Error during '// &
     &         'writing consolidated text section into the file '// &
     &          FILENAME )
           RETURN
      END IF
!
! --- Learn the actual length of toc section
!
      LEN_TOC = INT4_ALIGN( GVH%TOCS(ISEG)%NTOC*GVH__LCODE1_LEN, ALGN )
!
      GVH_PREF%NAME = GVH__TOCS
      GVH_PREF%LEN  = LEN_TOC
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
! --- Learn the size of the data section
!
      LEN_DATA = 0
      ADR_DATA = 0
      DO 470 J7=1,GVH%TOCS(ISEG)%NTOC
         IND_TOC = LCD(J7)%IND_TOC
         CALL GVH_EXCH_LCODE1 ( .FALSE., %VAL(GVH%TOCS(ISEG)%ADR), IND_TOC, &
     &                          GVH__GET, LCODE, DESCR, CLASS, TYP, DIMS, &
     &                          LEN_LCODE_DATA, ADR_LCODE_DATA, IER )
         LEN_DATA = LEN_DATA + LEN_LCODE_DATA
         ADR_ARRAY(J7) = ADR_LCODE_DATA ! save actual address
!
! ------ We put the offset with respect to the DATA section into the
! ------ ADR_DATA field
!
         CALL GVH_EXCH_LCODE1 ( .FALSE., %VAL(GVH%TOCS(ISEG)%ADR), IND_TOC, &
     &                          GVH__PUT, LCODE, DESCR, CLASS, TYP, DIMS, &
     &                          LEN_LCODE_DATA, ADR_DATA, IER )
         ADR_DATA = ADR_DATA + LEN_LCODE_DATA
 470  CONTINUE
      LEN_DATA = INT4_ALIGN( LEN_DATA, ALGN )
#ifdef ADR_32BIT
       ADR_TOCS_SAVE = GVH%TOCS(ISEG)%ADR
       LEN_TOC_SAVE  = LEN_TOC
       LEN_TOC = INT4_ALIGN( GVH%TOCS(ISEG)%NTOC*GVH__LCODE1_LEN__64BIT, ALGN )
       CALL ERR_PASS ( IUER, IER )
       CALL GVH_ALLOCATE ( GVH, LEN_TOC, GVH%TOCS(ISEG)%ADR, IER )
       IF ( IER .NE. 0 ) THEN
            CALL CLRCH ( STR )
            CALL INCH  ( LEN_TOC, STR )
            CALL ERR_LOG ( 4531, IUER, 'GVH_WRITE_BGV', 'Error during '// &
     &          'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &          ' bytes of dynamic memory for a temporary TOCS array' )
            RETURN
       END IF
       CALL MEMCPY ( %VAL(GVH%TOCS(ISEG)%ADR), %VAL(ADR_TOCS_SAVE), &
     &               %VAL(LEN_TOC_SAVE) ) 
       CALL GVH_TOCS_COMPAT ( TOCS_32BIT__64BIT, &
     &                        GVH%TOCS(ISEG)%NTOC, &
     &                        %VAL(GVH%TOCS(ISEG)%ADR) )
       GVH_PREF%LEN = LEN_TOC
#endif
!
! --- Write the prefix of tocs section
!
      IER = IUER
      CALL GVH_WRITE_BIN ( GVH_FILDES, GVH_PREF%NAME, SIZEOF(GVH_PREF), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4531, IUER, 'GVH_WRITE_BGV', 'Error during '// &
     &         'writing the prefix of the TOCS section into the file '// &
     &          FILENAME )
           RETURN
      END IF
!
      IER = IUER
      CALL GVH_WRITE_BIN ( GVH_FILDES, %VAL(GVH%TOCS(ISEG)%ADR), LEN_TOC, &
     &                     IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4532, IUER, 'GVH_WRITE_BGV', 'Error during '// &
     &         'writing consolidated toc section into the file '// &
     &          FILENAME )
           RETURN
      END IF
#ifdef ADR_32BIT
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_FREE ( GVH, GVH%TOCS(ISEG)%ADR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4533, IUER, 'GVH_WRITE_BGV', 'Trap of '// &
     &         'internal control: cannot release memory' )
           RETURN
      END IF
      GVH%TOCS(ISEG)%ADR = ADR_TOCS_SAVE 
      LEN_TOC = LEN_TOC_SAVE  
#endif
!
      GVH_PREF%NAME = GVH__DATA
      GVH_PREF%LEN  = LEN_DATA
!
! --- Write the prefix of data section
!
      IER = IUER
      CALL GVH_WRITE_BIN ( GVH_FILDES, GVH_PREF%NAME, SIZEOF(GVH_PREF), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4534, IUER, 'GVH_WRITE_BGV', 'Error during '// &
     &         'writing the prefix of the DATA section into the file '// &
     &          FILENAME )
           RETURN
      END IF
!
      LEN_HEAP = 0
      BYTES_WRITTEN = 0
      DO 480 J8=1,GVH%TOCS(ISEG)%NTOC
         IND_TOC = LCD(J8)%IND_TOC
         IER = IUER
         CALL GVH_EXCH_LCODE1 ( .FALSE., %VAL(GVH%TOCS(ISEG)%ADR), IND_TOC, &
     &                          GVH__GET, LCODE, DESCR, CLASS, TYP, DIMS, &
     &                          LEN_LCODE_DATA, ADR_DATA, IER )
         ADR_DATA = ADR_ARRAY(J8) ! get actual address
         IF ( DIMS(1) .LT. 0 ) THEN
              IER = 0
              CALL GVH_FREE ( GVH, ADR_TEMP_COPY, IER )
!
              IER = IUER
              CALL GVH_ALLOCATE ( GVH, LEN_LCODE_DATA, ADR_TEMP_COPY, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4535, IUER, 'GVH_WRITE_BGV', 'Error '// &
     &                 'in an attempt to allocate memory for temporary array' )
                   RETURN
              END IF
!
! ----------- Save array of descriptors
!
              CALL MEMCPY ( %VAL(ADR_TEMP_COPY), %VAL(ADR_DATA), &
     &                      %VAL(LEN_LCODE_DATA) )
              ADR_DATA_ORIG = ADR_DATA
!
              IER = IUER
              CALL MEMCPY ( LCODE_I8, LCODE )
              CALL GVH_LCODE_TAB_INQ ( %VAL(GVH%CACHE%LCODE_ADR), &
     &                            GVH%CACHE%NUM_LCODE, LCODE, LCODE_I8, &
     &                            GVH%LCODE_CACHE_I8, GVH%IND_LCODE_CACHE, &
     &                            DESCR, CLASS, TYP, DIMS, LEN_REC, LEN_DATA, &
     &                            SEG_IND, NUM_FIELDS, ADR_DATA, ADR_CONV, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4536, IUER, 'GVH_WRITE_BGV', 'Error in '// &
     &                 'GVH_LCODE_TAB_INQ ' )
                   RETURN 
              END IF
              DO 490 J9=1,NUM_FIELDS
!
! -------------- Get descriptor of the J9-th field of the lcode for an
! -------------- array of variable length
!
                 CALL MEMCPY ( GVH_DESC, %VAL(ADR_DATA), %VAL(SIZEOF(GVH_DESC)) )
!
! -------------- Replace the absolute address with the offset relative to
! -------------- beginning of HEAP section
!
                 GVH_DESC%ADR = LEN_HEAP
!
! -------------- ... and put the descriptor back
!
                 CALL MEMCPY ( %VAL(ADR_DATA), GVH_DESC, %VAL(SIZEOF(GVH_DESC)) )
!
                 LEN_HEAP = LEN_HEAP + GVH__TYPE_LEN(TYP)*GVH_DESC%DIMS(1)* &
     &                                                    GVH_DESC%DIMS(2)
                 ADR_DATA = ADR_DATA + SIZEOF(GVH_DESC)
 490          CONTINUE
              ADR_DATA = ADR_DATA_ORIG
         END IF
!
         IER = IUER
         CALL GVH_WRITE_BIN ( GVH_FILDES, %VAL(ADR_DATA), LEN_LCODE_DATA, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4537, IUER, 'GVH_WRITE_BGV', 'Error during '// &
     &            'writing data section for lcode '//LCODE//' into the file '// &
     &             FILENAME )
              RETURN
         END IF
         IF ( DIMS(1) .LT. 0 ) THEN
!
! ----------- Restore array of descriptors
!
              CALL MEMCPY ( %VAL(ADR_DATA), %VAL(ADR_TEMP_COPY), &
     &                      %VAL(LEN_LCODE_DATA) )
!
! ----------- ... and free dynamic memory for temporary array
!
              IER = IUER
              CALL GVH_FREE ( GVH, ADR_TEMP_COPY, IER )
              IF ( IER .NE. 0 ) THEN
                   WRITE ( 6, * ) ' ADR_DATA=',ADR_DATA
                   CALL ERR_LOG ( 4538, IUER, 'GVH_WRITE_BGV', 'Trap of '// &
     &                 'internal control: address does not exist :-(' )
                   RETURN
              END IF
         END IF
         BYTES_WRITTEN = BYTES_WRITTEN + LEN_LCODE_DATA
 480  CONTINUE
!
      IF ( BYTES_WRITTEN < LEN_DATA ) THEN
           IER = IUER
           CALL GVH_WRITE_BIN ( GVH_FILDES, PADDING, &
     &                          LEN_DATA-BYTES_WRITTEN, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4539, IUER, 'GVH_WRITE_BGV', 'Error during '// &
     &              'writing padding in the DATA section for lcode into '// &
     &              ' file '//FILENAME )
                RETURN
           END IF
      END IF
!
      GVH_PREF%NAME = GVH__HEAP
      GVH_PREF%LEN  = LEN_HEAP
!
! --- Write the prefix of the heap section
!
      IER = IUER
      CALL GVH_WRITE_BIN ( GVH_FILDES, GVH_PREF%NAME, SIZEOF(GVH_PREF), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4540, IUER, 'GVH_WRITE_BGV', 'Error during '// &
     &         'writing the prefix of the HEAP section into the file '// &
     &          FILENAME )
           RETURN
      END IF
!
      IF ( LEN_HEAP .GT. 0 ) THEN
!
! -------- Look for lcodes and pick up the lcode for arrays of variable length
!
           DO 4100 J10=1,GVH%TOCS(ISEG)%NTOC
              IND_TOC = LCD(J10)%IND_TOC
              IER = IUER
              CALL GVH_EXCH_LCODE1 ( .FALSE., %VAL(GVH%TOCS(ISEG)%ADR),   &
     &                               IND_TOC, GVH__GET, LCODE, DESCR, CLASS, &
     &                               TYP, DIMS, LEN_LCODE_DATA, ADR_DATA, IER )
              ADR_DATA = ADR_ARRAY(J10) ! get actual address
              IF ( DIMS(1) .LT. 0 ) THEN
!
! ---------------- Inquire information about this lcode
!
                   CALL MEMCPY ( LCODE_I8, LCODE )
                   CALL GVH_LCODE_TAB_INQ ( %VAL(GVH%CACHE%LCODE_ADR), &
     &                            GVH%CACHE%NUM_LCODE, LCODE, LCODE_I8, &
     &                            GVH%LCODE_CACHE_I8, GVH%IND_LCODE_CACHE, &
     &                            DESCR, CLASS, TYP, DIMS, LEN_REC, LEN_DATA, &
     &                            SEG_IND, NUM_FIELDS, ADR_DATA, ADR_CONV, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 4541, IUER, 'GVH_WRITE_BGV', &
     &                      'Error in GVH_LCODE_TAB_INQ ' )
                        RETURN 
                   END IF
!
! ---------------- Gather values of this lcode into one block
!
                   DO 4110 J11=1,NUM_FIELDS
!
! ------------------- Get descriptor
!
                      CALL MEMCPY ( GVH_DESC, %VAL(ADR_DATA), &
     &                                        %VAL(SIZEOF(GVH_DESC)) )
                      LEN_REC = GVH__TYPE_LEN(TYP)*GVH_DESC%DIMS(1)* &
     &                                             GVH_DESC%DIMS(2)
                      IF ( LEN_REC .GT. 0 ) THEN
!
! ------------------------ Write variable array on disc (if it is not empty)
!
                           IER = IUER
                           CALL GVH_WRITE_BIN ( GVH_FILDES, &
     &                                    %VAL(GVH_DESC%ADR), LEN_REC, IER )
                           IF ( IER .NE. 0 ) THEN
                                CALL ERR_LOG ( 4542, IUER, 'GVH_WRITE_BGV', &
     &                              'Error during writing heap section for '// &
     &                              'lcode '//LCODE//' into the file '// &
     &                               FILENAME )
                                RETURN
                           END IF
!
                           IAD_HEAP = IAD_HEAP + LEN_REC
                      END IF
                      ADR_DATA = ADR_DATA + SIZEOF(GVH_DESC)
 4110              CONTINUE
              END IF
 4100      CONTINUE
      END IF
!
! --- Deal done. Close the file
!
      IER = IUER
      CALL BINF_CLOSE ( GVH_FILDES, IER )
!
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END  !#!  GVH_WRITE_BGV  #!#
