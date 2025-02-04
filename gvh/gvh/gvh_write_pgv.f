      SUBROUTINE GVH_WRITE_PGV ( GVH, FILENAME, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GVH_WRITE_PGV
! *                                                                      *
! * ### 03-DEC-2001  GVH_WRITE_PGV  v1.0 (c)  L. Petrov  03-DEC-2001 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      TYPE ( GVH__STRU ) ::  GVH
      CHARACTER  FILENAME*(*)
      INTEGER*4  IUER
      INTEGER*4  GVH_UNIT
      CHARACTER  STR*32, KEYWORD*256, VALUE*256, TITLE*256, &
     &           LINE*1024, LCODE*8, DESCR*80
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, N1, N2, IOS, IP, NREC, &
     &           ILN, ILN_CHA, ICL, ITP, NUM_DATA_ELEM, &
     &           SEG_IND, NUM_FIELDS, IER
      INTEGER*8  LCODE_I8
      ADDRESS__TYPE  IAD, IAD_END
      INTEGER*4  MLEN_C1, MLEN_I2, MLEN_I4, MLEN_R4, MLEN_R8, ADIM1, ADIM2
      PARAMETER  ( MLEN_C1 = 32*1024*GVH__C1_LEN )
      PARAMETER  ( MLEN_I2 = 32*1024*GVH__I2_LEN  )
      PARAMETER  ( MLEN_I4 = 32*1024*GVH__I4_LEN  )
      PARAMETER  ( MLEN_R4 = 32*1024*GVH__R4_LEN  )
      PARAMETER  ( MLEN_R8 = 32*1024*GVH__R8_LEN  )
      INTEGER*2  ARRAY_I2(MLEN_I2)
      REAL*8     ARRAY_R8(MLEN_R8)
      INTEGER*4  CLASS, TYP, DIMS(2), LEN_REC, LEN_DATA
      ADDRESS__TYPE  ADR_DATA, ADR_CONV
      INTEGER*1  TAB(0:255), TAB_VAL
      INTEGER*4  IND2, IND_1, IND_2, DIM1
      IND2(IND_1,IND_2,DIM1) = DIM1*(IND_2-1) + IND_1
      INTEGER*4, EXTERNAL :: LIB$SCANC, IFIND_PL, GET_UNIT, I_LEN, ILEN
!
! --- intialization of the delimiters table
!
      CALL NOUT ( 256, TAB )
      TAB_VAL = 1
      TAB(10) = TAB_VAL ! Carriage return -- record delimiter
      TAB(26) = TAB_VAL ! chapter delimiter
!
      IF ( GVH%STATUS .NE. GVH__INITIALIZED ) THEN
           CALL ERR_LOG ( 4131, IUER, 'GVH_WRITE_PGV', 'The GVH data '// &
     &         'structure was not initialized. Please, use gvh_init first' )
           RETURN
      END IF
!
      IF ( GVH%CACHE%OBS_STATUS  .NE.  GVH__POPULATED ) THEN
           CALL ERR_LOG ( 4132, IUER, 'GVH_WRITE_PGV', 'The GVH '// &
     &         'observations cache table has not been populated' )
           RETURN
      END IF
!
      IF ( GVH%CACHE%LCODE_STATUS  .NE.  GVH__POPULATED ) THEN
           CALL ERR_LOG ( 4133, IUER, 'GVH_WRITE_PGV', 'The GVH '// &
     &         'lcodes cache table has not been populated' )
           RETURN
      END IF
!
! --- Open the output file
!
      GVH_UNIT = GET_UNIT () ! Get free Fortran i/o unit
      OPEN ( UNIT=GVH_UNIT, FILE=FILENAME, STATUS='UNKNOWN', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 4134, IUER, 'GVH_WRITE_PGV', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to open the output file '// &
     &          FILENAME )
           RETURN
      END IF
!
! --- Write the first line: format label
!
      WRITE ( GVH_UNIT, '(A)' ) GVH__PGV_LABEL
      NREC = 1 ! unitialize records counter
      DO 410 J1=1,GVH%NSEG ! cycle over segments
!
! ------ Write preamble header
!
         WRITE ( GVH_UNIT, '(A,I6,A)' ) '@PREA  section_length: ', &
     &           GVH%PREA(J1)%NKWD, ' keywords'
         NREC = NREC + 1
!
! ------ Printing contents of the preamble section
!
         DO 420 J2=1,GVH%PREA(J1)%NKWD
            CALL MEMCPY ( %REF(KEYWORD), %VAL(GVH%PREA(J1)%KWD_ADR(J2)), &
     &                                   %VAL(GVH%PREA(J1)%KWD_LEN(J2)) )
            CALL MEMCPY ( %REF(VALUE),   %VAL(GVH%PREA(J1)%VAL_ADR(J2)), &
     &                                   %VAL(GVH%PREA(J1)%VAL_LEN(J2)) )
!              type *,' j2=',j2,' kwl=',gvh.prea(j1).kwd_len(j2) ! %%
!              type *,' j2=',j2,' kva=',gvh.prea(j1).val_len(j2) ! %%
            WRITE ( GVH_UNIT, '(A,A,1X,A)' ) ' ', &
     &              KEYWORD(1:GVH%PREA(J1)%KWD_LEN(J2)-1), &
     &              VALUE(1:GVH%PREA(J1)%VAL_LEN(J2)-1)
            NREC = NREC + 1
 420     CONTINUE
!
! ------ Write the header of the text section
!
         WRITE ( GVH_UNIT, '(A,I6,A)' ) '@TEXT   section_length: ', &
     &           GVH%TEXT(J1)%NTIT, ' chapters'
         NREC = NREC + 1
         DO 430 J3=1,GVH%TEXT(J1)%NTIT
            CALL CLRCH ( STR )
            CALL INCH  ( J3, STR )
!
! --------- Extract chapter's title
!
            CALL MEMCPY ( %REF(TITLE), %VAL(GVH%TEXT(J1)%TITLE_ADR(J3)), &
     &                                 %VAL(GVH%TEXT(J1)%TITLE_LEN(J3)-1) )
            WRITE ( GVH_UNIT, '(A,A,1X,A)' ) '@@chapter ', &
     &              STR(1:I_LEN(STR)), TITLE(1:GVH%TEXT(J1)%TITLE_LEN(J3)-1)
            NREC = NREC + 1
            IAD = GVH%TEXT(J1)%BODY_ADR(J3)
            ILN_CHA = GVH%TEXT(J1)%BODY_LEN(J3)
            IAD_END = GVH%TEXT(J1)%BODY_ADR(J3) + ILN_CHA-1
!
! --------- Cycle on records in the chapter
!
            DO 440 J4=1,1024*1024*1024
!
! ------------ Seach for delimiter
!
               IP = LIB$SCANC ( %VAL(IAD), TAB, TAB_VAL, %VAL(ILN_CHA) )
               IF ( IP .EQ. 0 ) THEN
                    CALL ERR_LOG ( 4135, IUER, 'GVH_WRITE_PGV', 'Trap of '// &
     &                  'internal control: text section is corrupted' )
                    RETURN
               END IF
!
               ILN = MIN ( IP-1, ILN_CHA, LEN(LINE) )
               IF ( ILN .LE. 0 ) THEN
                    WRITE ( GVH_UNIT, '(A)' ) ' '
                    NREC = NREC + 1
                  ELSE
!
! ----------------- Write the next line of the chapter's body
!
                    CALL MEMCPY ( %REF(LINE), %VAL(IAD), %VAL(ILN) )
                    WRITE ( GVH_UNIT, '(A,A)' ) ' ', LINE(1:ILN)
                    NREC = NREC + 1
               END IF
               IAD = IAD + IP
               ILN_CHA = ILN_CHA - IP
               IF ( IAD .GE. IAD_END ) GOTO 840
 440        CONTINUE
 840        CONTINUE
 430     CONTINUE
!
! ------ TOCS section
!
         WRITE ( GVH_UNIT, '(A)' ) '@TOCS'
         NREC = NREC + 1
!
! ------ Cycle over records of the table of content
!
         NUM_DATA_ELEM = 0
         DO 450 J5=1,GVH%TOCS(J1)%NTOC
!
! --------- Retrieve contents of the definition of the J5-th lcode from the
! --------- table of conetnts
!
            IER = IUER
            CALL GVH_EXCH_LCODE1 ( .FALSE., %VAL(GVH%TOCS(J1)%ADR), J5, &
     &                             GVH__GET, LCODE, DESCR, CLASS, TYP,  &
     &                             DIMS, LEN_DATA, ADR_DATA, IER )
!
! --------- Retrieve contents of the defintion of the J5-th lcode from the
! --------- cache table of contents
!
            IER = IUER
            CALL MEMCPY ( LCODE_I8, LCODE )
            CALL GVH_LCODE_TAB_INQ ( %VAL(GVH%CACHE%LCODE_ADR), &
     &                               GVH%CACHE%NUM_LCODE, LCODE, LCODE_I8, &
     &                               GVH%LCODE_CACHE_I8, GVH%IND_LCODE_CACHE, &
     &                               DESCR, CLASS, TYP, DIMS, LEN_REC, &
     &                               LEN_DATA, SEG_IND, NUM_FIELDS, ADR_DATA, &
     &                               ADR_CONV, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 4136, IUER, 'GVH_WRITE_PGV', 'Error in '// &
     &               'GVH_LCODE_TAB_INQ ' )
                 RETURN 
            END IF
!
! --------- Format the output record
!
            CALL CLRCH ( LINE )
            LINE = ' '//LCODE
            ILN = 11
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
! --------- Now we write the tocs line down
!
            WRITE ( GVH_UNIT, '(A)' ) LINE(1:ILEN(LINE))
            NREC = NREC + 1
!
! --------- Count the number of elements in the data sections
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
 450     CONTINUE
!
! ------ Starting point for Anne-Marie!!
!
         WRITE ( GVH_UNIT, '(A)' ) '@DATA'
!
         DO 460 J6=1,GVH%CACHE%NUM_OBS
            WRITE ( GVH_UNIT, '(A,I6)' ) '@Observation ',J6
            DO 470 J7=1,GVH%TOCS(J1)%NTOC
!
! ------------ Retrieve contents of the definition of the J5-th lcode from the
! ------------ table of conetnts
!
               IER = IUER
               CALL GVH_EXCH_LCODE1 ( .FALSE., %VAL(GVH%TOCS(J1)%ADR), J7, &
     &                                GVH__GET, LCODE, DESCR, CLASS, TYP,  &
     &                                DIMS, LEN_DATA, ADR_DATA, IER )
               IF ( CLASS .EQ. GVH__BAS ) THEN
                    IF ( TYP .EQ. GVH__C1 ) THEN
                       ELSE IF ( TYP .EQ. GVH__I2 ) THEN
                         IER = IUER
                         CALL GVH_GLCODE ( GVH, LCODE, J6, 0, MLEN_I2, ADIM1, &
     &                                     ADIM2, ARRAY_I2, IER )
!
                         IF ( DIMS(1) .GT. 0 ) THEN
!                              WRITE ( 6, * ) ' adim1=',adim1,' adim2=',adim2 ! %%%
                            WRITE ( GVH_UNIT, '(A, I6)' ) &
     &                       (( ARRAY_I2(IND2(N1,N2,ADIM1)), N1=1,ADIM1), N2=1,ADIM2 )
                         END IF
                       ELSE IF ( TYP .EQ. GVH__I4 ) THEN
                       ELSE IF ( TYP .EQ. GVH__R4 ) THEN
                       ELSE IF ( TYP .EQ. GVH__R8 ) THEN
                         IER = IUER
                         CALL GVH_GLCODE ( GVH, LCODE, J6, 0, MLEN_R8, ADIM1, &
     &                                     ADIM2, ARRAY_R8, IER )
!
                         IF ( DIMS(1) .GT. 0 ) THEN
!                              type *,' adim1=',adim1,' adim2=',adim2 ! %%%
                            WRITE ( GVH_UNIT, * ) LCODE, &
     &                       (( ARRAY_R8(IND2(N1,N2,ADIM1)), N1=1,ADIM1), N2=1,ADIM2 )
                         END IF
                    END IF
               END IF
 470        CONTINUE
 460     CONTINUE
!
         WRITE ( GVH_UNIT, '(A)' ) '@END_OF_SEGMENT'
 410  CONTINUE
!
! --- That's it.
!
      CLOSE ( UNIT=GVH_UNIT )
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END  SUBROUTINE  GVH_WRITE_PGV  !#!#
