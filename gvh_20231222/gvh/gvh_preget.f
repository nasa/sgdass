      SUBROUTINE GVH_PREGET ( GVH, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  GVH_PREGET  creates caches table and sorts it. Routine *
! *   GVH_PREGET should be called after reading the last segment of      *
! *   data from external files and before the first call of GVH_GLCODE   *
! *   for extraction information related to lcodes.                      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   NUMOBS ( INTEGER*4 ) -- Index of the segment where the preamble    *
! *                           section will be added. The index should    *
! *                           be in the range of available segments.     *
! *  KEYWORD ( CHARACTER ) -- String with the keyword. The keyword       *
! *                           itself should not contain characters with  *
! *                           decimal codes 10 and 26.                   *
! *   VALUE ( CHARACTER  ) -- String with the value which corresponds to *
! *                           this keyword. The value itself should not  *
! *                           contain characters with decimal codes 10   *
! *                           and 26.                                    *
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
! *  ### 22-NOV-2001   GVH_PREGET   v1.2 (c)  L. Petrov 23-JUN-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      INTEGER*4  IUER
      TYPE     ( GVH__STRU        ) :: GVH
      TYPE     ( GVH_LCODE2__STRU ) :: GVH_LCODE2_REC
      TYPE     ( GVH_DESC__STRU   ) :: GVH_DESC
      TYPE     ( GVH_LCT__STRU    ) :: LCT(GVH__MTOC)
      CHARACTER  C_CLD(GVH__MTOC)*8, C_CLD_ORIG(GVH__MTOC)*8, LCODE*8, &
     &           DESCR*80, STR1*32, STR2*32, FIL_SEG1*256, FIL_SEG2*256
      INTEGER*4  LCODE_SEG_IND(GVH__MTOC)
      INTEGER*4  CLASS, TYP, DIMS(2), LEN_DATA
      ADDRESS__TYPE ADR_DATA, ADR_DATA_ORIG, OBS_TAB_ADR
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, IC, IS, IP, N_CLD, &
     &           IND_SEG1, IND_SEG2, IND_LC1, IND_LC2, IER
#ifdef GNU
      INTEGER*4, EXTERNAL :: GVH_COMPAR_LCT 
#else
      INTEGER*2, EXTERNAL :: GVH_COMPAR_LCT 
#endif
      INTEGER*4, EXTERNAL :: I_LEN, LTM_DIF
!
      IF ( GVH%STATUS .NE. GVH__INITIALIZED ) THEN
           CALL ERR_LOG ( 4241, IUER, 'GVH_PREGET', 'The GVH data '// &
     &         'structure was not initialized. Please, use gvh_init first' )
           RETURN
      END IF
!
      IER = IUER
      CALL GVH_EXCH_LCODE1 ( GVH%ENDIAN_SWAP, %VAL(GVH%TOCS(1)%ADR), &
     &                       1, GVH__GET, LCODE, DESCR, CLASS, TYP, DIMS,  &
     &                       LEN_DATA, ADR_DATA, IER )
      IF ( LCODE .NE. 'NUMB_OBS' ) THEN
           CALL ERR_LOG ( 4242, IUER, 'GVH_PREGET', 'Mandatory lcode '// &
     &         'NUMB_OBS  is not the 1-st LCODE of the database. The first '// &
     &         'lcode is '//LCODE )
           RETURN
      END IF
      CALL MEMCPY ( GVH%CACHE%NUM_OBS, %VAL(GVH%DATA(1)%ADR+ADR_DATA), &
     &              %VAL(LEN_DATA) )
      IF ( GVH%ENDIAN_SWAP ) CALL ENDIAN_CNV_I4 ( GVH%CACHE%NUM_OBS )
!
      IER = IUER
      CALL GVH_EXCH_LCODE1 ( GVH%ENDIAN_SWAP, %VAL(GVH%TOCS(1)%ADR), &
     &                       2, GVH__GET, LCODE, DESCR, CLASS, TYP, DIMS,  &
     &                       LEN_DATA, ADR_DATA, IER )
      IF ( LCODE .NE. 'NUMB_SCA' ) THEN
           CALL ERR_LOG ( 4243, IUER, 'GVH_PREGET', 'Mandatory lcode '// &
     &         'NUMB_SCA is not the 2-nd LCODE of the database. The first '// &
     &         'lcode is '//LCODE )
           RETURN
      END IF
      CALL MEMCPY ( GVH%CACHE%NUM_SCA, %VAL(GVH%DATA(1)%ADR+ADR_DATA), &
     &              %VAL(LEN_DATA) )
      IF ( GVH%ENDIAN_SWAP ) CALL ENDIAN_CNV_I4 ( GVH%CACHE%NUM_SCA )
!
      IER = IUER
      CALL GVH_EXCH_LCODE1 ( GVH%ENDIAN_SWAP, %VAL(GVH%TOCS(1)%ADR), &
     &                       3, GVH__GET, LCODE, DESCR, CLASS, TYP, DIMS,  &
     &                       LEN_DATA, ADR_DATA, IER )
      IF ( LCODE .NE. 'NUMB_STA' ) THEN
           CALL ERR_LOG ( 4244, IUER, 'GVH_PREGET', 'Mandatory lcode '// &
     &         'NUMB_STA is not the 3-rd LCODE of the database. The first '// &
     &         'lcode is '//LCODE )
           RETURN
      END IF
      CALL MEMCPY ( GVH%CACHE%NUM_STA, %VAL(GVH%DATA(1)%ADR+ADR_DATA), &
     &              %VAL(LEN_DATA) )
      IF ( GVH%ENDIAN_SWAP ) CALL ENDIAN_CNV_I4 ( GVH%CACHE%NUM_STA )
!
      IER = IUER
      CALL GVH_EXCH_LCODE1 ( GVH%ENDIAN_SWAP, %VAL(GVH%TOCS(1)%ADR), 4, &
     &                       GVH__GET, LCODE, DESCR, CLASS, TYP, DIMS,     &
     &                       LEN_DATA, ADR_DATA, IER )
      IF ( LCODE .NE. 'SITNAMES' ) THEN
           CALL ERR_LOG ( 4245, IUER, 'GVH_PREGET', 'Mandatory lcode '// &
     &         'SITNAMES is not the 4-th LCODE of the database. The first '// &
     &         'lcode is '//LCODE )
           RETURN
      END IF
      CALL MEMCPY ( GVH%CACHE%C_STA, %VAL(GVH%DATA(1)%ADR+ADR_DATA), &
     &              %VAL(LEN_DATA) )
!
      IER = IUER
      CALL GVH_EXCH_LCODE1 ( GVH%ENDIAN_SWAP, %VAL(GVH%TOCS(1)%ADR), &
     &                       5, GVH__GET, LCODE, DESCR, CLASS, TYP, DIMS,  &
     &                       LEN_DATA, ADR_DATA, IER )
      IF ( LCODE .NE. 'NOBS_STA' ) THEN
           CALL ERR_LOG ( 4246, IUER, 'GVH_PREGET', 'Mandatory lcode '// &
     &         'NOBS_STA is not the 5-th LCODE of the database. The first '// &
     &         'lcode is '//LCODE )
           RETURN
      END IF
      CALL MEMCPY ( GVH%CACHE%NOBS_STA, %VAL(GVH%DATA(1)%ADR+ADR_DATA), &
     &              %VAL(LEN_DATA) )
      IF ( GVH%ENDIAN_SWAP ) CALL ENDIAN_CNV_I4 ( GVH%CACHE%NOBS_STA )
!
      IER = IUER
      CALL GVH_EXCH_LCODE1 ( GVH%ENDIAN_SWAP, %VAL(GVH%TOCS(1)%ADR), 6, &
     &                       GVH__GET, LCODE, DESCR, CLASS, TYP, DIMS,     &
     &                       LEN_DATA, ADR_DATA, IER )
      IF ( LCODE .NE. 'OBS_TAB ' ) THEN
           CALL ERR_LOG ( 4247, IUER, 'GVH_PREGET', 'Mandatory lcode '// &
     &         'OBS_TAB is not the 6-th LCODE of the database. The first '// &
     &         'lcode is '//LCODE )
           RETURN
      END IF
!
! --- Extract the addres of OBS_TAB table
!
      OBS_TAB_ADR = GVH%DATA(1)%ADR + ADR_DATA
!
! --- Allocate memory for observations cache
!
      IF ( GVH%CACHE%OBS_ADR .NE. 0 ) THEN
           IER = IUER
           CALL GVH_FREE ( GVH, GVH%CACHE%OBS_ADR, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4248, IUER, 'GVH_PREGET', 'The error in an '// &
     &              'attempt to free dynamic memory for station cache table' )
                RETURN
           END IF
      END IF
!
      GVH%CACHE%OBS_LEN = GVH%CACHE%NUM_OBS*5*GVH__I4_LEN
      IER = IUER
      CALL GVH_ALLOCATE ( GVH, GVH%CACHE%OBS_LEN, GVH%CACHE%OBS_ADR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4249, IUER, 'GVH_PREGET', 'The error in an '// &
     &         'attempt to allocate dynamic memory for station cache table' )
           RETURN
      END IF
      GVH%CACHE%OBS_STATUS = GVH__INITIALIZED
!
! --- Create lcode cache table
!
      IF ( GVH%CACHE%LCODE_STATUS .EQ. GVH__INITIALIZED ) THEN
           IER = IUER
           CALL GVH_FREE ( GVH, GVH%CACHE%LCODE_ADR, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4250, IUER, 'GVH_PREGET', 'Erro rin freeing '// &
     &              'memory previously used for observation cache' )
                RETURN
           END IF
      END IF
!
      GVH%CACHE%NUM_LCODE = 0
      DO 410 J1=1,GVH%NSEG
         GVH%CACHE%NUM_LCODE = GVH%CACHE%NUM_LCODE + GVH%TOCS(J1)%NTOC
 410  CONTINUE
!
      GVH%CACHE%LCODE_LEN = GVH__LCODE2_LEN
      IER = IUER
      CALL GVH_ALLOCATE ( GVH, GVH__MTOC*GVH%CACHE%LCODE_LEN, &
     &                    GVH%CACHE%LCODE_ADR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4251, IUER, 'GVH_PREGET', 'The error in an '// &
     &         'attempt to allocate dynamic memory for station cache table' )
           RETURN
      END IF
      GVH%CACHE%LCODE_STATUS = GVH__INITIALIZED
!
! --- Put lcode names in C_CLD array and ...
!
      N_CLD = 0
      DO 420 J2=1,GVH%NSEG
         DO 430 J3=1,GVH%TOCS(J2)%NTOC
            N_CLD = N_CLD + 1
            IER = IUER
            CALL GVH_EXCH_LCODE1 ( GVH%ENDIAN_SWAP, %VAL(GVH%TOCS(J2)%ADR),  &
     &                             J3, GVH__GET, C_CLD(N_CLD), &
     &                             DESCR, CLASS, TYP, DIMS, LEN_DATA, &
     &                             ADR_DATA, IER )
            LCODE_SEG_IND(N_CLD) = J2
 430     CONTINUE
 420  CONTINUE
!
! --- ... then sort it
!
      CALL MEMCPY  ( %REF(C_CLD_ORIG), %REF(C_CLD), %VAL(LEN(C_CLD(1))*N_CLD) )
      CALL SORT_CH ( GVH%CACHE%NUM_LCODE, C_CLD )
!
! --- Put information into lcode table cache
!
      DO 440 J4=1,GVH%CACHE%NUM_LCODE
         DO 450 J5=1,GVH%NSEG
            DO 460 J6=1,GVH%TOCS(J5)%NTOC
               IER = IUER
               CALL GVH_EXCH_LCODE1 ( GVH%ENDIAN_SWAP, %VAL(GVH%TOCS(J5)%ADR), &
     &                                J6, GVH__GET, LCODE, DESCR, CLASS, TYP,  &
     &                                DIMS, LEN_DATA, ADR_DATA, IER )
               IF ( LCODE .EQ. C_CLD(J4) ) THEN
                    IS = J5
                    IC = J6
                    GOTO 850
               END IF
 460        CONTINUE
 450     CONTINUE
 850     CONTINUE
!
! ------ Extra check, whether the lcode is defined only once
!
         IF ( J4 > 1 ) THEN
              IF ( C_CLD(J4) == C_CLD(J4-1) ) THEN
                   IND_LC1 = LTM_DIF ( 1, N_CLD, C_CLD_ORIG, LCODE )
                   IND_LC2 = LTM_DIF ( 1, N_CLD-IND_LC1, &
     &                                 C_CLD_ORIG(IND_LC1+1), LCODE ) + IND_LC1
                   CALL CLRCH ( STR1 )
                   CALL CLRCH ( STR2 )
                   IF ( IND_LC1 > 0 ) THEN
                        CALL INCH  ( LCODE_SEG_IND(IND_LC1), STR1 ) 
                        FIL_SEG1 = GVH%FILENAME(LCODE_SEG_IND(IND_LC1))
                      ELSE 
                        STR1 = 'Undefined_1'
                        STR1 = 'Unknown_1'
                   END IF
!
                   IF ( IND_LC1 > 0 ) THEN
                        CALL INCH  ( LCODE_SEG_IND(IND_LC2), STR2 ) 
                        FIL_SEG2 = GVH%FILENAME(LCODE_SEG_IND(IND_LC2))
                      ELSE 
                        STR2 = 'Undefined_2'
                        STR2 = 'Unknown_2'
                   END IF
!                   CALL ERR_LOG ( 4252, IUER, 'GVH_PREGET', 'Lcode '// &
!     &                  C_CLD(J4)//' is defined more than once: '// &
!     &                  ' in segment '//STR1(1:I_LEN(STR1))//', file '// &
!     &                  FIL_SEG1(1:I_LEN(FIL_SEG1))//' and '// &
!     &                  ' in segment '//STR2(1:I_LEN(STR2))//', file '// &
!     &                  FIL_SEG2 )
!                   RETURN 
              END IF
         END IF
         ADR_DATA = ADR_DATA + GVH%DATA(IS)%ADR
         GVH_LCODE2_REC%LCODE   = LCODE
         GVH_LCODE2_REC%DESCR   = DESCR
         GVH_LCODE2_REC%CLASS   = CLASS
         GVH_LCODE2_REC%TYP     = TYP
         GVH_LCODE2_REC%SEG_IND = IS
         GVH_LCODE2_REC%DIMS(1) = DIMS(1)
         GVH_LCODE2_REC%DIMS(2) = DIMS(2)
!
! ------ Compute amount of memory for data of this lcode
! ------ First compute the number of fields
!
         IF ( CLASS .EQ. GVH__SES ) THEN
              GVH_LCODE2_REC%NUM_FIELDS = 1
            ELSE IF ( CLASS .EQ. GVH__SCA ) THEN
              GVH_LCODE2_REC%NUM_FIELDS = GVH%CACHE%NUM_SCA
            ELSE IF ( CLASS .EQ. GVH__STA ) THEN
              GVH_LCODE2_REC%NUM_FIELDS = 0
              DO 470 J7=1,GVH%CACHE%NUM_STA
                 GVH_LCODE2_REC%NUM_FIELDS = GVH_LCODE2_REC%NUM_FIELDS + &
     &                                       GVH%CACHE%NOBS_STA(J7)
 470          CONTINUE
            ELSE IF ( CLASS .EQ. GVH__BAS ) THEN
              GVH_LCODE2_REC%NUM_FIELDS = GVH%CACHE%NUM_OBS
         END IF
!
         IF ( DIMS(1) .GT. 0 ) THEN
              GVH_LCODE2_REC%LEN_REC = GVH__TYPE_LEN(TYP)*DIMS(1)*DIMS(2)
            ELSE
              GVH_LCODE2_REC%LEN_REC  = SIZEOF(GVH_DESC)
              ADR_DATA_ORIG = ADR_DATA ! Save the address of this lcode
              DO 480 J8=1,GVH_LCODE2_REC%NUM_FIELDS
!
! -------------- Get descriptor of the J8-th field of the lcode for an
! -------------- array of variable length
!
                 CALL MEMCPY ( GVH_DESC, %VAL(ADR_DATA), %VAL(SIZEOF(GVH_DESC)) )
                 IF ( GVH%ENDIAN_SWAP ) THEN
                      CALL ENDIAN_CNV_I4 ( GVH_DESC%DIMS(1) )
                      CALL ENDIAN_CNV_I4 ( GVH_DESC%DIMS(1) )
                      CALL ENDIAN_CNV_I4 ( GVH_DESC%ADR     )
                 END IF
!
! -------------- Transform offset to the actual address by adding the basis
! -------------- address
!
                 GVH_DESC%ADR = GVH_DESC%ADR + GVH%HEAP(IS)%ADR
!
! -------------- ... and put updated descriptor back:
!
                 CALL MEMCPY ( %VAL(ADR_DATA), GVH_DESC, %VAL(SIZEOF(GVH_DESC)) )
!
                 ADR_DATA = ADR_DATA + SIZEOF(GVH_DESC)
 480          CONTINUE
              ADR_DATA = ADR_DATA_ORIG ! Restore the address of this lcode
         END IF
         GVH_LCODE2_REC%LEN_DATA = GVH_LCODE2_REC%LEN_REC* &
     &                             GVH_LCODE2_REC%NUM_FIELDS
!
         GVH_LCODE2_REC%ADR_DATA = ADR_DATA
!
         GVH_LCODE2_REC%ADR_CONV = 0
!
! ------ Put the cache record to the array
!
         CALL MEMCPY ( %VAL(GVH%CACHE%LCODE_ADR + GVH%CACHE%LCODE_LEN*(J4-1)), &
     &                 %REF(GVH_LCODE2_REC), %VAL(GVH%CACHE%LCODE_LEN) )
         ADR_DATA = GVH_LCODE2_REC%ADR_DATA
         LEN_DATA = GVH_LCODE2_REC%LEN_DATA
         CALL GVH_EXCH_LCODE1 ( GVH%ENDIAN_SWAP, %VAL(GVH%TOCS(IS)%ADR), IC, &
     &                          GVH__PUT, LCODE, DESCR, CLASS, TYP, DIMS,    &
     &                          LEN_DATA, ADR_DATA, IER )
         CALL LIB$MOVC3 ( 8, %REF(LCODE), LCT(J4)%LCODE_I8 )
         LCT(J4)%IND_LCODE = J4
 440  CONTINUE
!
! --- Sort Integer*8 version of lcode tables. This version will be used for
! --- fast search
!
      CALL FOR_QSORT ( LCT, GVH%CACHE%NUM_LCODE, SIZEOF(LCT(1)), GVH_COMPAR_LCT )
      DO 490 J9=1,GVH%CACHE%NUM_LCODE
         GVH%LCODE_CACHE_I8(J9)  = LCT(J9)%LCODE_I8 
         GVH%IND_LCODE_CACHE(J9) = LCT(J9)%IND_LCODE
 490  CONTINUE 
      GVH%CACHE%LCODE_STATUS = GVH__POPULATED
!
! --- Copy observations table to cache and transform station indices
!
      IER = IUER
      CALL GVH_PUT_OBS_TAB ( GVH%ENDIAN_SWAP, GVH%CACHE%NUM_OBS,    &
     &                       GVH%CACHE%NUM_STA, GVH%CACHE%NOBS_STA, &
     &                       %VAL(OBS_TAB_ADR), %VAL(GVH%CACHE%OBS_ADR), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4253, IUER, 'GVH_PREGET', 'The error in an '// &
     &         'attempt to create observations cache table' )
           RETURN
      END IF
      GVH%CACHE%OBS_STATUS = GVH__POPULATED
!
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END  SUBROUTINE  GVH_PREGET  !#!#
