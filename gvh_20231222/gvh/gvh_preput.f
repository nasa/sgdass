      SUBROUTINE GVH_PREPUT ( GVH, NUMOBS, NUMSCA, NUMSTA, NOBS_STA, C_STA, &
     &                        OBS_TAB, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  GVH_PREPUT  inserts in the internal data structure     *
! *   values of 5 mandatory lcodes, creates caches table and sorts it.   *
! *   It is assumed that all lcodes have already been defined using      *
! *   GVH_PTOC. After running GVH_PREPUT contents of non-mandatory       *
! *   lcodes can be added to GVH by calling GVH_PLCODE or GVH_PHLCODE.   *
! *   Routine GVH_PREPUT must be run after the last call to GVH_PTOC and *
! *   before the first call of GVH_PLCODE of GVH_PHLCODE.                *
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
! *  ### 22-NOV-2001   GVH_PREPUT   v2.2 (c)  L. Petrov 22-NOV-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      INTEGER*4  NUMOBS, NUMSCA, NUMSTA, NOBS_STA(GVH__MSTA), &
     &           OBS_TAB(3,NUMOBS), IUER
      TYPE     ( GVH__STRU        ) :: GVH
      TYPE     ( GVH_DESC__STRU   ) :: GVH_DESC
      TYPE     ( GVH_LCODE2__STRU ) :: GVH_LCODE2_REC
      ADDRESS__TYPE  ADR_NUMOBS, ADR_NUMSCA, ADR_NUMSTA, ADR_NOBSSTA, &
     &               ADR_OBSTAB, ADR_SITNAMES
      INTEGER*4      LEN_NUMOBS, LEN_NUMSCA, LEN_NUMSTA, LEN_NOBSSTA, &
     &               LEN_OBSTAB, LEN_SITNAMES
      CHARACTER  C_STA(NUMSTA)*8
      CHARACTER  LCODE*8, DESCR*80
      TYPE     ( GVH_LCD__STRU ) :: LCD(GVH__MTOC)
      TYPE     ( GVH_LCT__STRU ) :: LCT(GVH__MTOC)
      INTEGER*4  CLASS, TYP, DIMS(2), LEN_DATA
      ADDRESS__TYPE ADR_DATA
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, K_LCD, &
     &           LEN_CACHE, NT, IER
      INTEGER*4    ALGN
      PARAMETER  ( ALGN = 16 )
#ifdef GNU
      INTEGER*4, EXTERNAL     :: GVH_COMPAR_LCD, GVH_COMPAR_LCT 
#else
      INTEGER*2, EXTERNAL     :: GVH_COMPAR_LCD, GVH_COMPAR_LCT 
#endif
      INTEGER*4, EXTERNAL     :: I_LEN, INT4_ALIGN
      ADDRESS__TYPE, EXTERNAL :: ADDRESS_ALIGN
!
      IF ( GVH%STATUS .NE. GVH__INITIALIZED ) THEN
           CALL ERR_LOG ( 4061, IUER, 'GVH_PREPUT', 'The GVH data '// &
     &         'structure was not initialized. Please, use gvh_init first' )
           RETURN
      END IF
!
      IF ( GVH%CACHE%OBS_STATUS == GVH__INITIALIZED .OR. &
     &     GVH%CACHE%OBS_STATUS == GVH__POPULATED        ) THEN
           CONTINUE 
         ELSE 
           GVH%TEMP_FLAG = GVH__MANDATORY
           NT = GVH%TOCS(1)%NTOC ! save the actual number of lcodes
           GVH%TOCS(1)%NTOC = 0
           IER = IUER
           CALL GVH_PTOC ( GVH, 'NUMB_OBS', GVH__I4, GVH__SES, 1, 1, &
     &                    'Number of observations in the session', 1, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4062, IUER, 'GVH_PREPUT', 'Error in attempt '// &
     &              'to add lcode "NUMB_OBS" to the table of contents' )
                RETURN
           END IF
!
           IER = IUER
           CALL GVH_PTOC ( GVH, 'NUMB_SCA', GVH__I4, GVH__SES, 1, 1, &
     &                    'Number of scans in the session', 1, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4063, IUER, 'GVH_PREPUT', 'Error in attempt '// &
     &              'to add lcode "NUMB_SCA" to the table of contents' )
                RETURN
           END IF
!
           IER = IUER
           CALL GVH_PTOC ( GVH, 'NUMB_STA', GVH__I4, GVH__SES, 1, 1, &
     &                    'Number of sites', 1, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4064, IUER, 'GVH_PREPUT', 'Error in attempt '// &
     &              'to add lcode "NUMB_STA" to the table of contents' )
                RETURN
           END IF
!
           IER = IUER
           CALL GVH_PTOC ( GVH, 'SITNAMES', GVH__C1, GVH__SES, 8, NUMSTA, &
     &                    'IVS site names', 1, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4065, IUER, 'GVH_PREPUT', 'Error in attempt '// &
     &              'to add lcode "SITNAMES" to the table of contents' )
                RETURN
           END IF
!
           IER = IUER
           CALL GVH_PTOC ( GVH, 'NOBS_STA', GVH__I4, GVH__SES, NUMSTA, 1, &
     &                    'Number of observations per site', 1, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4066, IUER, 'GVH_PREPUT', 'Error in attempt '// &
     &              'to add lcode "NOBS_STA" to the table of contents' )
                RETURN
           END IF
!
           IER = IUER
           CALL GVH_PTOC ( GVH, 'OBS_TAB ', GVH__I4, GVH__SES, 3, NUMOBS, &
     &         'Observation tables: scan index, indices of the first and the '// &
     &         'second station', 1, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4067, IUER, 'GVH_PREPUT', 'Error in an '// &
     &              'attempt to add lcode "OBS_TAB" to the table of contents' )
                RETURN
           END IF
           GVH%TEMP_FLAG = GVH__UNDEFINED
           IF ( NT > 0 ) GVH%TOCS(1)%NTOC = NT ! restore the actual number of lcodes
!
! -------- Create observations cache table
!
           GVH%CACHE%OBS_LEN = NUMOBS*5*GVH__I4_LEN
           IER = IUER
           CALL GVH_ALLOCATE ( GVH, GVH%CACHE%OBS_LEN, GVH%CACHE%OBS_ADR, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4068, IUER, 'GVH_PREPUT', 'The error in an '// &
     &              'attempt to allocate dynamic memory for station cache table' )
                RETURN
           END IF
           GVH%CACHE%OBS_STATUS = GVH__INITIALIZED
      END IF
!
! --- Create lcode cache table
!
      IF ( GVH%CACHE%LCODE_STATUS .EQ. GVH__POPULATED ) THEN
           IER = IUER
           CALL GVH_FREE ( GVH, GVH%CACHE%LCODE_ADR, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4069, IUER, 'GVH_PREPUT', 'The error in an '// &
     &              'attempt to free dynamic memory for station cache table' )
                RETURN
           END IF
      END IF
!
      GVH%CACHE%LCODE_LEN = GVH__LCODE2_LEN
      LEN_CACHE = 0
      DO 420 J2=1,GVH%NSEG
         LEN_CACHE = LEN_CACHE + GVH%TOCS(J2)%NTOC*GVH%CACHE%LCODE_LEN
 420  CONTINUE 
      IER = IUER
      CALL GVH_ALLOCATE ( GVH, LEN_CACHE, GVH%CACHE%LCODE_ADR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4070, IUER, 'GVH_PREPUT', 'The error in an '// &
     &         'attempt to allocate dynamic memory for lcode cache table' )
           RETURN
      END IF
      GVH%CACHE%LCODE_STATUS = GVH__INITIALIZED
!
      ADR_NUMOBS   = 0
      ADR_NUMSCA   = 0
      ADR_NUMSTA   = 0
      ADR_NOBSSTA  = 0
      ADR_OBSTAB   = 0
      ADR_SITNAMES = 0
!
! --- Put lcode names in LCD array and ...
!
      K_LCD = 0 
      DO 430 J3=1,GVH%NSEG
         DO 440 J4=1,GVH%TOCS(J3)%NTOC
            K_LCD = K_LCD + 1
            LCD(K_LCD)%IND_SEG = J3
            LCD(K_LCD)%IND_TOC = J4
            IER = IUER
            CALL GVH_EXCH_LCODE1 ( .FALSE., %VAL(GVH%TOCS(J3)%ADR), J4, &
     &                             GVH__GET, LCD(K_LCD)%LCODE, DESCR, &
     &                             CLASS, TYP, DIMS, LEN_DATA, ADR_DATA, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 4071, IUER, 'GVH_PREPUT', 'The error in an '// &
     &               'attempt to extract lcode' )
                 RETURN
            END IF
 440     CONTINUE
 430  CONTINUE 
!
! --- ... then sort it
!
      CALL FOR_QSORT ( LCD, K_LCD, SIZEOF(LCD(1)), GVH_COMPAR_LCD )
!
      DO 450 J5=1,K_LCD
!
! ------ get information about lcode 
!
         IER = IUER
         CALL GVH_EXCH_LCODE1 ( .FALSE., %VAL(GVH%TOCS(LCD(J5)%IND_SEG)%ADR), &
     &                          LCD(J5)%IND_TOC, GVH__GET, LCODE, DESCR, &
     &                          CLASS, TYP, DIMS, LEN_DATA, ADR_DATA, IER )
         IF ( TYP < 1 .OR. TYP > GVH__MTYPE ) THEN
              CALL ERR_LOG ( 4072, IUER, 'GVH_PREPUT', 'Trap of internal '// &
     &            'control while processing LCODE '//TRIM(LCODE)//' -- '// &
     &            'unsupported type' )
              RETURN
         END IF
!
         GVH_LCODE2_REC%LCODE   = LCODE
         GVH_LCODE2_REC%CLASS   = CLASS
         GVH_LCODE2_REC%TYP     = TYP
         GVH_LCODE2_REC%DIMS(1) = DIMS(1)
         GVH_LCODE2_REC%DIMS(2) = DIMS(2)
         GVH_LCODE2_REC%SEG_IND = J5
!
! ------ Compute amount of memory for data of this lcode
! ------ First compute the number of fields
!
         IF ( CLASS .EQ. GVH__SES ) THEN
              GVH_LCODE2_REC%NUM_FIELDS = 1
            ELSE IF ( CLASS .EQ. GVH__SCA ) THEN
              GVH_LCODE2_REC%NUM_FIELDS = NUMSCA
            ELSE IF ( CLASS .EQ. GVH__STA ) THEN
              GVH_LCODE2_REC%NUM_FIELDS = 0
              DO 460 J6=1,NUMSTA
                 GVH_LCODE2_REC%NUM_FIELDS = GVH_LCODE2_REC%NUM_FIELDS + NOBS_STA(J6)
 460          CONTINUE
              IF ( GVH_LCODE2_REC%NUM_FIELDS == 0 ) THEN
                   CALL ERR_LOG ( 4073, IUER, 'GVH_PREPUT', 'Trap of internal '// &
     &                 'control: NOBS_STA is zero' )
                   RETURN
              END IF
            ELSE IF ( CLASS .EQ. GVH__BAS ) THEN
              GVH_LCODE2_REC%NUM_FIELDS = NUMOBS
         END IF
!
         IF ( DIMS(1) .GT. 0 ) THEN
              GVH_LCODE2_REC%LEN_REC  = GVH__TYPE_LEN(TYP)*DIMS(1)*DIMS(2)
            ELSE
              GVH_LCODE2_REC%LEN_REC  = SIZEOF(GVH_DESC)
         END IF
!
         IF ( ADR_DATA .EQ. 0 ) THEN
!
! ----------- The address has not been assigned.
!
              GVH_LCODE2_REC%LEN_DATA = INT4_ALIGN( GVH_LCODE2_REC%LEN_REC* &
     &                                         GVH_LCODE2_REC%NUM_FIELDS, ALGN )
!
! ----------- Allocate memory for data buffer for this lcode
!
              IER = IUER
              CALL GVH_ALLOCATE ( GVH, GVH_LCODE2_REC%LEN_DATA, &
     &                            GVH_LCODE2_REC%ADR_DATA, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4074, IUER, 'GVH_PREPUT', 'The error in '// &
     &                 'an attempt to allocate dynamic memory for lcode '// &
     &                  LCODE )
                   RETURN
              END IF
!
! ----------- ... and put there zeroes
!
              CALL NOUT ( GVH_LCODE2_REC%LEN_DATA, %VAL(GVH_LCODE2_REC%ADR_DATA) )
            ELSE 
              GVH_LCODE2_REC%ADR_DATA = ADR_DATA 
              GVH_LCODE2_REC%LEN_DATA = LEN_DATA 
         END IF
!
         GVH_LCODE2_REC%ADR_CONV = 0
         CALL MEMCPY ( %VAL(GVH%CACHE%LCODE_ADR + GVH%CACHE%LCODE_LEN*(J5-1)), &
     &                 %REF(GVH_LCODE2_REC), %VAL(GVH%CACHE%LCODE_LEN) )
!
         IF ( LCODE .EQ. 'NUMB_OBS' ) THEN
              ADR_NUMOBS  = GVH_LCODE2_REC%ADR_DATA
              LEN_NUMOBS  = GVH_LCODE2_REC%LEN_DATA
         END IF
         IF ( LCODE .EQ. 'NUMB_SCA' ) THEN
              ADR_NUMSCA  = GVH_LCODE2_REC%ADR_DATA
              LEN_NUMSCA  = GVH_LCODE2_REC%LEN_DATA
         END IF
         IF ( LCODE .EQ. 'NUMB_STA' ) THEN
              ADR_NUMSTA = GVH_LCODE2_REC%ADR_DATA
              LEN_NUMSTA = GVH_LCODE2_REC%LEN_DATA
         END IF
         IF ( LCODE .EQ. 'NOBS_STA' ) THEN
              ADR_NOBSSTA = GVH_LCODE2_REC%ADR_DATA
              LEN_NOBSSTA = GVH_LCODE2_REC%LEN_DATA
         END IF
         IF ( LCODE .EQ. 'OBS_TAB ' ) THEN
              ADR_OBSTAB = GVH_LCODE2_REC%ADR_DATA
              LEN_OBSTAB = GVH_LCODE2_REC%LEN_DATA
         END IF
         IF ( LCODE .EQ. 'SITNAMES' ) THEN
              ADR_SITNAMES = GVH_LCODE2_REC%ADR_DATA
              LEN_SITNAMES = GVH_LCODE2_REC%LEN_DATA
         END IF
!
         ADR_DATA = GVH_LCODE2_REC%ADR_DATA
         LEN_DATA = GVH_LCODE2_REC%LEN_DATA
!
! ------ And now put updated information about this lcode
!
         CALL GVH_EXCH_LCODE1 ( .FALSE., %VAL(GVH%TOCS(LCD(J5)%IND_SEG)%ADR), &
     &                          LCD(J5)%IND_TOC, GVH__PUT, LCODE, DESCR, &
     &                          CLASS, TYP, DIMS, LEN_DATA, ADR_DATA, IER )
         CALL LIB$MOVC3 (  8, %REF(LCODE), LCT(J5)%LCODE_I8 )
         LCT(J5)%IND_LCODE = J5
 450  CONTINUE
!
      GVH%CACHE%NUM_LCODE = K_LCD
      GVH%CACHE%NUM_OBS = NUMOBS
      GVH%CACHE%NUM_SCA = NUMSCA
      GVH%CACHE%NUM_STA = NUMSTA
!
! --- Sort Integer*8 version of lcode tables. This version will be used for
! --- fast search
!
      CALL FOR_QSORT ( LCT, GVH%CACHE%NUM_LCODE, SIZEOF(LCT(1)), GVH_COMPAR_LCT )
      DO 470 J7=1,GVH%CACHE%NUM_LCODE
         GVH%LCODE_CACHE_I8(J7)  = LCT(J7)%LCODE_I8 
         GVH%IND_LCODE_CACHE(J7) = LCT(J7)%IND_LCODE
 470  CONTINUE 
!
      DO 480 J8=1,NUMSTA
         GVH%CACHE%NOBS_STA(J8) = NOBS_STA(J8)
         GVH%CACHE%C_STA(J8) = C_STA(J8)
 480  CONTINUE
!
      IF ( GVH%CACHE%OBS_STATUS == GVH__POPULATED ) THEN 
           CONTINUE 
         ELSE 
!
! -------- Now load 5 mandatory lcodes
!
           CALL MEMCPY ( %VAL(ADR_NUMOBS),   NUMOBS,      %VAL(LEN_NUMOBS)   )
           CALL MEMCPY ( %VAL(ADR_NUMSCA),   NUMSCA,      %VAL(LEN_NUMSCA)   )
           CALL MEMCPY ( %VAL(ADR_NUMSTA),   NUMSTA,      %VAL(LEN_NUMSTA)   )
           CALL MEMCPY ( %VAL(ADR_NOBSSTA),  NOBS_STA,    %VAL(LEN_NOBSSTA)  )
           CALL MEMCPY ( %VAL(ADR_OBSTAB),   OBS_TAB,     %VAL(LEN_OBSTAB)   )
           CALL MEMCPY ( %VAL(ADR_SITNAMES), %REF(C_STA), %VAL(LEN_SITNAMES) )
!
! -------- Copy observations table to cache and transform station indices
!
           IER = IUER
           CALL GVH_PUT_OBS_TAB ( .FALSE., NUMOBS, NUMSTA, NOBS_STA, OBS_TAB, &
     &                            %VAL(GVH%CACHE%OBS_ADR), IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4075, IUER, 'GVH_PREPUT', 'The error in an '// &
     &              'attempt to create observations cache table' )
                RETURN
           END IF
           GVH%CACHE%OBS_STATUS = GVH__POPULATED 
      END IF
!
      GVH%CACHE%LCODE_STATUS = GVH__POPULATED 
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END  SUBROUTINE  GVH_PREPUT  !#!#
