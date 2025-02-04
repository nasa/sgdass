      SUBROUTINE ARCPE_MAIN ( MLEN, NPARM1, IX1T3, ARR1, ARR2, STACM, &
     &                        B3DOBJ, B1B3DOBJ  )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  ARCPE_MAIN PROGRAM SPECIFICATION
!
! 1.1 Perform arc parameter elimination.
!
! 1.2 REFERENCES:
!
! 2.  ARCPE_MAIN INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'addcm.i'
      INCLUDE 'socom.i'
      INCLUDE 'precm.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'fast.i'
!C
      TYPE ( B3D__STRU ) ::    B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
!C
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: arcpe
!       CALLED SUBROUTINES: lists,reorder,elimin,outmt
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2 KBIT,STACM
!
      INTEGER*4 IX1T3(M_GPA), NPARM1, JS, JB, JA
      INTEGER*8 MLEN
      INTEGER*4 I4P0, I4P1, I4P60, I4P255, I4P256
      INTEGER*2 NUMDB, LDBNAM(5,15), IDBV(15)
      INTEGER*4 IDBE(15)
      REAL*8    ARR1(*), ARR2(*)
      DATA  I4P0, I4P1, I4P60, I4P255, I4P256 / 0, 1, 60, 255, 256 /
      CHARACTER  STR*16, STRN*128, FINAM_NRM*40
!cc
      CHARACTER*140 STRING
      EQUIVALENCE   ( ADDRBUF(1), STRING )
      CHARACTER  OUTNAM*(NAME_SIZE), SAVNAM*(NAME_SIZE)
      INTEGER*2  NP, ADDRBUF(70), IOCGM_SAVE
      INTEGER*2  TRIMLEN
      INTEGER*4  JBLOCKS, JRND_BLOCKS, FILDES, MAT_E
      INTEGER*8  NELEM 
      INTEGER*2  IDIRECT ( BLOCK_WORDS )
      COMMON    / SAVCGM / FILDES, IDIRECT
      COMMON    / NAMCGM / SAVNAM
      SAVE      / SAVCGM /, / NAMCGM /
!
      INTEGER*8    MEM_LEN, MLEN1, MLEN2, LEN8_BYTES, LEN8_BLOCKS
      ADDRESS__TYPE :: ADDR_GLO1, ADDR_GLO2, ADDR_MAT_GLO, ADDR_VEC_GLO, &
     &                 ADDR_AGG, ADDR_BG, MEM_ADDR
      REAL*8     FJD_BEG, FJD_END
      INTEGER*4  IVER, J1, J2, IUER
      LOGICAL*4  LEX
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
      ADDRESS__TYPE, EXTERNAL :: MAT_E4
!CCC
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   jmg  960610  Fix bug which occurs because
!                the fixed arc is treated differently than the free arcs.
!                Previously the EOP parameters were not estimated in the
!                forward solution -- only in the back.  Now they will be
!                estimated in both directions.  This requires the application
!                of constraints in arcpe to constrain the adjustments to zero.
!   pet  970211  Added the code for implementation B1D branch
!   pet  970228  Added the code for implementation B1B3D branch
!   pet  970602  Made correction: if IOCGM = 0 then the part of code which
!                makes CGM update is bypassed.
!   pet  970713  Added error message in the case of failure memory allocation
!   pet  980204  Added saving condition number of the local-local matrix
!                in B1B3D mode.
!   pet  990406  Added call of TIM_GET
!   pet  2001.05.10  pet  Increased amount of memory allocated for CGM
!                         by 256 bytes since it is read and written by
!                         256-bytes-long blocks.
!   pet  2001.05.10  pet  Added logic which prohibts writing the output CGM
!                         if its name is 'NONE'
!   pet  2002.09.26  pet  Added the code which updates the minimal Julian
!                         date of nominal start and maximal Julian date of
!                         nominal finish over all sessions of this global
!                         solution. Also added settting minimal and maximal
!                         Julian date for all stations snd sources if it is
!                         the first session
!
!CCC
!
! 5.  ARCPE_MAIN PROGRAM STRUCTURE
!
!
! --- Learn NUMDB  -- the number of database treated by LOOP now
! ---       LDBNAM -- data base name
! ---       IDBV   -- data base version (in 1-st element of array)
! ---       IDBE   -- number of observations (in 1-st element of array)
!
      STDALN = PRE_IP(1) .NE. 0
      CALL DBPOX ( NUMDB, LDBNAM, IDBV, IDBE )
!
! --- Formatting the string with database name
!
      CALL CLRCH ( STR )
      CALL LIB$MOVC3 ( 10, LDBNAM, STR  )
      IVER = INT4 ( IDBV(1) )
      STR(12:) = '<'
      CALL INCH ( IVER, STR(13:) )
      STR( I_LEN(STR)+1: ) = '>'
!
      IF ( FAST_DBG .EQ. F__APP ) THEN
           WRITE ( 6, 210 )  str(1:16), fast_mode, fast_dbg
 210       format ( 1X,' ARCPE:  session ',a,'  fast_mode=',i4, &
     &                   ' fast_dbg=',i4 )
      END IF
!
! --- Set timer
!
      CALL TIM_INIT()
!
      JS=I4P1+  M_GPA
      JB=I4P1+2*M_GPA
      JA=I4P1+3*M_GPA
!
! --- Rearrange matrix into another order
!
      CALL REORDER ( IX1T3, NPARM1, FAST_MODE, IARCS, IGLBLS, &
     &     ARR1, ARR2 )
!
      IF ( FAST_MODE .EQ. F__NONE .OR. FAST_MODE .EQ. F__PRD   .OR. &
     &     FAST_MODE .EQ. F__B3D     ) THEN
!
! -------- Elimination arc local parameters in old mode
!
           CALL ELIMIN ( ARR1, ARR1(JS), ARR1(JB), ARR1(JA), IARCS, IGLBLS )
!
! -------- Now write out the matrix
!
           CALL OUTMT ( ARR1, NPARM1, STACM )
        ELSE IF ( FAST_MODE .EQ. F__B1D  .OR.  FAST_MODE .EQ. F__B1B3D ) THEN
           IF ( FAST_MODE .EQ. F__B1D ) THEN
!
! ------------- Elimination arc local parameters in B1D mode
!
                IUER=-1
                CALL ARCPE_B1D ( IARCS, IGLBLS, JA, JB, ARR1, ARR2, &
     &                           ADDR_AGG, ADDR_BG, IUER )
!
! ------------- Now write out the matrix
!
                CALL OUTMT ( ARR1, NPARM1, STACM )
                IF ( FAST_DBG .EQ. F__TIM ) THEN
                     CALL TIM_GET ( 'ARCPE-02' )
                     CALL TIM_INIT()
                END IF
             ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
!
! ------------- Elimination arc local and segmented parameters in B1B3D mode
!
                IUER=-1
                CALL ARCPE_B1B3D ( B3DOBJ, B1B3DOBJ, ADDR_AGG, ADDR_BG, IUER )
                RCOND = B3DOBJ%RCOND
                CALL USE_GLBFIL_4 ( 'OWC' )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8501, -1, 'ARCPE_MAIN', &
     &                   'Error during parameter elimination while '// &
     &                   'database '//B3DOBJ%DBNAME_MES//' was processing' )
                     STOP 'ARCPE: Abnormal termination'
                ENDIF
!
                IF ( FAST_DBG .EQ. F__TIM ) THEN
                     CALL TIM_GET ( 'ARCPE-02' )
                     CALL TIM_INIT()
                END IF
!
! ------------- Form ARC-file
!
                CALL CLRCH ( FINAM_NRM )
                IUER = -1
                CALL ARCFILE_NAME ( FINAM_NRM, IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8502, -1, 'ARCPE_MAIN', &
     &                   'Error during forming name for arc-file while '// &
     &                    FINAM_NRM(1:I_LEN(FINAM_NRM))//' with temporary '// &
     &                   'database '//B3DOBJ%DBNAME_MES//' was processing' )
                     STOP 'ARCPE: Abnormal termination'
                ENDIF
!
! ------------- Writing fields of B1B3DOBJ and B3DOBJ objects in ARC file
!
                IUER=-1
                CALL WRNOR_B1B3D ( FINAM_NRM, B3DOBJ, B1B3DOBJ, IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8503, -1, 'ARCPE_MAIN', &
     &                   'Error during writing file '// &
     &                    FINAM_NRM(1:I_LEN(FINAM_NRM))//' with temporary '// &
     &                   'data structure for B1B3D algorithm while database '// &
     &                    B3DOBJ%DBNAME_MES//' was processing' )
                     STOP 'ARCPE: Abnormal termination'
                ENDIF
!
                IF ( FAST_DBG .EQ. F__TIM ) THEN
                     CALL TIM_GET ( 'ARCPE-03' )
                     CALL TIM_INIT()
                END IF
           END IF
!C
           IF ( IOCGM .GT. 0 ) THEN
!
! ----------- CGM update
!
! ----------- Preparing data for LISTS_CGM. They are to be transferred via
! ----------- addcm.i
!
              CALL CHAR2HOL ( INAMCG, ADDRBUF(1), INT2(1), INT2(64) )
              CALL CHAR2HOL ( SAVAF, ADDRBUF(33), INT2(1), INT2(64) )
              ADDRBUF(65)=1
              ADDRBUF(66)=1
!
! ----------- CGMINN = INAMCG -- name of the input CGM
! ----------- THINGN = SAVAF  -- name of the arcfile. But! It corresponds to
! -----------                    the real file name only in non-fast modes.
! -----------                    It is only a ghost and not actually used
! -----------                    in fast-mode.
!
              CGMINN=STRING(1:64)
              THINGN=STRING(65:128)
              ADORSB='ADD'
              ARORCG='ARC'
              CALL CHASHL ( CGMINN )
              INQUIRE ( FILE = CGMINN, EXIST=LEX )
!
! ----------- If file is not found try to merge it with environment variable
! ----------- CGM_DIR
!
              IF ( .NOT. LEX  .AND.  CGMINN(1:1) .NE. '/' .AND. &
     &                               CGMINN(1:1) .NE. ' '       ) THEN
                   CALL CLRCH ( STRN )
                   CALL GETENVAR ( 'PSOLVE_CGM_DIR', STRN )
                   IF ( STRN(1:1) .NE. ' ' ) THEN
                        IF ( STRN(ILEN(STRN):ILEN(STRN)) .NE. '/' .AND. &
     &                       ILEN(STRN) .LT. LEN(STRN) ) STRN(ILEN(STRN)+1:) = '/'
                        CGMINN = STRN(1:ILEN(STRN))//CGMINN
                   END IF
              END IF
!
! ----------- Get the Julian date of nonimal start and nominal end of the
! ----------- session
!
              CALL OBSTM ( FJD_BEG, FJD_END )
!
! ----------- Update if  ncessary minimal and maximal Julian date over all
! ----------- sessions
!
              IF ( .NOT. ARCPE_WORKED ) THEN
                   GLO_FJDOBS_MIN = FJD_BEG
                   GLO_FJDOBS_MAX = FJD_END
                   DO 410 J1=1,NUMSTA
                      STA_FJD_BEG(J1) = FJD_BEG
                      STA_FJD_END(J1) = FJD_END
                      NSES_STA(J1)    = 0
 410               CONTINUE
!
                   DO 420 J2=1,NUMSTR
                      SRC_FJD_BEG(J2) = FJD_BEG
                      SRC_FJD_END(J2) = FJD_END
                      NSES_SRC(J2)    = 0
  420             CONTINUE
              END IF
!
! ----------- Build complete list of parameters, including additions
!
              CALL LISTS_CGM()
!
! ----------- Generate and save name of output CGM file
!
! ----------- Now we make idiotic trick: to set IOCGM to 1 in the case if
! ----------- we don't want to save garbage called CGM. Jesus!!!!!
! ----------- What a fool wrote this software??? Why so trivial thing
! ----------- as writing intermediary results is made like a black magic???
! ----------- Why on earth a chain of silly subroutines is called instead of
! ----------- ONE routine??
!
              IOCGM_SAVE = IOCGM
              IF ( IOCGM .EQ. 2            .AND. &
     &             OUTCGM(1:4) .EQ. 'NONE' .AND. &
     &             TRIMLEN(OUTCGM) .EQ. 4        ) IOCGM = 1
!
              CALL CLRCH ( OUTNAM )
              CALL NAMES_CGM ( OUTNAM )
!
              IF ( IOCGM .EQ. 2            .AND. &
     &             OUTCGM(1:4) .EQ. 'NONE' .AND. &
     &             TRIMLEN(OUTCGM) .EQ. 4        ) IOCGM=IOCGM_SAVE
!
! ----------- Jugglering with memory. 1) Freeing ARR2
!
              CALL FREE_MEM ( LOC(ARR2) )
!
! ----------- Grabbing memory for the contribution global matrix and global
! ----------- vector if this arc
!
              MLEN1 = 8*(INT8(IGLBLS)*(INT8(IGLBLS)+1))/2
              MLEN2 = 8* INT8(IGLBLS)
              CALL GRAB_MEM ( IUER, MEM_LEN, MEM_ADDR, 2, &
     &                        MLEN1, ADDR_MAT_GLO,       &
     &                        MLEN2, ADDR_VEC_GLO        )
              IF ( IUER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH8  ( 8*(INT8(IGLBLS)*(INT8(IGLBLS)+3)/2), STR )
                   CALL ERR_LOG ( 8504, -1, 'ARCPE_MAIN', &
     &                 'Error during allocation '//STR(1:I_LEN(STR))// &
     &                 ' bytes of memory for updating CGM while database '// &
     &                  B3DOBJ%DBNAME_MES//' was processing' )
                   STOP 'ARCPE: Abnormal termination'
              END IF
!
! ----------- Moving some stuff from fields of ARR1 to MAT_GLO and VEC_GLO
!
              CALL COPY_V8 ( (INT8(IGLBLS)*(INT8(IGLBLS)+1))/2, &
     &                       %VAL(ADDR_AGG), %VAL(ADDR_MAT_GLO) )
              CALL COPY_V8 ( INT8(IGLBLS), %VAL(ADDR_BG), %VAL(ADDR_VEC_GLO) )
!
! ----------- Freeing ARR1
!
              CALL FREE_MEM ( LOC(ARR1) )
!
! ----------- Calculate the length of the data structure for further work
!
              NP = MAX ( NPARMF, NPARMT, NPARMC )
!@              NELEM=I4P1+I4P256*((MAT_E(MAX_PAR,NP)+I4P255)/I4P256) + I4P256
!@              MLEN1 = 8*NELEM
!@              MLEN2 = 8*NELEM
              MLEN1 = 8*MAT_E4 ( M_GPA, NP )
              MLEN2 = 8*MAT_E4 ( M_GPA, NP )
!
! ----------- Grabbing memory for CGM. We catch twice more memory since we will
! ----------- need it for reodering elemetns in CGM
!
              CALL GRAB_MEM ( IUER, MEM_LEN, MEM_ADDR, 2, &
     &                        MLEN1, ADDR_GLO1, &
     &                        MLEN2, ADDR_GLO2  )
!
! ----------- Zeroing seized pool of memory (It is important, since furhter
! ----------- software anticipates that memory is initialized by zeroes!)
!
              CALL NOUT8_R8  ( MLEN1,  %VAL(ADDR_GLO1) )
              CALL NOUT8_R8  ( MLEN2,  %VAL(ADDR_GLO2) )
!
! ----------- Update combined global matrix. Updated CGM is written dowm on disk
!
              CALL XDDER_FAST ( IARCS, IGLBLS, %VAL(ADDR_GLO1), &
     &                          %VAL(ADDR_GLO2), %VAL(ADDR_MAT_GLO), %VAL(ADDR_VEC_GLO) )
!
              IF ( IOCGM.EQ.2  .AND. &
     &             .NOT. ( OUTCGM(1:4) .EQ. 'NONE'  .AND. &
     &                     TRIMLEN(OUTCGM) .EQ. 4         )  ) THEN
!
! -------------- If this arc is the last -- then copying with combined global
! -------------- matrix in another place
!
!@                 JBLOCKS = JRND_BLOCKS(MAT_E(MAX_PAR,NPARMF)*REALL_WORDS)
!@                 JBLOCKS = JBLOCKS+JSOCOM_BLOCKS+JPARFIL_BLOCKS+JPLIST_BLOCKS+1
                 LEN8_BYTES  = 8*(3*M_GPA + INT8(NPARMF)*INT8(NPARMF+1)/2)
                 LEN8_BLOCKS = (LEN8_BYTES+INT8(255))/INT8(256) &
     &                         + JBLOCKS + JSOCOM_BLOCKS + JPARFIL_BLOCKS + JPLIST_BLOCKS + 1
                 CALL COPY_FILE ( SAVNAM(:TRIMLEN(SAVNAM)) , &
     &                            OUTNAM(:TRIMLEN(OUTNAM)), 'Q', JBLOCKS )
                 CALL COPY_FILE ( SAVNAM(:TRIMLEN(SAVNAM)), &
     &                            OUTNAM(:TRIMLEN(OUTNAM)), 'Q', INT(LEN8_BLOCKS,KIND=4) )

                 ONAMCG = OUTNAM
                 INAMCG = OUTNAM
                 CALL USE_GLBFIL ( 'OWC' )
              END IF
!
              IF ( .NOT. ARCPE_WORKED ) THEN
                   GLO_FJDOBS_MIN = FJD_BEG
                   GLO_FJDOBS_MAX = FJD_END
                ELSE
                   GLO_FJDOBS_MIN = MIN ( GLO_FJDOBS_MIN, FJD_BEG )
                   GLO_FJDOBS_MAX = MAX ( GLO_FJDOBS_MAX, FJD_END )
              END IF
!
! ----------- Set flag that ARCPE has processed at least one arc
!
              ARCPE_WORKED = .TRUE.
              CALL USE_GLBFIL ( 'OWC' )
           END IF
      END IF
!
      IF ( FAST_DBG .EQ. F__TIM ) THEN
           CALL TIM_GET ( 'ARCPE-04' )
           CALL TIM_INIT()
      END IF
!
      IF ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6)) ) CALL END_MN
!
      RETURN
      END  !#!  ARCPE_MAIN  #!#
