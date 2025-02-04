      SUBROUTINE GLO_BACK ( IARC, DBNAME_MES, GLBMEM, B3DOBJ, B1B3DOBJ, &
     &                      CNSTROBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GLO_BACK  processes the session under consideration in    *
! *   backward mode of global parameters estimation. It computes         *
! *   adjustments of local parameters, estimates of their covarinace     *
! *   matrix and dispersion of the adjustments.                          *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *       IARC ( INTEGER*4 ) -- Index of the session to be processed in  *
! *                             the list of sessions.                    *
! * DBNAME_MES ( CHARACTER ) -- Line with the database name and its      *
! *                             version for generating error messages.   *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     GLBMEM ( RECORD    ) -- Data structure which keeps addresses of  *
! *                             CGM, list of global parameters, global   *
! *                             socom, prfil and temporary normal        *
! *                             matrices. Defined in ../include/glbp.i   *
! *     B3DOBJ ( RECORD    ) -- Object with data structure for B3D       *
! *                             extension of SOLVE.                      *
! *   B1B3DOBJ ( RECORD    ) -- Object with data structure for B1B3D     *
! *                             extension of SOLVE.                      *
! *   CNSTROBJ ( RECORD    ) -- The data structure with information      *
! *                             about constraints (position where the    *
! *                             matrix should be modified and the value  *
! *                             of constraints).                         *
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
! *  2001.05.10 pet  1.6  Increased amount of memory allocated for CGM   *
! *                       by 256 bytes since it is read and written by   *
! *                       256-bytes-long blocks.                         *
! *  2002.10.04 pet  1.7  Added parameter CNSTROBJ in call of CRES_DO.   *
! *  2007.05.15 pet  1.8  Increased the the reparameterization margin    *
! *                       from MAX_ARC_STA to MAX_ARC_BSL.               *
! *  2016.09.08 pet  1.9  Decreased the the reparameterization margin    *
! *                       to MAX_ARC_BSL to MAX_ARC_STA.                 *
! *                                                                      *
! *  ###  02-MAR-99   GLO_BACK     v1.9  (c)  L. Petrov 08-SEP-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INCLUDE    'glbcm.i'
      INCLUDE    'glbc4.i'
      INCLUDE    'glbp.i'
      INCLUDE    'plist.i'
      INCLUDE    'erm.i'
      INCLUDE    'socom.i'
      INCLUDE    'socom_plus.i'
      INCLUDE    'precm.i'
      INCLUDE    'prfil.i'
      INCLUDE    'baccm.i'
      INCLUDE    'fast.i'
      INCLUDE    'buff2.i'
      INCLUDE    'cnstr.i'
!
      INTEGER*4  IARC, IUER
      CHARACTER  DBNAME_MES*(*)
      TYPE ( B3D__STRU ) ::  B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      TYPE ( GLB_MEM__STRU ) ::  GLBMEM  ! defined in glbp.i
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      CHARACTER         SAVNAM*(NAME_SIZE)
      COMMON / NAMARC / SAVNAM
!
      LOGICAL*2  KBIT
      LOGICAL*2  PURGARC, REALLY_CREATE, ARC_EXIST, KOUTPUT
      LOGICAL*4  SKIP_ARC
      CHARACTER  STR*32, DEBUG_ARCF*8
      REAL*8     VAL
      INTEGER*4  IER, MODE_BACK, SNGCHK_CMP, NP2, IPTR, PAGEWID
      INTEGER*8  M3, MATSIZE, NEW_MATSIZE
      CHARACTER  LBUF(CRES_BUF_LEN2)*120
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: MAT_E, I_LEN
      ADDRESS__TYPE, EXTERNAL :: MAT_E4
!
      IF ( FAST_DBG .EQ. F__APP  .OR.  FAST_DBG .EQ. F__PRI ) THEN
           WRITE ( 6, * ) ' GLO_BACK started for '//DBNAME_MES
      END IF
!
! --- Check: whether we have to allocate dynamic memory
!
      IF ( GLBMEM%LEN_GLO .EQ. 0 ) THEN
           CALL ERR_LOG ( 4181, IUER, 'GLO_BACK', 'Trap of internal '// &
     &         'control: data structure GLBMEM has not been initialized '// &
     &         'and dynamic memory has not been allocated' )
           RETURN
      END IF
!
      CALL USE_GLBFIL_4 ( 'ORC' )
!
! --- Unpacking bits
!
      KBATCH   =       KBIT ( PRE_IP(3), INT2( 1) )
      KMINOUT  =       KBIT ( PRE_IP(3), INT2( 2) )
      KLCLBSL  =       KBIT ( PRE_IP(3), INT2( 3) )
      KSCREEN  = .NOT. KBIT ( PRE_IP(3), INT2( 4) )
      KBACKSL  =       KBIT ( PRE_IP(3), INT2( 6) )
      KGLBBSL  =       KBIT ( PRE_IP(3), INT2( 7) )
      CORLN    =       KBIT ( PRE_IP(3), INT2( 9) )
      KGLOBALS =       KBIT ( PRE_IP(3), INT2(10) )
      KPOSELL  =       KBIT ( PRE_IP(3), INT2(13) )
      KFULLOUT = .NOT. KMINOUT
!
      IF ( FAST_DBG .EQ. F__TIM ) THEN
           CALL TIM_INIT() ! Set timer
      END IF
!
      CALL USE_PARFIL ( 'ORC' )  ! New of 
      CALL USE_COMMON ( 'ORC' )
      SOCOM_PLUS_FIRST = SPL__UNDF
      CALL SOCOM_EXT()
!
! --- Attempt to override FAST_MODE from environment variable and make test
! --- of eligibility FAST_MODE for this session
!
      CALL ERR_PASS    ( IUER, IER )
      CALL FAST_BYPASS ( IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4182, IUER, 'GLO_BACK', 'Error in trying '// &
     &         'to bypass fast_mode' )
           RETURN
      END IF
      SKIP_ARC = .FALSE.
!
! --- Get name of the arcfile -- SAVAF.
!
      CALL CLRCH ( SAVNAM )
      PURGARC       = .FALSE.
      REALLY_CREATE = .FALSE.
      CALL CREATE_ARCF ( SAVAF, PURGARC, IARCNM, ARCDIR, INT2(0), &
     &     KBIT(PRE_IBATCH, INT2(8)), ARC_EXIST, 'U', REALLY_CREATE, FS_FULL )
      CALL ACS_ARCFIL  ( SAVAF, KBIT( PRE_IBATCH, INT2(8)), 'C' )
!
! --- .. and save it in glbfil!
!
      CALL USE_GLBFIL ( 'OWC' )
!
      IF ( .NOT. ARC_EXIST ) THEN
           IF ( GLBMEM%LEN_LOC .GT. 0 ) THEN
!
! ------------- Free dynamic memory previously allocated for local parameters
!
                CALL FREE_MEM ( GLBMEM%ADR_LOC )
                GLBMEM%ADR_LOC =  0
                GLBMEM%LEN_LOC = -1
           END IF
!
! -------- Initialization of B3DOBJ,  B1B3DOBJ
!
           CALL F__CLR_IND ( 4, FAST_MODE, %VAL(0), B3DOBJ, B1B3DOBJ )
!
! -------- Grab dynamic memory for normal matrix in non fast mode
!
! -------- Calculation number of real*8 elements of dynamic memory needed
! -------- for non-fast mode. We increased amount of alotted memory for
! -------- the case if after reparameterization the size of array will
! -------- increase by MAX_ARC_BSL parameteres
!
           CALL ERR_PASS ( IUER, IER )
!
! -------- Grabbing memory for full normal matrix
!
           IF ( FAST_MODE .EQ. F__NONE ) THEN
!!                NP2 = MAX ( INT2(GLBMEM%L_GPA), NPARAM + MAX_ARC_BSL )
!@                MATSIZE = 256*( (MAT_E(MAX_PAR,NP2)+255)/256 ) *8 + 256
                NP2 = MAX ( GLBMEM%L_GPA, NPARAM + MAX_ARC_STA )
                MATSIZE = 8*MAT_E4 ( M_GPA, NP2 )
!
! ------------- We have to grab more memory for non-fast modes
!
                CALL GRAB_MEM ( IER, GLBMEM%LEN_LOC, GLBMEM%ADR_LOC,     1, &
     &                               2*MATSIZE,      GLBMEM%ADR_NORMARR2    )
                IF ( IER .NE. 0 ) THEN
                     WRITE ( 6, * ) ' NPARAM=',NPARAM, &
     &                              ' GLBMEM%L_GPA=', GLBMEM%L_GPA, &
     &                              ' MATSIZE= ', MATSIZE, ' NP2= ', NP2, &
     &                              ' NPARAM = ', NPARAM,  ' MAX_ARC_BSL= ', MAX_ARC_BSL, &
     &                              ' GLBMEM%L_GPA = ', GLBMEM%L_GPA
                     CALL CLRCH   ( STR )
                     CALL INCH    ( NPARAM, STR )
                     CALL ERR_LOG ( 4183, IUER, 'GLO_BACK', 'Error in '// &
     &                   'attempt to grab dynamic memory for two normal '// &
     &                   'matrices sized for '//STR(1:I_LEN(STR))// &
     &                   ' parameters ' )
#ifdef HPUX
                     CALL MEMORYMAP ( %VAL(1) )
#endif
                     RETURN
                END IF
                GLBMEM%ADR_NORMARR = GLBMEM%ADR_NORMARR2 + MATSIZE
!
! ------------- Zeroing grabbed dynamic memory
!
                CALL NOUT8 ( GLBMEM%LEN_LOC, %VAL(GLBMEM%ADR_LOC) )
           END IF
!
! -------- Restore parfil
!
           IF ( IARCNM .EQ. INT2(1) ) THEN
                CALL USE_PARFIL ( 'ORC' )
           END IF
!
! -------- Computation of normal equations
!
           CALL ERR_PASS ( IUER, IER )
           CALL PROC_DO  ( %VAL(GLBMEM%ADR_NORMARR), B3DOBJ, B1B3DOBJ, &
     &                     CNSTROBJ, SNGCHK_CMP, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4184, IUER, 'GLO_BACK', 'Error during '// &
     &              'computation of normal equations while the database '// &
     &               DBNAME_MES//' was processed' )
                RETURN
           END IF
!
           IF ( SNGCHK_CMP .EQ. SNGCHK_CMP__SKIP ) THEN
                SKIP_ARC = .TRUE.
             ELSE
!
! ------------- Make some operations of linear algebra under blocks of normal
! ------------- matrix
!
                CALL ERR_PASS ( IUER, IER )
                CALL GLO_ELIM ( 2, DBNAME_MES, GLBMEM, B3DOBJ, B1B3DOBJ, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 4185, IUER, 'GLO_BACK', 'Error during '// &
     &                   'parameter elimination procedure while the database '// &
     &                    DBNAME_MES//' was processed' )
                     RETURN
                END IF
!
                TGLBLS = GLBMEM%L_GPA
                CALL USE_GLBFIL ( 'OWC' )
!
! ------------- Read socom and parfile since GLO_ELIM called LISTS_ARC which
! ------------- spoiled socom and parfil
!
                CALL USE_COMMON ( 'ORC' )
                CALL SOCOM_EXT()
                CALL USE_PARFIL ( 'ORC' )
           END IF
        ELSE ! ARC_EXIST
!
! -------- It is tricky point. We have to set flags of that the data structures
! -------- with coeffiticents of linear spline for interpolation of EOP are not
! -------- yet initialized.
!
           CALL FLYBY_MAP_INIT()
      END IF
!
      IF ( .NOT. SKIP_ARC ) THEN
!
! -------- This session was not skiped. So we are ready for computation of
! -------- adjustments of local paraemters and their covarinace matrices.
! -------- First set mode of making back solution...
!
           IF ( ARC_EXIST ) THEN
                MODE_BACK = 2 ! use saved arc files
              ELSE
                MODE_BACK = 3 ! not use saved arc files but instead of it use
!                             ! information which is already in memory
           END IF
           IF ( FAST_MODE .EQ. F__B1B3D ) THEN
                NP2 = NPARAM + MAX_ARC_STA
!@                MATSIZE = 256*( (MAT_E(MAX_PAR,NP2)+255)/256 ) *8 + 256
                MATSIZE = 8*MAT_E4 ( M_GPA, NP2 )
                CALL GRAB_MEM ( IER, GLBMEM%LEN_LOC, GLBMEM%ADR_LOC,     1, &
     &                               MATSIZE,        GLBMEM%ADR_NORMARR2    )
                GLBMEM%ADR_NORMARR = GLBMEM%ADR_NORMARR2
                IF ( IER .NE. 0 ) THEN
                     WRITE ( 6, * ) ' NPARAM=',NPARAM
                     CALL CLRCH   ( STR )
                     CALL INCH    ( NPARAM, STR )
                     WRITE ( 6, * ) ' NPARAM=',NPARAM, &
     &                              ' GLBMEM%L_GPA=', GLBMEM%L_GPA, &
     &                              ' GLBMEM%LEN_GLO = ', GLBMEM%LEN_GLO, &
     &                              ' GLBMEM%LEN_LOC = ', GLBMEM%LEN_LOC, &
     &                              ' GLBMEM%L_ARC   = ', GLBMEM%L_ARC, &
     &                              ' NP2 = ', NP2, &
     &                              ' MATSIZE= ', MATSIZE, &
     &                              ' MAT_E4(M_GPA,NP2) = ', MAT_E4 ( M_GPA, NP2 ), &
     &                              ' Mem_mb = ', INT8(NP2)*(INT8(NP2+1))*INT8(4)/INT8(1024*1024)
                     CALL ERR_LOG ( 4186, IUER, 'GLO_BACK', 'Error in '// &
     &                   'attempt to grab dynamic memory for two normal '// &
     &                   'matrices sized for '//STR(1:I_LEN(STR))// &
     &                   ' parameters ' )
#ifdef HPUX
                     CALL MEMORYMAP ( %VAL(1) )
#endif
                     RETURN
                END IF
!
! ------------- Zeroing grabbed dynamic memory
!
                CALL NOUT8 ( GLBMEM%LEN_LOC, %VAL(GLBMEM%ADR_LOC) )
           END IF
!
! -------- Restore prfil -- it may be needed since it keeps psitd for piece
! -------- wise model of station positions
!
           CALL USE_PARFIL ( 'ORC' )
!
! -------- ... and then compute
!
           CALL ERR_PASS ( IUER, IER )
           CALL BACK_DO  ( MODE_BACK, B3DOBJ, B1B3DOBJ, GLBMEM%L_GPA, &
     &                     GLBMEM%ADR_GLO, GLBMEM%L_ARC, GLBMEM%ADR_NORMARR2, &
     &                     M3, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4187, IUER, 'GLO_BACK', 'Error during '// &
     &              'computation of local parameters and their covaraince '// &
     &              'while the database '//DBNAME_MES//' was processed' )
                RETURN
           END IF
      END IF
!
! --- Set some flags
!
      KOUTPUT = ISOLU.EQ.1 .OR. ( ICONT.EQ.0 .AND. ISOLU.EQ.0 )
      PURGARC = ARC_EXIST .AND. PURGARC .AND. ISOLU.EQ.1
!
      CALL USE_GLBFIL   ( 'OW' )  ! save glbcm since COVP, ADJST, CRES read it
      CALL USE_GLBFIL_4 ( 'WC' )  ! save glbcm since COVP, ADJST, CRES read it
!
! --- Purge an arc file if we don't need it any more
!
      IF ( PURGARC ) CALL ACS_ARCFIL ( SAVAF, KBIT( PRE_IBATCH, INT2(8)), &
     &    'P' )
      IF ( KOUTPUT ) THEN
           IF ( IARCNM .EQ. INT2(1) ) THEN
!
! ------------- Copying global SOCOM and PARFIL blocks from internal BATCH
! ------------- to socom and prfil
!
                CALL LIB$MOVC3 ( JSOCOM_BYTES,  %VAL(GLBMEM%ADR_GLO_SOCOM), &
     &                           PI_VAR )
                CALL LIB$MOVC3 ( JPARFIL_BYTES, %VAL(GLBMEM%ADR_GLO_PRFIL), &
     &                           VAXOF(1) )
!
! ------------- Compute adjustments for global parameters
!
                KGLOBALS = .TRUE.
                IPTR = 1
                CALL ERR_PASS ( IUER, IER )
                CALL ADJST_DO ( GLBMEM%L_GPA, %VAL(GLBMEM%ADR_GLO), &
     &                          CRES_BUF_LEN2, LBUF, IPTR, PAGEWID, CNSTROBJ, &
     &                          IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 4188, IUER, 'GLO_BACK', 'Errors during '// &
     &                   'computing and printing adjustments for global '// &
     &                   'parameters' )
                     RETURN
                END IF
                KGLOBALS = .FALSE.
                CALL SBIT       ( PRE_IP(3), INT2(10), INT2(0) )
!
! ------------- Restore local prfil for this session -- it may be needed since
! ------------- it keeps psitd for piecewise model of station positions
!
                CALL USE_PARFIL ( 'ORC' )
                CALL USE_COMMON ( 'ORC' )
           END IF
!
           IF ( .NOT. SKIP_ARC ) THEN
!
                 CALL USE_COMMON ( 'ORC' )
                 CALL SOCOM_EXT()
!
! -------------- Memory:
! -------------- non-fast: ADR_NORMARR2, length MATSIZE, then (adjacent address)
! --------------           ADR_NORMARR,  length MATSIZE. Local-local covariance
! --------------           matrix is in ADR_NORMARR
! --------------
! -------------- B1B3D:    ADR_NORMARR2, length MATSIZE. Local-local covariance
! --------------           matrix is there.
!
                 IF ( FAST_MODE .EQ. F__B1B3D ) THEN
!
! ------------------- Set the address ADR_NORMARR to ADR_NORMARR2 in order to
! ------------------- have the same address of local-local covaraince matrix
! ------------------- for fast and non-fast mode
!
                      GLBMEM%ADR_NORMARR = GLBMEM%ADR_NORMARR2
                   ELSE IF ( FAST_MODE .EQ. F__NONE ) THEN
!
! ------------------- We recalculate the size of arrays for local matrices.
! ------------------- First address of array ADR_NORMARR should follow just
! ------------------- the last byte of ADR_NORMARR2. It is legitimate since
! ------------------- NEW_MATSIZE is less than the old one. BACK_DO returns
! ------------------- variable M3 which keeps the size of the array ADR_NORMARR
! ------------------- which BACK assumed
!
                      NEW_MATSIZE = INT8(M3)*8
                      GLBMEM%ADR_NORMARR = GLBMEM%ADR_NORMARR2 + NEW_MATSIZE
                 END IF
!
! -------------- Computation of residuals
!
                 IPTR = 0
                 CALL CRES_DO ( INT2(0), GLBMEM%L_ARC, &
     &                          %VAL(GLBMEM%ADR_NORMARR), B3DOBJ, CNSTROBJ, &
     &                          CRES_BUF_LEN2, LBUF, IPTR, PAGEWID )
!
! -------------- Computation of adjustments to local parameters and
! -------------- putting them in listing
!
                 IPTR = 1
                 CALL ERR_PASS ( IUER, IER )
!
                 CALL ADJST_DO ( GLBMEM%L_ARC, %VAL(GLBMEM%ADR_NORMARR), &
     &                           CRES_BUF_LEN2, LBUF, IPTR, PAGEWID, CNSTROBJ, &
     &                           IER )
!
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 4189, IUER, 'GLO_BACK', 'Errors during '// &
     &                    'computing and printing adjustments while the '// &
     &                    'database '//DBNAME_MES//' was processed' )
                      RETURN
                 END IF
           END IF
      END IF
!
      CALL USE_GLBFIL ( 'ORC' )  ! Restore glbcm, since, f.e., CRES writes there
!
! --- Free memory allocated for local normal system if it was somehow
! --- allocated earlier
!
      IF ( GLBMEM%LEN_LOC .GT. 0 ) THEN
           CALL FREE_MEM ( GLBMEM%ADR_LOC )
           GLBMEM%ADR_LOC = 0
           GLBMEM%LEN_LOC = -1
      END IF
      IF ( .NOT. SKIP_ARC       .AND. &
     &     MODE_BACK .EQ.2      .AND. &
     &     GLBMEM%L_ARC .GT. 0        ) THEN
!
! -------- Free dynamic memory allocated by BACK_DO
!
           CALL FREE_MEM ( GLBMEM%ADR_NORMARR2 )
           GLBMEM%ADR_NORMARR  = 0
           GLBMEM%ADR_NORMARR2 = 0
           GLBMEM%L_ARC = -1
      END IF
!
! --- Free dynamic memory allocated for B3DOBJ and B1B3DOBJ  objects
!
      IF ( .NOT. SKIP_ARC  .AND.  FAST_MODE .EQ. F__B1B3D ) THEN
           CALL ERR_PASS      ( IUER, IER )
           CALL B1B3D_FREEMEM ( B3DOBJ, B1B3DOBJ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4190, IUER, 'GLO_BACK', 'Error in attempt to '// &
     &              'free dynamic memory allocated for B3D, B1B3D internal '// &
     &              'data structures while the database '//DBNAME_MES// &
     &              ' was processed' )
                RETURN
           END IF
      END IF
!
      IF ( FAST_DBG .EQ. F__APP  .OR.  FAST_DBG .EQ. F__PRI ) THEN
           WRITE ( 6, * ) ' GLO_BACK ended for '//DBNAME_MES
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GLO_BACK  !#!#
