#define NO_DEBUG
      SUBROUTINE GLO_ELIM ( MODE, DBNAME_MES, GLBMEM, B3DOBJ, B1B3DOBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GLO_ELIM  makes appropriate actions for parameter         *
! *   elimination step for the specific session in the forward run of    *
! *   the global solution. It                                            *
! *   a) get list of arc parameters;                                     *
! *   b) makes operations of linear algebra for elimination of the       *
! *      influence of ARC parameters from the matrix of global           *
! *      parameters to be contributed to CGM.                            *
! *   c) Makes cross reference table between global parameters of this   *
! *      session and parameters in CGM.                                  *
! *   d) Write modified system of session normal equations in ARC-file.  *
! *   e) Update lists in CGM-like commons socom and prfil if new source  *
! *      of station is added to the list of stations/sources in CGM.     *
! *   f) Updates CGM for the influence of this arc.                      *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *       MODE ( INTEGER*4 ) -- Mode switcher.                           *
! *                          MODE=1 -- made all steps a), b), c), d), e) *
! *                          MODE=2 -- made ONLY steps a), b).           *
! * DBNAME_MES ( CHARACTER ) -- Line with the database name and its      *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     GLBMEM ( RECORD    ) -- Data structure which keeps addresses of  *
! *                             CGM, list of global parameters, global   *
! *                             socom, prfil and temporary normal        *
! *                             matrices. Defined in                     *
! *                             $PSOLVE_ROOT/include/glbp.i              *
! *     B3DOBJ ( RECORD    ) -- Object with data structure for B3D       *
! *                             extension of SOLVE.                      *
! *   B1B3DOBJ ( RECORD    ) -- Object with data structure for B1B3D     *
! *                             extension of SOLVE.                      *
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
! *  2001.05.10 pet  2.3  Increased amount of memory allocated for CGM   *
! *                       by 256 bytes since it is read and written by   *
! *                       256-bytes-long blocks.                         *
! *                                                                      *
! *  2001.07.30 pet  2.4  Added the trap of internal control of the      *
! *                       situation when the number of global parameters *
! *                       exceeded the limit of the scratch file.        *
! *                                                                      *
! *  2002.09.26 pet  2.5  Added the code which updates the minimal       *
! *                       Julian date of nominal start and maximal       *
! *                       Julian date of nominal end over all sessions   *
! *                       of this global solution.                       *
! *                                                                      *
! *  ###  05-JAN-1999    GLO_ELIM  v2.52 (c)  L. Petrov 11-JUN-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'addcm.i'
      INCLUDE   'erm.i'
      INCLUDE   'socom.i'
      INCLUDE   'socom_plus.i'
      INCLUDE   'precm.i'
      INCLUDE   'prfil.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'glbp.i'
      INCLUDE   'fast.i'
!
      CHARACTER  DBNAME_MES*(*)
      INTEGER*4  MODE, IUER
      LOGICAL*4  F_MEMFAULT
      TYPE ( B3D__STRU ) ::  B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      TYPE ( GLB_MEM__STRU ) ::  GLBMEM  ! defined in glbp.i
!
      INTEGER*2    IPARM1(10,M_GPA), IPARM2(10,M_GPA), IPARM3(10,M_GPA)
      COMMON   / PARAM / IPARM1, IPARM2, IPARM3
      CHARACTER*20 LPARM_PROC(M_GPA), LPARM_GLO(M_GPA), LPARM_ARCPE(M_GPA)
      EQUIVALENCE (IPARM1,LPARM_PROC),(IPARM2,LPARM_GLO),(IPARM3,LPARM_ARCPE)
      CHARACTER   LPARM_NEW(M_GPA)*(L__GPA), LPARM_CGM(M_GPA)*(L__GPA)
      INTEGER*4   IXPTA(M_GPA), IXGTC(M_GPA), NPARM_PROC, NPARM_GLO, &
     &            NPARM_ARCPE, NPARM_NEW, NPARM_CGM, NLTA
      LOGICAL*2   STACM, KFIXED
      ADDRESS__TYPE :: ADR_AGG, ADR_BG, SHF_AGG, SHF_BG, &
     &                 ARR1_JA, ARR1_JB, ARR2_JA, ARR2_JB, ARR2_JS
      INTEGER*4   IOFF, J0, J1, J2, J3, J4, IER
      ADDRESS__TYPE :: POS_CGM1, POS_CGM2, POS_G1, POS_G2
      INTEGER*8   IND_CGM, IND_GG
      REAL*8      FJD_BEG, FJD_END
      CHARACTER   STR*16, STR2*16, FINAM_NRM*80, DEBUG_ARCF*8
      INTEGER*4   I4_ARG, J4_ARG
      INTEGER*8   I8_ARG, J8_ARG
      LOGICAL*2   FS_F(3), KOLD, KPURGABLE, REALLY_CREATE
      CHARACTER         SAVNAM*(NAME_SIZE)
      COMMON / NAMARC / SAVNAM
!
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INTEGER*8  LOCS
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*2, EXTERNAL :: KBIT
      INTEGER*4, EXTERNAL :: I_LEN
      LOCS(I8_ARG,J8_ARG) = min(I8_ARG,J8_ARG) + &
     &                     (max(I8_ARG,J8_ARG)*(max(I8_ARG,J8_ARG)-1))/2
!
      CALL CLRCH    (  DEBUG_ARCF )  
      CALL GETENVAR ( 'DEBUG_ARCF', DEBUG_ARCF )
!
      NPARM_ARCPE = 0
      F_MEMFAULT = .FALSE.
      IF ( MODE .NE. 1  .AND.  MODE .NE. 2 ) THEN
           CALL CLRCH   ( STR )
           CALL INCH    ( MODE, STR )
           CALL ERR_LOG ( 4131, IUER, 'GLO_ELIM', 'Wrong value of the '// &
     &         'actual argument MODE: '//STR(1:I_LEN(STR))//' one of '// &
     &         '1 or 2 was expected' )
           RETURN
      END IF
!
      IF ( FAST_DBG .EQ. F__APP ) THEN
           WRITE ( 6, * ) ' GLO_ELIM started '
      END IF
!
! --- Initialization
!
      DO 400 J0=1,M_GPA
         CALL CLRCH ( LPARM_PROC(J0)  )
         CALL CLRCH ( LPARM_GLO(J0)   )
         CALL CLRCH ( LPARM_ARCPE(J0) )
         CALL CLRCH ( LPARM_NEW(J0)   )
         CALL CLRCH ( LPARM_CGM(J0)   )
         IXPTA(J0) = -1
         IXGTC(J0) = -1
 400  CONTINUE
!
      STACM = KBIT ( PRE_IBATCH, INT2(8) )
!
      IF ( FAST_MODE .EQ. F__B1B3D ) THEN
!
! -------- Change SOCOM if re-parameterization took place during computation
! -------- of normal equations
!
           CALL ERR_PASS ( IUER, IER )
           CALL REPARAM  ( B3DOBJ%U_STA, B3DOBJ%UIS_STA, &
     &                     B3DOBJ%U_BAS, B3DOBJ%UIS_BAS, &
     &                     B3DOBJ%R_SOU, B3DOBJ%RIS_SOU, &
     &                     B3DOBJ%R_STA, B3DOBJ%RIS_STA, &
     &                     B3DOBJ%R_BAS, B3DOBJ%RIS_BAS, 0, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4132, IUER, 'GLO_ELIM', 'Error during attempt '// &
     &              'to reparameterize solution while database '// &
     &               DBNAME_MES//' was processing' )
                RETURN
           ENDIF
      END IF
!
! --- Calculation of various lists of the parameters to be estimated
! --- as well as their crossreference.
! --- LPARM_PROC -- list of all parameters in the session in PROC order
! --- NPARM_PROC -- total number of parameters in the session
! --- NPARM_GLO  -- number of global parameters to be exported to CGM from
! ---               this session
! --- IXPTA      -- the cross reference from sessions parameters in PROC order
! ---               to the ARPCE order
!
      CALL LISTS_ARC ( IXPTA, NPARM_PROC, NPARM_ARCPE, NPARM_GLO, NLTA, STACM, &
     &                 LPARM_PROC )
#ifdef DEBUG
   write ( 6, * ) 'glo_elim-175 nparm_proc= ', nparm_proc, ' nparm_glo= ', nparm_glo ! %%%%%
!   do 510 ioff=1,nparm_proc
!      write ( 6, * ) 'glo_elim-176 lparm_proc= ', lparm_proc(ioff), ' ixpta= ', ixpta(ioff)
! 510  continue 
#endif
!
! --- Apply EOP constraint if fixed arc
!
      KFIXED = KBIT ( PRE_IP(3), INT2(14) )
      IOFF   = 3*M_GPA
      IF ( KFIXED ) THEN
!
! -------- Suppress estimation of EOP by imposing very stiff constraints
!
           CALL FIX_EOP ( B3DOBJ, B1B3DOBJ, %VAL(GLBMEM%ADR_NORMARR), IOFF, &
     &                    LPARM_PROC, NPARM_PROC )
      ENDIF
      IF ( FAST_MODE .EQ. F__NONE ) THEN
!
! -------- Non-fast mode requires special action
!
           ARR1_JA = GLBMEM%ADR_NORMARR  + 8*(3*M_GPA)
           ARR1_JB = GLBMEM%ADR_NORMARR  + 8*(2*M_GPA)
           ARR2_JA = GLBMEM%ADR_NORMARR2 + 8*(3*M_GPA)
           ARR2_JB = GLBMEM%ADR_NORMARR2 + 8*(2*M_GPA)
           ARR2_JS = GLBMEM%ADR_NORMARR2 + 8*(  M_GPA)
!
! -------- Rearrange normal matrix from PROC order to ARCPE order in full mode
!
           CALL ABMOVE ( %VAL(ARR2_JA), %VAL(ARR2_JB), %VAL(ARR1_JA), &
     &          %VAL(ARR1_JB), IXPTA, NPARM_ARCPE )
      END IF
      IF ( FAST_DBG .EQ. F__TIM ) THEN
           CALL TIM_GET ( 'FORW-01' )
           CALL TIM_INIT()
      END IF
!
      IF ( DEBUG_ARCF == 'APP' ) THEN
           WRITE ( 6, 210 ) IARCNM, DBNAME_CH, FAST_MODE
 210       FORMAT ( 'GLO_ELIM-214 Arcnum: ', I6, ' DB_NAME: ', A, ' FAST_MODE: ', I1 )
      END IF
      IF ( FAST_MODE .EQ. F__NONE ) THEN
!
! -------- Elimination of arc local parameters in old, non-fast mode
!
           CALL ELIMIN ( %VAL(GLBMEM%ADR_NORMARR2), %VAL(ARR2_JS), &
     &                   %VAL(ARR2_JB), %VAL(ARR2_JA), IARCS, IGLBLS )
           GLBMEM%L_ARC = IGLBLS + IARCS
           IF ( MODE .EQ. 2 ) THEN
!
! ------------- MODE=2 -- deal done: good bye!
!
                CALL ERR_LOG ( 0, IUER )
                RETURN
           END IF
           IF ( FAST_DBG .EQ. F__TIM ) THEN
                CALL TIM_GET ( 'FORW-02' )
                CALL TIM_INIT()
           END IF
           ADR_AGG = ARR2_JA
           ADR_BG  = ARR2_JB
!
! -------- Now create arc-file
!
           CALL CLRCH ( SAVNAM )
           KPURGABLE = .FALSE.
           REALLY_CREATE = .TRUE.
           CALL CREATE_ARCF ( SAVAF, KPURGABLE, IARCNM, ARCDIR, NPARAM, &
     &                        STACM, KOLD, 'N', REALLY_CREATE, FS_F )
           CALL ACS_ARCFIL  ( SAVAF, KBIT( PRE_IBATCH, INT2(8)), 'C' )
!
! -------- Now write down the local matrix of normal equations in arc-file
!
           CALL OUTMT ( %VAL(GLBMEM%ADR_NORMARR2), NPARM_ARCPE, STACM )
           IF ( FAST_DBG .EQ. F__TIM ) THEN
                CALL TIM_GET ( 'FORW-03' )
                CALL TIM_INIT()
           END IF
        ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
!
! -------- Elimination of arc local and segmented parameters in B1B3D mode
!
           CALL ERR_PASS    ( IUER, IER )
           CALL ARCPE_B1B3D ( B3DOBJ, B1B3DOBJ, ADR_AGG, ADR_BG, IER )
           GLBMEM%L_ARC = B3DOBJ%N_GLO + B3DOBJ%N_LOC + B3DOBJ%N_SGM
!
! -------- Keep condition number
!
           RCOND = B3DOBJ%RCOND
           CALL USE_GLBFIL_4 ( 'OWC' )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4133, IUER, 'GLO_ELIM', 'Error during '// &
     &              'parameter elimination while the database '// &
     &               DBNAME_MES//' was processing' )
                RETURN
           ENDIF
!
           IF ( FAST_DBG .EQ. F__TIM ) THEN
                CALL TIM_GET ( 'FORW-02' )
                CALL TIM_INIT()
           END IF
           IF ( MODE .EQ. 2 ) THEN
!
! ------------- MODE=2 -- deal done: good bye!
!
                CALL ERR_LOG ( 0, IUER )
                RETURN
           END IF
!
! -------- Build the ARC-file name
!
           CALL CLRCH        ( FINAM_NRM )
           CALL ERR_PASS     ( IUER, IER )
           CALL ARCFILE_NAME ( FINAM_NRM, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4134, IUER, 'GLO_ELIM', 'Error during '// &
     &              'forming name for arc-file while the '// &
     &              'database '//B3DOBJ%DBNAME_MES//' was processing' )
                RETURN
           ENDIF
           IF ( DEBUG_ARCF(1:3) == 'YES' ) THEN
                WRITE ( 6, * ) 'GLO_ELIM-293 IARCNM = ', IARCNM, ' FINAM_NRM= ', TRIM(FINAM_NRM)
           END IF
!
! -------- Writing fields of B1B3DOBJ and B3DOBJ objects in the ARC file
!
           CALL ERR_PASS    ( IUER, IER )
           CALL WRNOR_B1B3D ( FINAM_NRM, B3DOBJ, B1B3DOBJ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4135, IUER, 'GLO_ELIM', 'Error during attempt '// &
     &              ' to write file '//FINAM_NRM(1:I_LEN(FINAM_NRM))//' with '// &
     &              'temporary data structure for B1B3D algorithm while '// &
     &              'the database '//DBNAME_MES//' was processing' )
                RETURN
           ENDIF
!
           IF ( FAST_DBG .EQ. F__TIM ) THEN
                CALL TIM_GET ( 'FORW-03' )
                CALL TIM_INIT()
           END IF
      END IF
!
! --- Get the Julian date of nonimal start and nominal end of the session
!
      CALL OBSTM ( FJD_BEG, FJD_END )
!
! --- Update if necessary minimal and maixmal Julian date over all sessions
!
      IF ( .NOT. ARCPE_WORKED ) THEN
           GLO_FJDOBS_MIN = FJD_BEG
           GLO_FJDOBS_MAX = FJD_END
!
! -------- Store the start and end datas
!
           DO 410 J1=1,NUMSTA
              STA_FJD_BEG(J1) = FJD_BEG
              STA_FJD_END(J1) = FJD_END
              STA_FJD_MID(J1) = (FJD_BEG + FJD_END)/2.0
              NSES_STA(J1)    = 0
 410       CONTINUE
!
           DO 420 J2=1,NUMSTR
              SRC_FJD_BEG(J2) = FJD_BEG
              SRC_FJD_END(J2) = FJD_END
              SRC_FJD_MID(J2) = (FJD_BEG + FJD_END)/2.0
              NSES_SRC(J2)    = 0
 420       CONTINUE
         ELSE
           GLO_FJDOBS_MIN = MIN ( GLO_FJDOBS_MIN, FJD_BEG )
           GLO_FJDOBS_MAX = MAX ( GLO_FJDOBS_MAX, FJD_END )
      END IF
!
! --- Set the flag inidicating that ARCPE has processed at least one session
!
      ARCPE_WORKED = .TRUE.
!
! --- Make a local copy of the list of global parameters in the GLBMEM
! --- data structure
!
      NPARM_CGM = GLBMEM%L_GPA
      CALL LIB$MOVC3 ( L__GPA*GLBMEM%L_GPA, %VAL(GLBMEM%ADR_C_GPA), LPARM_CGM )
!
! --- Update the lists of parameters in CGM, lists of stations and sources and
! --- their a priori in q_socom, q_prfil kept in GLBMEM and creation of
! --- a cross reference array IXGTC between the global parameters from this
! --- session to be moved to CGM and the list of paramters in CGM
!
      CALL ERR_PASS     ( IUER, IER )
      CALL LISTS_UPDATE ( GLBMEM, NPARM_GLO, LPARM_GLO, NPARM_CGM, LPARM_CGM, &
     &     NPARM_NEW, LPARM_NEW, IXGTC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4136, IUER, 'GLO_ELIM', 'Error during attempt '// &
     &          'to update internal lists while the database '//DBNAME_MES// &
     &          ' was processing' )
           RETURN
      END IF
!
      IF ( NPARM_CGM .GT. NRMFL_PARMS ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR2 )
           CALL INCH  ( NPARM_CGM,   STR  )
           CALL INCH  ( NRMFL_PARMS, STR2 )
           CALL ERR_LOG ( 4137, IUER, 'GLO_ELIM', 'Error in processing '// &
     &          'the database '//DBNAME_MES//' the number of global '// &
     &          'parameters '//STR(1:I_LEN(STR))//' exceeded the limit '// &
     &           STR2(1:I_LEN(STR2))//' for which scratch files were sized. '// &
     &          'This is a fatal error. Recommendation: to increase '// &
     &          'the limit by executing solve_reset and then to launch '// &
     &          'your solution anew' )
           RETURN
      END IF
!
      IF ( NPARM_CGM .GT. GLBMEM%NPAR_CGM ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL GLO_MEM_FAULT ( B3DOBJ, B1B3DOBJ, NPARM_CGM, NPARM_ARCPE, &
     &                          DBNAME_MES, FINAM_NRM, ADR_AGG, ADR_BG, &
     &                          GLBMEM, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4138, IUER, 'GLO_ELIM', 'Failure to recover '// &
     &              'from memory fault during processing database '//DBNAME_MES )
                RETURN 
           END IF 
      END IF ! end of recovery after memory fault
!
! --- Now copy back the list of global parameters to the GLBMEM data structure
!
      GLBMEM%L_GPA = NPARM_CGM
      CALL LIB$MOVC3 ( L__GPA*GLBMEM%L_GPA, LPARM_CGM, %VAL(GLBMEM%ADR_C_GPA) )
!
      IF ( NPARM_GLO .GT. 0 ) THEN
!
! -------- Updating global-global normal matrix and normal vector
!
           DO 430 J3=1,NPARM_GLO
              POS_CGM1 = IXGTC(J3)
              IF ( FAST_MODE .EQ. F__NONE ) THEN
                   POS_G1 = J3 + IARCS
                 ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
                   POS_G1 = J3
              END IF
!
! ----------- Update appropriate element of combined global vector
!
              IF ( SUBTRACT_ARC ) THEN
                   CALL R8ELEM_SUB ( %VAL(GLBMEM%ADR_CGV), POS_CGM1, &
     &                               %VAL(ADR_BG),         POS_G1    )
                 ELSE
                   CALL R8ELEM_ADD ( %VAL(GLBMEM%ADR_CGV), POS_CGM1, &
     &                               %VAL(ADR_BG),         POS_G1    )
              END IF
!
              DO 440 J4=1,J3
                 POS_CGM2 = IXGTC(J4)
                 IND_CGM  = LOCS ( POS_CGM1, POS_CGM2 )
                 IF ( FAST_MODE .EQ. F__NONE ) THEN
                      POS_G2 = J4 + IARCS
                    ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
                      POS_G2 = J4
                 END IF
                 IND_GG = LOCS ( POS_G1, POS_G2 )
!
! --------------- Update appropriate element of combined global matrix
!
                 IF ( SUBTRACT_ARC ) THEN
                      CALL R8ELEM_SUB ( %VAL(GLBMEM%ADR_CGM), IND_CGM, &
     &                                  %VAL(ADR_AGG),        IND_GG   )
                    ELSE
                      CALL R8ELEM_ADD ( %VAL(GLBMEM%ADR_CGM), IND_CGM, &
     &                                  %VAL(ADR_AGG),        IND_GG   )
                 END IF
 440          CONTINUE
 430       CONTINUE
      END IF
!
      IF ( IARCNM == 1  .AND. FL_EERM ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL EXPAND_CGM_EERM ( B3DOBJ, B1B3DOBJ, NPARM_ARCPE, &
     &                            DBNAME_MES, FINAM_NRM, ADR_AGG, ADR_BG, &
     &                            GLBMEM, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4144, IUER, 'GLO_ELIM', 'Error during '// &
     &              'reading an attempt to expand CGM in order '// &
     &              'to accomulate all Empirical Earth Rotation Model '// &
     &              'parameters' )
                RETURN
           END IF
!
           IF ( FAST_DBG .EQ. F__APP ) THEN
                WRITE ( 6, * ) ' EXPANDD_CGM_EERM ended CGM expansion'
           END IF
      END IF
!
      IF ( FAST_DBG .EQ. F__TIM ) THEN
           CALL TIM_GET ( 'FORW-04' )
           CALL TIM_INIT()
      END IF
      IF ( FAST_DBG .EQ. F__APP ) THEN
           WRITE ( 6, * ) ' GLO_ELIM ended '
      END IF
#ifdef DEBUG
   write ( 6, * ) 'glo_elim-461 GLBMEM%L_GPA= ', GLBMEM%L_GPA ! %%%%
#endif
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GLO_ELIM  #!#
