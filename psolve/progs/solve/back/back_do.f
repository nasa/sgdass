      SUBROUTINE BACK_DO ( MODE, B3DOBJ, B1B3DOBJ, L_GPA, ADR_GPA, &
     &                     N_ARR1, ADR_ARR1, M3, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  BACK_DO  is a part of a set of routines which makes       *
! *   global solution. It computes adjustments of local parameters of    *
! *   the current session and calculates their covariance matrix.        *
! *   BACK_DO suuports FULL, B1B3D and TRAIN, NO TRAIN modes.            *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      MODE ( INTEGER*4 ) -- mode switcher.                            *
! *                         MODE = 1 -- both CGM^(-1) and arc files are  *
! *                                     to be read from disk.            *
! *                         MODE = 2 -- CGM^(-1) is assumed to be        *
! *                                     already in memory and will not   *
! *                                     be read from disk while arc file *
! *                                     are to be read from disk.        *
! *                         MODE = 3 -- CGM^(-1) and arc file are        *
! *                                     assumed to be already in memory  *
! *                                     and will not be read from disk.  *
! *     L_GPA ( INTEGER*4 ) -- Number of global parameters of the run.   *
! *                            Defined only in MODE = 2,3.               *
! *   ADR_GPA ( INTEGER*8 ) -- Address of memory block for CGM^(-1).     *
! *                            Defined only in MODE = 2,3.               *
! *  ADR_ARR1 ( INTEGER*8 ) -- Address of memory block for intermediary  *
! *                            matrices and vectors of local parameters. *
! *                            Defined only in MODE = 3.                 *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *       M3 ( INTEGER*8 ) -- Maximal number parametersL; max(global,   *
! *                            local) which we used for computationof    *
! *                            amount of memory.                         *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    N_ARR1 ( INTEGER*8 ) -- Number of local parameters of the session.*
! *                            Defined only in MODE = 3.                 *
! *    B3DOBJ ( RECORD    ) -- Object with data structure for B3D        *
! *                            extension of SOLVE. Input value is        *
! *                            defined if MODE = 3 and FAST_MODE=F_NONE  *
! *  B1B3DOBJ ( RECORD    ) -- Object with data structure for B1B3D      *
! *                            extension of SOLVE. Input value is        *
! *                            defined if MODE = 3 and FAST_MODE=F_NONE  *
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
! *  WHEN       WHO  WHAT                                                *
! *  05-MAR-99  pet  1.0  Extracted from previous ../back/back.f         *
! *  15-APR-99  pet  1.1  Changed logic of grabbing dynamic memory.      *
! *  16-APR-99  pet  1.2  Added output parameter NP2.                    *
! *  15-MAY-99  pet  1.3  Corrected an error in computation of amount of *
! *                       allotted memory in MODE=2 for NON-FAST case.   *
! *  2001.05.10 pet  1.4  Increased amount of memory allocated for CGM   *
! *                       by 256 bytes since it is read and written by   *
! *                       256-bytes-long blocks.                         *
! *  2001.05.15 pet  1.5  Initialized memory by zero just after grabbing *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE 'solve.i'
!
      INCLUDE 'prfil.i'
      INCLUDE 'erm.i'
      INCLUDE 'socom.i'
      INCLUDE 'socom_plus.i'
      INCLUDE 'precm.i'
      INCLUDE 'baccm.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'fast.i'
!
      INTEGER*4  IUER, L_GPA
      INTEGER*8  M3
      ADDRESS__TYPE :: ADR_GPA, ADR_ARR1
!
      CHARACTER FINAM_NRM*80, STR*32, STR1*32, STR2*32
      CHARACTER SAVNAM*(NAME_SIZE)
      CHARACTER LPARMG(M_GPA)*20
      COMMON / NAMARC   / SAVNAM
      INTEGER*2  IRNSV(2)
      INTEGER*4  NP, IX1T3(M_GPA), J1
      REAL*8    VAL
      INTEGER*4 MAT_E, IT, IER
      INTEGER*2 INT2_ARG
      INTEGER*4 INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*8        MATSIZE, MEM_LEN
      ADDRESS__TYPE :: ADR_ARR2, MEM_ADR
      INTEGER*4        MODE, N_ARR1
!
      TYPE ( B3D__STRU   ) :: B3DOBJ
      TYPE ( B1B3D__STRU ) :: B1B3DOBJ
      INTEGER*4, EXTERNAL :: I_LEN
      ADDRESS__TYPE, EXTERNAL :: MAT_E4
!
! --- Set of traps of internal control
!
      IF ( MODE .NE. 1 .AND.  MODE .NE. 2 .AND.MODE .NE. 3 ) THEN
           CALL CLRCH   ( STR )
           CALL INCH    ( MODE, STR )
           CALL ERR_LOG ( 8511, IUER, 'BACK_DO', 'Wrong value of '// &
     &         'actual parameter MODE: '//STR(1:I_LEN(STR))//' -- one of '// &
     &         ' 1, 2, 3 was expected' )
           RETURN
      END IF
!
      IF ( ( MODE .EQ. 2  .OR.  MODE .EQ. 3 ) .AND. L_GPA .LE. 0   ) THEN
           WRITE ( 6, * ) ' MODE = ',MODE
           CALL CLRCH   ( STR )
           CALL INCH    ( L_GPA, STR )
           CALL ERR_LOG ( 8512, IUER, 'BACK_DO', 'Wrong value of '// &
     &         'actual parameter L_GPA '//STR(1:I_LEN(STR))//' -- a positive '// &
     &         'value was expected' )
           RETURN
      END IF
!
      IF ( MODE .EQ. 3  .AND.  N_ARR1 .LE. 0 ) THEN
           WRITE ( 6, * ) ' MODE = ',MODE
           CALL CLRCH   ( STR )
           CALL INCH    ( N_ARR1, STR )
           CALL ERR_LOG ( 8513, IUER, 'BACK_DO', 'Wrong value of '// &
     &         'actual parameter N_ARR1 '//STR(1:I_LEN(STR))// &
     &         ' -- a positive value was expected' )
           RETURN
      END IF
!
! --- Set timer
!
      CALL TIM_INIT()
!
      IF ( FAST_MODE .EQ. F__B1B3D ) THEN
!
! -------- Fast mode
!
           IF ( MODE .EQ. 1  .OR.  MODE .EQ. 2 ) THEN
!
! ------------- Get arcfile name
!
                CALL CLRCH ( FINAM_NRM )
                FINAM_NRM = SAVAF
!
! ------------- Reading fields of B1B3DOBJ and B3DOBJ objects
!
                CALL ERR_PASS ( IUER, IER )
                CALL RDNOR_B1B3D ( FINAM_NRM, B3DOBJ, B1B3DOBJ, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8514, IUER, 'BACK_DO', &
     &                   'Error during reading file '// &
     &                    FINAM_NRM(1:I_LEN(FINAM_NRM))//' with temporary '// &
     &                   'data structure for B1B3D algorithm while database '// &
     &                    B3DOBJ%DBNAME_MES//' was processing' )
                     RETURN
                ENDIF
!
! ------------- Set vebosity mode
!
                IF ( FAST_DBG .EQ. F__PRI ) THEN
                     IT = 1
                   ELSE
                     IT = 0
                END IF
!
! ------------- Make reparameterization (if needed)
!
                CALL ERR_PASS ( IUER, IER )
                CALL REPARAM  ( B3DOBJ%U_STA, B3DOBJ%UIS_STA, &
     &                          B3DOBJ%U_BAS, B3DOBJ%UIS_BAS, &
     &                          B3DOBJ%R_SOU, B3DOBJ%RIS_SOU, &
     &                          B3DOBJ%R_STA, B3DOBJ%RIS_STA, &
     &                          B3DOBJ%R_BAS, B3DOBJ%RIS_BAS, IT, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8515, IUER, 'BACK_DO', 'Error '// &
     &                   'during attempt to reparameterize solution while '// &
     &                   'database '//B3DOBJ%DBNAME_MES//' was processing' )
                     RETURN
                ENDIF
!
! ------------- Extraction condition number of the local-local matrix and
! ------------- rewriting it to GLBC4
!
                RCOND = B3DOBJ%RCOND
                CALL USE_GLBFIL_4 ( 'OWC' )
          END IF
      END IF  ! fast_mode
!
! --- Combine the appropriate global and arc parameters. It calculates the
! --- lists of the global and local parameters as well as their cross
! --- references
!
      CALL ERR_PASS ( IUER, IER )
      CALL PMCMB    ( IRNSV, IX1T3, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8516, IUER, 'BACK_DO', 'Error in pmcmb' )
           RETURN
      END IF
!
      IF ( MODE .EQ. 2  .OR.  MODE .EQ. 3 ) THEN
           IF ( L_GPA .NE. NPARM2 ) THEN
                CALL CLRCH (         STR1 )
                CALL INCH  ( L_GPA,  STR1 )
                CALL CLRCH (         STR2 )
                CALL INCH  ( NPARM2, STR2 )
                CALL ERR_LOG ( 8517, IUER, 'BACK_DO', 'Trap of '// &
     &              'internal control: L_GPA -- external number of global '// &
     &              'parameters: '//STR1(1:I_LEN(STR1))//' does not '// &
     &              'coincide with NPARM2 -- the number of global '// &
     &              'parameters computed internally: '//STR2 )
                CALL GET_NAMES ( LPARMG, INT2(20), M_GPA, NPARM2, TRUE__L2, TRUE__L2 )
                DO 410 J1=1,NPARM2
                   WRITE ( 6, 210 ) J1, LPARMG(J1)
 210               FORMAT ( 'Glo_par: ', I6, ' Par: ', A )
 410            CONTINUE 
                RETURN
           END IF
      END IF
!
      IF ( MODE .EQ. 3 ) THEN
           IF ( N_ARR1 .NE. NPARM1 ) THEN
                CALL CLRCH (         STR1 )
                CALL INCH  ( N_ARR1, STR1 )
                CALL CLRCH (         STR2 )
                CALL INCH  ( NPARM1, STR2 )
                CALL ERR_LOG ( 8518, IUER, 'BACK_DO', 'Trap of '// &
     &              'internal control: N_ARR1 -- external number of all '// &
     &              'parameters of the session : '//STR1(1:I_LEN(STR1))// &
     &              ' does not coincide with NPARM1 -- the number of all '// &
     &              'parameters in the session computed internally: '//STR2 )
                RETURN       
           END IF
      END IF
!
! --- Computation of size of necessary memory
! --- NPARM1 -- number of all parameters of this sessions
! --- NPARM2 -- number of global parameters in entire run
! --- NPARM3 -- number of all parameters of this sessions
!
      NP = MAX ( NPARM1, NPARM2, NPARM3 )
!
! --- Grab dymanic memory
!
      CALL ERR_PASS ( IUER, IER )
      IF ( MODE .EQ. 1 ) THEN
!
! -------- Case when CGM^(-1) will be read from disk and arc file also. We have
! -------- to allocate memory for both CGM^(-1) and local parameters.
! -------- NB: ADR_ARR1 and ADR_ARR2 should have adjacent addresses. This
! -------- feature is used on non-fast mode.
!
           MATSIZE = 8*MAT_E4( M_GPA, NP ) 
           CALL GRAB_MEM ( IER, MEM_LEN,  MEM_ADR, 1, 2*MATSIZE, ADR_ARR1 )
           CALL NOUT8 ( MATSIZE, %VAL(MEM_ADR) )
           ADR_ARR2 = ADR_ARR1 + MATSIZE
         ELSE IF ( MODE .EQ. 2  ) THEN
!
! -------- Case when CGM^(-1) is supplied as actual parameter and memory for it
! -------- is already allocated. But we have to allocate memory for local
! -------- parameters. Arcfile fill be read there in NON-fast mode and then
! -------- output covariance matrix of local parameters will be in this area.
! -------- output covariance matrix of local parameters will be there in fast
! -------- mode also
!
           IF ( FAST_MODE .EQ. F__B1B3D ) THEN
                M3 = MAT_E4 ( M_GPA, NP ) 
                MATSIZE = 8*MAT_E4 ( M_GPA, NP ) 
                CALL GRAB_MEM ( IER, MEM_LEN, MEM_ADR, 1, MATSIZE, &
     &                          ADR_ARR1 )
                CALL NOUT8 ( MATSIZE, %VAL(MEM_ADR) )
              ELSE
                M3 = MAT_E4 ( M_GPA, NP ) 
                MATSIZE = 8*MAT_E4 ( M_GPA, NP ) 
!
! ------------- We should use twice more memory in NON-FAST mode to be
! ------------- compatible with the logic of pre-DEC96 SOLVE. It uses extra
! ------------- memory LOCATED JUST AFTER ARR1 ADDRESS SPACE for keeping
! ------------- intermediary matrices.
!
                CALL GRAB_MEM ( IER, MEM_LEN, MEM_ADR, 1, 2*MATSIZE, &
     &                          ADR_ARR1 )
                CALL NOUT8 ( 2*MATSIZE, %VAL(MEM_ADR) )
           END IF
           IF ( IER .NE. 0 ) THEN
                WRITE ( 6, * ) ' NP = ',NP,' MATSIZE = ',MATSIZE,' MODE = ',MODE, &
     &                         ' FAST_MODE = ',FAST_MODE,' (back)MODE = ',MODE
                WRITE ( 6, * ) ' NPARM1 = ',NPARM1, ' NPARM2 = ',NPARM2, &
     &                         ' NPARM3 = ',NPARM3
                CALL CLRCH ( STR ) 
                CALL IINCH ( MEM_LEN, STR ) 
                CALL ERR_LOG ( 8519, IUER, 'BACK_DO', 'Failure to grab '// &
     &               STR(1:I_LEN(STR))//' bytes of operative memory' )
                RETURN       
           END IF
           ADR_ARR2 = ADR_GPA
         ELSE IF ( MODE .EQ. 3 ) THEN
!
! -------- We don't need to grab memory but we have to compute once more the
! -------- size of memory which we have -- M3 and to pass this value to
! -------- BACK_MAIN -- it is important for NON-fast mode
!
           M3 = MAT_E4 ( M_GPA, NP ) 
           IER = 0
           ADR_ARR2 = ADR_GPA
      END IF
!
      IF ( IER .NE. 0 ) THEN
           WRITE ( 6, * ) ' NP = ',NP,' MATSIZE = ',MATSIZE,' MODE = ',MODE, &
     &            ' FAST_MODE = ',FAST_MODE,' (back)MODE = ',MODE
           WRITE ( 6, * ) ' NPARM1 = ',NPARM1, ' NPARM2 = ',NPARM2, &
     &     ' NPARM3 = ',NPARM3
           CALL CLRCH ( STR )
           CALL IINCH ( MEM_LEN, STR )
           IUER = -1
           CALL ERR_LOG ( 8520, IUER, 'BACK_DO', 'Error in attempt '// &
     &         'to grab '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN
      END IF
!
      IF ( FAST_DBG .EQ. F__TIM ) THEN
!
! -------- Timing printout
!
           CALL TIM_GET ( 'BACK-01' )
           CALL TIM_INIT()
      END IF
!
! --- Making the main job
!
      CALL ERR_PASS ( IUER, IER )
      CALL BACK_MAIN ( M3, MODE, N_ARR1, %VAL(ADR_ARR1), %VAL(ADR_ARR2), &
     &     B3DOBJ, B1B3DOBJ, IRNSV, IX1T3, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8521, IUER, 'BACK_DO', 'Error in BACK_MAIN' )
           RETURN
      END IF
!
      IF ( MODE .EQ. 1 ) THEN
!
! -------- Free dynamic memory in the case of mode 1
!
           CALL FREE_MEM ( MEM_ADR )
         ELSE IF ( MODE .EQ. 2 ) THEN
!
! -------- ... but we don't free dynamic memory in mode 2. We only save
! -------- the number of parameters in this session
!
           N_ARR1 = NPARM1
         ELSE IF ( MODE .EQ. 3 ) THEN
!
! -------- Nothing to do in mode 3
!
           CONTINUE
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  BACK_DO  #!#
