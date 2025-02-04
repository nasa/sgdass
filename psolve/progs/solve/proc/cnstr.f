#define NO_DEBUG
      SUBROUTINE  CNSTR ( B3DOBJ, B1B3DOBJ, NOR_MAT, NOR_VEC, CNSTROBJ, IUER )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INTEGER*4   NUM_WTS
      CHARACTER   LPARM_WT(3,MAX_STA)*20
!
! 1.  CNSTR PROGRAM SPECIFICATION
!
! 1.1 This program reads the data substitution file, extracts correlations,
!     deduces appropriate covariances, adds these covariances to the normal
!     equations.
!
! 1.2 REFERENCES:
!
! 2.  CNSTR INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*4  IUER
      REAL*8     NOR_MAT(*), NOR_VEC(*), WT(3,MAX_STA)
!
!     NOR_MAT - Normal equation matrix
!
! 2.3 OUTPUT Variables:
!
!     NOR_MAT - Modified normal equation matrix
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INCLUDE 'erm.i'
      INCLUDE 'socom.i'
      INCLUDE 'socom_plus.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'glbc3.i'
      INCLUDE 'cnstr.i'
!
! 2.5 SUBROUTINE INTERFACER
!
!     CALLING SUBROUTINES: pstlp
!       CALLED SUBROUTINES: do_eop, do_atm, do_clk, do_eop_a1
!
! 3.  LOCAL VARIABLES
!
!
      LOGICAL*4     TRUE_L4, FALSE_L4
      PARAMETER   ( TRUE_L4  = .TRUE.  )
      PARAMETER   ( FALSE_L4 = .FALSE. )
      LOGICAL*4     WAS_ALL_EST, FL_UGL_OBSOLETE
      INTEGER*2     IWDS, ISTS, ISTR_LEN
      PARAMETER   ( ISTR_LEN=20, IWDS=10, ISTS=1 )
      INTEGER*2     IPARM(IWDS,M_GPA), IPARM_GLO(IWDS,M_GPA)
      CHARACTER     LPARM(M_GPA)*20,   LPARM_GLO(M_GPA)*20, &
     &              LPARM_LOC(M_GPA)*20
      EQUIVALENCE ( LPARM, IPARM )
      EQUIVALENCE ( LPARM_GLO, IPARM_GLO )
      INTEGER*4     IPRM_INDX(M_GPA), END_LPARM, NPARM, NPARM_GLO, NPARM_LOC
      INTEGER*4     I, J, ITEMP(4), IX_ALL_GLO(M_GPA), NUM_USER_PART_SAVE
      INTEGER*4     J0, J1, J2, J3, MEND, IND_PMX, IND_PMY, IND_UT1, CNI_SAVE, &
     &              NUM_BREAKS 
      CHARACTER     WHO(3)*20, TEMP*8, OBJ_SAVE*10
      EQUIVALENCE  (ITEMP(1),TEMP)
      CHARACTER     WHO_STA(MAX_ARC_STA)*8, DUMSTR*8, ERRSTR*255, BUFSTR*80, &
     &              STR*20, FNAME*128
      LOGICAL*4     LEX, LIST_FL
      LOGICAL*2,    EXTERNAL :: KBIT
      LOGICAL*4     CHECK_STABIT
      DATA          WHO / 3*'                    ' /
!
! 4. HISTORY
!  WHO  WHEN   WHAT
!  AEE  910515 Enhanced error messages written to the error file.
!  AEE  911120 Allowed for condition of having no_constraints for
!              an ARC (for rates).
!  jmg  960610 Remove holleriths.
!  pet  970116 Added support imposing constraints for B3D parametrization
!  pet  970131 Added support B3D parametrization for constraint CONSTRAINT_CM
!              "Combed" source code.
!  pet  970226 Added support imposing constraints for B1B3D parametrization
!  pet  970331 Rewrote imposing constraints for segemnted parameters: atm,
!              clock and eop.
!  pet  970401 Put temporary detour imposing constraints for station segmented
!              clock in SEG_CNSTR
!  pet  970425 Cleaned text.
!  pet  970603 Improved error message
!  pet  970610 Added support constraints on EOP and EOP rate for the case when
!              we estimate segmented EOP
!  pet  970927 Added support constraints on source coordinates and
!              no-net-translation ( they can be apply only in independent mode,
!              since SOLVE imposed them in NORML in global mode )
!  pet  971024 Corrected OLDCLO_OCT97 and UNFCLO_OCT97 bugs. Ceased support of
!              non-uniform clocks for SEG_CNSTR. DO_CLK_9612 supports case of
!              clocks with non-uniform segments
!  pet  971128 Added test before applying No-Net-Translation constraints: they
!              will be applied only if all coordinates of all stations will
!              be estimated.
!  pet  971203 Added logic for bypassing deselected station
!  pet  980109 Added logic for imposing constraints for archaic atmosphere
!              parameterization when atmophere rates are estimated
!  pet  980122 Improved debugging messages
!  JMG  980212 Modified so that if we are doing a sinex solution we do not
!              constrain EOP or station position from Control file.
!              Instead, we apply weak constraints.
!  KDB  980311 fix typo.  (Wt was declared as max_stat, not max_sta.)
!  pet  980330 Fixed a bug: OPTIN was trying to impose both constraints:
!              LIN_COM and NNT_POS, what was wrong: only LIN_COM constraint
!              should be imposed in interactive solution. Added also a checkL
!              LIN_COM constrint will be set only if positions of all selected
!              stations are estiamted, else variable-flag kcentermass will be
!              set up to .FALSE.
!  pet  980722 Changed calls of constraint_cm, do_blc, sinex_cns in order to
!              pass values of sigmas as actual parameters
!  pet  980722 Added capacity not to impose constraqints on baseline dependent
!              clocks
!  pet  2000.01.19  Added support of user local constraints. CNSTR reads the
!                   file with user local constraints ( ULC<initials> ) when
!                   a) user partials were computed; b) file exists, transforms
!                   indices from that file and applies constraints.
!                   It is assumed that 1) ULC-file has the same format as
!                   CSR-file; 2) ULC-file contains constraints for local user
!                   parameters only; 3) indices of the element in the file are
!                   counted from the the furst user-parameter.
!  pet  2001.03.12  Changed the arguments list for CNSTR and DO_USER
!  pet  2002.03.12  Made changes in code in order to allow three constraints:
!                   NNT_POS, NNR_POS, NNR_SRC to be imposed on local parameters
!  pet  2002.03.13  Remove fossils of the old logic with handling so-called
!                   sinex-constraints
!  pet  2002.05.09  Added NNT_POS, NNR_POS, NNR_SRC constraints. Chenge the
!                   logic when information aboout constraints is printed in
!                   listing
!  pet  2002.09.25  Changed entirely format of user locval constraints and
!                   the logic of applying this constraints. Modifed calls of
!                   numerous constraints, DO_BLC, DO_ATM_O, DO_CLK_O, DO_NUT,
!                   DO_GRADR, DO_SRC in order co take into account changes
!                   in logic of these subroutines.
!  pet  2003.04.28  Fixed a bug: the previous version may change NUM_USER_PART
!                   from the value of the number of user parameters for this
!                   session only to the total number of global user parameters.
!                   It may result in serious error if CNSTR was called in
!                   the back run of the global solution.
!  pet  2003.05.06  Added logic for local user constraints programs which was
!                   somehow lost during previous updates.
!  jwr  2003.05.15  TRUE__L2 and FALSE__L2 introduced in  -I2 conversion.
!  pet  2003.08.12  Replaced CRES_PRE98 with CRES_STYLE
!  pet  2003.08.22  Added once again logic for local user constraints programs 
!                   which was somehow lost during previous updates.
!
!     CNSTR PROGRAM STRUCTURE
!
!   fill the array 'WHO_STA' with station names for further processing
!
!CCCCC
      INCLUDE   'fast.i'
      TYPE ( B3D__STRU ) ::    B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      INTEGER*4  IER
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: I_LEN
!c    Transformation INT2 --> INT4
!
! --- Test: how many stations we have
!
      IF ( NUMSTA .GT. MAX_ARC_STA ) THEN
           CALL FERR ( INT2(16), 'Too many stations in cnstr', INT2(0), &
     &          INT2(0) )
      ENDIF
!
! --- Putting in array WHO_STA the list of stations names
!
      DO I=1,NUMSTA
         WHO_STA(I) = ISITN_CHR(I)
      ENDDO
!
      IF ( SOCOM_PLUS_FIRST .NE. SPL__DONE ) THEN
           CALL ERR_LOG ( 8591, IUER, 'CNSTR', 'socom_plus has '// &
     &         'not been initialized. It is fatal error' )
           RETURN
      END IF
#ifdef DEBUG
  write ( 6, * ) 'cnstr-186 ' ; call flush ( 6) ! %%%
#endif
!
! --- Get parameter names as character*20 strings and put them in array LPARM
! --- This list will contain all parameters: local and global
!
      KGLOBONLY = .FALSE.
      CALL GET_NAMES ( LPARM, ISTR_LEN, M_GPA, NPARM, TRUE__L2, FALSE__L2  )
!
! --- Make a copy of LPARM
!
      CALL LIB$MOVC3 ( 20*NPARM, LPARM, LPARM_LOC )
      NPARM_LOC = NPARM
!
      IF ( ISLTY2 .EQ. 'I' ) THEN
!
! -------- No global parameters
!
           NPARM_GLO = 0
!
! -------- All parameters are local
!
         ELSE
!
! -------- First we have to get a list of global parameters.
! -------- Save socom since further we will destroy it
!
           CALL USE_COMMON ( 'OWC' )
!
! -------- Save  NUM_USER_PART -- we need to keep the number of user parameters
! -------- for this session only
!
           NUM_USER_PART_SAVE = NUM_USER_PART
!
! -------- Read part of GLB  -file where information about global
! -------- parameters is kept
!
           CALL USE_GLBFIL_2 ( 'ORC' )
!
! -------- Setting flag for further call of GET_NAMES which will point out
! -------- on neccesity to build list of only ghlobal parameters
!
           CALL DEPAR()
!
! -------- Building a list of global parameters -- LPARM_GLO
!
           KGLOBONLY = .TRUE.
           CALL GET_NAMES ( LPARM_GLO, ISTR_LEN, M_GPA, NPARM_GLO, TRUE__L2, &
     &                      TRUE__L2 )
           KGLOBONLY = .FALSE.
!
! -------- Read SOCOM once more since DEPAR destructed some data structures
!
           CALL USE_COMMON ( 'ORC' )
!
! -------- Make a cross reference table form all parameters to global
!
           CALL CXEPAR_OPT20 ( LPARM, IX_ALL_GLO, NPARM, LPARM_GLO, NPARM_GLO )
!
! -------- Now "update" the list LPARM_LOC -- it will have empty lines for
! -------- local parameters. Thus, global parameters will be erased, and
! -------- cnstr will not be in  a position to set constraints on them.
! -------- A cunning trick, isn't it?
!
           DO 410 J1=1,NPARM
              IF ( IX_ALL_GLO(J1) .GT. 0 ) THEN
                   LPARM_LOC(J1) = '                    '
              END IF
 410       CONTINUE
!
! -------- Restore NUM_USER_PART
!
           NUM_USER_PART = NUM_USER_PART_SAVE
      END IF
!
      CALL GETENVAR ( 'USER_GLOBAL_CONSTRAINT', STR )
      IF ( STR .EQ. 'OBSOLETE'  .OR.  STR .EQ. 'obsolete' ) THEN
           FL_UGL_OBSOLETE = .TRUE.
         ELSE
           FL_UGL_OBSOLETE = .FALSE.
      END IF
!
!%  write ( 6, * ) 'cnstr-267 ' ; call flush ( 6) ! %%%
      IF ( ISLTY2 .EQ. 'I' .AND. KUSER_CONST .AND. .NOT. FL_UGL_OBSOLETE ) THEN
!
! -------- Check wheteher the user constraint program exists
!
           IF ( FAST_DBG .EQ. F__PRI ) THEN
                write ( 6, * ) ' cnstr: before user: n_equat=',cnstrobj%n_equat
           ENDIF
           INQUIRE ( FILE=USER_CONST_PROG, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 8592, IUER, 'CNSTR', 'User constraint '// &
     &              'program '//USER_CONST_PROG(1:I_LEN(USER_CONST_PROG))// &
     &              ' was not found' )
                RETURN
           END IF
!
! -------- Run user constraints program for local mode
!
           CALL RUN_PROG ( USER_CONST_PROG, 'WAIT', INT2(1) )
!
           CNI_SAVE = CNSTROBJ%CNS_TYP
           OBJ_SAVE = CNSTROBJ%OBJ_NAM
!
! -------- Read equations of constraints
!
           CALL ERR_PASS   ( IUER, IER )
           CALL READ_CNSTR ( CNSTROBJ, CNI__ULC, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8593, IUER, 'CNSTR', 'Error in an '// &
     &              'attempt to read user local constraints in CNSTROBJ '// &
     &              'object from scratch file '//FNAME )
                RETURN
           END IF
           CNSTROBJ%CNS_TYP = CNI_SAVE
           CNSTROBJ%OBJ_NAM = OBJ_SAVE
           IF ( FAST_DBG .EQ. F__PRI ) THEN
                write ( 6, * ) ' cnstr: after  user: n_equat=',cnstrobj%n_equat
           ENDIF
         ELSE IF ( ISLTY2 .NE. 'I' ) THEN
!
! -------- Aga. In batch mode user constraints are imposed in a user program.
! -------- It can write user local constraints in the file. 
! -------- Remember: ULCCxx file was removed by BATCH(prcess) before running
! -------- user program and before proc.
!
           FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'ULCC'//PRE_LETRS
           INQUIRE ( FILE=FNAME, EXIST=LEX )
           IF ( LEX ) THEN
                CNI_SAVE = CNSTROBJ%CNS_TYP
                OBJ_SAVE = CNSTROBJ%OBJ_NAM
!
! ------------- Read equations of constraints
!
                CALL ERR_PASS   ( IUER, IER )
                CALL READ_CNSTR ( CNSTROBJ, CNI__ULC, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8594, IUER, 'CNSTR', 'Error in an '// &
     &                   'attempt to read user local constraints in '// &
     &                   'CNSTROBJ object from scratch file '//FNAME )
                     RETURN
                END IF
                CNSTROBJ%CNS_TYP = CNI_SAVE
                CNSTROBJ%OBJ_NAM = OBJ_SAVE
                IF ( FAST_DBG .EQ. F__PRI ) THEN
                     write ( 6, * ) ' cnstr: after reading local user '// &
     &                              'constrains: n_equat=',cnstrobj%n_equat, &
     &                              ' n_ecnst=',cnstrobj%n_ecnst 
                ENDIF
           END IF
      END IF
!%  write ( 6, * ) 'cnstr-337 ' ; call flush ( 6) ! %%%
!
! --- Imposing constraints for segmented parameters
!
      IF ( SOLVE_EMULATION .EQ. 0 ) THEN
           CALL ERR_PASS  ( IUER, IER )
           CALL SEG_CNSTR ( INT4(NUMSTA), WHO_STA, NPARM, LPARM, &
     &                      CNSTROBJ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8595, IUER, 'CNSTR', 'Error during '// &
     &              'imposing constraints for segmented parameters' )
                RETURN
           END IF
           IF ( FAST_DBG .EQ. F__PRI ) THEN
                WRITE ( 6, * ) ' seg_cnstr     fast_mode = ', fast_mode, ' n_cnstr = ', &
     &                                 cnstrobj%n_equat
           END IF
!%  write ( 6, * ) 'cnstr-354 ' ; call flush ( 6) ! %%%
!
           IF ( .NOT. UNF_CLO  .AND.  KBIT( CONSTRAINT_BITS, INT2(3) ) ) THEN
!
! ------------- Apply clock constraints for the case when clock rates were
! ------------- esitmated
!
                CALL ERR_PASS  ( IUER, IER )
                CALL DO_CLK_O ( WHO_STA, LPARM, NPARM, CNSTROBJ, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8596, IUER, 'CNSTR', 'Error during '// &
     &                   'imposing constraints for clock rates' )
                     RETURN
                END IF
!
                IF ( FAST_DBG .EQ. F__PRI ) THEN
                     WRITE ( 6, * ) ' do_clk_o      fast_mode = ',fast_mode, &
     &                      ' n_cnstr = ',cnstrobj%n_equat
                     WRITE ( 6, * ) ' This arc has old style clocks'
                ENDIF
           ENDIF
!%  write ( 6, * ) 'cnstr-375 ' ; call flush ( 6) ! %%%
!
           IF ( .NOT. UNF_ATM  .AND.  KBIT( CONSTRAINT_BITS, INT2(2) ) ) THEN
!
! ------------- Apply atmosphere constraints for the case when atmosphere rates
! ------------- were esitmated. It will work actually only for the case when the
! ------------- atmosphere rate is estimated and will not change anything in
! ------------- other case. DO_ATM_O is valid for full case only. Error message
! ------------- occur in other case.
!
!%  write ( 6, * ) 'cnstr-385 ' ; call flush ( 6) ! %%%
                CALL ERR_PASS ( IUER, IER )
                CALL DO_ATM_O ( WHO_STA, LPARM, NPARM, CNSTROBJ, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8597, IUER, 'CNSTR', 'Error '// &
     &                   'during imposing constraints for segmented '// &
     &                   'parameters' )
                     RETURN
                ENDIF
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8598, IUER, 'CNSTR', 'Error during '// &
     &                   'imposing constraints for atmosphere rates' )
                     RETURN
                END IF
                IF ( FAST_DBG .EQ. F__PRI ) THEN
                     WRITE ( 6, * ) ' do_atm_o      fast_mode = ',fast_mode, &
     &                      ' n_cnstr = ', cnstrobj%n_equat
                     WRITE ( 6, * ) ' This database has old style atmosphere'
                END IF
           ENDIF
        ELSE ! Emulation mode
!%  write ( 6, * ) 'cnstr-400 ' ; call flush ( 6) ! %%%
!
! ------- Imposing constraints for the special emulation mode
!
          IF ( KBIT( CONSTRAINT_BITS, INT2(2) ) ) THEN ! apply atmosphere constraints
               IF ( FL_SINEX_MAKE ) THEN
                    CALL ERR_LOG  ( 8599, IUER, 'CNSTR', 'Constraint '// &
     &                  'on clocks in emulation mode are not compatible '// &
     &                  'with  the request to generate a listing in '// &
     &                  'SINEX format.' )
                    RETURN
               END IF
!
               CALL DO_ATM_9612 ( WHO_STA, LPARM, NPARM, NOR_MAT, CNSTROBJ )
               IF ( FAST_DBG .EQ. F__PRI ) THEN
                    WRITE ( 6, * ) ' do_atm_9612   fast_mode = ',fast_mode, &
     &                     ' n_cnstr = ', cnstrobj%n_equat
               END IF
          ENDIF
          IF ( KBIT( CONSTRAINT_BITS, INT2(3) ) ) THEN ! apply clock constraints
               IF ( .NOT. OLD_CLOCKS ) THEN
                    IF ( FL_SINEX_MAKE ) THEN
                         CALL ERR_LOG  ( 8600, IUER, 'CNSTR', &
     &                       'Constraint on clocks in emulation mode are '// &
     &                       'not compatible with  the request to generate '// &
     &                       'a listing in SINEX format.' )
                         RETURN
                    END IF
!
                    CALL DO_CLK_9612 ( FAST_MODE, FAST_DBG, WHO_STA, LPARM, &
     &                   NPARM, NOR_MAT, B3DOBJ, B1B3DOBJ, CNSTROBJ )
                    IF ( FAST_DBG .EQ. F__PRI ) THEN
                         WRITE ( 6, * ) ' do_clk_9612   fast_mode = ',fast_mode, &
     &                          ' n_cnstr = ', cnstrobj%n_equat
                    END IF
                  ELSE IF ( OLD_CLOCKS ) THEN
!
! ----------------- Apply clock constraints for the case when clock rates were
! ----------------- esitmated
!
                    CALL ERR_PASS ( IUER, IER )
                    CALL DO_CLK_O ( WHO_STA, LPARM, NPARM, CNSTROBJ, IER )
                    IF ( FAST_DBG .EQ. F__PRI ) THEN
                         WRITE ( 6, * ) ' do_clk_o      fast_mode = ',fast_mode, &
     &                          ' n_cnstr = ',cnstrobj%n_equat
                         WRITE ( 6, * ) ' This arc has old style clocks'
                    END IF
               ENDIF
          ENDIF
      END IF
!
      IF ( KCENTERMASS ) THEN
!
! -------- NB: Word cetnermass is misleading. It has nothing common with mass.
! -------- It is merely an akward name for the no-net-translation constraints.
! -------- Check: was all station coordiantes for all stations estimated?
!
           WAS_ALL_EST = .TRUE.
           DO I=1,NUMSTA
!
! ----------- Test: is the I-th station in solution
!
              IF ( CHECK_STABIT ( I ) ) THEN
                      IF ( .NOT. KBIT ( LSITEC(1,1), I ) ) WAS_ALL_EST = .FALSE.
                      IF ( .NOT. KBIT ( LSITEC(1,2), I ) ) WAS_ALL_EST = .FALSE.
                      IF ( .NOT. KBIT ( LSITEC(1,3), I ) ) WAS_ALL_EST = .FALSE.
              END IF
           ENDDO
!
           IF ( WAS_ALL_EST ) THEN
                IF ( FL_SINEX_MAKE ) THEN
                     CALL ERR_LOG  ( 8601, IUER, 'CNSTR', &
     &                   'NO_NET_TRANSLATION consrtaints cannot be used '// &
     &                   'togeatherwith requrest to generate listing in '// &
     &                   'SINEX format. This constraint is an obsolete '// &
     &                   'feature. Use NO_NET_TRANSLATION_POSITION  instead '// &
     &                   'of it.' )
                     RETURN
                END IF
!
! ------------- Constraint some linear combination of station positions.
!
                CALL GET_CM_WTS   ( LPARM_WT, WT, NUM_WTS )
                IF ( NUM_WTS > 0 ) THEN
                     CALL CONSTRAIN_CM ( FAST_MODE, FAST_DBG, LIN_STA_SIGMA, NPARM, &
     &                                   LPARM, LPARM_WT, WT, NUM_WTS, NOR_MAT, &
     &                                   B3DOBJ, B1B3DOBJ, CNSTROBJ )
!
                     IF ( FAST_DBG .EQ. F__PRI ) THEN
                          WRITE ( 6, * ) ' constrain_cm  fast_mode = ',fast_mode,' n_cnstr = ', &
     &                                     cnstrobj%n_equat
                     END IF
                END IF
              ELSE
!
! ------------- If not, then flag of applying linear combination on station
! ------------- positions constraint will be cleared and variable kcentermass
! ------------- from glbc4 will be set .FALSE.
!
                KCENTERMASS = .FALSE.
                CALL USE_GLBFIL_4 (  'OWC' )
           ENDIF
      ENDIF
!
! --- Add weak constraint to baseline clocks to prevent failures to invert
!
!%   write ( 6, * ) 'cnstr-506 ' ; call flush ( 6) ! %%%
      IF ( KBSL_CONST ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL DO_BLC ( WHO_STA, LPARM, NPARM, BAS_CLK_SIGMA, CNSTROBJ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG  ( 8602, IUER, 'CNSTR', 'Error in an '// &
     &              'attempt to impose constraints on baseline dependent '// &
     &              'clocks' )
                RETURN
           END IF
!
           IF ( FAST_DBG .EQ. F__PRI ) THEN
                WRITE ( 6, * ) ' do_blc        fast_mode = ',fast_mode,' n_cnstr = ', &
     &                                 cnstrobj%n_equat
           END IF
      END IF
!
! --- CONSTRAINT_BITS is bit array for types of constraints to apply
!
      IF ( KBIT( CONSTRAINT_BITS, INT2(7) ) ) THEN
!%  write ( 6, * ) 'cnstr-526 ' ; call flush ( 6) ! %%%
           CALL CINDEX_PARM ( 'NUT', IPRM_INDX, LPARM, NPARM, &
     &                         END_LPARM, WHO, TRUE__L2, dumstr )
           CALL ERR_PASS ( IUER, IER )
           CALL DO_NUT ( IPRM_INDX, END_LPARM, WHO, CNSTROBJ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG  ( 8603, IUER, 'CNSTR', 'Error in an '// &
     &              'attempt to impose constraints on nutation angle '// &
     &              'constraints' )
                RETURN
           END IF
           IF ( FAST_DBG .EQ. F__PRI ) THEN
                WRITE ( 6, * ) ' do_nut        fast_mode = ',fast_mode,' n_cnstr = ', &
     &                                 cnstrobj%n_equat
           END IF
      ENDIF
!
!%  write ( 6, * ) 'cnstr-543 ' ; call flush ( 6) ! %%%
      IF ( KBIT( CONSTRAINT_BITS, INT2(1) ) ) THEN
!
! -------- Constrain Earth orientation offsets
!
! -------- First locate all the earth orientation offset parameters in the
! -------- parameter list
!
           CALL CINDEX_PARM ( 'EOP', IPRM_INDX, LPARM, NPARM, END_LPARM, &
     &                         WHO, TRUE__L2, DUMSTR )
#ifdef DEBUG
  write ( 6, * ) 'cnstr-558 NPARM= ', NPARM, ' end_lparm = ', end_lparm, IPRM_INDX(1:END_LPARM+1) ; call flush ( 6 )
#endif
           IF ( IPRM_INDX(1).NE.0 ) THEN ! make sure at least 1 offset has
!                                        ! been chosen
! ------------ The earth orientation offset constraints will currently only
! ------------ work if there are only three offsets - one apiece for x-wobble,
! ------------ y-wobble and ut1, with each offset at the same epoch
!
               IF ( END_LPARM .GT. 3 ) THEN
!
! ----------------- In the case when we estimated segmented EOP the list of
! ----------------- EOP contains more than 3 parmeters. We revise this list
! ----------------- leaving offset only for the initial epoch
!
                    MEND = END_LPARM
                    IND_PMX = 0
                    IND_PMY = 0
                    IND_UT1 = 0
                    DO 420 J2=1,MEND
!
! -------------------- Test: is it the first appearance of X wobble parameter?
!
                       IF ( IND_PMX .EQ. 0   .AND. &
     &                      LPARM( IPRM_INDX(J2) )(1:8) .EQ. 'X WOBBLE' ) THEN
                          IND_PMX = IPRM_INDX(J2)
                       END IF
!
! -------------------- Test: is it the first appearance of Y wobble parameter?
!
                       IF ( IND_PMY .EQ. 0   .AND. &
     &                      LPARM( IPRM_INDX(J2) )(1:8) .EQ. 'Y WOBBLE' ) THEN
                          IND_PMY = IPRM_INDX(J2)
                       END IF
!
! -------------------- Test: is it the first appearance of UT1 parameter?
!
                       IF ( IND_UT1 .EQ. 0   .AND. &
     &                      LPARM( IPRM_INDX(J2) )(1:7) .EQ. 'UT1-TAI' ) THEN
                          IND_UT1 = IPRM_INDX(J2)
                       END IF
 420                CONTINUE
!
! ----------------- Build new revised list
!
                    END_LPARM = 0
                    IF ( IND_PMX .GT. 0 ) THEN
!
! ---------------------- Add  X wobble
!
                         END_LPARM = END_LPARM + 1
                         IPRM_INDX(END_LPARM) = IND_PMX
                         WHO(END_LPARM) = LPARM(IND_PMX)
                    END IF
                    IF ( IND_PMY .GT. 0 ) THEN
!
! ---------------------- Add  Y wobble
!
                         END_LPARM = END_LPARM + 1
                         IPRM_INDX(END_LPARM) = IND_PMY
                         WHO(END_LPARM) = LPARM(IND_PMY)
                    END IF
                    IF ( IND_UT1 .GT. 0 ) THEN
!
! ---------------------- Add  UT1
!
                         END_LPARM = END_LPARM + 1
                         IPRM_INDX(END_LPARM) = IND_UT1
                         WHO(END_LPARM) = LPARM(IND_UT1)
                    END IF
               END IF
!
               IF ( END_LPARM .NE. 3 ) THEN
                    CALL CLRCH ( STR )
                    IF ( END_LPARM .GT. 3 ) THEN
                         ERRSTR = 'PROC (CNSTR) error: Attempt to constrain '// &
     &                            'Earth orientation parameters while '// &
     &                            'treating them as segmented parameters. '// &
     &                            'Such a combination is not supported'
                         CALL FERR ( INT2(199), ERRSTR, INT2(0), INT2(0) )
                      ELSE
                         CALL INCH  ( END_LPARM, STR )
                         CALL CLRCH ( ERRSTR )
                         ERRSTR = 'PROC (CNSTR) error: must parameterize '// &
     &                            '3 EOP parameters, but you try to estimate '// &
     &                             STR(1:I_LEN(STR))//' EOP parameters'
                         CALL FERR ( INT2(129), ERRSTR, INT2(0), INT2(0) )
                     END IF
                     CALL ERR_LOG ( 0, IUER )
                     RETURN
                  ELSE IF ( EOP_STYLE(1) .NE. EOP_STYLE(2) ) THEN
                     ERRSTR = 'PROC (CNSTR) error: eop styles can not be mixed'
                     CALL FERR ( INT2(130), ERRSTR, INT2(0), INT2(0) )
                     CALL ERR_LOG ( 0, IUER )
                     RETURN
                  ELSE IF ( EOP_STYLE(1) .EQ. 0  .AND.  NROT .NE. 1 ) THEN
                     ERRSTR = 'PROC (CNSTR) error: EOP offsets must be '// &
     &                        'parameterized at the same epoch'
                     CALL FERR ( INT2(131), ERRSTR, INT2(0), INT2(0) )
                     CALL ERR_LOG ( 0, IUER )
                     RETURN
               ENDIF
#ifdef DEBUG
               write ( 6, * ) 'cnstr-654 ' ; call flush ( 6) ! %%%
#endif
!
! ------------ Apply constraints
!
               CALL ERR_PASS ( IUER, IER )
               CALL DO_EOP   ( IPRM_INDX, END_LPARM, WHO, CNSTROBJ, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 8604, IUER, 'CNSTR', 'Failure to '// &
     &                  'impose constraints on EOP' )
                    RETURN
               END IF
!
               IF ( FAST_DBG .EQ. F__PRI ) THEN
                    WRITE ( 6, * ) ' do_eop        fast_mode = ',fast_mode, &
     &                     ' n_cnstr = ', cnstrobj%n_equat
               END IF
           ENDIF
      ENDIF
!
      IF ( KBIT( CONSTRAINT_BITS, INT2(6) ) )  THEN
!
! -------- Constrain Earth orientation rates
!
! -------- First locate all the Earth orientation rate parameters in the
! -------- parameter list
!
           CALL CINDEX_PARM ( 'EOR', IPRM_INDX, LPARM, NPARM, END_LPARM, &
     &                         WHO, TRUE__L2, DUMSTR )
           IF ( IPRM_INDX(1) .NE. 0 ) THEN ! make sure at least 1 offset has
!                                          ! been chosen
! ----------- The earth orientation rate constraints will currently only work
! ----------- if there are only three rates - one apiece for x-wobble, y-wobble
! ----------- and ut1, with each offset at the same epoch
!
              IF ( END_LPARM .NE. 3 ) THEN
                   CALL CLRCH ( BUFSTR )
                   BUFSTR = 'in cnstr.f, PROC, end_lparm = '
                   CALL INCH ( END_LPARM, BUFSTR(I_LEN(BUFSTR)+2:) )
                   CALL PRCH ( BUFSTR(1:I_LEN(BUFSTR))//CHAR(13)//CHAR(10) )
                   CALL CLRCH ( ERRSTR )
                   ERRSTR = 'PROC(cnstr) error: you must all parameterize '// &
     &                      '3 EOP rate '
                   CALL FERR ( INT2(569), ERRSTR, INT2(0), INT2(0) )
                   CALL ERR_LOG ( 0, IUER )
                   RETURN
                ELSE IF ( EOP_STYLE(1) .NE. EOP_STYLE(2) ) THEN
                   WRITE ( ERRSTR, "('cnstr error: eop styles can not ', &
     &                               'be mixed')" )
                   CALL FERR ( INT2(130), ERRSTR, INT2(0), INT2(0) )
                   CALL ERR_LOG ( 0, IUER )
                   RETURN
                ELSE IF ( EOP_STYLE(1) .EQ. 0 .AND. NROT .NE. 1 ) THEN
                   ERRSTR = '(PROC(cnstr)  error: eo offsets must be '// &
     &                      'parameterized at the same epoch'
                   CALL FERR ( INT2(131), ERRSTR, INT2(0), INT2(0) )
                   CALL ERR_LOG ( 0, IUER )
                   RETURN
              ENDIF
!
              CALL ERR_PASS ( IUER, IER )
              CALL DO_REOP  ( IPRM_INDX, END_LPARM, WHO, CNSTROBJ, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8605, IUER, 'CNSTR', 'Error in '// &
     &                 'an attempt to impose constraints on EOP rate' )
                   RETURN
              END IF
              IF ( FAST_DBG .EQ. F__PRI ) THEN
                   WRITE ( 6, * ) ' do_reop       fast_mode = ',fast_mode,' n_cnstr = ', &
     &                                    cnstrobj%n_equat
              END IF
           ENDIF
      ENDIF
!%  write ( 6, * ) 'cnstr-727 ' ; call flush ( 6) ! %%%
!
      IF ( KBIT( CONSTRAINT_BITS, INT2(8) ) ) THEN ! apply gradient constraints
!
! -------- Apply gradient parameter constraints. There are no flags for these
! -------- constraints like those for standard atmosphere and clock params since
! -------- this is currently a special version.
!
           CALL ERR_PASS ( IUER, IER )
           CALL DO_GRADR ( FAST_MODE, WHO_STA, LPARM, NPARM, CNSTROBJ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG  ( 8606, IUER, 'CNSTR', 'Error in an '// &
     &              'attempt to impose constraints on atmopshere gradients '// &
     &              'and their rates' )
                RETURN
           END IF
           IF ( FAST_DBG .EQ. F__PRI ) THEN
                WRITE ( 6, * ) ' do_gradr      fast_mode = ',fast_mode, &
     &                         ' n_cnstr = ', cnstrobj%n_equat
           END IF
      ENDIF
!
      NUM_BREAKS = 0 
      DO 430 J3=1,NUMSTA
         IF ( NUM_BRK(J3) > 0 ) THEN
              NUM_BREAKS = NUM_BREAKS + 1
         END IF
 430  CONTINUE 
!
!%  write ( 6, * ) 'cnstr-771 ' ; call flush ( 6) ! %%%
      IF ( ISTASP .NE. 0  .OR.  KBIT( DEFCMP, INT2(5) ) ) THEN
           IF ( CRES_STYLE == CRES__PRE98 .OR. & ! Suppress in compat. mode
     &          ISLTY2 == 'F'                  ) THEN  ! Suppress in forward run
!
                LIST_FL = .FALSE.              
              ELSE 
                LIST_FL = .TRUE.
           END IF
!
! -------- No net translation for positions
!
           CALL ERR_PASS ( IUER, IER )
           CALL NNT_POS  ( FAST_MODE, FAST_DBG, LIST_FL, FALSE_L4, LPARM_LOC, &
     &                     NPARM, NOR_MAT, NNT_POS_RTP, NNT_POS_SIGMA, &
     &                     B3DOBJ, CNSTROBJ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG  ( 8607, IUER, 'CNSTR', 'Error in an '// &
     &              'attempt to impose local NNT_POS constraints' )
                RETURN
           END IF
      END IF
!%  write ( 6, * ) 'cnstr-793 ' ; call flush ( 6) ! %%%
!
      IF ( ISTASP .NE. 0  .OR.  KBIT( DEFCMP, INT2(6) ) ) THEN
           IF ( CRES_STYLE == CRES__PRE98 .OR. & ! Suppress in compat. mode
     &          ISLTY2 == 'F'                  ) THEN  ! Suppress in forward run
!
                LIST_FL = .FALSE.              
              ELSE 
                LIST_FL = .TRUE.
           END IF
!
! -------- No net rotation for positions
!
           CALL ERR_PASS ( IUER, IER )
           CALL NNR_POS  ( FAST_MODE, FAST_DBG, LIST_FL, FALSE_L4, LPARM_LOC, &
     &                     NPARM, NOR_MAT, NNR_POS_RTP, NNR_POS_SIGMA, &
     &                     B3DOBJ, CNSTROBJ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG  ( 8608, IUER, 'CNSTR', 'Error in an '// &
     &              'attempt to impose local NNR_POS constraints' )
                RETURN
           END IF
      END IF
!%  write ( 6, * ) 'cnstr-816 ' ; call flush ( 6) ! %%%
!
      IF ( ISRCSP .NE. 0  .OR.  KBIT( DEFSRC, INT2(7) ) ) THEN
!
! -------- No net rotation for sources
!
           CALL ERR_PASS ( IUER, IER )
           IF ( ISLTY2 .EQ. 'F' ) LIST_FL = .FALSE. ! Suppress in forward run
           CALL NNR_SRC  ( FAST_MODE, FAST_DBG, LIST_FL, FALSE_L4, LPARM_LOC, &
     &          NPARM, NOR_MAT, NNR_SRC_SIGMA, NNR_SRC_RTP, B3DOBJ, CNSTROBJ, &
     &          IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG  ( 8609, IUER, 'CNSTR', 'Error in an '// &
     &              'attempt to impose local NNR_SRC constraints' )
                RETURN
           ENDIF
      ENDIF
!
!%  write ( 6, * ) 'cnstr-834 ' ; call flush ( 6) ! %%%
      IF ( ISLTY2 .EQ. 'I' ) THEN
!
! -------- Independent mode
!
           IF ( KUSER_CONST .AND. FL_UGL_OBSOLETE ) THEN
!
! ------------- Apply user constraints
!
                CALL ERR_PASS ( IUER, IER )
                CALL DO_USER_CONSTRAINT ( FALSE_L4, LPARM, NPARM, CNSTROBJ, &
     &               NOR_MAT, NOR_VEC, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8610, IUER, 'CNSTR', 'Error '// &
     &                   'during imposing user constraints' )
                     RETURN
                ENDIF
           END IF
!%  write ( 6, * ) 'cnstr-844  KSRC_CONST= ', KSRC_CONST, ' SRC_COO_SIGMA = ', SRC_COO_SIGMA ; call flush ( 6) ! %%%
!
! -------- Applying some constraints on global parameters. This is done only
! -------- in independent mode, since these constraints are being imposed by
! -------- NORML when solution is running in global mode.
!
! -------- Apply weak sonstraints at the source positions
!
           IF ( KSRC_CONST ) THEN
                CALL ERR_PASS ( IUER, IER )
                CALL DO_SRC ( FAST_MODE, FAST_DBG, FALSE_L4, TRUE_L4, IWDS, &
     &               NPARM, IPARM, 0, IPARM, SRC_COO_SIGMA, 0.0D0, CNSTROBJ, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8611, IUER, 'CNSTR', 'Error '// &
     &                   'in an attempt to apply constraints on source '// &
     &                   'position' )
                     RETURN
                ENDIF
           ENDIF
         ELSE
!
! -------- Global mode (Backward or forward)
!
#ifdef DEBUG
      write ( 6, * ) 'cnstr-867  KSRC_CONST= ', KSRC_CONST, ' SRC_COO_SIGMA = ', SRC_COO_SIGMA ; call flush ( 6) ! %%%
      do 400 j0=1,nparm
         call lib$movc3 ( 20, iparm(1,j0), str(1:20) )
!!         call memcpy ( str(1:20), iparm(1,j0) )
         write ( 6, '("PROC (cnstr)-875 J0= ", I5, " Param: ", A,2X,A )' ) j0, lparm(j0), str(1:20)
 400  continue
#endif
           IF ( KSRC_CONST ) THEN
!
! ------------- Apply constraints on positions of the sources which are treated
! ------------- as local (arc) parameters
!
                CALL ERR_PASS ( IUER, IER )
                CALL DO_SRC ( FAST_MODE, FAST_DBG, FALSE_L4, FALSE_L4, IWDS, &
     &                        NPARM, IPARM, NPARM_GLO, IPARM_GLO, &
     &                        SRC_COO_SIGMA, 0.0D0, CNSTROBJ, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8612, IUER, 'CNSTR', 'Error '// &
     &                   'in an attempt to apply constraints on source '// &
     &                   'position' )
                     RETURN
                ENDIF
           ENDIF
      ENDIF
!%  write ( 6, * ) 'cnstr-873 ' ; call flush ( 6) ! %%%
!
      IF ( SOU_ADM_FLAG .NE. SOUADM__NO  .AND.  SOU_ADM_CNS > 0.0D0 ) THEN
!
! -------- Impose constrinat on source structure admittance
! 
           CALL ERR_PASS   ( IUER, IER ) 
           CALL SOUADM_CNS ( NPARM_LOC, LPARM_LOC, FALSE__L4, CNSTROBJ, &
     &                       IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8613, IUER, 'CNSTR', 'Error in '// &
     &              'an attempt to apply source structure admittance '// &
     &              'constraints' )
                RETURN
           END IF
      END IF
!
      IF ( IOS_EST .NE. IOS__UNDF  .AND. IOS_SIG .NE. 0.0D0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL IOS_CNSTR ( NPARM, LPARM, IOS_SIG, CNSTROBJ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8614, IUER, 'CNSTR', 'Error in '// &
     &              'an attempt to impose constraints on ionosphere '// &
     &              'scale parameter' )
                RETURN
           END IF
      END IF
!
#ifdef DEBUG
  write ( 6, * ) 'PROC cnstr-891 ' ; call flush ( 6) ! %%%
#endif
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  CNSTR  !#!#
