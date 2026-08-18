      SUBROUTINE PSTLP ( B3DOBJ, B1B3DOBJ, CNSTROBJ, ARR, IUER )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  PSTLP PROGRAM SPECIFICATION
!
! 1.1 Constrain the normal equations, if necessary, and write them to disk.
!
! 1.2 REFERENCES:
!
! 2.  PSTLP INTERFACE
!
! 2.1 Parameter File
      INCLUDE   'solve.i'
      INCLUDE   'glbc3.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'precm.i'
      INCLUDE   'cnstr.i'
!
! 2.2 INPUT Variables: None
      REAL*8    ARR(*)
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE  'socom.i'
      INCLUDE  'fast.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: proc
!       CALLED SUBROUTINES: cnstr
!
! 3.  LOCAL VARIABLES
      INTEGER*4  JA, JB
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   mwh  940201  Implement dynamic memory allocation for normal matrix
!   pet  970128  Added stuff for supporting B3D algorithm. Added comments.
!                      Routine was actully rewrote
!   pet  970226  Added stuff for supporting B1B3D algorithm.
!   jmg  970227  Added saving normal matrix of constraints
!   pet  970927  Added minor changes to support imposing weak constraints on
!                source position and No-net-translation constraints in
!                independent mode
!   pet  971007  Added ability to write down normal matrix of constraints
!   pet  980120  Changed the logic for imposing constraints and writing them
!                on disk
!   pet  980211  Changed the arguments list of apply_cnstr in order to
!                support update of the elements of normal vector
!   pet  1999.05.31  Made CNSTROBJ a formal parameter
!   pet  2000.01.19  Minor change: added support og handling error core of the
!                    routine CNSTR
!   pet  2000.11.28 Forced Solve remove the scratch file with constraints
!                   before an attempt to apply constraints otherwise the
!                   stale constraint file may remain in disk in the case when
!                   no constraints are imposed.
!   pet  2001.03.09 Changed thte number of arguments fo call of
!                   do_user_constrain
!   pet  2002.03.14 Added initialization of new added counters in CNSTROBJ
!   pet  2002.05.08 Added code for grabbing dynamic memory for weight matrix of
!                   constraints
!   pet  2002.09.25 Added call of INIT_CNS routine.
!   pet  2014.09.08 Added support of local user constraints
!
! 5.  PSTLP PROGRAM STRUCTURE
!
!   now that we have formed the normal equations, constrain them if
!   so requested
!
!
      TYPE ( B3D__STRU ) ::  B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      CHARACTER  FINAM_NRM*128, STR*80
      INTEGER*8  LEN_WEI_CNS, MEM_LEN
      ADDRESS__TYPE :: ADR_WEI_CNS, MEM_ADR
      INTEGER*4  IUER, IER
      LOGICAL*4  FL_NOFD_IGNORE, FL_FULL_WEI 
      CHARACTER, EXTERNAL :: GET_DBNAME*10
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
      LOGICAL*2, EXTERNAL :: KBIT
!
!%   write ( 6, * ) 'pstlp-85 ' ; call flush ( 6 ) ! %%%%%%%%%%
      JA = 1 + 3*M_GPA
      JB = 1 + 2*M_GPA
!
! --- Remove the previous file with local constraints
!
      CALL UNLINK ( PRE_SCR_DIR(1:PRE_SD_LEN)//'CSPR'//PRE_LETRS//CHAR(0) )
!
! --- Now determine: whether we should apply constraints or not?
!
      IF ( ( CONSTRAINT_BITS .NE. 0 ) .OR. ( ISLTY2 .EQ. 'I' .AND. &
     &       (KSRC_CONST .OR. KBIT( DEFCMP, INT2(5) ) ) ) ) THEN
!
! -------- Initialization of the CNSTROBJ object
!
!%   write ( 6, * ) 'pstlp-100 ' ; call flush ( 6 ) ! %%%%%%%%%%
           CALL ERR_PASS ( IUER, IER )
           CALL INIT_CNS ( CNI__LOC, GET_DBNAME(), CNSTROBJ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8361, IUER, 'PSTLP', 'Failure in '// &
     &              'attempt to initialize CNSTROBJ' )
                RETURN
           END IF
!
! -------- Compute constraint equations
!
           CALL ERR_PASS ( IUER, IER )
           CALL CNSTR ( B3DOBJ, B1B3DOBJ, ARR(JA), ARR(JB), CNSTROBJ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8361, IUER, 'PSTLP', 'Failure in '// &
     &              'attempt to define constraint equations' )
                RETURN
           END IF
!
           CALL GETENVAR ( 'NOFD_IGNORE', STR )
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:3) == 'YES' .OR.  STR(1:2) == 'ON' ) THEN
                FL_NOFD_IGNORE = .TRUE.
             ELSE 
                FL_NOFD_IGNORE = .FALSE.
           END IF
           IF ( FAST_DBG .EQ. F__TIM ) THEN
                WRITE ( 6, * ) 'N_EQUAT,N_ECNST,N_OFD= ', CNSTROBJ%N_EQUAT, CNSTROBJ%N_ECNST, CNSTROBJ%N_OFD
           END IF
!
! -------- Grab dynamic memory for weight matrix of constraints
!
           IF ( CNSTROBJ%N_OFD > 0  .AND.  .NOT. FL_NOFD_IGNORE ) THEN
                LEN_WEI_CNS = 8*(CNSTROBJ%N_EQUAT*(CNSTROBJ%N_EQUAT+1))/2
                CALL ERR_PASS ( IUER, IER )
                CALL GRAB_MEM ( IER, MEM_LEN, MEM_ADR, 1, &
     &                               LEN_WEI_CNS, ADR_WEI_CNS  )
                IF ( IER .NE. 0 ) THEN
                     CALL IINCH   ( MEM_LEN, STR )
                     CALL ERR_LOG ( 8362, IUER, 'PSTLP', 'Failure to '// &
     &                   'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                   'memory for weight matrix of constraints' )
                     RETURN
                END IF
                FL_FULL_WEI = .TRUE.
              ELSE 
                FL_FULL_WEI = .FALSE.
           END IF 
!
! -------- Applying constraints. Constraint changes some elements of the normal
! -------- matrix
!
!%   write ( 6, * ) 'pstlp-154 m_gpa= ',m_gpa, ' ja= ', ja  ; call flush ( 6 ) ! %%%%%%%%%%
           CALL ERR_PASS    ( IUER, IER )
           CALL APPLY_CNSTR ( FAST_MODE, FAST_DBG, CNSTROBJ, NPARAM, &
     &                        ARR(JA), ARR(JB), B3DOBJ, B1B3DOBJ, FL_FULL_WEI, &
     &                        %VAL(ADR_WEI_CNS), IER )
           IF ( FL_FULL_WEI ) THEN
                CALL FREE_MEM ( MEM_ADR ) ! Free dynamic memory
           END IF
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8363, IUER, 'PSTLP', 'Error in '// &
     &              'attempt to apply constraints' )
                RETURN
           END IF
!%   write ( 6, * ) 'pstlp-167 ' ; call flush ( 6 ) ! %%%%%%%%%%
!
! -------- Writing them on disk
!
           CALL ERR_PASS    ( IUER, IER )
           CALL WRITE_CNSTR ( CNSTROBJ, CNI__LOC, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8364, IUER, 'PSTLP', 'Error in '// &
     &              'attempt to write down information about constaint '// &
     &              'equation in temporary scratch file' )
                RETURN
           END IF
           IF ( FAST_DBG .EQ. F__TIM ) THEN
!
! ------------- Timing printout
!
                CALL TIM_GET ( 'PROC-04' )
                CALL TIM_INIT()
           END IF
      END IF
!
      CALL CLRCH ( FINAM_NRM )
      FINAM_NRM = PRE_SCR_DIR(1:PRE_SD_LEN)//'NRMF'//PRE_LETRS
!
! --- Write the normal equations onto disk
!
!%   write ( 6, * ) 'pstlp-193 ' ; call flush ( 6 ) ! %%%%%%%%%%
      IF ( TRAIN ) THEN
         IF ( FAST_MODE .EQ. F__NONE  .OR.  FAST_MODE .EQ. F__PRD  .OR. &
     &        FAST_MODE .EQ. F__B1D      )  THEN
!
! ----------- FULL case
!
              CALL USE_NRMFIL ( ARR, NPARAM, 'OWC' )
!
              IF ( FAST_DBG .EQ. F__PRI ) THEN
!
! ---------------- DEBUG-mode: writing down normal matrices and normal vectors
!
                   call matview_w ( '/tmp/nor_full.mat', 3, nparam, &
     &                  nparam, arr(ja), 'Full normal matrix', '()',1, 1, &
     &                  ier )
                    call matview_w ( '/tmp/nor_full.vec', 1, nparam, 1, &
     &                   arr(jb),'Full normal vector', '()', 1, 1, ier )
              END IF
            ELSE IF ( FAST_MODE .EQ. F__B3D ) THEN
!
! ------------ B3D case
!
               CALL ERR_PASS  ( IUER, IER )
               CALL WRNOR_B3D ( FINAM_NRM, B3DOBJ, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 8365, IUER, 'PSTLP', 'Writing '// &
     &                  'normal equations failed to the file '// &
     &                   FINAM_NRM(1:I_LEN(FINAM_NRM))//' failed'  )
                    RETURN
               END IF
               IF ( FAST_DBG .EQ. F__PRI ) THEN
!
! ----------------- Copying set of normal submatrices in tmp-disk for debugging
!
                    CALL SYSTEM ( 'cp '//FINAM_NRM(1:I_LEN(FINAM_NRM))// &
     &                                ' /tmp/nor_b3d.bin'//CHAR(0) )
               END IF
            ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
!
! ------------ B1B3D case
!
               CALL ERR_PASS    ( IUER, IER )
               CALL WRNOR_B1B3D ( FINAM_NRM, B3DOBJ, B1B3DOBJ, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 8366, IUER, 'PSTLP', 'Writing '// &
     &                  'normal equations failed to the file '// &
     &                   FINAM_NRM(1:I_LEN(FINAM_NRM))//' failed'  )
                    RETURN
               END IF
               IF ( FAST_DBG .EQ. F__PRI ) THEN
!
! ----------------- Copying set of normal submatrices in tmp-disk for debugging
!
                    CALL SYSTEM ( 'cp '//FINAM_NRM(1:I_LEN(FINAM_NRM))// &
     &                                ' /tmp/nor_b1b3d.bin'//CHAR(0) )
               END IF
         END IF
      END IF
!%   write ( 6, * ) 'pstlp-zz ' ; call flush ( 6 ) ! %%%%%%%%%%
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  PSTLP  #!#
