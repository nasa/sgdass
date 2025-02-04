      SUBROUTINE GLO_FORW ( IARC, DBNAME_MES, GLBMEM, B3DOBJ, B1B3DOBJ, &
     &                      CNSTROBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GLO_FORW  processes  the session in forward mode of       *
! *   global parameters estimation. It creates normal equations, makes   *
! *   arithmetic operation for parameter elimination and update combine  *
! *   global matrix (CGM) and combined global vector. If the arc is the  *
! *   last arc of the run then  GLO_FORW  rearranges CGM and writes it   *
! *   down on disk.                                                      *
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
! *  2007.05.15 pet  1.7  Increased the the reparameterization margin    *
! *                       from MAX_ARC_STA to MAX_ARC_BSL.               *
! *  2016.09.08 pet  1.9  Decreased the the reparameterization margin    *
! *                       to MAX_ARC_BSL to MAX_ARC_STA.                 *
! *                                                                      *
! *  ###  04-JAN-99    GLO_FORW    v1.9  (c)  L. Petrov 08-SEP-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INCLUDE    'glbcm.i'
      INCLUDE    'glbc4.i'
      INCLUDE    'glbp.i'
      INCLUDE    'plist.i'
      INCLUDE    'fast.i'
      INCLUDE    'socom.i'
      INCLUDE    'cnstr.i'
!
      INTEGER*4  IARC, IUER
      CHARACTER  DBNAME_MES*(*)
!
      TYPE ( B3D__STRU ) ::  B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      TYPE ( GLB_MEM__STRU ) ::  GLBMEM  ! defined in glbp.i
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
!
      INTEGER*4  IER, SNGCHK_CMP
      INTEGER*8  MATSIZE
      CHARACTER  STR2*32, DEBUG_ARCF*8
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: MAT_E, I_LEN, ILEN
      ADDRESS__TYPE :: MAT_E4
!
! --- Check: whether we have to allocate dynamic memory
!
#ifdef DEBUG
   write ( 6, * ) 'glo_forw-83  GLBMEM%L_GPA= ', GLBMEM%L_GPA ! %%%%
#endif
      IF ( FAST_DBG .EQ. F__APP ) THEN
           WRITE ( 6, * ) ' GLO_FORW started for '//DBNAME_MES
      END IF
!
      IF ( GLBMEM%LEN_GLO .EQ. 0 ) THEN
           CALL ERR_LOG ( 4111, IUER, 'GLO_FORW', 'Error of internal '// &
     &         'control: data structure GLBMEM has not been initialized '// &
     &         'and dynamic memory has not been allocated' )
           RETURN
      END IF
!
! --- Read socom block (may be once more) and extend it
!
      CALL USE_COMMON ( 'ORC' )
      CALL SOCOM_EXT()
!
! --- Attempt to override FAST_MODE from environment variable and make test
! --- of eligibility FAST_MODE for this session
!
      CALL ERR_PASS    ( IUER, IER )
      CALL FAST_BYPASS ( IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4112, IUER, 'GLO_FORW', 'Error in trying '// &
     &                   'to bypass fast_mode' )
           RETURN
      END IF
!
! --- Initialization of B3DOBJ,  B1B3DOBJ
!
      CALL F__CLR_IND ( 4, FAST_MODE, %VAL(0), B3DOBJ, B1B3DOBJ )
!
! --- Grab dynamic memory for normal matrix in non fast modes
!
      IF ( FAST_MODE .EQ. F__NONE ) THEN
!
! -------- Calculation number of real*8 elements of dynamic memory needed
! -------- for non-fast mode. We increased amount of alotted memory for
! -------- the case if after reparameterization the size of array will increase
! -------- by MAX_ARC_BSL parameters
!
!!           MATSIZE = 256*( (MAT_E( MAX_PAR, INT2(NPARAM +  MAX_ARC_BSL))+ &
!!     &                     255)/256 ) * 8 +256
!!           MATSIZE = 256*( (MAT_E( MAX_PAR, INT2(NPARAM + MAX_ARC_STA))+ &
!!     &                     255)/256 ) * 8 +256
             MATSIZE = 8*MAT_E4 ( M_GPA, NPARAM + MAX_ARC_STA )
!
! -------- Grabbing memory for full normal matrix in non-fast modes
!
           CALL ERR_PASS ( IUER, IER )
           CALL GRAB_MEM ( IER, GLBMEM%LEN_LOC, GLBMEM%ADR_LOC,      1, &
     &                          2*MATSIZE,      GLBMEM%ADR_NORMARR2     )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH   ( STR2 )
                CALL INCH    ( NPARAM, STR2 )
                CALL ERR_LOG ( 4113, IUER, 'GLO_FORW', 'Error in '// &
     &              'attempt to grab dynamic memory for two normal matrices '// &
     &              'sized for '//STR2(1:I_LEN(STR2))//' parameters ' )
                RETURN
           END IF
           GLBMEM%ADR_NORMARR = GLBMEM%ADR_NORMARR2 + MATSIZE
!
! -------- Zeroing grabbed dynamic memory
!
           CALL NOUT8 ( GLBMEM%LEN_LOC, %VAL(GLBMEM%ADR_LOC) )
      END IF
!
! --- Computation of the normal equations
!
      CALL ERR_PASS ( IUER, IER )
      CALL PROC_DO  ( %VAL(GLBMEM%ADR_NORMARR), B3DOBJ, B1B3DOBJ, CNSTROBJ, &
     &                SNGCHK_CMP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4114, IUER, 'GLO_FORW', 'Error during computation '// &
     &         'of normal equations while the database '//DBNAME_MES// &
     &         ' was processed' )
           RETURN
      END IF
      IF ( GIM_EST .OR. GIM_RGR ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN   
      END IF
!
      CALL TIM_INIT() ! Set timer
      IF ( SNGCHK_CMP .NE. SNGCHK_CMP__SKIP ) THEN
!
! -------- Update of CGM for the observations of this session
!
           CALL ERR_PASS ( IUER, IER )
           CALL GLO_ELIM ( 1, DBNAME_MES, GLBMEM, B3DOBJ, B1B3DOBJ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4115, IUER, 'GLO_FORW', 'Error during '// &
     &              'parameter elimination procedure while the database '// &
     &               DBNAME_MES//' was processed' )
                RETURN
           END IF
!
           TGLBLS = GLBMEM%L_GPA
           CALL USE_GLBFIL ( 'OWC' )
      END IF
!
! --- Free dynamic memory which was previous grabbed for anlysis data this
! --- session
!
      IF ( FAST_MODE .EQ. F__NONE ) THEN
           CALL FREE_MEM ( GLBMEM%ADR_LOC )
           GLBMEM%LEN_LOC = -1
        ELSE IF ( FAST_MODE .EQ. F__B3D ) THEN
!
           CALL ERR_PASS    ( IUER, IER )
           CALL B3D_FREEMEM ( B3DOBJ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4116, IUER, 'GLO_FORW', 'Error '// &
     &              'during freeing dynamic memory in B3D mode' )
                RETURN
           END IF
        ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
!
! -------- Freeing dynamic memory
!
           CALL ERR_PASS ( IUER, IER )
           CALL B1B3D_FREEMEM ( B3DOBJ, B1B3DOBJ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4117, IUER, 'GLO_FORW', 'Error '// &
     &              'during freeing dynamic memory in  B1B3D mode' )
                RETURN
           END IF
      END IF
!
      IF ( IOCGM .EQ. 2 ) THEN
           CALL TIM_INIT() ! start timer
!
! -------- This session is the last session of the run. Reordering CGM to the
! -------- order of parameters to which routine partl generates them and
! -------- therfore SOLVE expects to have them: first stations coordinates,
! -------- then station velocities, then source positions then other global
! -------- parameters.
!
#ifdef DEBUG
   write ( 6, * ) 'glo_forw-217 GLBMEM%L_GPA= ', GLBMEM%L_GPA ! %%%%
#endif
           CALL ERR_PASS ( IUER, IER )
           CALL GLO_REORDER ( GLBMEM, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4118, IUER, 'GLO_FORW', 'Error during '// &
     &              'attempt to reorder parameters in CGM before writing '// &
     &              'it on disk while the last session was processed' )
                RETURN
           END IF
#ifdef DEBUG
   write ( 6, * ) 'glo_forw-226 GLBMEM%L_GPA= ', GLBMEM%L_GPA ! %%%%
#endif
!
! -------- Saving the final CGM on disk
!
           CALL ERR_PASS ( IUER, IER )
           CALL GLO_SAVE ( 3, GLBMEM, DBNAME_MES, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4119, IUER, 'GLO_FORW', 'Error during '// &
     &              'attempt to save permanent CGM on disk while the '// &
     &              'database '//DBNAME_MES//' was processed' )
                RETURN
           END IF
!
           IF ( FAST_DBG .EQ. F__TIM ) THEN
                CALL TIM_GET ( 'FORW-05' )
           END IF
      END IF
!
      IF ( FAST_DBG .EQ. F__APP ) THEN
           WRITE ( 6, * ) ' GLO_FORW ended for '//DBNAME_MES
      END IF
#ifdef DEBUG
   write ( 6, * ) 'glo_forw-245 GLBMEM%L_GPA= ', GLBMEM%L_GPA ! %%%%
#endif
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GLO_FORW  !#!#
