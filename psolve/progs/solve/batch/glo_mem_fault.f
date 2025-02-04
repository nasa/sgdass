      SUBROUTINE GLO_MEM_FAULT ( B3DOBJ, B1B3DOBJ, NPARM_CGM, NPARM_ARCPE, &
     &                           DBNAME_MES, FINAM_NRM, ADR_AGG, ADR_BG, &
     &                           GLBMEM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GLO_MEM_FAULT makes corrective actions when memory fault   *
! *   occurred: amount of dynamic memory grabbed for keeping CGM         *
! *   in operative memory appeared to be unsufficient                    *
! *                                                                      *
! *   We do:                                                             *
! *   1)* write down vectors and matrices related to this session        *
! *       in file on disk;                                               *
! *   2)  free dynamic memory kept by local data strucutres              *
! *   3)  free dynamic memory kept by CGM                                *
! *   4)  grab dynamic memory for CGM                                    *
! *   5)  read CGM                                                       *
! *   6)* read matrices related to this session from file on disk;       *
! *   7)  Update CGM                                                     *
! *   (points with asterisk are executed in MEMFAULT_REP mode )          *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! * DBNAME_MES ( CHARACTER ) -- Line with the database name and its      *
! *     B3DOBJ ( RECORD    ) -- Object with data structure for B3D       *
! *                             extension of SOLVE.                      *
! *   B1B3DOBJ ( RECORD    ) -- Object with data structure for B1B3D     *
! *                             extension of SOLVE.                      *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! * ADR_AGG ( INTEGER*8      ) -- Address of global-glboal block of      *
! *                               normal matrix for this session.        *
! *  ADR_BG ( INTEGER*8      ) -- Address of global vector of right-hand *
! *                               side of normal equations.              *
! *  GLBMEM ( RECORD         ) -- Data structure which keeps addresses   *
! *                               of CGM, list of global parameters,     *
! *                               global socom, prfil and temporary      *
! *                               normal matrices. Defined in            *
! *                               $PSOLVE_ROOT/include/glbp.i            *
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
! *  ### 05-JAN-1999  GLO_MEM_FAULT  v1.1 (c) L. Petrov 16-MAY-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'erm.i'
      INCLUDE   'socom.i'
      INCLUDE   'socom_plus.i'
      INCLUDE   'precm.i'
      INCLUDE   'prfil.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'glbp.i'
      INCLUDE   'fast.i'
      TYPE ( B3D__STRU     ) :: B3DOBJ
      TYPE ( B1B3D__STRU   ) :: B1B3DOBJ
      TYPE ( GLB_MEM__STRU ) :: GLBMEM  ! defined in glbp.i
      CHARACTER  DBNAME_MES*(*), FINAM_NRM*(*)
      INTEGER*4  NPARM_CGM, NPARM_ARCPE
      INTEGER*8  MATSIZE
      ADDRESS__TYPE :: ADR_AGG, ADR_BG
      INTEGER*4  IUER
!
      ADDRESS__TYPE :: SHF_AGG, SHF_BG
      CHARACTER  STR*16, STR2*16
      INTEGER*4  TGLBLS_TEMP
      INTEGER*4  IER
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*2, EXTERNAL ::  KBIT
      INTEGER*4, EXTERNAL ::  MAT_E, I_LEN
      ADDRESS__TYPE, EXTERNAL ::  MAT_E4
!
#ifdef DEBUG
   write ( 6, * ) 'BATCH glo_mem_fault(85) ' ; call flush ( 6 ) ! %%%%%%%%%%%%
#endif
      IF ( FAST_DBG .EQ. F__APP ) THEN
           WRITE ( 6, * ) ' GLO_ELIM started memory fault fixing '
      END IF
!  
! --- Saving current SOCOM and PRFIL
!  
      CALL USE_COMMON ( 'OWC' )
      CALL USE_PARFIL ( 'OWC' )
! 
! --- Saving CGM in temporary file CGMFxx
!  
      CALL ERR_PASS ( IUER, IER )
      CALL GLO_SAVE ( 2, GLBMEM, DBNAME_MES, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4211, IUER, 'GLO_MEM_FAULT', 'Errors during '// &
    &          'attempt to save CGM for memory fault fixing' )
           RETURN
      END IF
!  
! ----Remember address shift relative beginning
!     
      SHF_AGG = ADR_AGG - GLBMEM%ADR_NORMARR2
      SHF_BG  = ADR_BG  - GLBMEM%ADR_NORMARR2
!           
      IF ( FAST_MODE .EQ. F__NONE ) THEN
!           
! -------- Freeing dynamic memory kept for session related data
!           
           CALL FREE_MEM ( GLBMEM%ADR_LOC )
         ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
!           
! --------- Freeing dynamic memory kept by data structure for this
! --------- session
!           
            CALL ERR_PASS ( IUER, IER )
            CALL B1B3D_FREEMEM ( B3DOBJ, B1B3DOBJ, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 4212, IUER, 'GLO_MEM_FAULT', 'Error '// &
    &                'during freeing dynamic memory in  B1B3D mode' )
                 RETURN
            END IF
      END IF
!           
! ----Set a a flag forcing GLO_INIT to make hole in address space
!           
      GLBMEM%L_GPA = 0
!  
      IF ( GLBMEM%LEN_GLO .GT. 0 ) THEN
!  
! -------- Freeing dynamic memory where CGM was kept previously
!  
           CALL FREE_MEM ( GLBMEM%ADR_GLO )
           GLBMEM%LEN_GLO = -1
      END IF
!  
! --- Setting (for a short time) the new number of global parameters --
! --- it is used by GLO_INIT for calculation amount of dynamic memory
! --- needed
!  
      TGLBLS_TEMP = TGLBLS
      TGLBLS = NPARM_CGM
!  
! --- Grabbing dynamic memory for CGM
!  
      CALL ERR_PASS ( IUER, IER )
      CALL GLO_INIT ( TRUE__L2, ' ', GLBMEM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4213, IUER, 'GLO_MEM_FAULT', 'Errors during '// &
    &          'attempt to initialize internal data structures and '// &
    &          'grab appropriate amount of dynamic memory' )
           RETURN
      END IF
!  
! --- Reading CGM, list of global parameters, global SOCOM and PARFIL
! --- from  CGMFxx file
!  
      CALL ERR_PASS ( IUER, IER )
      CALL GLO_RESTORE ( 2, GLBMEM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4214, IUER, 'GLO_MEM_FAULT', 'Errors during '// &
    &          'attempt to read CGM from temporary file after fixing '// &
    &          'memory fault' )
           RETURN
      END IF
      TGLBLS = TGLBLS_TEMP ! restore previous value of TGLBLS
!  
! --- Restoring SOCOM and PRFIL
!  
      CALL USE_COMMON ( 'ORC' )
      CALL USE_PARFIL ( 'ORC' )
!  
! --- Grab dynamic memory for normal matrix in non fast modes
!  
      IF ( FAST_MODE .EQ. F__NONE ) THEN
!  
! -------- Calculation number of real*8 elements of dynamic memory
! -------- needed for non-fast mode.
!  
!@           MATSIZE = 256*( (MAT_E(MAX_PAR,NPARM_ARCPE)+255)/256 ) * 8 + 256
           MATSIZE = 8*MAT_E4 ( M_GPA, NPARM_ARCPE )
!  
! -------- Grabbing memory for full normal matrix in non-fast modes
!  
           CALL ERR_PASS ( IUER, IER )
           CALL GRAB_MEM ( IER, GLBMEM%LEN_LOC, GLBMEM%ADR_LOC,     1, &
    &                           MATSIZE,        GLBMEM%ADR_NORMARR2   )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH   ( STR2 )
                CALL INCH    ( NPARAM, STR2 )
                CALL ERR_LOG ( 4215, IUER, 'GLO_MEM_FAULT', 'Error in '// &
    &               'attempt to grab dynamic memory for two normal '// &
    &               'matrices sized for '//STR2(1:I_LEN(STR2))// &
    &               ' parameters after memory fault' )
                RETURN
           END IF
!  
! -------- Zeroing grabbed dynamic memory
!  
           CALL NOUT8 ( GLBMEM%LEN_LOC, %VAL(GLBMEM%ADR_LOC) )
!  
! -------- Reading intermediary matrices and vectors for this arc
! -------- back
!  
           CALL ACS_ARCFIL   ( SAVAF, KBIT( PRE_IBATCH, INT2(8)), 'O' )
           CALL USE_ARCF_MAT ( %VAL(GLBMEM%ADR_NORMARR2), NPARM_ARCPE, 'R' )
           CALL ACS_ARCFIL   ( SAVAF, KBIT( PRE_IBATCH, INT2(8)), 'C' )
!  
! -------- Restore some important addresses
!  
           ADR_BG  = GLBMEM%ADR_NORMARR2 + SHF_BG
           ADR_AGG = GLBMEM%ADR_NORMARR2 + SHF_AGG
        ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
!  
! -------- Reading fields of B1B3DOBJ and B3DOBJ objects
!  
           CALL ERR_PASS ( IUER, IER )
           CALL RDNOR_B1B3D ( FINAM_NRM, B3DOBJ, B1B3DOBJ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4216, IUER, 'GLO_MEM_FAULT', 'Error '// &
    &               'during reading file '// &
    &                FINAM_NRM(1:I_LEN(FINAM_NRM))//' with '// &
    &               'temporary data structure for B1B3D algorithm' )
                RETURN
           ENDIF
!  
! -------- Copying addresses of matrices and vectors cotributions to
! -------- CGM
!  
           ADR_AGG = B1B3DOBJ%AD_W00
           ADR_BG  = B1B3DOBJ%AD_Z00
      END IF  ! fast_mode
      IF ( FAST_DBG .EQ. F__APP ) THEN
           WRITE ( 6, * ) ' GLO_ELIM ended memory fault fixing '
      END IF
#ifdef DEBUG
   write ( 6, * ) 'BATCH glo_mem_fault(242) ' ; call flush ( 6 ) ! %%%%%%%%%%%%
#endif
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GLO_MEM_FAULT  !#!#
