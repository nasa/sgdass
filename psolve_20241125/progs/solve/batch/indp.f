       SUBROUTINE INDP ( IARC, DBNAME_MES, GLBMEM, B3DOBJ, B1B3DOBJ, CNSTROBJ, &
     &                  IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  INDP  processes the session in independent mode.          *
! *   It computes adjustments of parameters, estimates of their          *
! *   covarinace matrix and dispersion of the adjustments.               *
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
! *     GLBMEM ( RECORD    ) -- Data structure which keeps addresses.    *
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
! *  2007.05.15 pet  1.5  Increased the the reparameterization margin    *
! *                       from MAX_ARC_STA to MAX_ARC_BSL.               *
! *                                                                      *
! *  ###  03-APR-1999     INDP    v1.5  (c)  L. Petrov  15-MAY-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INCLUDE    'glbcm.i'
      INCLUDE    'glbc4.i'
      INCLUDE    'glbp.i'
      INCLUDE    'socom.i'
      INCLUDE    'precm.i'
      INCLUDE    'prfil.i'
      INCLUDE    'fast.i'
      INCLUDE    'cnstr.i'
!
      INTEGER*4  IARC, IUER
      CHARACTER  DBNAME_MES*(*)
      TYPE ( B3D__STRU     ) ::  B3DOBJ
      TYPE ( B1B3D__STRU   ) ::  B1B3DOBJ
      TYPE ( GLB_MEM__STRU ) ::  GLBMEM  ! defined in glbp.i
      TYPE ( CNSTR__STRU   ) ::  CNSTROBJ
!
      INTEGER*4  IER, SNGCHK_CMP
      INTEGER*8  MATSIZE
      CHARACTER  STR2*32
      LOGICAL*2  KBIT, F_IO_NRM
      INTEGER*4  IPTR, PAGEWID
      CHARACTER  LBUF(CRES_BUF_LEN2)*120
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: MAT_E, I_LEN
      ADDRESS__TYPE, EXTERNAL :: MAT_E4
!
! --- Check: whether we have to allocate dynamic memory
!
      IF ( FAST_DBG .EQ. F__APP ) THEN
           WRITE ( 6, * ) ' INDP started for '//DBNAME_MES
      END IF
!
      CALL USE_GLBFIL   ( 'OR' )
      CALL USE_GLBFIL_2 ( 'R'  )
      CALL USE_GLBFIL_4 ( 'RC' )
!
! --- Unpacking bits
!
      KBATCH   =       KBIT ( PRE_IP(3), INT2( 1) )
      KMINOUT  =       KBIT ( PRE_IP(3), INT2( 2) )
      KLCLBSL  =       KBIT ( PRE_IP(3), INT2( 3) )
      KSCREEN  = .NOT. KBIT ( PRE_IP(3), INT2( 4) )
      KBACKSL  =       KBIT ( PRE_IP(3), INT2( 6) )
      KGLBBSL  =       KBIT ( PRE_IP(3), INT2( 7) )
      KGLOBALS =       KBIT ( PRE_IP(3), INT2(10) )
      KPOSELL  =       KBIT ( PRE_IP(3), INT2(13) )
      KFULLOUT = .NOT. KMINOUT
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
           CALL ERR_LOG ( 4211, IUER, 'INDP', 'Error in trying '// &
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
! --- Calculation number of real*8 elements of dynamic memory needed
! --- for non-fast mode. We increased amount of alotted memory for
! --- the case if after reparameterization the size of array will increase
! --- by MAX_ARC_BSL parameters
!
!@      MATSIZE = 256*( (MAT_E( MAX_PAR, INT2(NPARAM+MAX_ARC_BSL))+255)/256)*8
      MATSIZE = 8*MAT_E4 ( M_GPA, NPARAM+MAX_ARC_BSL )
!
! --- Grabbing memory for full normal matrix. IT will be used in in non-fast
! --- modes for keeping normal equations, but will be sued in B3D mode also for
! --- putting there expanded covariance matrix
!
      CALL ERR_PASS ( IUER, IER )
      CALL GRAB_MEM ( IER, GLBMEM%LEN_LOC, GLBMEM%ADR_LOC,    1, &
     &                     MATSIZE,        GLBMEM%ADR_NORMARR    )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH   ( STR2 )
           CALL INCH    ( NPARAM, STR2 )
           CALL ERR_LOG ( 4212, IUER, 'INDP', 'Error in '// &
     &         'attempt to grab dynamic memory for a normal matrix '// &
     &         'sized for '//STR2(1:I_LEN(STR2))//' parameters ' )
           RETURN
      END IF
!
! --- Zeroing grabbed dynamic memory
!
      CALL NOUT8( GLBMEM%LEN_LOC, %VAL(GLBMEM%ADR_LOC) )
!
! --- Computation of the normal equations
!
      CALL ERR_PASS ( IUER, IER )
      CALL PROC_DO  ( %VAL(GLBMEM%ADR_NORMARR), B3DOBJ, B1B3DOBJ, CNSTROBJ, &
     &                SNGCHK_CMP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4213, IUER, 'INDP', 'Error during computation '// &
     &         'of normal equations while the database '//DBNAME_MES// &
     &         ' was processed' )
           RETURN
      END IF
!
      IF ( SNGCHK_CMP .NE. SNGCHK_CMP__SKIP ) THEN
           F_IO_NRM     = .FALSE.
           GLBMEM%L_ARC = NPARAM
!
! -------- Solving normal equations
!
           CALL ERR_PASS   ( IUER, IER )
           CALL NORML_MAIN ( NPARAM, %VAL(GLBMEM%ADR_NORMARR), B3DOBJ, &
     &          CNSTROBJ, ' ', F_IO_NRM, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4214, IUER, 'INDP', 'Error in solving normal '// &
     &              'equations while the database '//DBNAME_MES// &
     &              ' was processed' )
                RETURN
           END IF
!
           IF ( .NOT. KMINOUT  .OR.  KSCREEN ) THEN
!
! ------------- Printing covariance matrix if needed
!
                CALL COVP_MAIN ( %VAL(GLBMEM%ADR_NORMARR), F_IO_NRM )
           ENDIF
!
! -------- Computation of residuals
!
           IPTR = 0
           CALL CRES_DO  ( INT2(0), NPARAM, %VAL(GLBMEM%ADR_NORMARR), B3DOBJ, &
     &          CNSTROBJ, CRES_BUF_LEN2, LBUF, IPTR, PAGEWID )
!
! -------- Computation and printing adjustments
!
           IPTR = 1
           CALL ERR_PASS ( IUER, IER )
           CALL ADJST_DO ( NPARAM, %VAL(GLBMEM%ADR_NORMARR), CRES_BUF_LEN2, &
     &                     LBUF, IPTR, PAGEWID, CNSTROBJ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4215, IUER, 'INDP', 'Errors during computing '// &
     &              'and printing adjustments' )
                RETURN
           END IF
      END IF
!
      IF ( FAST_MODE .EQ. F__B3D ) THEN
!
! -------- Free memory grabed in B3D data structures in B3D mode
!
           CALL ERR_PASS    ( IUER, IER )
           CALL B3D_FREEMEM ( B3DOBJ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4216, IUER, 'INDP', 'Error '// &
     &              'during freeing dynamic memory in B3D mode' )
                RETURN
           END IF
      END IF
!
! --- Free dynamic memory which was previous grabbed for data anlysis this
! --- session
!
      CALL FREE_MEM ( GLBMEM%ADR_LOC )
      GLBMEM%ADR_LOC =  0
      GLBMEM%LEN_LOC = -1
!
      IF ( FAST_DBG .EQ. F__APP ) THEN
           WRITE ( 6, * ) ' INDP ended for '//DBNAME_MES
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  INDP  #!#
