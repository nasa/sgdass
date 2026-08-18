      PROGRAM    PROC_LAUNCH
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      CHARACTER    STR*32
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = PSOLVE__STACK_SIZE_IN_GIGABYTES * GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
      CALL ERR_MODE ( 'NO_PROBE' ) 
      CALL PROC_HEAD()
      END  PROGRAM  PROC_LAUNCH
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PROC_HEAD()
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  PROC_HEAD PROGRAM SPECIFICATION
!
! 1.1
!     Calls a subroutine PROC_DO which makes normal equations of
!     conditions.
!
!
! 1.2 REFERENCES:
!
! 2.  INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: none
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'glbcm.i'
      INCLUDE 'socom.i'
      INCLUDE 'precm.i'
      INCLUDE 'oborg.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'fast.i'
      INCLUDE 'heo.i'
      INCLUDE 'cnstr.i'
      INCLUDE 'buff2.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: None
!       CALLED SUBROUTINES: prelp,loop,pstlp
!
! 3.  LOCAL VARIABLES
      LOGICAL*2 KBIT
      INTEGER*4  SNGCHK_CMP
      TYPE ( B3D__STRU   ) ::  B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
!
      CHARACTER    STR*128, STR1*32, STR2*32
      INTEGER*8    MATSIZE, STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = PSOLVE__STACK_SIZE_IN_GIGABYTES * GB )
      ADDRESS__TYPE ::  ADR_ARR
      INTEGER*4   IUER
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: MAT_E, I_LEN, ILEN
      ADDRESS__TYPE, EXTERNAL :: MAT_E4
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!C
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   mwh  940201  Implement dynamic memory allocation for normal matrix
!   pet  970205  Added B3D stuff and "coombed" comments
!   pet  970226  Added B1B3D stuff
!   pet  980120  Made grabbing dynamic memory for allocation full normal
!                matrix mandadory only in FULL mode.
!   pet  980514  Impotant bug fixed: procedure FAST_BYPASS was moved from loop
!                to proc. The true is that FAST_BYPASS changes fast_mode if
!                session is not eligible for fast algorithm. But dynamic memory
!                is allocated in according with USED value of fast_mode. For
!                this reason FAST_BYPASS should be located before grabbing
!                dynamic memory
!   pet  980707  Added writing status of completion of singularity check to
!                the pipe. It is done to provide possibility to skip the
!                database which fails the check
!   pet  981230  Increased amount of allocated memory for ARR
!   pet  990103  Rewrote and changed name
!   pet  1999.05.03   Moved zeroing B3DOBJ from loop to proc_head
!   pet  1999.05.31   Made CNSTROBJ a formal parameter
!   pet  2004.11.09   Added reading HEO file if needed.
!
! 5.  PROC PROGRAM STRUCTURE
!
!C
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
!
      CALL PRE_PROG()
      INCLUDE 'proc_version.i' ! Set revision date of the current version
!
! --- Screen initialiszation in the interactive mode
!
      IF ( ISCREEN == ISCREEN_NO ) KSCREEN = .FALSE. 
      IF ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6)) ) CALL START_MN()
      CALL SET_SIGNAL_CTRLC ( 3 )
!
! --- Read GLBFIL
!
      CALL USE_GLBFIL   ( 'OR' )
      CALL USE_GLBFIL_2 ( 'R'  )
      CALL USE_GLBFIL_3 ( 'R'  )
      CALL USE_GLBFIL_4 ( 'RC' )
      VTD_ADR = 0
      CALL USE_GLBFIL_4 ( 'OWC' )
!
      CALL USE_COMMON ( 'ORC' )
      CALL SOCOM_EXT()
      CALL TIM_INIT()
!
! --- Attempt to override FAST_MODE from environment variable and make test
! --- of eligibility FAST_MODE for this session
!
      IUER = -1
      CALL FAST_BYPASS ( IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 8301, -1, 'PROC_HEAD', 'Error in '// &
     &         'trying to bypass fast_mode' )
           STOP 'PROC(proc_head) Abnormal termination'
      END IF
      IF ( FAST_MODE .EQ. F__NONE  .OR.  FAST_MODE .EQ. F__UND  .OR. &
     &     FAST_MODE .EQ. F__PRD                                     ) THEN
!
! -------- Calculation number of real*8 elements of dynamic memory needed
! -------- for non-fast mode. We increased amount of alotted mamory for
! -------- the case if after reparameterization the size of array will increase
! -------- by MAX_ARC_STA bytes
!
!%           MATSIZE = 256*INT8( (MAT_E( M_GPA, INT2(NPARAM+MAX_ARC_STA) )+ &
!%     &                      255)/256 ) *8
            MATSIZE = 8*MAT_E4 ( M_GPA, NPARAM + MAX_ARC_STA )
!
! -------- Grabbing memory for full normal matrix in non-fast modes
!
           CALL GET_MEM ( MATSIZE, ADR_ARR )
           IF ( ADR_ARR .EQ. 0 ) THEN
                CALL CLRCH   ( STR1 )
                CALL IINCH8  ( MATSIZE, STR1 )
                CALL CLRCH   ( STR2 )
                CALL INCH    ( NPARAM, STR2 )
                CALL ERR_LOG ( 8302, -1, 'PROC_HEAD', 'Error in '// &
     &              'attempt to grab '//STR1(1:I_LEN(STR1))//' bytes memory '// &
     &              ' for the normal matrix sized for '//STR2(1:I_LEN(STR2))// &
     &              ' parameters ' )
                STOP 'PROC(proc_head) Abnormal termination'
           END IF
         ELSE
           ADR_ARR = 0
      END IF
!
! --- Initialization of B3DOBJ,  B1B3DOBJ
!
      CALL F__CLR_IND ( 4, FAST_MODE, %VAL(0), B3DOBJ, B1B3DOBJ  )
      IF ( ILEN(FINAM_HEO) .GT. 0 ) THEN
           IUER = -1
           CALL GHEO ( FINAM_HEO, NAME_HEO, L_HEO, STAT_HEO, &
     &                 ADR_HEO, HEO_EPOCH_SEC, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 8303, -1, 'PROC_HEAD', 'Error '// &
     &              'during attempt to read the file with harmonic Earth '// &
     &              'orientation parameters '//FINAM_HEO )
                STOP 'PROC(proc_head) Abnormal termination'
           END IF
           STAT_HEO  = HEO__READ
           CALL USE_GLBFIL_4 ( 'OWC' )
      END IF
!
      IUER = -1
      CALL PROC_DO ( %VAL(ADR_ARR), B3DOBJ, B1B3DOBJ, CNSTROBJ, SNGCHK_CMP, &
     &               IUER )
!
      IF ( IUER .NE. 0 ) THEN
           STOP 'PROC(proc_head) Abnornal termination'
      END IF
!
! --- Free dynamic memory which was previous grabbed
!
      IF ( FAST_MODE .EQ. F__NONE  .OR.  FAST_MODE .EQ. F__UND  .OR. &
     &     FAST_MODE .EQ. F__PRD                                     ) THEN
           CALL FREE_MEM ( ADR_ARR )
        ELSE IF ( FAST_MODE .EQ. F__B3D ) THEN
!
           IUER = -1
           CALL B3D_FREEMEM ( B3DOBJ, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 8304, -1, 'PROC_HEAD', 'Error '// &
     &              'during freeing dynamic memory in B3D mode' )
                STOP 'PROC(proc_head) Abnormal termination'
           END IF
        ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
!
! -------- Freeing dynamic memory
!
           IUER = -1
           CALL B1B3D_FREEMEM ( B3DOBJ, B1B3DOBJ, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 8305, -1, 'PROC_HEAD', 'Error '// &
     &              'during freeing dynamic memory in  B1B3D mode' )
                STOP 'PROC(proc_head) Abnormal termination'
           END IF
      END IF
!
! --- Set status
!
      CALL STATUS_SET ( 'PROC', STA__END )
!
      CALL END_PROG()
      END  SUBROUTINE  PROC_HEAD  !#!#
