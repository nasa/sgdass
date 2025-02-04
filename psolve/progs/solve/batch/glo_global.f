      SUBROUTINE GLO_GLOBAL ( DBNAME_MES, GLBMEM, B3DOBJ, B1B3DOBJ, CNSTROBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GLO_GLOBAL  imposes constraints on global parameters,     *
! *   inverts the combined global matrix, computes adjustments to global *
! *   parameters, estimates of their covarinace matrix, dispersion of    *
! *   the adjustments and writes down results in spool file.             *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
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
! *  2001.05.10  pet  1.6  Increased amount of memory allocated for CGM  *
! *                        by 256 bytes since it is read and written by  *
! *                        256-bytes-long blocks.                        *
! *                                                                      *
! *  2007.11.14  pet  1.8  Fixed a bug: re-initialization of SOCOM_EXT   *
! *                        is needed.                                    *
! *                                                                      *
! *  ###  13-DEC-2000   GLO_GLOBAL  v1.9 (c)  L. Petrov 27-MAR-2023 ###  *
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
      INTEGER*4  IUER
      TYPE ( B3D__STRU ) ::  B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      TYPE ( GLB_MEM__STRU ) ::  GLBMEM  ! defined in glbp.i
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      CHARACTER         SAVNAM*(NAME_SIZE)
      COMMON / NAMARC / SAVNAM
!
      LOGICAL*2  KBIT
      INTEGER*2  INT2_ARG
      INTEGER*4  IPTR, PAGEWID, IER
      CHARACTER  LBUF(CRES_BUF_LEN2)*120, DBNAME_MES*(*)
      INTEGER*4, EXTERNAL :: I_LEN
!
      IF ( FAST_DBG .EQ. F__APP  .OR.  FAST_DBG .EQ. F__PRI ) THEN
           WRITE ( 6, * ) ' GLO_GLOBAL started'
      END IF
!
! --- Check: whether we have to allocate dynamic memory
!
      IF ( GLBMEM%LEN_GLO .EQ. 0 ) THEN
           CALL ERR_LOG ( 4191, IUER, 'GLO_GLOBAL', 'Trap of internal '// &
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
      KSCREEN  = .NOT. KBIT ( PRE_IP(3), INT2( 4) ) ! old
      KBACKSL  =       KBIT ( PRE_IP(3), INT2( 6) )
      KGLBBSL  =       KBIT ( PRE_IP(3), INT2( 7) )
      CORLN    =       KBIT ( PRE_IP(3), INT2( 9) )
      KGLOBALS =       KBIT ( PRE_IP(3), INT2(10) )
      KPOSELL  =       KBIT ( PRE_IP(3), INT2(13) )
      KFULLOUT = .NOT. KMINOUT
      IF ( DBNAME_MES .EQ. 'CGM' ) THEN
           KSCREEN  = .FALSE.
           KBATCH   = .TRUE.
      END IF
!
      CALL TIM_INIT() ! Set timer
!
      CALL USE_COMMON ( 'ORC' )
      SOCOM_PLUS_FIRST = SPL__UNDF
      CALL SOCOM_EXT()
!
! --- Copying global SOCOM and PARFIL blocks from internal BATCH
! --- to socom and prfil
!
      CALL LIB$MOVC3 ( JSOCOM_BYTES,  %VAL(GLBMEM%ADR_GLO_SOCOM), PI_VAR   )
      CALL LIB$MOVC3 ( JPARFIL_BYTES, %VAL(GLBMEM%ADR_GLO_PRFIL), VAXOF(1) )
      FL_SPESOL = .FALSE.
      FL_HPESOL = .FALSE.
      FL_EERM   = .FALSE.
      FL_EHEO   = .FALSE.
      SOCOM_PLUS_FIRST = SPL__UNDF
      CALL SOCOM_EXT()
!
      KGLOBALS = .TRUE.
      IPTR = 1
      CALL ERR_PASS ( IUER, IER )
      CALL ADJST_DO ( GLBMEM%L_GPA, %VAL(GLBMEM%ADR_GLO), &
     &                CRES_BUF_LEN2, LBUF, IPTR, PAGEWID, CNSTROBJ, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4192, IUER, 'GLO_GLOBAL', 'Errors during '// &
     &         'computing and printing adjustments for global parameters' )
           RETURN
      END IF
      KGLOBALS = .FALSE.
      CALL SBIT       ( PRE_IP(3), INT2(10), INT2(0) )
!
! --- Restore local prfil for this session -- it may be needed since
! --- it keeps psitd for piecewise model of station positions
!
      CALL USE_PARFIL ( 'ORC' )
!
      CALL USE_GLBFIL ( 'ORC' )  ! Restore glbcm it since, f.e.
!                                ! CRES writes there
!
! --- Free memory allocated for local normal system if it was somehow
! --- allocated earlier
!
      IF ( GLBMEM%LEN_LOC .GT. 0 ) THEN
           CALL FREE_MEM ( GLBMEM%ADR_LOC )
           GLBMEM%ADR_LOC = 0
           GLBMEM%LEN_LOC = -1
      END IF
      IF ( KSPOOL ) THEN
           WRITE ( 23, "( 1H1, 'Run ', I5, '-', I4 )" ) IRNCD
      ENDIF
!
      IF ( FAST_DBG .EQ. F__APP  .OR.  FAST_DBG .EQ. F__PRI ) THEN
           WRITE ( 6, * ) ' GLO_GLOBAL ended'
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GLO_GLOBAL  #!#
