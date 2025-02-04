      SUBROUTINE GLO_RESTORE ( IPAR, GLBMEM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GLO_RESTORE  reads CGM and related glbfil blocks...       *
! *   (if IPAR=1 or 3). It is assumed that the CGM was previously saved  *
! *   on disk using GLO_SAVE.                                            *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *       IPAR ( INTEGER*4 ) -- Mode switcher:                           *
! *                        IPAR = 1 -- CGM will be read from backup      *
! *                                    copy, from CGMBxx file. Glbcm     *
! *                                    common area will be restored to   *
! *                                    the state before CGM backup.      *
! *                        IPAR = 2 -- CGM will be read from the         *
! *                                    emergency backup copy, from       *
! *                                    CGMFxx file. Glbcm common area    *
! *                                    will be not been changed.         *
! *                        IPAR = 3 -- CGM will be read from the         *
! *                                    permanent backup copy. The file   *
! *                                    name is kept in glbcm.i as        *
! *                                    INAMCG.                           *
! *                        IPAR = 4 -- CGM^(-1) will be read from the    *
! *                                    the COVFxx file which keeps       *
! *                                    the result of inversion of CGM.   *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     GLBMEM ( RECORD    ) -- Data structure which keeps addresses of  *
! *                             CGM, list of global parameters, global   *
! *                             socom, prfil and temporary normal        *
! *                             matrices. Defined in ../include/glbp.i   *
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
! *  ###  06-JAN-99    GLO_RETORE  v1.2  (c)  L. Petrov  15-MAY-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INCLUDE    'plist.i'
      INCLUDE    'glbcm.i'
      INCLUDE    'glbc4.i'
      INCLUDE    'glbp.i'
      INCLUDE    'precm.i'
      INCLUDE    'socom.i'
      INCLUDE    'prfil.i'
      INCLUDE    'fast.i'
!
      TYPE ( GLB_MEM__STRU ) ::  GLBMEM  ! defined in glbp.i
      CHARACTER                  SAVNAM*(NAME_SIZE)
      COMMON   / NAMCGM        / SAVNAM
      INTEGER*4  IPAR, IUER, FILDES, NP, IER
      CHARACTER  CGM_NAME*160, STR*20, STR1*20
      LOGICAL*4  LEX
      INTEGER*2  IDIRECT(128)
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      IF ( FAST_DBG .EQ. F__APP ) THEN
           WRITE ( 6, * ) ' GLO_RESTORE started '
      END IF
!
! --- Set the file name where the CGM will be written
!
#ifdef DEBUG
   write ( 6, * ) 'BATCH glo_restore(76) ' ; call flush ( 6 ) ! %%%%%%%%%%%%
#endif
      CALL CLRCH ( CGM_NAME )
      IF ( IPAR .EQ. 1 ) THEN
!
! -------- Backup copy
!
           CGM_NAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'CGMB'//PRE_LETRS
!
! -------- Keep CGM_NAME to provide (possible) interface with norml_main
!
           SAVNAM   = CGM_NAME
         ELSE IF ( IPAR .EQ. 2 ) THEN
!
! -------- Temporary copy
!
           CGM_NAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'CGMF'//PRE_LETRS
         ELSE IF ( IPAR .EQ. 3 ) THEN
!
! -------- Input CGM
!
           CGM_NAME = INAMCG
         ELSE IF ( IPAR .EQ. 4 ) THEN
!
! -------- Permanent copy
!
           CGM_NAME = ONAMCG
         ELSE IF ( IPAR .EQ. 5 ) THEN
!
! -------- CGM^(-1) permanent copy
!
           CGM_NAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'COVF'//PRE_LETRS
         ELSE
!
! -------- ?? Wrong parameters IPAR
!
           WRITE ( 6, * ) ' IPAR = ',IPAR
           CALL ERR_LOG ( 4161, IUER, 'GLO_RESTORE', 'Wrong value of IPAR' )
           RETURN
      END IF
!
      IF ( ILEN(CGM_NAME) .EQ. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IPAR, STR )
           CALL ERR_LOG ( 4162, IUER, 'GLO_RESTORE', 'Name of CGM which is '// &
     &         'to be read is empty. IPAR = '//STR )
           RETURN
      ENDIF
!
      INQUIRE ( FILE=CGM_NAME, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 4163, IUER, 'GLO_RESTORE', 'File with CGM which is '// &
     &         'to be read: '//CGM_NAME(1:I_LEN(CGM_NAME))//' was not found' )
           RETURN
      ENDIF
!
! --- Open CGM file and read an array IDIRECT
!
      CALL CGM_ACCESS ( IDIRECT, CGM_NAME(1:I_LEN(CGM_NAME)), FILDES, 'O' )
!
! --- Reading CGM common blocks
!
      CALL CGM_COM ( IDIRECT, CGM_NAME(1:I_LEN(CGM_NAME)), FILDES, 'R' )
!
! --- Copy the number of parametes and the list of parameters in CGM from the
! --- arrays in PLIST common area. It is an interface for CGM_CREATE, CGM_COM
!
      NP = PARM_NUM
      GLBMEM%L_GPA = PARM_NUM
      IF ( GLBMEM%L_GPA .GT. GLBMEM%NPAR_CGM ) THEN
           WRITE ( 6, * ) ' TGLBLS = ',TGLBLS
           CALL CLRCH ( STR )
           CALL INCH  ( GLBMEM%NPAR_CGM, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( GLBMEM%L_GPA, STR1 )
           CALL ERR_LOG ( 4164, IUER, 'GLO_RESTORE', 'Trap of internal '// &
     &         'control the number of parameters of the CGM to be read ('// &
     &          STR(1:I_LEN(STR))//') is larger than number of '// &
     &         'parameters ('//STR1(1:I_LEN(STR1))//') was sized' )
           RETURN
      END IF
!
      CALL LIB$MOVC3 ( L__GPA*M_GPA, %REF(CPARM_NAMES), %VAL(GLBMEM%ADR_C_GPA) )
!
! --- Copying global SOCOM and PARFIL blocks from internal BATCH data structures
! --- to GLBMEM data structures
!
      CALL LIB$MOVC3 ( JSOCOM_BYTES,  PI_VAR,   %VAL(GLBMEM%ADR_GLO_SOCOM) )
      CALL LIB$MOVC3 ( JPARFIL_BYTES, VAXOF(1), %VAL(GLBMEM%ADR_GLO_PRFIL) )
!
! --- Reading CGM matrix
!
      CALL CGM_MAT ( IDIRECT, CGM_NAME(1:I_LEN(CGM_NAME)), FILDES, &
     &     %VAL(GLBMEM%ADR_CGM_BLO), NP, 'R' )
!
! --- Closing CGM file
!
      CALL CGM_ACCESS ( IDIRECT, CGM_NAME(1:I_LEN(CGM_NAME)), FILDES, 'C' )
      IF ( FAST_DBG .EQ. F__APP ) THEN
           WRITE ( 6, * ) ' GLO_RESTORE ended ipar = ',ipar
      END IF
!
      IF ( L_SPE    > 0       .AND.  &
     &     PARM_NUM > 0       .AND.  &
     &     ADR_SPE .NE. 0            ) THEN
!
           CALL ERR_PASS ( IUER, IER )
           CALL UPDATE_SPE ( PARM_NUM, CPARM_NAMES, L_SPE, &
     &                       %VAL(ADR_SPE), IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4165, IUER, 'GLO_RESTORE', 'Error in '// &
     &              'an attempt to restore estimation status from the '// &
     &              'parameter list' )
                RETURN 
           END IF
           CALL SPESOL_CREATE ( %VAL(ADR_SPE) )
      END IF
#ifdef DEBUG
   write ( 6, * ) 'BATCH glo_restore(194) ' ; call flush ( 6 ) ! %%%%%%%%%%%%
#endif
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GLO_RESTORE  #!#
