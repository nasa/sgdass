      SUBROUTINE GLO_SAVE ( IPAR, GLBMEM, DBNAME_MES, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GLO_SAVE  writes dowm GCM and (if IPAR=1 or 3) writes     *
! *   down information in GLBFIL about savng CGM.                        *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *       IPAR ( INTEGER*4 ) -- Mode switcher:                           *
! *                        IPAR = 1 -- backup copy is made. CGM is       *
! *                                    written in CGMBxx file. GLBFIL is *
! *                                    written in disk to keep the       *
! *                                    status of CGM backup.             *
! *                        IPAR = 2 -- emergency copy of the CGM. CGM    *
! *                                    is written in CGMFxx file, but    *
! *                                    GLBFIL is not written in disk.    *
! *                        IPAR = 3 -- saving CGM in the permanent file. *
! *                                    CGM is written in the file        *
! *                                    detemined in the control file.    *
! *                                    GLBFIL is written in disk to keep *
! *                                    the status of CGM backup.         *
! *     GLBMEM ( RECORD    ) -- Data structure which keeps addresses of  *
! *                             CGM, list of global parameters, global   *
! *                             socom, prfil and temporary normal        *
! *                             matrices. Defined in ../include/glbp.i   *
! * DBNAME_MES ( CHARACTER ) -- Line with database name + version number *
! *                             Format: "$yyMMMddTT <vv>"                *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
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
! *  pet 2001.05.11  Changed the logic: if IPAR == 3 (permanent CGM),    *
! *                  but the name of permanent CGM saved in glbcm.i is   *
! *                  'NONE', thenbackup CGM will be created. It is       *
! *                  effectively eqivalent to slipping in IPAR == 2 mode.*
! *                                                                      *
! *  ###  06-JAN-99    GLO_SAVE    v1.2  (c)  L. Petrov  18-APR-99  ###  *
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
      INTEGER*4  IPAR, IUER, FILDES
      CHARACTER  CGM_NAME*160, DBNAME_MES*(*)
      INTEGER*4  SARREC
      INTEGER*2  IEOPLL, KERR, IDIRECT(128)
      INTEGER*4  NP
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
#ifdef DEBUG
   write ( 6, * ) 'glo_save GLBMEM%L_GPA = ', GLBMEM%L_GPA ! %%%%%%%%%%%%%
#endif
      IF ( FAST_DBG .EQ. F__APP ) THEN
           WRITE ( 6, * ) ' GLO_SAVE started '
      END IF
!
! --- Set the file name where the CGM will be written
!
      CALL CLRCH ( CGM_NAME )
      IF ( IPAR .EQ. 1 ) THEN
!
! -------- Backup copy
!
           CGM_NAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'CGMB'//PRE_LETRS
         ELSE IF ( IPAR .EQ. 2 ) THEN
!
! -------- Temporary copy
!
           CGM_NAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'CGMF'//PRE_LETRS
         ELSE IF ( IPAR .EQ. 3 ) THEN
!
! -------- Permanent copy needed)
!
           IF ( OUTCGM(1:4) .EQ. 'NONE' .AND. I_LEN(OUTCGM) .EQ. 4 ) THEN
!
! ------------- output CGM is NONE? Then make a backup copy only...
!
                CGM_NAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'CGMB'//PRE_LETRS
              ELSE
                CALL NAMES_CGM ( CGM_NAME )
           END IF
         ELSE
!
! -------- ?? Wrong parameters IPAR
!
           WRITE ( 6, * ) ' IPAR = ',IPAR
           CALL ERR_LOG ( 4151, IUER, 'GLO_CAVE', 'Wrong value of IPAR' )
           RETURN
      END IF
!
      NP = GLBMEM%L_GPA
!
! --- Create the file with CGM and write down there array IDIRECT. Read
! --- IDIRECT array of the file already exist.
!
      CALL CGM_CREATE8 ( IDIRECT, CGM_NAME(1:I_LEN(CGM_NAME)), FILDES, NP, &
     &                  'U', KERR )
      IF ( KERR .NE. INT2(0) ) THEN
           CALL ERR_LOG ( 4152, IUER, 'GLO_CAVE', 'Error during attempt to '// &
     &         'create/read CGM file '//CGM_NAME(1:I_LEN(CGM_NAME))// &
     &         ' during saving CGM' )
           RETURN
      END IF
!
! --- Save session name
!
      SAVED_ARCNAME_MES = DBNAME_MES
!
      IF ( IPAR .EQ. 1  .OR.  IPAR .EQ. 3 ) THEN
!
! -------- Setting flags "restart is not possible" to prevent further attempt
! -------- to restore if failure during solution saving occur
!
           KMORED = .FALSE.
           COPIED = .FALSE.
           CALL USE_GLBFIL ( 'OWC' )
      END IF
!
! --- Copy the number of parametes and the list of parameters in CGM to the
! --- arrays from PLIST common area. It is an interface for CGM_CREATE, CGM_COM
!
      PARM_NUM = NP
      CALL LIB$MOVC3 ( L__GPA*M_GPA, %VAL(GLBMEM%ADR_C_GPA), %REF(CPARM_NAMES) )
!
! --- Copying global SOCOM and PARFIL blocks from internal BATCH data structures
! --- to socom and parfil area to provide interface with CGM_COM
!
      CALL LIB$MOVC3 ( JSOCOM_BYTES,  %VAL(GLBMEM%ADR_GLO_SOCOM), PI_VAR   )
      CALL LIB$MOVC3 ( JPARFIL_BYTES, %VAL(GLBMEM%ADR_GLO_PRFIL), VAXOF(1) )
!
! --- Writing CGM common blocks
!
      CALL CGM_COM ( IDIRECT, CGM_NAME(1:I_LEN(CGM_NAME)), FILDES, 'W' )
!
! --- Writing CGM matrix
!
      CALL CGM_MAT ( IDIRECT, CGM_NAME(1:I_LEN(CGM_NAME)), FILDES, &
     &               %VAL(GLBMEM%ADR_CGM_BLO), NP, 'W' )
!
! --- Closing CGM
!
      CALL CGM_ACCESS ( IDIRECT, CGM_NAME(1:I_LEN(CGM_NAME)), FILDES, 'C' )
!
      IF ( IPAR .EQ. 1  .OR.   IPAR .EQ. 3 ) THEN
!
! -------- Save variables associated with the arc just finished
! -------- That haven't been saved already
!
! -------- end of the sarfile sarfXX
!
           CALL SARST ( SARREC, INT2(0) )
           ISARRC = SARREC
!
! -------- Set BUILT TRUE, indicating we have completed an arc
!
           BUILT  = .TRUE.
!
! -------- Set KMORED to indicate the recovering is possible
!
           KMORED = .TRUE.
!
! -------- Copy perishables variables
!
           PARCNM = IARCNM
           PONAMC = ONAMCG
           PSLTY2 = ISLTY2
           PIPASS = IIPASS
!
! -------- Mark the current position in spool file and return it
!
           CALL EOF_SPOOL ( PSPPOS, 'G' )
           IF ( IEOPLL .NE. 0 ) CALL EOF_SPLLK ( EOPLPOS, PRE_SCR_DIR, &
     &                                           EOPL_BASE, EOPL_LU, 'G' )
!
           PARCRC    = IARCRC
           PTIME0    = STIME0
           PTIMP0    = STIMP0
           PLAST     = SLAST
           PSARRC    = ISARRC
           PWRMS(1)  = CWRMS(1)
           PWRMS(2)  = CWRMS(2)
           PFACT(1)  = CFACT(1)
           PFACT(2)  = CFACT(2)
           PNPARAM   = CNPARAM
           PNCSUM    = CNCSUM
           PKCSUM    = CKCSUM
           PCSHARE   = CSHARE
           TRAIN_CGM = TRAIN
!
! -------- Set COPIED .TRUE., indicating we have a backup copy of all
! -------- crucial variables and data
!
           COPIED = .TRUE.
!
           IF ( IPAR .EQ. 3 ) THEN
!
! ------------- Write down output CGM names. NB: we have to set PONAMC also
! ------------- to be compatible with saving/restoring logic
!
                ONAMCG = CGM_NAME
                PONAMC = CGM_NAME
           END IF
!
! -------- Write glbcm
!
           CALL USE_GLBFIL ( 'OWC' )
      END IF
      IF ( FAST_DBG .EQ. F__APP ) THEN
           WRITE ( 6, * ) ' GLO_SAVE ended '
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GLO_SAVE  #!#
