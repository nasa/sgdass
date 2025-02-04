      SUBROUTINE GLO_NORM ( DBNAME_MES, GLBMEM, B3DOBJ, CNSTROBJ, IUER )
! ************************************************************************
! *                                                                      *
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
! *  ###  01-MAR-1999   GLO_NORM   v2.4 (c)  L. Petrov  28-MAR-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'astro_constants.i'
      INCLUDE    'solve.i'
      INCLUDE    'glbcm.i'
      INCLUDE    'glbc4.i'
      INCLUDE    'glbp.i'
      INCLUDE    'socom.i'
      INCLUDE    'prfil.i'
      INCLUDE    'cnstr.i'
      INCLUDE    'fast.i'
      CHARACTER  DBNAME_MES*(*)
      TYPE ( GLB_MEM__STRU ) :: GLBMEM  ! defined in glbp.i
      TYPE ( B3D__STRU     ) :: B3DOBJ
      TYPE ( CNSTR__STRU   ) :: CNSTROBJ
      ADDRESS__TYPE :: MEM_LEN, MEM_ADR
      INTEGER*2  IPRES_KCOV, IRNCD_SAVE(2)
      LOGICAL*2  F_READ_CGM
      INTEGER*4  IUER
      REAL*8     TAI_GLO
      INTEGER*4  MJD_GLO, IER
!
      CHARACTER*(NAME_SIZE) SAVNAM
      COMMON   / NAMCGM /   SAVNAM
!
       IF ( FAST_DBG .EQ. F__APP  .OR.  FAST_DBG .EQ. F__PRI ) THEN
           WRITE ( 6, * ) ' GLO_NORM started '
      END IF
      IPRES_KCOV = IPRES ! copy bit array IPRES from the current socom
!
! --- We will not be reading CGM since it is already in GLBMEM
!
      IF ( DBNAME_MES .EQ. 'CGM' ) THEN
           F_READ_CGM = .TRUE.
           CGM_TYPE   = .TRUE.
         ELSE
           F_READ_CGM = .FALSE.
      END IF
!
! --- Copying global SOCOM and PARFIL blocks from internal BATCH data
! --- structures to socom and parfil area to provide interface
! --- with NORML_MAIN
!
      CALL LIB$MOVC3 ( JSOCOM_BYTES,  %VAL(GLBMEM%ADR_GLO_SOCOM), PI_VAR   )
      CALL LIB$MOVC3 ( JPARFIL_BYTES, %VAL(GLBMEM%ADR_GLO_PRFIL), VAXOF(1) )
      IPRES = IPRES_KCOV ! replace bit array IPRES_KCOV from the batch socom to
!                        ! the current, CGM socom.
      IF ( FAST_DBG .EQ. F__TIM ) THEN
           CALL TIM_GET ( 'NORML-01' )
           CALL TIM_INIT()
      END IF
      IF ( DBNAME_MES .EQ. 'CGM' ) THEN
!
! -------- A case of global-only mode with the input CGM without a session file:
! -------- we need tp load VTD configuration file in VTD data stuctgure
!
           MJD_GLO = NINT(SIT_EST_EPOCH - 2400000.5D0)
           TAI_GLO =     (SIT_EST_EPOCH - 2400000.5D0 - MJD_GLO)*86400.0D0
           CALL ERR_PASS ( IUER, IER )
           CALL GLO_LOAD_VTD ( VTD_CONF_GLB, MJD_GLO, TAI_GLO, %VAL(VTD_ADR), IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4161, IUER, 'GLO_NORM', 'Error in loading global '// &
     &              'VTD configuration file '//VTD_CONF_GLB )
                RETURN
           END IF
      END IF
!
! --- Call a procedure for imposing global constraints and inversion of CGM.
! --- We say to the procedure that CGM is already in memory.
!
      CALL ERR_PASS   ( IUER, IER )
      CALL NORML_MAIN ( GLBMEM%L_GPA, %VAL(GLBMEM%ADR_GLO), B3DOBJ, CNSTROBJ, &
     &     SAVNAM, F_READ_CGM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4162, IUER, 'GLO_NORM', 'Error in NORML_MAIN' )
           RETURN
      END IF
!
! --- Save run code (date of the solution)
!
      IRNCD_SAVE(1) = IRNCD(1)
      IRNCD_SAVE(2) = IRNCD(2)
!
! --- Now we re-read global socom. It is important since norml_main may
! --- spoil it! Goodness!
!
      CALL LIB$MOVC3 ( JSOCOM_BYTES, %VAL(GLBMEM%ADR_GLO_SOCOM), PI_VAR )
!
! --- Restore run code...
!
      IRNCD(1) = IRNCD_SAVE(1)
      IRNCD(2) = IRNCD_SAVE(2)
!
! --- .. and IPRES bit field ...
!
      IPRES = IPRES_KCOV
!
! --- ... and save it back
!
      CALL LIB$MOVC3 ( JSOCOM_BYTES, PI_VAR, %VAL(GLBMEM%ADR_GLO_SOCOM) )
!
      IF ( FAST_DBG .EQ. F__APP  .OR.  FAST_DBG .EQ. F__PRI ) THEN
           WRITE ( 6, * ) ' GLO_NORM ended '
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GLO_NORM  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GLO_LOAD_VTD ( VTD_CONF_FILE, MJD_BEG, TAI_BEG, VTD, IUER )
! ************************************************************************
! *                                                                      *
! *   Roujtine GLO_LOAD_VTD
! *                                                                      *
! *  ### 28-MAR-2023  GLO_LOAD_VTD  v1.0 (c)  L. Petrov 28-MAR-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE    'astro_constants.i'
      INCLUDE    'solve.i'
      INCLUDE    'glbcm.i'
      INCLUDE    'glbc4.i'
      INCLUDE    'glbp.i'
      INCLUDE    'socom.i'
      INCLUDE    'prfil.i'
      INCLUDE    'vtd.i'
      CHARACTER  VTD_CONF_FILE*(*)
      TYPE      ( VTD__TYPE ) VTD
      INTEGER*4  MJD_BEG, IUER
      REAL*8     TAI_BEG
      REAL*8     TAI_END
      INTEGER*4  J1, J2, J3, MJD_END, IER
      REAL*8,    EXTERNAL :: MJD_SEC_TO_JD 
!
      MJD_END = MJD_BEG + 1
      TAI_END = TAI_BEG
!
! --- First quit VTD
!
      IER = 0
      CALL VTD_QUIT ( VTD, IER )
!
! --- Then init VTD
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL VTD_INIT ( VTD,  IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7691, IUER, 'GLO_LOAD_VTD', 'Error in an attempt to '// &
     &         'initialize VTD oibject' )
           RETURN 
      END IF
!
! --- Then load the configuration file
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL VTD_CONF ( VTD_CONF_FILE, VTD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7692, IUER, 'GLO_LOAD_VTD', 'Error in an attempt '// &
     &         'to read configuration file '//VTD_CONF_FILE )
           RETURN 
      END IF
!
! --- Disable computation of position variations, ionospheric  contribution,
! --- and slant path delay, since they are date-specifi and are interpolated
! --- for a shrot period of time
!
      DO 510 J1=1,VTD__M_PSF
         VTD%CONF%POSVAR_MOD(J1) = VTD__UNDF
         CALL CLRCH ( VTD%CONF%POSVAR_FIL(J1) )
 510  CONTINUE 
      DO 520 J2=1,VTD__M_IOF
         CALL CLRCH ( VTD%CONF%IONO_FILE(J2) )
 520  CONTINUE 
      DO 530 J3=1,VTD__M_EPD
         CALL CLRCH ( VTD%CONF%DIR_EPD(J3) )
 530  CONTINUE 
      VTD%CONF%FINAM_STAECC = 'NONE'
      CALL CLRCH ( VTD%CONF%IONO_MODEL )
!
! --- Load gloval stations and sources to use VTD
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL VTD_LOAD  ( VTD, INT(NUMSTA,KIND=4), ISITN_CHR, INT(NUMSTR,KIND=4), ISTRN_CHR, &
     &                 MJD_BEG, TAI_BEG, MJD_END, TAI_END, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7693, IUER, 'GLO_LOAD_VTD', 'Error in an '// &
     &         'attempt to load the data into VTD data structure' )
           RETURN
      END IF
      TIME0 = MJD_SEC_TO_JD ( VTD%STA(1)%MJD_REF, 0.D0 )/YEAR__TO__DAY
!
! --- Again load VTD configuration file to restore disabled options
!
      CALL USE_GLBFIL ( 'OWC' )
      CALL ERR_PASS ( IUER, IER ) 
      CALL VTD_CONF ( VTD_CONF_FILE, VTD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7694, IUER, 'GLO_LOAD_VTD', 'Error in an attempt '// &
     &         'to read configuration file '//VTD_CONF_FILE )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE GLO_LOAD_VTD  !#!#
