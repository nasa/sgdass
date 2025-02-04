      SUBROUTINE PAMB_SOL ( PAMB_VER, DBNAME, IDB2, IDBF, N_OBS, ML_OBSER, &
     &           MA_OBSER, IADR_OBSSCA, IADR_OBSSTA, IADR_OBSBAS, IADR_RES, &
     &           IADR_PAMBI, IADR_OBSAOB, OBSHLD, DBOBJ, PLACE, B3DOBJ, &
     &           B1B3DOBJ, NCREC, RST, CHIOBJ, EQUMEM, EQUMEM_INIT_WAS, &
     &           FL_SOL_BYPASS, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine PAMB_SOL reads sratch files, find preliminary    *
! *   group delay solution solution and calculates residuals. It grabs   *
! *   dynamic memory for the fololowung work.                            *
! *                                                                      *
! * _______________________ Input parameters: __________________________ *
! *                                                                      *
! *    PAMB_VER ( INTEGER*4 ) -- Verbosity mode.                         *
! *      DBNAME ( CHARACTER ) -- Database name.                          *
! *        IDB2 ( INTEGER*2 ) -- Index of the considered database in the *
! *                              scratch file.                           *
! *        IDBF ( INTEGER*4 ) -- Index the first observation of the      *
! *                              database in the scratch file.           *
! *       N_OBS ( INTEGER*4 ) -- Total number of observations in the     *
! *                              session.                                *
! * FL_SOL_BYPASS ( LOGICAL*4 ) -- If true, then the routine bybasses    *
! *                                generating preliminary solution.      *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! *    ML_OBSER ( INTEGER*8 ) -- The lentgh (in bytes) of the grabbed    *
! *                              dynamic memory.                         *
! *    MA_OBSER ( INTEGER*8 ) -- The first address of the grabbed        *
! *                              memory.                                 *
! * IADR_OBSSCA ( INTEGER*8 ) -- Address of the array of data structures *
! *                              which keeps scan-dependent information  *
! *                              about the session.                      *
! * IADR_OBSSTA ( INTEGER*8 ) -- Address of the array data structures    *
! *                              which keeps station dependent           *
! *                              information about the session.          *
! * IADR_OBSBAS ( INTEGER*8 ) -- Address of the array data structures    *
! *                              which keeps baseline dependent          *
! *                              information about the session.          *
! *    IADR_RES ( INTEGER*8 ) -- Address of the array of data structures *
! *                              which keeps information about residuals *
! *  IADR_PAMBI ( INTEGER*8 ) -- Address of the array of data structures *
! *                              which keeps information about phase     *
! *                              delays, their errors, ambiguities and   *
! *                              etc.                                    *
! * IADR_OBSAOB ( INTEGER*8 ) -- Address of the of data structures which *
! *                              keeps additional information about      *
! *                              observables and some quantities to be   *
! *                              missed in database/superfiles and which *
! *                              are essential for making phase delay    *
! *                              ambiguity resolving.                    *
! *      OBSHLD ( RECORD    ) -- Data structure which keeps the current  *
! *                              status of the database and some         *
! *                              session-dependent information.          *
! *       DBOBJ ( RECORD    ) -- Data structure which keeps general      *
! *                              information about the database such as  *
! *                              lists of the objects.                   *
! *      PLACE  ( RECORD    ) -- Object with data structure for place of *
! *                              parameters in the list of derivatives.  *
! *      B3DOBJ ( RECORD    ) -- Object with data structure for B3D      *
! *                              extension of SOLVE.                     *
! *    B1B3DOBJ ( RECORD    ) -- Object with data structure for B1B3D    *
! *                              extension of SOLVE.                     *
! *       NCREC ( RECORD    ) -- Data structure for transferring         *
! *                              parameters between SOLVE cutil          *
! *                              subroutines: NCORT, SOCAL, ATMPART.     *
! *         RST ( RECORD    ) -- Data structure keeping the statisitcs   *
! *                              of postfit residuals.                   *
! *      CHIOBJ ( RECORD    ) -- Object with data structure for keeping  *
! *                              accumulators of the chi-squares and     *
! *                              their mathematical expectations.        *
! *      EQUMEM ( RECORD    ) -- Object with data structure for keeping  *
! *                              equations of conditions in memory.      *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! * EQUMEM_INIT_WAS ( LOGICAL*4  ) -- Flag. If .TRUE. means that equmem  *
! *                                   datastructure has been already     *
! *                                   initialized.                       *
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
! *  ###  12-NOV-97    PAMB_SOL    v1.4  (c)  L. Petrov 09-OCT-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INCLUDE   'prfil.i'
      INCLUDE   'precm.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'oborg.i'
      INCLUDE   'obser.i'
      INCLUDE   'fast.i'
      INCLUDE   'pamb.i'
      INCLUDE   'ncrec.i'
      INCLUDE   'equmem.i'
      TYPE ( HLD_O__STRU   ) ::  OBSHLD
      TYPE ( PLACE__STRU   ) ::  PLACE
      TYPE ( B3D__STRU     ) ::  B3DOBJ
      TYPE ( B1B3D__STRU   ) ::  B1B3DOBJ
      TYPE ( NCREC__STRU   ) ::  NCREC
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( RST_O__STRU   ) ::  RST
      TYPE ( CHIACC__STRU  ) ::  CHIOBJ
      TYPE ( EQUMEM__STRU  ) ::  EQUMEM
      CHARACTER  DBNAME*(*)
      INTEGER*4  PAMB_VER, IDBF, N_OBS, ITHR, IUER
      INTEGER*8  ML_OBSER
      ADDRESS__TYPE :: MA_OBSER, &
     &           IADR_OBSSCA, IADR_OBSSTA, IADR_OBSBAS, IADR_RES, IADR_PAMBI, &
     &           IADR_OBSAOB
      INTEGER*4  R_SOU, RIS_SOU(MO_SOU), R_STA, RIS_STA(MO_STA), R_BAS, &
     &           RIS_BAS(MO_BAS), SNGCHK_CMP
      LOGICAL*4  EQUMEM_INIT_WAS, FL_SOL_BYPASS
      INTEGER*2  IDB2
      CHARACTER  STR*20
      INTEGER*4  ELIM_VRB__SAVE, J1, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( PAMB_VER .GE. 1 ) THEN
           WRITE ( 6, FMT='(A)' ) 'Database '//DBNAME//' is being read'
      END IF
!
! --- Scanning of the observations and calculation some statistics
! --- of the sessions, building the lists of the objects. This
! --- information will be used for calculation amount of dynamic
! --- memory needed for temporary bypass data structures
!
      CALL ERR_PASS ( IUER, IER )
      CALL DB_SCAN  ( DBNAME, IDB2, IDBF, N_OBS, DBOBJ, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6231, IUER, 'PAMB_SOL', 'Error duiring '// &
     &         'initialization of data structure detected while '// &
     &         'database '//DBNAME//' was processing' )
           RETURN
      END IF
!
      IF ( PAMB_VER .GE. 1 ) THEN
           WRITE ( 6, FMT='(A)' ) 'Database '//DBNAME//' has been read'
      END IF
!
      IF ( DBOBJ%U_OBS .LE. 0 ) THEN
           CALL ERR_LOG ( 6232, IUER, 'PAMB_SOL', 'No one observation '// &
     &         'which can be used in solution were found in database '// &
     &          DBNAME//' . Check whether ionosphere correction '// &
     &         'has been calculated and applied' )
           RETURN
      END IF
!
! --- Grabbing dynamic memory. Comment: we grab twice more memory for AOB
! --- file for rary case when the length of AOB file is longer than the
! --- length of the database
!
      CALL ERR_PASS ( IUER, IER )
      CALL GRAB_MEM ( IER,  ML_OBSER,                                   MA_OBSER, 6, &
     &                INT8(ML_SCA)*INT8(DBOBJ%L_SCA),                   IADR_OBSSCA, &
     &                INT8(ML_STA)*INT8(DBOBJ%L_STA)*INT8(DBOBJ%L_SCA), IADR_OBSSTA, &
     &                INT8(ML_BAS)*INT8(N_OBS),                         IADR_OBSBAS, &
     &                INT8(ML_RES)*INT8(N_OBS),                         IADR_RES,    &
     &                INT8(ML_PAM)*INT8(N_OBS),                         IADR_PAMBI,  &
     &                INT8(ML_AOB)*INT8(N_OBS*2),                       IADR_OBSAOB  )
!
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( ML_SCA*DBOBJ%L_SCA + &
     &                  ML_STA*DBOBJ%L_STA*DBOBJ%L_SCA + &
     &                  ML_RES*N_OBS                   + &
     &                  ML_BAS*N_OBS                   + &
     &                  ML_PAM*N_OBS                   + &
     &                  ML_AOB*N_OBS,                     STR )
           CALL ERR_LOG ( 6233, IUER, 'PAMB_SOL', 'Error during the '// &
     &                 'attempt to grab '//STR(1:I_LEN(STR))//' bytes of '// &
     &                 'dynamic memory for allocation internal data '// &
     &                 'structures while database '//DBNAME//' was processing' )
           RETURN
      END IF
!
! --- Zeroing entire pool of grabbed dynamic memory
!
      CALL NOUT ( ML_OBSER, %VAL(MA_OBSER) )
!
! --- Switching mode of calculation of the covariance matrix to "FULL"
!
      FAST_COV = F__FUL
      CALL USE_GLBFIL_4 ( 'OWC' )
!
      IF ( EQUMEM_FLAG ) THEN
           IF ( .NOT. EQUMEM_INIT_WAS ) THEN
!
! -------------- Initialize EQUMEM data structure if EQUMEM_FLAG is set and
! -------------- we have not initialized this data structure before
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL EQUMEM_INIT ( EQUMEM, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 6234, IUER, 'PAMB_SOL', 'Error during '// &
     &                    'attempt to initiialize EQUMEM data strucutre' )
                      RETURN
                 END IF
                 EQUMEM_INIT_WAS = .TRUE.
            END IF
          ELSE
            EQUMEM%USE_FLAG = .FALSE.
      END IF
!
! --- Make parameterization check
!
      CALL ERR_PASS ( IUER, IER )
      CALL SNGCHK ( DBOBJ, -1, R_SOU, RIS_SOU, R_STA, RIS_STA, R_BAS, &
     &              RIS_BAS, SNGCHK_CMP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6235, IUER, 'PAMB_SOL', 'Error during '// &
     &         'singularity check' )
           RETURN
      END IF
!
! --- Unprotecting all observations (protecting means that some
! --- observations may be barred from elimination or restoration)
!
      CALL UNPROT_RES ( N_OBS, %VAL(IADR_RES) )
!
      DBOBJ%F_AMB = ELIM_AMB
      DBOBJ%F_ION = ELIM_ION
      DBOBJ%F_AMB_CHANGED = .FALSE.
!
! --- Making initial LSQ solution. Estimates and FULL covariance matrix
! --- are calculaterd. Postfit residuals and their statistics are also
! --- calculated and stored in temporary data structures
!
      ELIM_VRB__SAVE = ELIM_VRB
      ELIM_VRB = PAMB_VER
      CALL ERR_PASS ( IUER, IER )
      IF ( FL_SOL_BYPASS ) THEN
           ITHR = 2
         ELSE 
           ITHR = 1
      END IF
      CALL SOL_INIT ( ELIM_VRB, 0, ITHR, N_OBS, DBOBJ%L_STA, &
     &                DBOBJ%L_SCA, IDB2, IDBF, PLACE, B3DOBJ, B1B3DOBJ, &
     &                OBSHLD, DBOBJ, NCREC, %VAL(IADR_OBSSCA), &
     &                %VAL(IADR_OBSSTA), %VAL(IADR_OBSBAS), %VAL(IADR_RES), &
     &                RST, CHIOBJ, EQUMEM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6236, IUER, 'PAMB_SOL', 'Error during the '// &
     &         'attempt to obtain initial solution while database '// &
     &          DBNAME//' was processing' )
           RETURN
      END IF
      ELIM_VRB = ELIM_VRB__SAVE
!
      CALL INI_PAMBI ( N_OBS, %VAL(IADR_PAMBI) )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  PAMB_SOL  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PAMB_OPEN ()
! ************************************************************************
! *                                                                      *
! *   Auxilary routine PAMB_OPEN  opens some files and load content of   *
! *   some include blocks which will be used by PAMB.                    *
! *                                                                      *
! *  ###  07-NOV-97    PAMB_OPEN   v1.0  (c)  L. Petrov  07-NOV-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
!
! --- Get info from PARFIL
!
      CALL USE_PARFIL('ORC')
!
! --- Loading socom.i
!
      CALL USE_COMMON ( 'ORC' )
      CALL SOCOM_EXT()
!
! --- Open the NAMFIL
!
      CALL OPENNAMFIL()
!
! --- Open spool file
!
      CALL USE_SPOOL ( 'O' )
!
! --- Get info from GLBFIL
!
      CALL USE_GLBFIL   ( 'OR' )
      CALL USE_GLBFIL_2 ( 'R'  )
      CALL USE_GLBFIL_4 ( 'RC' )
!
! --- Get flyby a prioris and modify PARFL common accordingly
!
      CALL FLYBY_APRIOR()
      RETURN
      END  !#!  PAMB_OPEN  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PAMB_CLOSE ( IDB2 )
! ************************************************************************
! *                                                                      *
! *   Auxilary routine PAMB_CLOSE  closes some files which used by PAMB  *
! *   and sets flag: database with index IDB2 was not analysed in order  *
! *   to prevent failure REWAY and/or CRES.                              *
! *                                                                      *
! *  ###  07-NOV-97    PAMB_CLOSE   v1.1 (c)  L. Petrov  20-APR-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INTEGER*2  IDB2
!
! --- Close spool file and rewind it to the end
!
      CALL USE_SPOOL  ( 'C' )
!
! --- Setting flag: session has not been analysed to prevent failure of REWAY
! --- or CRES
!
      CALL SBIT ( IDBEST, IDB2, INT2(0) )
      CALL USE_COMMON ( 'OWC' )
!
      RETURN
      END  !#!  PAMB_CLOSE  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE INI_PAMBI ( N_OBS, PAMBI )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine  INI_PAMBI  initiliaze PAMBI data strucutre.     *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     N_OBS ( INTEGER*4 ) -- Total number of observations in the       *
! *                            session.                                  *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     PAMBI ( RECORD    ) -- Array of data structures keeping          *
! *                            information about phase delays, their     *
! *                            errors, ambiguities and etc.              *
! *                                                                      *
! *  ###  01-OCT-98   INI_PAMBI    v1.0  (c)  L. Petrov  01-OCT-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'pamb.i'
      INTEGER*4  N_OBS
      TYPE ( PAMBI__STRU ) ::  PAMBI(N_OBS)
      INTEGER*4  J1
!
      DO 410 J1=1,N_OBS
         PAMBI(J1)%RES_PX_GXS = 0.0D0
         PAMBI(J1)%ERR_PX_GXS = 0.0D0
         PAMBI(J1)%RES_PS_GXS = 0.0D0
         PAMBI(J1)%ERR_PS_GXS = 0.0D0
         PAMBI(J1)%RES_P_PXS  = 0.0D0
         PAMBI(J1)%ERR_P_PXS  = 0.0D0
!
         PAMBI(J1)%NPHAMB_X   = 0
         PAMBI(J1)%NPHAMB_S   = 0
         PAMBI(J1)%STATUS_X   = 0
         PAMBI(J1)%STATUS_S   = -1
 410  CONTINUE
!
      RETURN
      END  !#!  INI_PAMBI  #!#
