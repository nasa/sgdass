      PROGRAM    PAMB_LAUNCH
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
      CALL PAMB()
      END  PROGRAM  PAMB_LAUNCH
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PAMB
! ************************************************************************
! *                                                                      *
! *     Program  PAMB  is the part of SOLVE software for geodetic VLBI   *
! *   data analysis. PAMB resolves phase delay ambiguities using a       *
! *   variety of algorithms, allows to suppress/restore phase delay      *
! *   observables, reweight them, to plot them and to write updated      *
! *   status of phase delays and their ambiguities in oborg area.        *
! *                                                                      *
! *      PAMB allows to carry out these operations in both interactive   *
! *   and automatic mode.                                                *
! *                                                                      *
! *     Restrictions:                                                    *
! *                                                                      *
! *   1) It is assumed that S-band data are available: either thay are   *
! *      strored in oborg area or they cann be read from AOB file.       *
! *   2) PAMB works only in B3D mode.                                    *
! *                                                                      *
! *   WHEN       WHO  VERS.  WHAT                                        *
! *                                                                      *
! *   07-NOV-97  pet  v 0.0  Beginning of the developing.                *
! *                                                                      *
! *   21-DEC-97  pet  v 0.6  Pre-release of beta version.                *
! *                                                                      *
! *   24-MAR-98  pet  v 1.0  Release of first version.                   *
! *                                                                      *
! *   03-APR-98  pet  v 1.1  Minor bug fixed in pamb.f:                  *
! *                          variable idbe was defined as INTEGER*2, but *
! *                          should be defined as INTEGER*4  .           *
! *                                                                      *
! *   06-MAY-98  pet  v 1.2  Minor changes to accommodate new scheme for *
! *                          keeping suppression status.                 *
! *                                                                      *
! *   28-MAY-98  pet  v 1.3  Substantial changes in plotting option.     *
! *                          Added capacity to plot triangle miclosures. *
! *                                                                      *
! *   13-JUL-98  pet  v 1.4  Added option singularity check.             *
! *                                                                      *
! *   25-SEP-98  pet  v 1.5  Added new plotting options.                 *
! *                                                                      *
! *   01-OCT-98  pet  v 1.6  Added SCATIE algotithm. Changed menu        *
! *                          interface. Rearranged PAMBI and OBSBAS data *
! *                          structures. Added "Station ph-gr res"       *
! *                          plotting type.                              *
! *                                                                      *
! *   21-OCT-98  pet  v 1.7  Added check of ambiguity spacings           *
! *                          consitency.                                 *
! *                                                                      *
! *   03-NOV-98  pet  v 2.0  Added support of algorithm SCADAM.          *
! *                                                                      *
! *   19-DEC-98  pet  v 2.1  Added action: frezeing/unfreezing           *
! *                          suppression status for phase delay          *
! *                          observables.                                *
! *                                                                      *
! *   23-DEC-98  pet  v 2.2  Updated PARU. Added support of SCADAM,      *
! *                          SCATIE, PAMB_FREEZE in  PARU.               *
! *                                                                      *
! *   2000.03.31 pet  v 2.3  Updated for using equmem data structure by  *
! *                          ELIM, MILE, UPWEI.                          *
! *                                                                      *
! *   2003.12.08 pet  v 2.31 Fixed a sleeping bug: an argument for the   *
! *                          first call to PAMB_MARES was omiited. :-(   *
! *                                                                      *
! *   2007.03.09 pet  v 2.32 Added support of PAMB_NO_FAST kludge        *
! *                          environment variable.                       *
! *                                                                      *
! *   2020.10.01 pet  v 2.33 Added setting stacksize.                    *
! *                                                                      *
! *  ###  07-NOV-97      PAMB      v2.33 (c)  L. Petrov 01-OCT-2020 ###  *
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
      INCLUDE   'cals.i'
      INCLUDE   'gvh.i'
      INCLUDE   'equmem.i'
      INTEGER*8  ML_OBSER, ML_UOBSER
      ADDRESS__TYPE :: MA_OBSER,  MA_UOBSER
      ADDRESS__TYPE :: IADR_OBSSCA, IADR_OBSSTA, IADR_OBSBAS, IADR_RES, &
     &                 IADR_URES,   IADR_PAMBI, IADR_OBSAOB
      INTEGER*4  J2, J3, J4, J5, J6, IP
      INTEGER*4  PAMB_VER, IACT, IACT_FREEZE, IACT_PLOT, IUER, IER, IDBF, &
     &           N_OBS, ISIM, IOBS_USED, ISR, ISR_LAST, ISS_SOU(MO_SOU), &
     &           IBL_LAST, ITRI_LAST(3), IPLSTA_LAST, IPLFD_STA
      INTEGER*4  ELIM_VRB__SAVE, FAST_COV__SAVE
      LOGICAL*2  KBIT, K_MN
      INTEGER*2  LDBNAM(5,15), IDBV(15), IDB2, IDATYP_OLD
      INTEGER*4  IDBE(15), KAMB, KAMB_S, KAMB_X, IOBS_SPAC
      CHARACTER  CDBNAM(15)*10
      EQUIVALENCE ( CDBNAM, LDBNAM(1,1) )
      LOGICAL*4  F_OPTIN, UNWEIGHT_UPDATE, AMBIG_UPDATE, LSEL_SOU(MO_SOU), &
     &           F_AMB, F_SUP, F_NWT
      REAL*8     INIT_WEI
      CHARACTER  VER_PAMB*21, STR*80, ADD_MES*80, MES*80, GET_VERSION*54
      CHARACTER  DBNAME*16, PARU_FIL*255, ASIM*1
      TYPE ( CALS_STRU     ) ::  CALS
      TYPE ( PLACE__STRU   ) ::  PLACE
      TYPE ( B3D__STRU     ) ::  B3DOBJ
      TYPE ( B1B3D__STRU   ) ::  B1B3DOBJ
      TYPE ( HLD_O__STRU   ) ::  OBSHLD
      TYPE ( NCREC__STRU   ) ::  NCREC
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( CHIACC__STRU  ) ::  CHIOBJ
      TYPE ( RST_O__STRU   ) ::  RST, RSTU
      TYPE ( SCAINF__STRU  ) ::  SCAINF
      TYPE ( EQUMEM__STRU  ) ::  EQUMEM
      TYPE ( GVH__STRU     ) ::  GVH
      CHARACTER  USER_NAME*80, USER_REALNAME*80, USER_E_ADDRESS*80, &
     &           PAMB_NO_FAST*32
      INTEGER*4  ICOND, DB_NUM 
      LOGICAL*4  EQUMEM_INIT_WAS, FL_BATCH
      INTEGER*2     IBUFF(64)
      CHARACTER     CBUFF*128
      EQUIVALENCE ( IBUFF, CBUFF )
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = PSOLVE__STACK_SIZE_IN_GIGABYTES * GB )
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: IFIND_PL, ILEN, I_LEN
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
!
      CALL PRE_PROG()
      CALL SET_PATHS()
      INCLUDE 'pamb_version.i' ! Set revision date of the current version
      VER_PAMB = GET_VERSION ()
!
      K_MN = KSCREEN                  .AND. &   ! Screen mode
     &       KBIT ( PRE_IP ( 2 ), INT2(6) )  ! Interactide mode
!
! --- Reading buffer in order to learn the mode
!
      CALL USE_BUFFER ( IBUFF, INT2(64), 'ORC' )
      CALL CLRCH ( PARU_FIL )
      IF ( IBUFF(1) .EQ. INT2(1) ) THEN
           PARU_FIL  = CBUFF(3:)
           FL_BATCH = .TRUE.
         ELSE 
           FL_BATCH = .FALSE.
      END IF
!
      CALL GETINFO_USER ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
!
! --- Openning necessary files and loading common saved area
!
      CALL PAMB_OPEN()
!
! --- Initialization
!
      ML_OBSER        = -1
      DBOBJ%STATUS    = DBOBJ__UNF
      IUER            = -1
      F_OPTIN         = .FALSE.
      PAMB_VER        =  PAMB_VRB
      UNWEIGHT_UPDATE = .FALSE.
      AMBIG_UPDATE    = .FALSE.
!
      IF ( PAMB_PLOT_TYPE  .LT. PAMB_PTP_MIN  .OR. &
     &     PAMB_PLOT_TYPE  .GT. PAMB_PTP_MAX        ) THEN
           PAMB_PLOT_TYPE = PAMB_PTP_MIN
      END IF
!
      IF ( PAMB_PSL_TYPE  .LT.  PSL__MIN  .OR. &
     &     PAMB_PSL_TYPE  .GT.  PSL__MAX        ) THEN
           PAMB_PSL_TYPE = PSL__MIN
      END IF
!
      F_AMB    = .TRUE.
      F_SUP    = .TRUE.
      F_NWT    = .TRUE.
      INIT_WEI = PAMB_INIWEI
!
      SCAINF%XGR_LIM     = PAMB_XGRLIM
      SCAINF%SGR_LIM     = PAMB_SGRLIM
      SCAINF%XPH_LIM     = PAMB_XPHLIM
      SCAINF%SPH_LIM     = PAMB_SPHLIM
      SCAINF%DEFRG       = PAMB_DEFRG
      SCAINF%ARFMS       = PAMB_ARFMS
      SCAINF%FRZTR       = PAMB_FRZTR
      SCAINF%ARFFLO      = PAMB_ARFFLO
      SCAINF%SPL_SPAN    = PAMB_SPLSPAN
      SCAINF%SPL_CNST    = PAMB_SPL_CNST
      SCAINF%MSC_CONTROL = PAMB_MSC
      SCAINF%ARF_TYPE    = PAMB_ARFTYPE
      SCAINF%FID_STA     = PAMB_FSTA
      SCAINF%PLOT_INI    = PAMB_PLOTINI
      SCAINF%PLOT_FIN    = PAMB_PLOTFIN
!
      DO 420 J2=1,MG_STA
         SCAINF%P_STA(J2) = KBIT ( PAMB_PSTA, INT2(J2) )
 420  CONTINUE
!
      B3DOBJ%MEM_STAT = F__UND
      FAST_COV__SAVE  = FAST_COV
!
! --- Learn NUMDB  -- the number of database treated by LOOP now
! ---       LDBNAM -- data base name
! ---       IDBV   -- data base version (in 1-st element of array)
! ---       IDBE   -- number of observations (in 1-st element of array)
!
      CALL DBPOX ( NUMDB, LDBNAM, IDBV, IDBE )
!
      N_OBS = 0
      CALL USE_COMMON ( 'ORC' )
      CALL SOCOM_EXT()
      CALL USE_GLBFIL_4 ( 'OR' )
      VTD_ADR = 0
      VTD_STATUS = 0
      CALL USE_GLBFIL_4 ( 'WC' )
!
! --- Scan databases in scratch file
!
      DO 430 J3=1,NUMDB
!
! ------ Determine IDBF -- index of the first observation for this
! ------ database in SCRATCH file, N_OBS -- the number of observations
! ------ in scratch file, IDB2 -- the index of the database in the
! ------ list of databases in scratch file
!
         IF ( KBIT(IDBSEL,INT2(J3)) ) THEN
              IF ( J3 .EQ. 1 ) THEN
                   IDBF  = 1
                ELSE
                   IDBF  = IDBE(J3-1) + 1
              END IF
              N_OBS = IDBE(J3) - IDBF + 1
              IDB2 = INT2(J3)
!
! ----------- Form the name of the analyzed database
!
              CALL CLRCH ( DBNAME )
              DBNAME(1:12) = CDBNAM(J3)(1:10)//' <'
              CALL INCH ( INT4(IDBV(J3)), DBNAME(13:) )
              DBNAME( ILEN(DBNAME)+1: ) = '>'
              GOTO 830
         END IF
 430  CONTINUE
      CALL ERR_LOG ( 6201, IUER, 'PAMB', 'No one database have been '// &
     &    'included in solution' )
      GOTO 810
 830  CONTINUE
!
      CALL CHIN ( DBNAME(1:8), DB_NUM )
      IF ( DB_NUM > 19700101 .AND. DB_NUM < 20700001 ) THEN
           CONTINUE 
         ELSE 
           IF ( DBNAME(9:9) .NE. 'X'  .AND.  DBNAME(9:10) .NE. 'DX'  ) THEN
                CALL ERR_LOG ( 6202, IUER, 'PAMB', 'Only database for '// &
     &              'X-band may be used by PAMB: '//DBNAME// &
     &              ' is not good database' )
                GOTO 810
           END IF
      END IF
!
      DO 440 J4=1,MO_SOU
         LSEL_SOU(J4) = 0
         ISS_SOU(J4)  = 0
 440  CONTINUE
      ISR = 0
!
! --- Checking fast mode
!
      IF ( FAST_MODE .NE. F__B3D   .AND.   FAST_MODE .NE. F__B1B3D ) THEN
           CALL CLRCH ( STR )
           IP = IFIND_PL ( FM_VAR, FM_VAL, FAST_MODE )
           IF ( IP .LE. 0 ) THEN
                STR = 'Undefined'
              ELSE
                STR = FM_STR ( IP )
           END IF
           CALL GETENVAR  ( 'PAMB_NO_FAST', PAMB_NO_FAST )
           IF ( PAMB_NO_FAST == 'YES' ) THEN
                CONTINUE 
              ELSE 
                CALL ERR_LOG ( 6203, IUER, 'PAMB', 'FAST mode "'//STR(1:I_LEN(STR))// &
     &              '" is not supported by PAMB. Only B3D mode is supported. '// &
     &              'Please change the fast mode' )
                GOTO 810
           END IF
      END IF
!
! --- Reading calibration status
!
      CALL ERR_PASS ( IUER, IER )
      CALL CALS_R   ( IDB2, 0, 0, CALS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6204, IUER, 'PAMB', 'Error in reading calibration '// &
     &         'status from database '//DBNAME )
           GOTO 810
      END IF
!
! --- Gtabbing additional dynamic memory for plotting
!
      CALL ERR_PASS ( IUER, IER )
      CALL GRAB_MEM ( IER,  ML_UOBSER,                MA_UOBSER,  1, &
     &                      INT8(ML_RES)*INT8(N_OBS), IADR_URES      )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6205, IUER, 'PAMB', 'Error during grabbing '// &
     &         'memory for internal data structures for phase delay '// &
     &         'ambiguity resolution algorithm' )
           GOTO 810
      END IF
!
      IBL_LAST     = 0
      IPLFD_STA    = -1
      IPLSTA_LAST  = -1
      ITRI_LAST(1) = 1
      ITRI_LAST(2) = 2
      ITRI_LAST(3) = 3
      EQUMEM_INIT_WAS = .FALSE.
!
! --- Main PAMB loop: reading user input in menu and execution of menu command
!
 910  CONTINUE
      DO WHILE ( .NOT. F_OPTIN )
!
! ------ Reading user input in menu
!
         CALL PAMB_MENU ( VER_PAMB, DBNAME, IDATYP, OPP_STATUS, IACT, &
     &                    PAMB_VER, F_OPTIN )
         PAMB_VRB = PAMB_VER
         CALL USE_GLBFIL_4 ( 'OWC' )
         IF ( F_OPTIN ) GOTO 810
!
         IF ( IACT .NE. 14  .AND.  DBOBJ%STATUS .NE. DBOBJ__DON ) THEN
!
! ----------- It was the first call of PAMB routines. Making preliminary
! ----------- solution. Grabbing dynamic memory for internal data structure
!
              CALL ERR_PASS ( IUER, IER )
              CALL PAMB_SOL ( PAMB_VER, DBNAME, IDB2, IDBF, N_OBS, ML_OBSER, &
     &             MA_OBSER, IADR_OBSSCA, IADR_OBSSTA, IADR_OBSBAS, IADR_RES, &
     &             IADR_PAMBI, IADR_OBSAOB, OBSHLD, DBOBJ, PLACE, B3DOBJ, &
     &             B1B3DOBJ, NCREC, RST, CHIOBJ, EQUMEM, EQUMEM_INIT_WAS, &
     &             .FALSE., IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6206, IUER, 'PAMB', 'Error during '// &
     &                 'attempt to produce solution while database '//DBNAME// &
     &                 ' was processing' )
                   GOTO 810
              END IF
!
! ----------- Putting initial information in fields of PAMBI data strucutre
!
              CALL ERR_PASS   ( IUER, IER )
              CALL PAMB_MARES ( 0, N_OBS, DBOBJ, %VAL(IADR_OBSSCA), &
     &                          %VAL(IADR_OBSBAS), %VAL(IADR_RES), &
     &                          %VAL(IADR_OBSAOB), %VAL(IADR_PAMBI), KAMB, &
     &                          IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6207, IUER, 'PAMB', 'Error during making '// &
     &                 'phase delay residuals while database '//DBNAME// &
     &                 ' was processing' )
                   GOTO 810
              END IF
         END IF
!
         IF ( IACT .EQ. 11 ) THEN
!
! ----------- Take parameters from menu
!
              CALL PAMB_INIT_MENU ( VER_PAMB, F_AMB, F_SUP, F_NWT, INIT_WEI, &
     &                              IACT )
              IF ( IACT .EQ. 1 ) THEN
                 PAMB_INIWEI = INIT_WEI
                 CALL USE_GLBFIL_4 ( 'OWC' )
!
                 IF ( PAMB_VER .GE. 1 ) THEN
                      WRITE (  6, * ) 'Phase delays are being initialized'
                 END IF
!
                 PAMB_STATUS = INT2(0)
                 CALL USE_COMMON ( 'OWC' )
!
! -------------- Update of oborg area for the additional obserbles for S-band.
! -------------- Ambiguities, suppression status, phase delay reweight constant
! -------------- can be initialized
!
                 CALL ERR_PASS   ( IUER, IER )
                 CALL PAMB_INIT  ( IDB2, DBNAME, DBOBJ, OBSHLD, &
     &                             %VAL(IADR_OBSBAS), CHIOBJ, %VAL(IADR_PAMBI), &
     &                             F_AMB, F_SUP, F_NWT, INIT_WEI, IOBS_USED, &
     &                             IOBS_SPAC, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 6208, IUER, 'PAMB', 'Error during '// &
     &                    'attempt to initialize phase delays and their '// &
     &                    'ambiguities while database '// &
     &                     DBNAME(1:I_LEN(DBNAME))//' was being processed' )
                      GOTO 810
                 END IF
                 IF ( PAMB_VER .GE. 1 ) THEN
                      WRITE (  6, *  ) 'Phase delays have been initialized'
                      WRITE (  6, '(I5,A)' ) IOBS_USED, ' phase delays are '// &
     &                                                  'in use'
                      IF ( IOBS_SPAC .EQ. 0 ) THEN
                           WRITE (  6, '(A)' )  ' No phase delays were rejected'
                        ELSE
                           WRITE (  6, '(I5,A)' ) IOBS_SPAC, ' phase delays '// &
     &                       'were rejected (inconsistent ambiguity spacing)'
                      END IF
                 END IF
                 CALL HIT_CONT ( %VAL(0), %VAL(0) )
              END IF  ! IACT=1
         END IF ! IACT 11
!
         IF ( IACT .EQ. 12 ) THEN
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, * ) 'Aob file is being read'
              END IF
!
! ----------- Update of oborg area for the additional observables for S-band
!
              CALL ERR_PASS   ( IUER, IER )
              CALL AOB_UPDATE ( 3, IDB2, DBNAME, DBOBJ, %VAL(IADR_OBSAOB), &
     &                          %VAL(IADR_OBSBAS), %VAL(IADR_PAMBI), IER )
              IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 6209, IUER, 'PAMB', 'Database '// &
     &                 DBNAME(1:I_LEN(DBNAME))//' doesn''t have '// &
     &                'information needed for phase delay ambiguity '// &
     &                'resolution. Attempt to find missed information in '// &
     &                'AOB-file failed.' )
                  GOTO 810
              END IF
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, * ) 'Aob file has been read'
                   CALL HIT_CONT ( %VAL(0), %VAL(0) )
              END IF
         END IF
!C
         IF ( IACT .EQ. 13  ) THEN
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, * ) 'Ambiguities are being rewritten into '// &
     &                             'scratch area'
              END IF
!
! ----------- Saving phase delay ambiguity and and their downweight
! ----------- status on oborg scratch area
!
              CALL ERR_PASS  ( IUER, IER )
              CALL PAMB_SAVE ( IDBF, N_OBS, DBOBJ, %VAL(IADR_OBSBAS), &
     &                         %VAL(IADR_RES), %VAL(IADR_PAMBI), &
     &                         .TRUE., .TRUE., IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6210, IUER, 'PAMB', 'Error during '// &
     &                 'attempt to update phase save ambiguities while ' // &
     &                 'database '//DBNAME(1:I_LEN(DBNAME))//' was processing' )
                   GOTO 810
              END IF
!
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, * ) 'Ambiguities has been rewritten into '// &
     &                             'scratch area'
                   CALL HIT_CONT ( %VAL(0), %VAL(0) )
              END IF
              AMBIG_UPDATE = .FALSE.
         END IF ! IACT=13
!
         IF ( IACT .EQ. 14  ) THEN
!
! ----------- Call menu for select of freezing/unfreezing phase delay
! ----------- observables action
!
              IACT_FREEZE = -1
              CALL PAMB_FREEZE_MENU ( IACT_FREEZE )
!
              CALL ERR_PASS ( IUER, IER )
              IF ( IACT_FREEZE .NE. -1 ) THEN
                   IF ( PAMB_VER .GE. 1 ) THEN
                        WRITE (  6, * ) 'Suppression status of pahse delays '// &
     &                                  'is being changed'
                   END IF
!
! ---------------- Freezing/unfreezeing supression status of  phase delay
! ---------------- observables
!
                   CALL ERR_PASS    ( IUER, IER )
                   CALL PAMB_FREEZE ( IDBF, N_OBS, IACT_FREEZE, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6211, IUER, 'PAMB', 'Error during '// &
     &                      'attempt to freeze/unfreeze suppression status '// &
     &                      'of phase delay observables while database '// &
     &                       DBNAME(1:I_LEN(DBNAME))//' was processing' )
                        GOTO 810
                   END IF
!
                   IF ( PAMB_VER .GE. 1 ) THEN
                        IF ( IACT_FREEZE .GE. 21  .AND. &
     &                       IACT_FREEZE .LE. 29        ) THEN
                             WRITE (  6, * ) 'Suppression status of phase '// &
     &                                       'delay observables has been frozen'
                          ELSE IF ( IACT_FREEZE .GE. 31  .AND. &
     &                              IACT_FREEZE .LE. 39        ) THEN
                             WRITE (  6, * ) 'Suppression status of phase '// &
     &                                       'delay observables was unfrozen'
                        END IF
!
                        CALL HIT_CONT ( %VAL(0), %VAL(0) )
                   END IF
              END IF
         END IF ! IACT = 14
!
         IF ( IACT .EQ. 101 ) THEN
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, * ) 'Ambiguity resolution algorithm MIXOB '// &
     &                             'is running'
              END IF
!
! ----------- Making phase delay residuals and resolving phase delay ambiguities
! ----------- using MIXOB algorithm
!
              CALL ERR_PASS   ( IUER, IER )
              CALL PAMB_MARES ( 1, N_OBS, DBOBJ, %VAL(IADR_OBSSCA), &
     &                          %VAL(IADR_OBSBAS), %VAL(IADR_RES), &
     &                          %VAL(IADR_OBSAOB), %VAL(IADR_PAMBI), KAMB, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6212, IUER, 'PAMB', 'Error during making '// &
     &                 'phase delay residuals and resolving ambiguities '// &
     &                 'using MIXOB algorithm while database '//DBNAME// &
     &                 ' was processing' )
                   GOTO 810
              END IF
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, * ) KAMB,' ambiguities were changed'
                   WRITE (  6, * ) 'Ambiguity resolution algorithm MIXOB '// &
     &                             'finished'
                   CALL HIT_CONT ( %VAL(0), %VAL(0) )
              END IF
              AMBIG_UPDATE = .TRUE.
         END IF
!
         IF ( IACT .EQ. 102  ) THEN
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, * ) 'Ambiguity resolution algorithm OSCRO '// &
     &                             'is running'
              END IF
!
! ----------- Resolving phase delay ambiguties for X-band using OSCRO algorithm
!
              CALL ERR_PASS  ( IUER, IER )
              CALL OSCRO_SES ( DBOBJ%L_OBS, DBOBJ, %VAL(IADR_OBSBAS), &
     &                         %VAL(IADR_RES), %VAL(IADR_PAMBI), PAMB__XBAND, &
     &                         KAMB_X, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6213, IUER, 'PAMB', 'Error during '// &
     &                 'attempt to apply phase delay ambiguity resolution ' // &
     &                 'algorithm OSCRO for X-band while the database '// &
     &                  DBNAME(1:I_LEN(DBNAME))//' was processing' )
                   GOTO 810
              END IF
!
! ----------- Resolving phase delay ambiguties for S-band using OSCRO algorithm
!
              CALL ERR_PASS  ( IUER, IER )
              CALL OSCRO_SES ( DBOBJ%L_OBS, DBOBJ, %VAL(IADR_OBSBAS), &
     &                         %VAL(IADR_RES), %VAL(IADR_PAMBI), PAMB__SBAND, &
     &                         KAMB_S, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6214, IUER, 'PAMB', 'Error during '// &
     &                 'attempt to apply phase delay ambiguity resolution ' // &
     &                 'algorithm OSCRO for S-band while the database '// &
     &                  DBNAME(1:I_LEN(DBNAME))//' was processing' )
                   GOTO 810
              END IF
!
              IF ( PAMB_VER .GE. 1 ) THEN
                   KAMB = KAMB_X + KAMB_S
                   WRITE (  6, * ) KAMB,' ambiguities were changed'
                   WRITE (  6, * ) 'Ambiguity resolution algorithm OSCRO '// &
     &                             'finished'
                   CALL HIT_CONT ( %VAL(0), %VAL(0) )
              END IF
              AMBIG_UPDATE = .TRUE.
         END IF
!C
         IF ( IACT .EQ. 103  ) THEN
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, * ) 'Permanent ambiguity elimination '// &
     &                             'algorithm CLOPA is running'
              END IF
!
! ----------- Elimination permanent phase delay ambiguity for S-band using
! ----------- CLOPA algorithm
!
              CALL ERR_PASS  ( IUER, IER )
              CALL CLOPA     ( DBOBJ%L_OBS, DBOBJ, %VAL(IADR_OBSBAS), &
     &                         %VAL(IADR_RES), %VAL(IADR_PAMBI), PAMB__SBAND, &
     &                         PAMB_VER, KAMB_X, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6215, IUER, 'PAMB', 'Error during '// &
     &                 'attempt to apply phase delay ambiguity resolution ' // &
     &                 'algorithm CLOPA for S-band while the database '// &
     &                  DBNAME(1:I_LEN(DBNAME))//' was processing' )
                   GOTO 810
              END IF
!
! ----------- Elimination permanent phase delay ambiguity for X-band using
! ----------- CLOPA algorithm
!
              CALL ERR_PASS  ( IUER, IER )
              CALL CLOPA     ( DBOBJ%L_OBS, DBOBJ, %VAL(IADR_OBSBAS), &
     &                         %VAL(IADR_RES), %VAL(IADR_PAMBI), PAMB__XBAND, &
     &                         PAMB_VRB, KAMB_S, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6216, IUER, 'PAMB', 'Error during '// &
     &                 'attempt to apply phase delay ambiguity resolution ' // &
     &                 'algorithm CLOPA for X-band while the database '// &
     &                  DBNAME(1:I_LEN(DBNAME))//' was processing' )
                   GOTO 810
              END IF
!
              IF ( PAMB_VER .GE. 1 ) THEN
                   KAMB = KAMB_X + KAMB_S
                   WRITE (  6, * ) KAMB,' ambiguities were changed'
                   WRITE (  6, * ) 'Permanent ambiguity elimination '// &
     &                             'algorithm CLOPA finished'
                   CALL HIT_CONT ( %VAL(0), %VAL(0) )
              END IF
              AMBIG_UPDATE = .TRUE.
         END IF
!C
         IF ( IACT .EQ. 104  ) THEN
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, * ) 'Ambiguity resolution algorithm SCATIE '// &
     &                             'is running'
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL SCATIE_DO ( DBOBJ, %VAL(IADR_OBSSCA), %VAL(IADR_OBSBAS), &
     &                         %VAL(IADR_PAMBI), KAMB, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6217, IUER, 'PAMB', 'Error during '// &
     &                 'attempt to resolve phase delay ambiguities using '// &
     &                 'SCATIE algorithm occurred when the database '// &
     &                  DBNAME(1:I_LEN(DBNAME))//' was processing' )
                   CALL HIT_CONT ( %VAL(0), %VAL(0) )
                   GOTO 910
              END IF
!
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, * ) KAMB,' ambiguities were changed'
                   WRITE (  6, * ) 'Ambiguity resolution algorithm SCATIE '// &
     &                             'finished'
                   CALL HIT_CONT ( %VAL(0), %VAL(0) )
              END IF
              IF ( KAMB .GT. 0 ) AMBIG_UPDATE = .TRUE.
         END IF
!C
         IF ( IACT .EQ. 105  ) THEN
              IF ( PAMB_VER .GE. 1 ) THEN
                   WRITE (  6, * ) 'Ambiguity resolution algorithm SCADAM '// &
     &                             'is running'
              END IF
!
! ----------- Call SCADAM algorithm
!
              CALL ERR_PASS ( IUER, IER )
              CALL SCADAM_MAIN ( PAMB_VER, DBOBJ, %VAL(IADR_OBSSCA), &
     &                           %VAL(IADR_OBSBAS), %VAL(IADR_PAMBI), SCAINF, &
     &                           ICOND, KAMB, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6218, IUER, 'PAMB', 'Error during '// &
     &                 'attempt to resolve phase delay ambiguities using '// &
     &                 'SCADAM algorithm occurred when the database '// &
     &                  DBNAME(1:I_LEN(DBNAME))//' was processing' )
                   CALL HIT_CONT ( %VAL(0), %VAL(0) )
                   GOTO 910
              END IF
!
! ----------- Store setting for SCADAM algortihm
!
              DO 450 J5=1,MG_STA
                 IF ( SCAINF%P_STA(J5) ) THEN
                      CALL SBIT ( PAMB_PSTA, INT2(J5), INT2(1)  )
                    ELSE
                      CALL SBIT ( PAMB_PSTA, INT2(J5), INT2(0)  )
                 END IF
 450          CONTINUE
              PAMB_FSTA     = SCAINF%FID_STA
!
              PAMB_XGRLIM   = SCAINF%XGR_LIM
              PAMB_SGRLIM   = SCAINF%SGR_LIM
              PAMB_XPHLIM   = SCAINF%XPH_LIM
              PAMB_SPHLIM   = SCAINF%SPH_LIM
              PAMB_DEFRG    = SCAINF%DEFRG
              PAMB_ARFMS    = SCAINF%ARFMS
              PAMB_FRZTR    = SCAINF%FRZTR
              PAMB_ARFFLO   = SCAINF%ARFFLO
              PAMB_SPLSPAN  = SCAINF%SPL_SPAN
              PAMB_SPL_CNST = SCAINF%SPL_CNST
              PAMB_MSC      = SCAINF%MSC_CONTROL
              PAMB_ARFTYPE  = SCAINF%ARF_TYPE
              PAMB_FSTA     = SCAINF%FID_STA
              PAMB_PLOTINI  = SCAINF%PLOT_INI
              PAMB_PLOTFIN  = SCAINF%PLOT_FIN
!
              CALL USE_GLBFIL_4 ( 'OWC' )
!
              IF ( PAMB_VER .GE. 1  .AND. ICOND .GT. 0 ) THEN
                   WRITE (  6, * ) KAMB,' ambiguities were changed'
                   WRITE (  6, * ) 'Ambiguity resolution algorithm SCADAM '// &
     &                             'finished'
                   CALL HIT_CONT ( %VAL(0), %VAL(0) )
                ELSE IF ( PAMB_VER .GE. 1  .AND. ICOND .LT. 0 ) THEN
                   WRITE (  6, * ) 'Ambiguity resolution algorithm SCADAM '// &
     &                             'finished without ambiguity resolving'
                   CALL HIT_CONT ( %VAL(0), %VAL(0) )
              END IF
              IF ( KAMB .GT. 0 ) AMBIG_UPDATE = .TRUE.
         END IF
!
         IF ( IACT .EQ. 111 .OR.  IACT == 211 ) THEN
!
! ----------- Change of solution type
!
              CALL CLEAR ( 0, 0 )
              IDATYP_OLD = IDATYP
              CALL START_MN()
              IF ( IACT == 111 ) THEN
                   CALL CHANGE_DATATYPE ( .FALSE. )
                 ELSE 
                   CALL CHANGE_DATATYPE ( .TRUE. )
              END IF
!
! ----------- Stop curses. Eliminate its harmful influence
!
              CALL END_MN()
              CALL UN_CURSES ()
              CALL CLEAR ( 0, 0 )
!
! ----------- Check: was the solution type really changed?
!
              IF ( IDATYP .NE. IDATYP_OLD ) THEN
!
! -------------- Solution type was changed. Write down SOCOM.
!
                 CALL USE_COMMON ( 'OWC' )
                 CALL PARCN()
!
                 IF ( PAMB_VER .GE. 1 ) THEN
                      CALL DATYP_SHOW ( IDATYP, STR )
                      WRITE (  6, FMT='(A)' ) 'Solution type "'// &
     &                      STR(1:I_LEN(STR))//'" was set'
                 END IF
!
                 IF ( PAMB_VER .GE. 1 ) THEN
                      WRITE ( 6, FMT='(A)' ) 'Database '//DBNAME// &
     &                                       ' is being read'
                 END IF
!
! -------------- Scanning of the observations and calculation some statistics
! -------------- of the sessions, building the lists of the objects. This
! -------------- information will be used for calculation amount of dynamic
! -------------- memory needed for temorary bypass data structures
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL DB_SCAN  ( DBNAME, IDB2, IDBF, N_OBS, DBOBJ, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 6219, IUER, 'PAMB', 'Error during '// &
     &                    'initialization of data structure detected while '// &
     &                    'database '//DBNAME//' was processing' )
                      GOTO 810
                 END IF
                 IF ( PAMB_VER .GE. 1 ) THEN
                      WRITE ( 6, FMT='(A)' ) 'Database '//DBNAME// &
     &                                       ' has been read'
                      WRITE (  6, '(A,I5,A)' ) '      ',DBOBJ%U_OBS, &
     &                                     ' observations are in use'
                      WRITE ( 23, '(A,I5,A)' ) '      ',DBOBJ%U_OBS, &
     &                                     ' observations are in use'
                 END IF
!
! -------------- Switching mode of calculation of the covariance matrix to
! -------------- "FULL"
!
                 FAST_COV = F__FUL
                 IF ( EQUMEM_FLAG ) THEN
!
! ------------------- Free memory grabbed by equmem
!
                      IER = 0
                      CALL EQUMEM_END ( EQUMEM, IER )
!
! ------------------- Initialized EQUMEM
!
                      CALL ERR_PASS    ( IUER, IER )
                      CALL EQUMEM_INIT ( EQUMEM, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 6220, IUER, 'PAMB', 'Error during '// &
     &                         'attempt to initiialize EQUMEM data strucutre' )
                           GOTO 810
                      END IF
                 END IF
!
! -------------- Making initial LSQ solution. Estimates and FULL covariance
! -------------- matrix are calculated. Postfit residuals and their statistics
! -------------- are also calculated and stored in temporary data structures
!
                 ELIM_VRB__SAVE = ELIM_VRB
                 ELIM_VRB = PAMB_VER
                 CALL ERR_PASS ( IUER, IER )
                 CALL SOL_INIT ( ELIM_VRB, 1, 1, N_OBS, DBOBJ%L_STA, &
     &                DBOBJ%L_SCA, IDB2, IDBF, PLACE, B3DOBJ, B1B3DOBJ, OBSHLD, &
     &                DBOBJ, NCREC, %VAL(IADR_OBSSCA), %VAL(IADR_OBSSTA), &
     &                %VAL(IADR_OBSBAS), %VAL(IADR_RES), RST, CHIOBJ, EQUMEM, &
     &                IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 6221, IUER, 'PAMB', 'Error during '// &
     &                    'attempt to obtain initial solution while database '// &
     &                     DBNAME//' was processing' )
                      GOTO 810
                 END IF
                 ELIM_VRB = ELIM_VRB__SAVE
!
! -------------- Printing status message
!
                 IF ( PAMB_VER .GE. 1 ) THEN
                      CALL DATYP_SHOW ( IDATYP, STR )
                      WRITE (  6, FMT='(A)' ) 'Solution "'//STR(1:I_LEN(STR))// &
     &                      '" was updated'
                      CALL HIT_CONT ( %VAL(0), %VAL(0) )
                 END IF
              END IF
         END IF
!
         IF ( IACT .EQ. 112  ) THEN
!
! ----------- Set singularity check coontrol
!
              CALL SET_SNGCHK ( SNGCHK_ACTION, SNGCHK_SOUMIN, SNGCHK_STAMIN, &
     &                          SNGCHK_BASMIN )
              CALL USE_GLBFIL_4 ( 'OWC' )
         END IF
!
         IF ( IACT .EQ. 121  ) THEN
!
! ----------- Resolving phase delay ambiguties in automatic mode using PARU
!
              CALL ERR_PASS  ( IUER, IER )
              CALL PARU_MAIN ( VER_PAMB, PAMB_VER, FL_BATCH, N_OBS, IDBF, IDB2, &
     &             ML_OBSER, MA_OBSER, &
     &             DBOBJ, NCREC, OBSHLD, %VAL(IADR_OBSSCA), %VAL(IADR_OBSSTA), &
     &             %VAL(IADR_OBSBAS), %VAL(IADR_RES), %VAL(IADR_PAMBI), PLACE, &
     &             B3DOBJ, B1B3DOBJ, RST, CHIOBJ, SCAINF, EQUMEM, PARU_FIL, &
     &             GVH, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6222, IUER, 'PAMB', 'Error during '// &
     &                 'attempt to resolve phase delay ambiguities in '// &
     &                 'automatic mode while the database '// &
     &                  DBNAME(1:I_LEN(DBNAME))//' was processing' )
                   CALL HIT_CONT ( %VAL(0), %VAL(0) )
              END IF
              AMBIG_UPDATE = .TRUE.
         END IF
!
         IF ( IACT .EQ. 131 .OR. &
     &        IACT .EQ. 132 .OR. &
     &        IACT .EQ. 133      ) THEN
!
              CALL ERR_PASS  ( IUER, IER )
              CALL PAMB_ELIM ( IACT, PAMB_VER, DBOBJ%L_OBS, IDBF, IDB2, &
     &             DBOBJ, NCREC, OBSHLD, %VAL(IADR_OBSSCA), %VAL(IADR_OBSSTA), &
     &             %VAL(IADR_OBSBAS), %VAL(IADR_RES), %VAL(IADR_PAMBI), PLACE, &
     &             B3DOBJ, B1B3DOBJ, RST, CHIOBJ, UNWEIGHT_UPDATE, EQUMEM, &
     &             IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6223, IUER, 'PAMB', 'Error during the '// &
     &                 'operation of outlier elimination/resotration or '// &
     &                 'weight update while the database '// &
     &                  DBNAME(1:I_LEN(DBNAME))//' was processing' )
                   GOTO 810
              END IF
         END IF
!
         IF ( IACT .EQ. 141  ) THEN
!
! ----------- Selection of plot type via menu interface
!
 920          CONTINUE
              CALL PAMB_PLOT_MENU ( VER_PAMB, MO_SOU, ISR, ISS_SOU, LSEL_SOU, &
     &                              PAMB_PLOT_BAND, PAMB_PLOT_TYPE, &
     &                              PAMB_PSL_TYPE, IACT_PLOT )
              IF ( ISR .EQ. 0 ) ISR_LAST = 0
              IF ( IACT_PLOT .EQ. 1  ) THEN
!
! ---------------- Selction of one source
!
                   CALL CLRCH ( MES )
                   MES = 'Source select'
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL SELSOU ( MES, DBOBJ, 1, LSEL_SOU, ISR_LAST, ISR, IER )
                   IF ( ISR .GT. 0 ) ISR_LAST = ISR
                ELSE IF ( IACT_PLOT .EQ.  2 ) THEN
!
! ---------------- Selction of severeal sources
!
                   CALL CLRCH ( MES )
                   MES = 'Multiple source select'
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL SELSOU ( MES, DBOBJ, 12, LSEL_SOU, ISR_LAST, ISR, IER )
                   IF ( ISR .GT. 0 ) ISR_LAST = ISR
                   ISR = 0
                   DO 460 J6=1,DBOBJ%L_SOU
                      IF ( LSEL_SOU(J6) ) THEN
                           ISR = ISR-1
                           ISS_SOU(-ISR) = DBOBJ%LIS_SOU(J6)
                      END IF
 460               CONTINUE
                ELSE IF ( IACT_PLOT .EQ.  3 ) THEN
              END IF
!
              IF ( IACT_PLOT .GT. 0 ) THEN
                   IF ( IPLFD_STA .EQ. -1 ) THEN
!
! --------------------- Fiducial station was not yet selected -- set default
! --------------------- values
!
                        IPLFD_STA    = IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, &
     &                                            DBOBJ%UIS_STA(1) )
                        IPLSTA_LAST  = IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, &
     &                                            DBOBJ%UIS_STA(2) )
                   END IF
!
! ---------------- Plotting the residuals
!
                   CALL CLRCH ( ADD_MES )
                   CALL ERR_PASS   ( IUER, IER )
                   CALL PAMB_SHOWRES ( ADD_MES, N_OBS, DBOBJ, %VAL(IADR_OBSSCA), &
     &                  %VAL(IADR_OBSBAS), %VAL(IADR_PAMBI), %VAL(IADR_RES), &
     &                  RST, IPLSTA_LAST, IPLFD_STA, IBL_LAST, ISR, ISS_SOU, &
     &                  ITRI_LAST, %VAL(IADR_URES), RSTU, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6224, IUER, 'PAMB', 'Error during '// &
     &                      'attempt to plot residuals of the solution while '// &
     &                      'database '//DBNAME(1:I_LEN(DBNAME))// &
     &                      ' was processing' )
                        CALL HIT_CONT ( %VAL(0), %VAL(0) )
                   END IF
                   GOTO 920
              END IF
         END IF
      END DO
 810  CONTINUE
      IF ( UNWEIGHT_UPDATE ) THEN
!
! -------- Saving observations unweight flag
!
           CALL ELIM_SAVE ( N_OBS, IDB2, IDBF, DBOBJ, %VAL(IADR_RES), &
     &                      %VAL(IADR_OBSBAS) )
!
! -------- Writing down updated covariance matrix and vector of extimates
!
           CALL ELIM_WRIEST ( B3DOBJ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6225, IUER, 'PAMB', 'Error during attempt '// &
     &              'to write down updated covarianve matrix and vector '// &
     &              'of the estiamtes for allocation full normal matrix '// &
     &              'while database '//B3DOBJ%DBNAME_MES//' was processing' )
                GOTO 840
           END IF
           WRITE ( 23, FMT='(A)' ) 'PAMB  Unweight flags are updated in '// &
     &                             'scratch area'
           UNWEIGHT_UPDATE = .FALSE.
      END IF
      IF ( AMBIG_UPDATE ) THEN
           CALL PRCH ( 'Phase delay ambiguities for some observations '// &
     &                 'have been changed' )
           CALL PRCH ( CHAR(13)//CHAR(10) )
           CALL PRCH ( 'Are you really going to discard these changes '// &
     &                 ' (Y/N) [Y]  ? '//CHAR(1) )
!
! -------- Awaiting for the user's reply
!
           CALL INSIM ( ASIM, ISIM )
           CALL TRAN  ( 11, ASIM, ASIM )
           IF ( ASIM .EQ. 'N' ) THEN
                F_OPTIN = .FALSE.
                GOTO 910
           END IF
      END IF
!
 840  CONTINUE
      FAST_COV = FAST_COV__SAVE
      CALL USE_GLBFIL_4 ( 'OWC' )
!
! --- Close some files
!
      CALL PAMB_CLOSE ( IDB2 )
!
! --- ... And freeing dynamic memory
!
      IF ( ML_OBSER  .GT. 0 ) THEN
           CALL FREE_MEM ( MA_OBSER  )
           ML_OBSER = 0
      END IF
      IF ( ML_UOBSER .GT. 0 ) THEN
           CALL FREE_MEM ( MA_UOBSER )
           ML_UOBSER = 0
      END IF
!
      IF ( EQUMEM_INIT_WAS ) THEN
           CALL ERR_PASS   ( IUER, IER )
           CALL EQUMEM_END ( EQUMEM, IER )
           IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 6626, IUER, 'PAMB', 'Error during '// &
     &               'attempt to free dynamic memory occupied by EQUMEM '// &
     &               'data strucutre' )
                 CALL HIT_CONT ( %VAL(0), %VAL(0) )
           END IF
      END IF
!
! --- Good bye!
!
      IF ( .NOT. F_OPTIN ) CALL HIT_CONT ( %VAL(0), %VAL(0) )
      CALL END_PROG()
      END  SUBROUTINE  PAMB  !#!#
