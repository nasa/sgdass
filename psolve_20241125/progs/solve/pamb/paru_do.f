      SUBROUTINE PARU_DO ( PARU_FIL, GVH, PAMB_VER, INC_VERS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PARU_DO
! *                                                                      *
! *  ### 04-JUN-2007     PARU_DO   v1.0 (c)  L. Petrov  04-JUN-2007 ###  *
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
      CHARACTER  PARU_FIL*(*)
      TYPE ( PAR__STRU     ) :: PAR
      TYPE ( CALS_STRU     ) :: CALS
      TYPE ( PLACE__STRU   ) :: PLACE
      TYPE ( B3D__STRU     ) :: B3DOBJ
      TYPE ( B1B3D__STRU   ) :: B1B3DOBJ
      TYPE ( HLD_O__STRU   ) :: OBSHLD
      TYPE ( NCREC__STRU   ) :: NCREC
      TYPE ( DBOBJ_O__STRU ) :: DBOBJ
      TYPE ( CHIACC__STRU  ) :: CHIOBJ
      TYPE ( RST_O__STRU   ) :: RST, RSTU
      TYPE ( SCAINF__STRU  ) :: SCAINF
      TYPE ( EQUMEM__STRU  ) :: EQUMEM
      TYPE ( GVH__STRU     ) :: GVH
      INTEGER*4  INC_VERS, IUER
      CHARACTER  USER_NAME*80, USER_REALNAME*80, USER_E_ADDRESS*80, &
     &           PAMB_NO_FAST*32
      INTEGER*8  ML_OBSER, ML_UOBSER
      ADDRESS__TYPE :: MA_OBSER,  MA_UOBSER
      ADDRESS__TYPE :: IADR_OBSSCA, IADR_OBSSTA, IADR_OBSBAS, IADR_RES, &
     &                 IADR_URES,   IADR_PAMBI, IADR_OBSAOB
      INTEGER*4  J1, J2, J3, J4, J5, J6, IP
      INTEGER*4  PAMB_VER, IACT, IACT_FREEZE, IACT_PLOT, IER, IDBF, &
     &           N_OBS, ISIM, IOBS_USED, ISR, ISR_LAST, ISS_SOU(MO_SOU), &
     &           IBL_LAST, ITRI_LAST(3), IPLSTA_LAST, IPLFD_STA
      INTEGER*4  ELIM_VRB__SAVE, FAST_COV__SAVE
      LOGICAL*2  K_MN
      INTEGER*2  LDBNAM(5,15), IDBV(15), IDB2, IDATYP_OLD, SUPMET_SAVE
      INTEGER*4  IDBE(15), KAMB, KAMB_S, KAMB_X, IOBS_SPAC, DB_NUM
      CHARACTER  CDBNAM(15)*10, DBNAME*16, STR*128, STR1*128, VER_PAMB*54
      LOGICAL*4  F_OPTIN, UNWEIGHT_UPDATE, AMBIG_UPDATE, LSEL_SOU(MO_SOU), &
     &           F_AMB, F_SUP, F_NWT, EQUMEM_INIT_WAS, FL_BATCH, &
     &           FL_SOL_BYPASS 
      REAL*8     INIT_WEI
      INTEGER*4  L_STA, L_SOU, MJD_BEG, MJD_END
      CHARACTER  C_STA(MAX_ARC_STA)*8, C_SOU(MAX_ARC_SRC)*8
      REAL*8     TAI_BEG, TAI_END
      CHARACTER, EXTERNAL :: GET_VERSION*54, GET_CDATE*19
      LOGICAL*2, EXTERNAL :: KBIT
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IFIND_PL
!
      INCLUDE 'pamb_version.i' ! Set revision date of the current version
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
      ELIM_AMB = .FALSE.
      ELIM_ION = .FALSE.
      DBOBJ%STATUS    = DBOBJ__UNF
      DBOBJ%F_AMB     = ELIM_AMB 
      DBOBJ%F_ION     = ELIM_ION 
      DBOBJ%F_AMB_CHANGED = .FALSE.
      IUER            = -1
      F_OPTIN         = .FALSE.
      UNWEIGHT_UPDATE = .FALSE.
      AMBIG_UPDATE    = .FALSE.
!
      F_AMB    = .TRUE.
      F_SUP    = .TRUE.
      F_NWT    = .TRUE.
      INIT_WEI = PAMB_INIWEI
      INC_VERS = 0
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
!
      CALL CHIN ( DBNAME_CH(1:8), DB_NUM )
      IF ( DB_NUM > 19700101 .AND. DB_NUM < 20700001 ) THEN
           CONTINUE 
         ELSE 
           IF ( DBNAME_CH(9:9) .NE. 'X'  .AND.  DBNAME_CH(9:10) .NE. 'DX'  ) THEN
                CALL ERR_LOG ( 6202, IUER, 'PARU_DO', 'Only database for '// &
     &              'X-band may be used by PAMB: '//DBNAME_CH// &
     &              ' is not good database' )
                RETURN 
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
      FL_SOL_BYPASS = .FALSE.
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
                FL_SOL_BYPASS = .TRUE.
                CALL GETENVAR ( 'PARU_NO_FAST', STR1 )
                IF ( STR1 .NE. 'YES' ) THEN
                     CALL ERR_LOG ( 6203, IUER, 'PARU_DO', 'FAST mode "'// &
     &                    STR(1:I_LEN(STR))// &
     &                    '" is not supported by PAMB. Only B3D mode '// &
     &                    'is supported. Please change the fast mode. '// &
     &                    'You may also try to use kludge environment '// &
     &                    'variable PARU_NO_FAST if you need to bypass '// &
     &                    'normal matrix computation' )
                     RETURN 
                END IF
           END IF
      END IF
!
! --- Reading calibration status
!
      IDB2 = 1
      IDBF = 1
      N_OBS = IDBE(1)
      CALL ERR_PASS ( IUER, IER )
      CALL CALS_R   ( IDB2, 0, 0, CALS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6204, IUER, 'PARU_DO', 'Error in reading '// &
     &         'calibration status from database '//DBNAME_CH )
           RETURN 
      END IF
!
! --- Gtabbing additional dynamic memory for plotting
!
      CALL ERR_PASS ( IUER, IER )
      CALL GRAB_MEM ( IER,  ML_UOBSER,                MA_UOBSER,  1, &
     &                      INT8(ML_RES)*INT8(N_OBS), IADR_URES      )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6205, IUER, 'PARU_DO', 'Error during grabbing '// &
     &         'memory for internal data structures for phase delay '// &
     &         'ambiguity resolution algorithm' )
           RETURN 
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
      DBNAME = DBNAME_CH
!
      IF ( PAMB_VER .GE. 1 ) THEN
           WRITE (  6, '(A,A,A,I3,A)' ) ' Started processing database ', &
     &                                  DBNAME_CH, '  version ', DBNAME_VER, &
     &                                  ' on '//GET_CDATE()
           WRITE ( 23, '(A,A,A,I3,A)' ) ' Started processing database ', &
     &                                  DBNAME_CH, '  version ', DBNAME_VER, &
     &                                  ' on '//GET_CDATE()
      END IF
!
! --- It was the first call of PAMB routines. Making preliminary
! --- solution. Grabbing dynamic memory for internal data structure
!
      IF ( SUPMET == SUPMET__META  .AND. .NOT. META_SUP ) THEN
!
! -------- Initialize AUTO_SUP flag
!
           IF ( PAMB_VER .GE. 1 ) THEN
                WRITE (  6, * ) '     Initialize AUTO_SUP bits for database ', DBNAME
                WRITE ( 23, * ) '     Initialize AUTO_SUP bits for database ', DBNAME
           END IF
           SUPMET_SAVE = SUPMET
           SUPMET = SUPMET__SNGBA
           CALL INIT_META_SUP ( )
           SUPMET = SUPMET_SAVE
      END IF
!
! --- Make initial solution
!
      CALL ERR_PASS ( IUER, IER )
      CALL PAMB_SOL ( PAMB_VER, DBNAME, IDB2, IDBF, N_OBS, ML_OBSER, &
     &     MA_OBSER, IADR_OBSSCA, IADR_OBSSTA, IADR_OBSBAS, IADR_RES, &
     &     IADR_PAMBI, IADR_OBSAOB, OBSHLD, DBOBJ, PLACE, B3DOBJ, &
     &     B1B3DOBJ, NCREC, RST, CHIOBJ, EQUMEM, EQUMEM_INIT_WAS, &
     &     FL_SOL_BYPASS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6206, IUER, 'PARU_DO', 'Error during '// &
     &         'attempt to produce solution while database '//DBNAME_CH// &
     &         ' was processing' )
           RETURN 
      END IF
!
! --- Putting initial information in fields of PAMBI data strucutre
!
      CALL ERR_PASS   ( IUER, IER )
      CALL PAMB_MARES ( 0, N_OBS, DBOBJ, %VAL(IADR_OBSSCA), &
     &                  %VAL(IADR_OBSBAS), %VAL(IADR_RES), &
     &                  %VAL(IADR_OBSAOB), %VAL(IADR_PAMBI), KAMB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6207, IUER, 'PAMB', 'Error during making '// &
     &          'phase delay residuals while database '//DBNAME// &
     &          ' was processing' )
           RETURN 
      END IF
!
!@      CALL ERR_PASS ( IUER, IER )
!@      CALL VTD_LOAD ( %VAL(VTD_ADR), L_STA, C_STA, L_SOU, C_SOU, &
!@     &                MJD_BEG, TAI_BEG, MJD_END, TAI_END, IER )
!@      IF ( IER .NE. 0 ) THEN
!@           CALL ERR_LOG ( 9041, IUER, 'PARU_DO', 'Error in '// &
!@     &         'an attempt to load files with auxiliary information '// &
!@     &         'need for GETDB' )
!@           RETURN 
!@      END IF
!
! --- Compiling and executing PARU actions
!
      CALL ERR_PASS ( IUER, IER )
      CALL PARU_MAIN ( VER_PAMB, PAMB_VER, .TRUE., N_OBS, IDBF, IDB2, &
     &          ML_OBSER, MA_OBSER, &
     &          DBOBJ, NCREC, OBSHLD, %VAL(IADR_OBSSCA), %VAL(IADR_OBSSTA), &
     &          %VAL(IADR_OBSBAS), %VAL(IADR_RES), %VAL(IADR_PAMBI), &
     &          PLACE, B3DOBJ, B1B3DOBJ, RST, CHIOBJ, SCAINF, EQUMEM, &
     &          PARU_FIL, INC_VERS, GVH, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6208, IUER, 'PARU_DO', 'Error during attempt '// &
     &         'to process database '//DBNAME_CH )
           CALL FREE ( MA_UOBSER )
           RETURN 
      END IF
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
                CALL ERR_LOG ( 6207, IUER, 'PARU_DO', 'Error during '// &
     &              'attempt to free dynamic memory occupied by EQUMEM '// &
     &              'data strucutre' )
                RETURN 
           END IF
      END IF
!
      IF ( B3DOBJ%MEM_STAT .EQ. F__MSL  .OR. &
     &     B3DOBJ%MEM_STAT .EQ. F__MFL        ) THEN
!
! -------- Freeing dynamic memory if it was previously allocated
!
           CALL ERR_PASS ( IUER, IER )
           CALL B3D_FREEMEM ( B3DOBJ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6208, IUER, 'PARU_DO', 'Error during '// &
     &              'attempt to free dynamic memory occupied by B3DOBJ'// &
     &              'data strucutre' )
                RETURN 
           END IF
         ELSE
           B3DOBJ%MEM_STAT   = F__MFR
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE   PARU_DO  !#!#
