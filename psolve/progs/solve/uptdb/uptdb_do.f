      SUBROUTINE UPTDB_DO ( GVH, VCAT, OPCODE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  UPTDB_DO 
! *                                                                      *
! *  ### 30-NOV-2005   UPTDB_DO    v1.9  (c)  L. Petrov  25-MAR-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'erm.i'
      INCLUDE   'socom.i'
      INCLUDE   'socom_plus.i'
      INCLUDE   'gvh.i'
      INCLUDE   'vcat.i'
      INCLUDE   'vtd.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'glbc2.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'prfil.i'
      INCLUDE   'precm.i'
      INCLUDE   'oborg.i'
      INTEGER*4  IUER
      TYPE     ( GVH__STRU  ) :: GVH
      TYPE     ( VCAT__TYPE ) :: VCAT
      INTEGER*4  OPCODE
      INTEGER*4, ALLOCATABLE :: OBS_TAB(:,:), CAL_STS(:,:)
      CHARACTER  GVF_DB_DIR*128, GVF_ENV_DIR*128
      CHARACTER  STR*128, C_STA(MAX_ARC_STA)*8, TH_PROG*64, TH_RUDAT*26, HIST_FIL*128
      CHARACTER  STA_CLRF(MAX_ARC_STA)*8, STA_CLBR(MAX4_BRK)*8, CAL_NAMES(M__CAL)*8
      INTEGER*2  IERR_I2
      INTEGER*4  M__HIS
      PARAMETER  ( M__HIS = 8192 )
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, NOBS_STA(MAX_ARC_STA), &
     &           N_BAS, NUM_CLBR, NUM_CLRF, L_ACM, BAND_2ND, N_GRAMB(2), &
     &           N_PHAMB(2), MJD_EOP_TAB, IP, CAL_ION_STS, CAL_AVL_STS, &
                 CAL_APL_STS, L_CAL, TEC_STS_STA(MAX_ARC_STA), &
     &           MJD_CLBR(MAX4_BRK), NUM_BAS_RW_OLD, CLASS, TYP, NUM_FIELDS, &
     &           SEG_IND, LEN_REC, LEN_DATA, DIMS(2), IND_REP, &
     &           LAST_SCA_STA(MAX_ARC_STA), VERS, NLIN, NCHP, IND_BEG, L_LIN, &
     &           OPCODE_USE, IER
      INTEGER*2  IER2
      CHARACTER  RW_BAS_NAM_OLD(2,MAX_ARC_BSL)*8, STR_VERS*54, BUF(M__HIS)*256, TITLE*128
      CHARACTER  USER_NAME*128, USER_REALNAME*128, USER_E_ADDRESS*128, WORK_DIR*128
      REAL*8,    ALLOCATABLE :: RW_DEL_OLD(:,:), RW_RAT_OLD(:,:)
      ADDRESS__TYPE  ADR_DATA
      INTEGER*4  SEG_TH1, SEG_SL1
      INTEGER*4    VTD__NDER_USED
      PARAMETER  ( VTD__NDER_USED = 17 )
      REAL*8     EFF_FREQ_ARR(3,2), EOP_TAB(MAX_EROT_VALUES,3), &
     &           EOP_VAL(3,2), NUT_DER(2), UTC_CLBR(MAX4_BRK)
      REAL*8     TAU_GR, TAU_PH, RATE_PH, DER_DEL(VTD__NDER), &
     &           DER_RAT(VTD__NDER), TAI_EOP_TAB 
      LOGICAL*1  LEX
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      CHARACTER, EXTERNAL :: GET_TZ_CDATE*26
      LOGICAL*2, EXTERNAL :: KBIT
      LOGICAL*4, EXTERNAL :: CHECK_STABIT
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
      CHARACTER, EXTERNAL :: GET_VERSION*54
!
      SEG_TH1 = 2 
      SEG_SL1 = 3
      STR_VERS = GET_VERSION()
!
! --- Check for the repository name
!
      IND_REP = LTM_DIF ( 0, VCAT%NREPS, VCAT%GVF_REP_NAME, VCAT_REPO )
      IF ( IND_REP < 1 ) THEN
           CALL ERR_LOG ( 6311, IUER, 'UPTDB_DO', 'VCAT repository name '// &
     &          TRIM(VCAT_REPO)//' defined in the environment variable '// &
     &          'VCAT_REPO is not defined in VCAT control file '// &
     &          VCAT%CONF_FILE )
           RETURN 
      END IF
      GVF_ENV_DIR = VCAT%GVF_ENV_DIR(IND_REP)
      GVF_DB_DIR  = VCAT%GVF_DB_DIR(IND_REP)
!
      IF ( ILEN(ENV_FINAM) == 0 ) THEN
           CALL ERR_LOG ( 6312, IUER, 'UPTDB_DO', 'No envelop file '// &
     &                         'is avialable' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_INIT ( GVH,  IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6313, IUER, 'UPTDB_DO', 'Failure to initialize GVH' )
           RETURN 
      END IF
      GVH%NSEG = MAX(SEG_TH1, SEG_SL1, 1)
!
      ALLOCATE ( OBS_TAB(3,NUMOBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*3*NUMOBS, STR )
           CALL ERR_LOG ( 6314, IUER, 'UPTDB_DO', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for OBS_TAB' )
           RETURN 
      END IF
!
! --- Make the first pass through oborg in order to build OBS_TAB table
!
      CALL NOUT_I4 ( INT4(MAX_ARC_STA), NOBS_STA     )
      CALL NOUT_I4 ( INT4(MAX_ARC_STA), LAST_SCA_STA )
      CALL ACS_OBSFIL ( 'O' )
      DO 410 J1=1,NUMOBS
         CALL USE_OBSFIL ( IOBSFIL, J1, 'R' )
         IF ( LAST_SCA_STA(ISITE(1)) .NE. NSCA ) THEN
              NOBS_STA(ISITE(1)) = NOBS_STA(ISITE(1)) + 1
              LAST_SCA_STA(ISITE(1)) = NSCA
         END IF
         IF ( LAST_SCA_STA(ISITE(2)) .NE. NSCA ) THEN
              NOBS_STA(ISITE(2)) = NOBS_STA(ISITE(2)) + 1
              LAST_SCA_STA(ISITE(2)) = NSCA
         END IF
         OBS_TAB(1,J1) = NSCA
         OBS_TAB(2,J1) = ISITE(1)
         OBS_TAB(3,J1) = ISITE(2)
410  CONTINUE 
     CALL ACS_OBSFIL ( 'C' )
!
! --- Check stations, Build array C_STA with repaired station names
!
      CALL GETCARD ( INT2(1), 'CLCT', INT2(1), STR, IERR_I2 )
      CALL CHIN ( STR(6:8), L_CAL )
!
      ALLOCATE ( CAL_STS(NUMSTA,L_CAL) )
      CALL NOUT_I4 ( L_CAL*INT4(NUMSTA), CAL_STS )
!
      NUM_CLBR = 0
      NUM_CLRF = 0
      DO 420 J2=1,NUMSTA
         C_STA(J2) = ISITN_CHR(J2)
         CALL VTD_NAME_REPAIR ( C_STA(J2) )
         IF ( CHECK_STABIT ( INT2(J2) ) ) THEN
              IF ( NUM_BRK(J2) > 0 ) THEN
                   DO 430 J3=1,NUM_BRK(J2)
                      NUM_CLBR = NUM_CLBR + 1
                      STA_CLBR(NUM_CLBR) = C_STA(J2)
                      CALL JD_TO_MJD_SEC ( JDATE_BRK(J3,J2),   &
     &                                     MJD_CLBR(NUM_CLBR), &
     &                                     UTC_CLBR(NUM_CLBR)  )
 430               CONTINUE 
              END IF
              IF ( .NOT. KBIT ( ICLSTA(1,1),INT2(J2)) ) THEN
                   NUM_CLRF = NUM_CLRF + 1
                   STA_CLRF(NUM_CLRF) = C_STA(J2)
              ENDIF
         END IF
!
         TEC_STS_STA(J2) = 0
         IF ( L_CAL > 0 ) THEN
!
! ----------- Read calibration status from NAMFIL
!
              IF ( J2 == 1 ) THEN
                   IP = 1
                ELSE
                   IP = 0
              END IF
              CALL GETCARD ( INT2(1), 'CALS', INT2(IP), STR, IERR_I2 )
              CALL CHIN ( STR(17:21), CAL_ION_STS )
              CALL CHIN ( STR(25:28), CAL_AVL_STS )
              CALL CHIN ( STR(32:35), CAL_APL_STS )
!
! ----------- And according to this built_fields set up bits of CAL_STS
!
              IF ( KBIT ( CAL_ION_STS, INT2(1) ) ) THEN
                   TEC_STS_STA(J2) = IBSET ( TEC_STS_STA(J2), 0 ) 
              END IF
              IF ( KBIT ( CAL_ION_STS, INT2(4) ) ) THEN
                   TEC_STS_STA(J2) = IBSET ( TEC_STS_STA(J2), 1 ) 
              END IF
!
              DO 440 J4=1,L_CAL
                 IF ( KBIT ( CAL_APL_STS, INT2(J4) ) ) THEN
                      DO 450 J5=FIRST__DTP,LAST__DTP
                         CAL_STS(J2,J4) = IBSET ( CAL_STS(J2,J4), J5 )
 450                  CONTINUE 
                 END IF
 440          CONTINUE 
         END IF
 420  CONTINUE 
      IF ( NUM_CLRF .EQ. 0 ) THEN
!
! -------- No clock reference station was found. Strange. But what to do?
! -------- Let's set the first non-deselected station as a reference. 
! -------- It is better then abnormal termination, isn't it?
!
           DO 460 J6=1,NUMSTA
              IF ( CHECK_STABIT( INT2(J6) ) ) THEN
                   NUM_CLRF = 1
                   STA_CLRF(NUM_CLRF) = C_STA(J6)
                   GOTO 860
              END IF
 460      CONTINUE 
 860      CONTINUE 
      END IF
!
      IF ( L_CAL > 0 ) THEN
!
! -------- Remove the user calibration if it is hte last one
!
           CALL GET_CLN_CARD ( INT2(1), 'CALN', INT2(L_CAL), CAL_NAMES, IERR_I2 )
           IF ( CAL_NAMES(L_CAL) == 'user cal' ) L_CAL = L_CAL - 1
      END IF
!
      N_BAS = (INT4(NUMSTA)*(INT4(NUMSTA)-1))/2
!
      CALL GETCARD ( INT2(1), 'ACM ', INT2(1), STR, IERR_I2 )
      CALL CHIN ( STR(8:10), L_ACM )
      IF ( L_ACM < 0 .OR. L_ACM > M_ACM ) L_ACM = 0
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL UPTDB_PTOC ( GVH, SEG_TH1, SEG_SL1, INT4(NUMSTA), INT4(NUMSTR), &
     &                  N_BAS, NUM_CLRF, NUM_CLBR, L_ACM, L_CAL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6315, IUER, 'UPTDB_DO', 'Error in an attempt to '// &
     &                   'create the table of contents' ) 
           DEALLOCATE ( CAL_STS )
           RETURN 
      END IF
!
! === End of table of contents definitions
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_PREPUT ( GVH, NUMOBS, NUMSCA, INT4(NUMSTA), NOBS_STA, C_STA, &
     &                  OBS_TAB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6316, IUER, 'UPTDB_DO', 'Error in an attempt to '// &
     &                   'insert mandatory lcodes an initialize cache' )
           DEALLOCATE ( CAL_STS )
           RETURN 
      END IF
!
      IF ( SEG_TH1 > 0 ) THEN
           CALL CLRCH ( TH_PROG  )
           CALL CLRCH ( TH_RUDAT )
           IF ( CALCV .GT. 0.0D0 ) THEN
                CALL CLRCH ( TH_PROG )
                TH_PROG = 'Calc '
                WRITE ( UNIT=TH_PROG(6:10), FMT='(F5.2)' ) CALCV
             ELSE 
                TH_PROG = VTD__LABEL
           END IF
!
           TH_RUDAT = GET_TZ_CDATE()
!
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_PLCODE ( GVH, 'TH_PROG ', 1, 1, TH_PROG, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6317, IUER, 'UPTDB_DO', 'Error in '// &
     &              'putting "TH_PROG " lcode' )
                DEALLOCATE ( CAL_STS )
                RETURN 
           END IF
!
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_PLCODE ( GVH, 'TH_RUDAT', 1, 1, TH_RUDAT, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6318, IUER, 'UPTDB_DO', 'Error in '// &
     &              'putting "TH_RUDAT " lcode' )
                DEALLOCATE ( CAL_STS )
                RETURN 
           END IF
!
           CALL JD_TO_MJD_SEC ( UT1INB(1), MJD_EOP_TAB, TAI_EOP_TAB )
           DO 480 J8=1,MAX_EROT_VALUES
              EOP_TAB(J8,1) = WOBYYB(J8)/MAS__TO__RAD
              EOP_TAB(J8,2) = WOBXXB(J8)/MAS__TO__RAD
              EOP_TAB(J8,3) = UT1PTB(J8)*UT1__TO__E3
 480       CONTINUE 
!
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_PLCODE ( GVH, 'N_APREOP', 1, 1, INT4(MAX_EROT_VALUES), IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6319, IUER, 'UPTDB_DO', 'Error in '// &
     &              'putting "N_APREOP" lcode' )
                DEALLOCATE ( CAL_STS )
                RETURN 
           END IF
!
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_PLCODE ( GVH, 'MJD_EOP ', 1, 1, MJD_EOP_TAB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6320, IUER, 'UPTDB_DO', 'Error in '// &
     &              'putting "MJD_EOP " lcode' )
                DEALLOCATE ( CAL_STS )
                RETURN 
           END IF
!
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_PLCODE ( GVH, 'TAI_EOP ', 1, 1, TAI_EOP_TAB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6321, IUER, 'UPTDB_DO', 'Error in '// &
     &              'putting "TAI_EOP " lcode' )
                DEALLOCATE ( CAL_STS )
                RETURN 
           END IF
!
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_PLCODE ( GVH, 'STEP_EOP', 1, 1, UT1INB(2)*86400.0D0, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6322, IUER, 'UPTDB_DO', 'Error in '// &
     &              'putting "STEP_EOP" lcode' )
                DEALLOCATE ( CAL_STS )
                RETURN 
           END IF
!
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_PLCODE ( GVH, 'EOP_TAB ', 1, 1, EOP_TAB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6323, IUER, 'UPTDB_DO', 'Error in '// &
     &              'putting "EOP_TAB " lcode' )
                DEALLOCATE ( CAL_STS )
                RETURN 
           END IF
      END IF
!
! --- Get the name of the history file
!
      CALL GETENVAR ( 'PSOLVE_WORK_DIR', WORK_DIR )
      IF ( ILEN(WORK_DIR) == 0 ) WORK_DIR = SOLVE_WORK_DIR
      HIST_FIL = TRIM(WORK_DIR)//'/'//'HIST'//PRE_LETRS
!
! --- Check whether it exists. Read it if exists
!
      INQUIRE ( FILE=HIST_FIL, EXIST=LEX ) 
      IF ( LEX ) THEN
           CALL RD_TEXT ( HIST_FIL, M__HIS, BUF, NLIN, IER )
           READ ( UNIT=BUF(1)(19:23), FMT='(I5)', IOSTAT=IER ) NCHP
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6324, IUER, 'UPTDB_DO', 'Error in parsing '// &
     &              'the first line of the history file '//HIST_FIL )
                RETURN 
           END IF
           IND_BEG = 0
           L_LIN   = 0
           DO 490 J9=2,NLIN
              IF ( BUF(J9)(1:12) == '  @@chapter:' ) THEN
                   IF ( IND_BEG > 0 ) THEN
                        CALL ERR_PASS ( IUER, IER ) 
                        CALL GVH_PTEXT_CHP ( GVH, SEG_SL1, %REF(TRIM(TITLE)), L_LIN, BUF(IND_BEG), IER )
                        IF ( IER .NE. 0 ) THEN
                             CALL ERR_LOG ( 6325, IUER, 'UPTDB_DO', 'Error in an '// &
     &                           'attempt to put history section, version 1, to the GVH file' )
                             RETURN 
                        END IF
                   END IF
                   IND_BEG = J9+1
                   IP = INDEX ( BUF(J9), ',' ) 
                   TITLE = BUF(J9)(IP+2:)
                   L_LIN = 0
                 ELSE
                   L_LIN = L_LIN + 1
              END IF 
 490       CONTINUE 
!
           CALL ERR_PASS ( IUER, IER ) 
           CALL GVH_PTEXT_CHP ( GVH, SEG_SL1, TRIM(TITLE), L_LIN, BUF(IND_BEG), IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6326, IUER, 'UPTDB_DO', 'Error in an '// &
     &              'attempt to put history section to the GVH file' )
                RETURN 
           END IF
      END IF
!
      BUF(1) = 'The database has been updated by pSolve routine uptdb'
      BUF(2) = STR_VERS
      IF ( OPCODE == 1 .OR. ( OPCODE == 3 .AND. DBNAME_VER > 1 ) ) THEN
           CALL INCH ( DBNAME_VER, STR )
           BUF(3) = 'The old version '//TRIM(STR)//' of '//TRIM(DBNAME_CH)//' database has been updated'
           OPCODE_USE = 1
         ELSE IF ( OPCODE == 2 .OR. ( OPCODE == 3 .AND. DBNAME_VER == 1 ) ) THEN
           CALL INCH ( DBNAME_VER+1, STR )
           BUF(3) = 'The new version '//TRIM(STR)//' of '//TRIM(DBNAME_CH)//' database has been created'
           OPCODE_USE = 2
      END IF
      CALL CLRCH ( BUF(4) )
      CALL GETINFO_USER ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
      BUF(4) = 'Analyst: '//TRIM(USER_REALNAME)
      BUF(5) = 'Update date '//GET_TZ_CDATE()
      NLIN = 5
      TITLE = 'Comments on update of the geo VLBI database '//DBNAME_CH
!      
      CALL ERR_PASS ( IUER, IER ) 
      CALL GVH_PTEXT_CHP ( GVH, SEG_SL1, TRIM(TITLE), NLIN, BUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6327, IUER, 'UPTDB_DO', 'Error in an '// &
     &         'attempt to put history section to the GVH file' )
           RETURN 
      END IF
!
      IF ( SEG_SL1 > 0 ) THEN
           CALL ERR_PASS   ( IUER, IER )
           CALL UPTDB_SOLSETUP ( GVH, N_BAS, NUM_CLRF, STA_CLRF, NUM_CLBR, &
     &                           STA_CLBR, MJD_CLBR, UTC_CLBR, L_ACM, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6328, IUER, 'UPTDB_DO', 'Error in an '// &
     &              'attempt to put in the database solution setup' )
                DEALLOCATE ( CAL_STS )
                RETURN 
           END IF
!
           EDIT_STS = 0
           EDIT_STS = IBSET ( EDIT_STS, GRPONL__DTP )
           EDIT_STS = IBSET ( EDIT_STS, G_GXS__DTP  )
           EDIT_STS = IBSET ( EDIT_STS, GX__DTP     )
!
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_PLCODE ( GVH, 'EDIT_STS', 1, 1, EDIT_STS, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6329, IUER, 'UPTDB_DO', 'Error in '// &
     &               'putting "EDIT_STS" lcode' )
                DEALLOCATE ( CAL_STS )
                RETURN 
           END IF
!
           IF ( L_CAL > 0 ) THEN
                CALL ERR_PASS   ( IUER, IER )
                CALL GVH_PLCODE ( GVH, 'CAL_STS ', 1, 0, CAL_STS, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 6331, IUER, 'UPTDB_DO', 'Error in '// &
     &                   'putting "CAL_STS " lcode' )
                     DEALLOCATE ( CAL_STS )
                     RETURN 
                END IF
           END IF
!            
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_PLCODE ( GVH, 'TEC_STS ', 0, 0, TEC_STS_STA, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6332, IUER, 'UPTDB_DO', 'Error in '// &
     &              'putting "TEC_STS " lcode' )
                DEALLOCATE ( CAL_STS )
                RETURN 
           END IF
!            
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_PLCODE ( GVH, 'DB_VERS ', 0, 0, DBNAME_VER, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6333, IUER, 'UPTDB_DO', 'Error in '// &
     &              'putting "DB_VERS " lcode' )
                DEALLOCATE ( CAL_STS )
                RETURN 
           END IF
      END IF
      DEALLOCATE ( CAL_STS )
!
      CALL ACS_OBSFIL ( 'O' )
      DO 4100 J10=1,NUMOBS
         CALL USE_OBSFIL ( IOBSFIL, J10, 'R' )
!
         BAND_2ND = 0
         BAND_2ND = IBSET ( BAND_2ND, OBS__2BN )
         IF ( KBIT ( ICORR, INT2(4) ) ) THEN
              BAND_2ND = IBSET ( BAND_2ND, OBS__2BN )
         END IF
!
         BAND_2ND = IBSET ( BAND_2ND, AVL__2BN )
         IF ( LQUAL_S_CHR == ' 1' .OR. &
     &        LQUAL_S_CHR == ' 2' .OR. &
     &        LQUAL_S_CHR == ' 3' .OR. &
     &        LQUAL_S_CHR == ' 4' .OR. &
     &        LQUAL_S_CHR == ' 5' .OR. &
     &        LQUAL_S_CHR == ' 6' .OR. &
     &        LQUAL_S_CHR == ' 7' .OR. &
     &        LQUAL_S_CHR == ' 8' .OR. &
     &        LQUAL_S_CHR == ' 9'      ) THEN
!
              BAND_2ND = IBSET ( BAND_2ND, DET__2BN )
         END IF
!
         CALL NOUT_R8 ( 6, EFF_FREQ_ARR )
         EFF_FREQ_ARR(1,1) = EFFREQ  *1.D6   
         EFF_FREQ_ARR(2,1) = PHEFFREQ*1.D6
         EFF_FREQ_ARR(3,1) = REFFREQ *1.D6
         EFF_FREQ_ARR(1,2) = EFFREQ_S  *1.D6   
         EFF_FREQ_ARR(2,2) = PHEFFREQ_S*1.D6
         EFF_FREQ_ARR(3,2) = REFFREQ_S *1.D6
!
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_PLCODE ( GVH, 'EFF_FREQ', J10, 0, EFF_FREQ_ARR, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 6334, IUER, 'UPTDB_DO', 'Error in '// &
     &            'putting "EFF_FREQ" lcode' )
              RETURN 
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_PLCODE ( GVH, 'UV_COOR ', J10, 0, UV_COOR, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 6335, IUER, 'UPTDB_DO', 'Error in '// &
     &            'putting "UV_COOR" lcode' )
              RETURN 
         END IF
!
         N_GRAMB(1) = NUMAMB
         N_GRAMB(2) = NUMAMB_S
!
         N_PHAMB(1) = NPHAM4  
         N_PHAMB(2) = NPHAM4_S
!
         IF ( SEG_SL1 > 0 ) THEN
              CALL AUTO_SUP_UPD ( ISITE, ISTAR, ELEV, AUTO_SUP )
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_PLCODE ( GVH, 'BAND_2ND', J10, 1, BAND_2ND, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6336, IUER, 'UPTDB_DO', 'Error in '// &
     &                 'putting "BAND_2ND" lcode' )
                   RETURN 
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL GVH_PLCODE ( GVH, 'N_GRAMB ', J10, 0, N_GRAMB, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6337, IUER, 'UPTDB_DO', 'Error in '// &
     &                 'putting "N_GRAMB " lcode' )
                   RETURN 
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL GVH_PLCODE ( GVH, 'N_PHAMB ', J10, 0, N_PHAMB, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6338, IUER, 'UPTDB_DO', 'Error in '// &
     &                 'putting "N_PHAMB " lcode' )
                   RETURN 
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_PLCODE ( GVH, 'AUTO_SUP', J10, 1, AUTO_SUP, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6339, IUER, 'UPTDB_DO', 'Error in '// &
     &                 'putting "AUTO_SUP" lcode' )
                   RETURN 
              END IF
!
              IF ( SUPMET .NE. SUPMET__META ) THEN
!
! ---------------- Set suppression flag in the compatibility mode
!
                   IF ( IUNW == 0 ) THEN
                        USER_SUP = IBCLR ( USER_SUP, SLV__MAX_SOLTYP-1 )
                        USER_SUP = IBCLR ( USER_SUP, INT4(GRPRAT__DTP) )
                        USER_SUP = IBCLR ( USER_SUP, INT4(GRPRAT__DTP) )
                        USER_SUP = IBCLR ( USER_SUP, INT4(SNBRAT__DTP) )
                        USER_SUP = IBCLR ( USER_SUP, INT4(GRPONL__DTP) )
                        USER_SUP = IBCLR ( USER_SUP, INT4(PHSONL__DTP) )
                        USER_SUP = IBCLR ( USER_SUP, INT4(SNBONL__DTP) )
                        USER_SUP = IBCLR ( USER_SUP, INT4(RATONL__DTP) )
                        USER_SUP = IBCLR ( USER_SUP, INT4( G_GXS__DTP) )
                        USER_SUP = IBCLR ( USER_SUP, INT4(    GX__DTP) )
                        USER_SUP = IBCLR ( USER_SUP, INT4(    GS__DTP) )
                        USER_SUP = IBCLR ( USER_SUP, INT4( SNG_X__DTP) )
                        USER_SUP = IBCLR ( USER_SUP, INT4( SNG_S__DTP) )
                      ELSE
                        USER_SUP = IBSET ( USER_SUP, SLV__MAX_SOLTYP-1 )
                        USER_SUP = IBSET ( USER_SUP, INT4(GRPRAT__DTP) )
                        USER_SUP = IBSET ( USER_SUP, INT4(GRPRAT__DTP) )
                        USER_SUP = IBSET ( USER_SUP, INT4(SNBRAT__DTP) )
                        USER_SUP = IBSET ( USER_SUP, INT4(GRPONL__DTP) )
                        USER_SUP = IBSET ( USER_SUP, INT4(PHSONL__DTP) )
                        USER_SUP = IBSET ( USER_SUP, INT4(SNBONL__DTP) )
                        USER_SUP = IBSET ( USER_SUP, INT4(RATONL__DTP) )
                        USER_SUP = IBSET ( USER_SUP, INT4( G_GXS__DTP) )
                        USER_SUP = IBSET ( USER_SUP, INT4(    GX__DTP) )
                        USER_SUP = IBSET ( USER_SUP, INT4(    GS__DTP) )
                        USER_SUP = IBSET ( USER_SUP, INT4( SNG_X__DTP) )
                        USER_SUP = IBSET ( USER_SUP, INT4( SNG_S__DTP) )
                   END IF
!
                   IF ( IUNWP == 0 ) THEN
                        USER_SUP = IBCLR ( USER_SUP, INT4(PHSRAT__DTP) )
                        USER_SUP = IBCLR ( USER_SUP, INT4(PX_GXS__DTP) )
                        USER_SUP = IBCLR ( USER_SUP, INT4(PS_GXS__DTP) )
                        USER_SUP = IBCLR ( USER_SUP, INT4( PX_GX__DTP) )
                        USER_SUP = IBCLR ( USER_SUP, INT4( PX_GS__DTP) )
                        USER_SUP = IBCLR ( USER_SUP, INT4( PS_GX__DTP) )
                        USER_SUP = IBCLR ( USER_SUP, INT4( PS_GS__DTP) )
                        USER_SUP = IBCLR ( USER_SUP, INT4( P_PXS__DTP) )
                        USER_SUP = IBCLR ( USER_SUP, INT4(    PX__DTP) )
                        USER_SUP = IBCLR ( USER_SUP, INT4(    PS__DTP) )
                      ELSE IF ( IUNWP .NE. 0 ) THEN
                        USER_SUP = IBSET ( USER_SUP, INT4(PHSRAT__DTP) )
                        USER_SUP = IBSET ( USER_SUP, INT4(PX_GXS__DTP) )
                        USER_SUP = IBSET ( USER_SUP, INT4(PS_GXS__DTP) )
                        USER_SUP = IBSET ( USER_SUP, INT4( PX_GX__DTP) )
                        USER_SUP = IBSET ( USER_SUP, INT4( PX_GS__DTP) )
                        USER_SUP = IBSET ( USER_SUP, INT4( PS_GX__DTP) )
                        USER_SUP = IBSET ( USER_SUP, INT4( PS_GS__DTP) )
                        USER_SUP = IBSET ( USER_SUP, INT4( P_PXS__DTP) )
                        USER_SUP = IBSET ( USER_SUP, INT4(    PX__DTP) )
                        USER_SUP = IBSET ( USER_SUP, INT4(    PS__DTP) )
                   END IF
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_PLCODE ( GVH, 'USER_SUP', J10, 1, USER_SUP, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6340, IUER, 'UPTDB_DO', 'Error in '// &
     &                 'putting "USER_SUP" lcode' )
                   RETURN 
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_PLCODE ( GVH, 'USER_REC', J10, 1, USER_REC, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6341, IUER, 'UPTDB_DO', 'Error in '// &
     &                 'putting "USER_REC" lcode' )
                   RETURN 
              END IF
         END IF
!
         TAU_GR = DT*1.D-6 - TAU_ACM
         TAU_PH = TAU_GR
         RATE_PH = RT - RATE_ACM 
         DER_DEL = 0.0D0
         DER_RAT = 0.0D0
!
         DER_DEL(VTD__DER_ST1X) = BP(1,1,1) 
         DER_DEL(VTD__DER_ST1Y) = BP(2,1,1) 
         DER_DEL(VTD__DER_ST1Z) = BP(3,1,1) 
         DER_DEL(VTD__DER_ST2X) = BP(1,2,1) 
         DER_DEL(VTD__DER_ST2Y) = BP(2,2,1) 
         DER_DEL(VTD__DER_ST2Z) = BP(3,2,1) 
         DER_DEL(VTD__DER_RA) = SP(1,1)   
         DER_DEL(VTD__DER_DL) = SP(2,1)   
         DER_DEL(VTD__DER_E1) = ROTP(2,1) 
         DER_DEL(VTD__DER_E2) = ROTP(1,1) 
         DER_DEL(VTD__DER_E3) = ROTP(3,1)/UT1__TO__E3
!
         DER_DEL(VTD__DER_AT1)  =  AP(1,1)   
         DER_DEL(VTD__DER_AT2)  =  AP(2,1)   
         DER_DEL(VTD__DER_ATN1) = -AGRAD_PART(1,1,1) 
         DER_DEL(VTD__DER_ATN2) =  AGRAD_PART(2,1,1) 
         DER_DEL(VTD__DER_ATE1) =  AGRAD_PART(1,2,1) 
         DER_DEL(VTD__DER_ATE2) =  AGRAD_PART(2,2,1) 
!!
         DER_RAT(VTD__DER_ST1X) = BP(1,1,2) 
         DER_RAT(VTD__DER_ST1Y) = BP(2,1,2)
         DER_RAT(VTD__DER_ST1Z) = BP(3,1,2)
         DER_RAT(VTD__DER_ST2X) = BP(1,2,2)
         DER_RAT(VTD__DER_ST2Y) = BP(2,2,2)
         DER_RAT(VTD__DER_ST2Z) = BP(3,2,2)
         DER_RAT(VTD__DER_RA) = SP(1,2)
         DER_RAT(VTD__DER_DL) = SP(2,2)   
         DER_RAT(VTD__DER_E1) = ROTP(2,2) 
         DER_RAT(VTD__DER_E2) = ROTP(1,2) 
         DER_RAT(VTD__DER_E3) = ROTP(3,2)/UT1__TO__E3
!
         DER_RAT(VTD__DER_AT1)  =  AP(1,2)   
         DER_RAT(VTD__DER_AT2)  =  AP(2,2)   
         DER_RAT(VTD__DER_ATN1) = -AGRAD_PART(1,1,2) 
         DER_RAT(VTD__DER_ATN2) =  AGRAD_PART(2,1,2) 
         DER_RAT(VTD__DER_ATE1) =  AGRAD_PART(1,2,2) 
         DER_RAT(VTD__DER_ATE2) =  AGRAD_PART(2,2,2) 
!
         EOP_VAL(1,1) = Y_POLE
         EOP_VAL(2,1) = X_POLE
         EOP_VAL(3,1) = UT1_M_TAI*UT1__TO__E3
         EOP_VAL(1,2) = YP_RATE 
         EOP_VAL(2,2) = XP_RATE
         EOP_VAL(3,2) = UT1_RATE*UT1__TO__E3
!
         NUT_DER(1) = NUTP(1,1) 
         NUT_DER(2) = NUTP(2,1) 
!
         IF ( SEG_TH1 > 0 ) THEN
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_PLCODE ( GVH, 'THGR_DEL', J10, 1, TAU_GR, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6342, IUER, 'UPTDB_DO', 'Error in '// &
     &                 'putting "THGR_DEL" lcode' )
                   RETURN 
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_PLCODE ( GVH, 'THPH_DEL', J10, 1, TAU_PH, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6343, IUER, 'UPTDB_DO', 'Error in '// &
     &                 'putting "THPH_DEL" lcode' )
                   RETURN 
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_PLCODE ( GVH, 'THPH_RAT', J10, 1, RATE_PH, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6344, IUER, 'UPTDB_DO', 'Error in '// &
     &                 'putting "THPH_RAT" lcode' )
                   RETURN 
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_PLCODE ( GVH, 'DER_DEL ', J10, 1, DER_DEL, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6345, IUER, 'UPTDB_DO', 'Error in '// &
     &                 'putting "DER_DEL " lcode' )
                   RETURN 
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_PLCODE ( GVH, 'DER_RAT ', J10, 1, DER_RAT, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6346, IUER, 'UPTDB_DO', 'Error in '// &
     &                 'putting "DER_RAT " lcode' )
                   RETURN 
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_PLCODE ( GVH, 'AZIMUTH ', J10, 1, AZ(1), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6347, IUER, 'UPTDB_DO', 'Error in '// &
     &                 'putting "AZIMUTH" lcode for the first station' )
                   RETURN 
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_PLCODE ( GVH, 'AZIMUTH ', J10, 2, AZ(2), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6348, IUER, 'UPTDB_DO', 'Error in '// &
     &                 'putting "AZIMUTH" lcode for the second station' )
                   RETURN 
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_PLCODE ( GVH, 'ELEV    ', J10, 1, ELEV(1), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6349, IUER, 'UPTDB_DO', 'Error in '// &
     &                 'putting "ELEV    " lcode for the first station' )
                   RETURN 
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_PLCODE ( GVH, 'ELEV    ', J10, 2, ELEV(2), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6350, IUER, 'UPTDB_DO', 'Error in '// &
     &                 'putting "ELEV    " lcode for the second station' )
                   RETURN 
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_PLCODE ( GVH, 'NUT_DER ', J10, 1, NUT_DER, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6351, IUER, 'UPTDB_DO', 'Error in '// &
     &                 'putting "NUT_DER " lcode for the second station' )
                   RETURN 
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_PLCODE ( GVH, 'APR_EOP ', J10, 1, EOP_VAL, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6352, IUER, 'UPTDB_DO', 'Error in '// &
     &                 'putting "APR_EOP " lcode for the second station' )
                   RETURN 
              END IF
         END IF
 4100 CONTINUE 
      CALL ACS_OBSFIL ( 'C' )
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL UPTDB_WRITE ( GVH, OPCODE_USE, ENV_FINAM, GVF_DB_DIR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6353, IUER, 'UPTDB_DO', 'Error in an attempt '// &
     &         'write updated GVH file' )
           RETURN 
      END IF
      DEALLOCATE ( OBS_TAB )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  UPTDB_DO  !#!#
