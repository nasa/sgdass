      SUBROUTINE GETDB_PARAM ( VTD, GVH, FL_PARAM, MJD_BEG, TAI_BEG, MJD_END, &
     &                         TAI_END, L_SOU, C_SOU, L_STA, C_STA, FL_BATCH, &
     &                         FL_SOU_USE_DB_IGNORE, RW_BAS_NAM, RW_BAS_DEL, &
     &                         RW_BAS_RAT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GETDB_PARAM
! *                                                                      *
! *  ### 30-NOV-2005  GETDB_PARAM  v1.7 (c)  L. Petrov  29-FEB-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INCLUDE   'prfil.i'
      INCLUDE   'gvh.i'
      INCLUDE   'vtd.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'glbc2.i'
      INCLUDE   'glbc3.i'
      INCLUDE   'glbc4.i'
!
      TYPE    ( GVH__STRU ) :: GVH
      TYPE    ( VTD__TYPE ) :: VTD
      INTEGER*4  MJD_BEG, MJD_END, L_SOU, L_STA, IUER
      LOGICAL*4  FL_BATCH
      LOGICAL*4  FL_PARAM, FL_SOU_USE_DB_IGNORE
      CHARACTER  C_SOU(L_SOU)*(*), C_STA(L_STA)*(*)
      REAL*8     TAI_BEG, TAI_END
      CHARACTER  RW_BAS_NAM(2,MAX_ARC_BSL)*8, TH_PROG*128 
      REAL*8     RW_BAS_DEL(0:SLV__MAX_SOLTYP-1,MAX_ARC_BSL), &
     &           RW_BAS_RAT(0:SLV__MAX_SOLTYP-1,MAX_ARC_BSL)
!
      INTEGER*4  M_NAV, M_PAR
      PARAMETER  ( M_NAV = 16 ) ! Number of points for computing average nutation angles
      PARAMETER  ( M_PAR = 64 )
!                               
      REAL*8     TDB_BEG, TARG_TDB_BEG, RDUMMY, &
     &           JD_UTC_BEG, NUTPSI_WAHR_AVE, NUTEPS_WAHR_AVE, PARS(M_PAR)
      REAL*8     JDPTB(MAX_EROT_VALUES), SPL_UT1(MAX_EROT_VALUES), &
     &           WORK_ARR(MAX_EROT_VALUES), JD_EP, TIM_TAI
      REAL*8     TARG_TDB, UT1_M_TDB, E1, E2, DPSI, DEPS, E1_RATE, E2_RATE, &
     &           DPSI_RATE, DEPS_RATE, DPSI_WAHR, DEPS_WAHR, STEP_NAV, &
     &           E1_GDS, E2_GDS, DPSI_GDS, DEPS_GDS, TAI_M_UT1, &
     &           UTC_CLBR(MAX4_BRK), JD_CLBR(MAX4_BRK), TJD, JD_STEP
      REAL*8     ATM_CNST, CLO_CNST, POL_CNST, UT1_CNST, GRR_CNST, GRO_CNST
      CHARACTER, ALLOCATABLE :: C_BAS(:)*16
      INTEGER*2, ALLOCATABLE :: DGCL_EST(:,:)
      INTEGER*4  BSCL_EST(MAX_ARC_BSL), BAS_USE(MAX_ARC_BSL)
      REAL*8,    ALLOCATABLE :: CLO_CNS(:,:),  ATM_CNS(:,:),  &
     &                          TLOF_CNS(:,:), TLRT_CNS(:,:), &
     &                          STPS_CNS(:,:), SOCO_CNS(:,:), &
     &                          BSCL_CNS(:,:)
      REAL*8     CLO_INTR(0:SLV__MAX_SOLTYP-1), &
     &           ATM_INTR(0:SLV__MAX_SOLTYP-1), &
     &           TIL_INTR(0:SLV__MAX_SOLTYP-1), &
     &           EOP_CNS(11,0:SLV__MAX_SOLTYP-1)
      REAL*8     CROSS_NUT_E3
      INTEGER*4  NUM_CLBR, NUM_CLRF, MJD_CLBR(MAX4_BRK), STA_USE(MAX_ARC_SRC), &
     &           SOU_USE(MAX_ARC_SRC), EOP_EST(11), STPS_EST(3,MAX_ARC_STA), &
     &           SOCO_EST(2,MAX_ARC_SRC), L_PAR
      INTEGER*2  KLOC_I2, JATM_I2, ISTAD_I2, JCLOCK_I2, K_I2, IORD_I2
      LOGICAL*2  FL_CONT, FAIL_L2, FL_BAS_USE, FL_CLO_CNS, FL_ATM_CNS
      CHARACTER  CDUMMY*4, STR*80, STR1*128, STR2*128, STA_CLBR(MAX4_BRK)*8, &
     &           STA_CLRF(MAX_ARC_SRC)*8, STA_NAM*8, DESCR*80, OUT*256
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, DIMS(2), &
     &           NUM_TRYS, MJD_TAB, IP, IXC, IND_STA, N_STA, N_SOU, N_BAS, &
     &           L_BAS, U_BAS, CLASS, TYP, NUM_FIELDS, SEG_IND, &
     &           LEN_REC, LEN_DATA, NUM_BAND, N_AVBAND, IER
      ADDRESS__TYPE :: ADR_DATA
      LOGICAL*4  FL_SOLVE, FL_NUSOLVE
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*2, EXTERNAL :: KBIT
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      REAL*8,    EXTERNAL :: MJD_SEC_TO_JD, FSPL8
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IXMN8, LTM_DIF
  num_band = 1
!
      IF ( INDEX ( GVH%GENERATOR, 'nuSolve' ) > 0 ) THEN
           FL_NUSOLVE = .TRUE.
         ELSE
           FL_NUSOLVE = .FALSE.
      END IF
!
      N_BAS = (INT4(NUMSTA)*(INT4(NUMSTA)-1))/2
      N_STA = NUMSTA
      N_SOU = NUMSTR
!
      CALL ERR_PASS      ( IUER, IER )
      CALL GVH_INQ_LCODE ( GVH, 'NUM_CLRF', DESCR, CLASS, TYP, DIMS, &
     &                          NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                          ADR_DATA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9101, IUER, 'GETDB_PARAM', 'Error in '// &
     &         'inquiring lcode NUM_CLRF' )
           RETURN
      END IF
!
      IF ( CLASS .NE. 0 ) THEN
           CALL GVH_GLCODE ( GVH, 'NUM_CLRF', 1, 1, 4, DIMS(1), DIMS(2), &
     &                       NUM_CLRF, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9102, IUER, 'GETDB_PARAM', 'Error in '// &
     &              'getting lcode NUM_CLRF' )
                RETURN
           END IF
           FL_SOLVE = .TRUE.
         ELSE
           NUM_CLRF = 0
           FL_SOLVE = .FALSE.
           FL_PARAM = .FALSE.
      END IF
!
      IF ( NUM_CLRF < 0 .OR. NUM_CLRF > MAX_ARC_STA ) THEN
           CALL CLRCH ( STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( NUM_CLRF, STR )
           CALL INCH  ( INT(MAX_ARC_STA,KIND=4), STR1 )
           CALL ERR_LOG ( 9103, IUER, 'GETDB_PARAM', 'Trap of internal control: '// &
     &         'LCODE NUM_CLRF '//TRIM(STR)//' is out of range [1, '//TRIM(STR1)// &
     &         '] -- most probable your database file is damaged. Please, transform it '// &
     &         'to vda format and check whether it can be repaired' )
           RETURN
      END IF
!
      IF ( NUM_CLRF > 0 ) THEN
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'STA_CLRF', 1, 1, 8*NUM_CLRF, &
     &                       DIMS(1), DIMS(2), STA_CLRF, IER )
           IF ( IER .NE. 0 ) THEN
                WRITE ( 6, * ) 'NUM_CLRF= ', NUM_CLRF
                CALL ERR_LOG ( 9104, IUER, 'GETDB_PARAM', 'Error in '// &
     &              'getting lcode STA_CLRF' )
                RETURN
           END IF
      END IF
!
      IF ( FL_SOLVE ) THEN
           NUM_CLBR = 0
           IF ( FL_SOLVE ) THEN
                IER = 0
              ELSE
                CALL ERR_PASS   ( IUER, IER )
           END IF
           CALL GVH_GLCODE ( GVH, 'NUM_CLBR', 1, 1, 4, DIMS(1), DIMS(2), &
     &                       NUM_CLBR, IER )
           IF ( IER .NE. 0 .AND. .NOT. FL_NUSOLVE ) THEN
                WRITE ( 6, * ) 'NUM_CLBR= ', NUM_CLBR
                CALL ERR_LOG ( 9105, IUER, 'GETDB_PARAM', 'Error in '// &
     &               'getting lcode NUM_CLBR' )
                RETURN
           END IF
!
           IF ( NUM_CLBR < 0 .OR. NUM_CLBR > MAX4_BRK ) THEN
                CALL CLRCH ( STR )
                CALL CLRCH ( STR1 )
                CALL INCH  ( NUM_CLBR, STR  )
                CALL INCH  ( MAX4_BRK, STR1 )
                CALL ERR_LOG ( 9106, IUER, 'GETDB_PARAM', 'Trap of internal control: '// &
     &              'LCODE NUM_CLBR '//TRIM(STR)//' is out of range [1, '//TRIM(STR1)// &
     &              '] -- most probable your database file is damaged. Please, '// &
     &              'transform it to vda format and check whether it can be repaired' )
               RETURN
           END IF
      END IF
!
      KBSL_CONST = .FALSE.
      BM_REF_CL = 0
      CLOCK_REF_BITS(1) = 0
      CLOCK_REF_BITS(2) = 0
      JD_UTC_BEG = MJD_SEC_TO_JD ( MJD_BEG, TAI_BEG + UTC_M_TAI )
!
      IF ( NUM_CLBR > 0 ) THEN
           CLK_BRK_STAT = .TRUE.
         ELSE
           CLK_BRK_STAT = .FALSE.
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'NUM_BAND', 0, 0, 4, DIMS(1), DIMS(2), &
     &                  NUM_BAND, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8720, IUER, 'GETDB_FILL_SOCOM', 'Error in '// &
     &         'getting lcode NUM_BAND' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'N_AVBAND', 0, 0, 4, DIMS(1), DIMS(2), &
     &                  N_AVBAND, IER )
      IF ( IER .NE. 0 ) THEN
           N_AVBAND = NUM_BAND
      END IF
!
      IF ( NUM_CLBR > 0 ) THEN
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'STA_CLBR', 1, 1, 8*NUM_CLBR, &
     &                       DIMS(1), DIMS(2), STA_CLBR, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9107, IUER, 'GETDB_PARAM', 'Error in '// &
     &              'getting lcode STA_CLBR' )
                RETURN
           END IF
!
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'MJD_CLBR', 1, 1, 4*NUM_CLBR, &
     &                       DIMS(1), DIMS(2), MJD_CLBR, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9108, IUER, 'GETDB_PARAM', 'Error in '// &
     &              'getting lcode MJD_CLBR' )
                RETURN
           END IF
!
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'UTC_CLBR', 1, 1, 8*NUM_CLBR, &
     &                       DIMS(1), DIMS(2), UTC_CLBR, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9109, IUER, 'GETDB_PARAM', 'Error in '// &
     &              'getting lcode MJD_CLBR' )
                RETURN
           END IF
      END IF
!
      IF ( FL_SOLVE ) THEN
           CALL ERR_PASS      ( IUER, IER )
           CALL GVH_INQ_LCODE ( GVH, 'BAS_USE ', DESCR, CLASS, TYP, DIMS, &
     &                          NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                          ADR_DATA, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9110, IUER, 'GETDB_PARAM', 'Error in '// &
     &             'inquiring lcode BAS_USE ' )
                RETURN
           END IF
         ELSE
           CLASS = 0
      END IF
      IF ( CLASS == 0 ) THEN
           FL_BAS_USE = .FALSE.
!
! -------- Set bits all baseline are selected
!
           IBLSEL_P = -1
           IBLSEL_G = -1
         ELSE
           FL_BAS_USE = .TRUE.
      END IF
!
      IF ( FL_BAS_USE ) THEN
           CALL ERR_PASS      ( IUER, IER )
           CALL GVH_INQ_LCODE ( GVH, 'BSCL_EST', DESCR, CLASS, TYP, DIMS, &
     &                          NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                          ADR_DATA, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9111, IUER, 'GETDB_PARAM', 'Error in '// &
     &             'inquiring lcode BSCL_EXT ' )
                RETURN
           END IF
!
           IF ( CLASS .NE. 0 ) THEN
                CALL ERR_PASS ( IUER, IER )
                CALL GVH_GLCODE ( GVH, 'BSCL_EST', 1, 1, 4*N_BAS*SLV__MAX_SOLTYP, &
     &                            DIMS(1), DIMS(2), BSCL_EST, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 9112, IUER, 'GETDB_PARAM', 'Error in '// &
     &                   'getting "BSCL_EST" lcode' )
                     RETURN
                END IF
              ELSE
                BSCL_EST = 0
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'BAS_USE ', 1, 1, 4*N_BAS, DIMS(1), DIMS(2), &
     &                       BAS_USE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9113, IUER, 'GETDB_PARAM', 'Error in '// &
     &              'getting "BAS_USE " lcode' )
                RETURN
           END IF
!
           L_BAS = 0
           U_BAS = 0
!
           DO 410 J1=1,N_STA
              IF ( J1 < N_STA ) THEN
                   DO 420 J2=J1+1,N_STA
                      L_BAS = L_BAS + 1
                      IF ( N_AVBAND == 2 ) THEN
!
! ------------------------ Dial band case
!
                           IF ( BTEST ( BAS_USE(L_BAS), G_GXS__DTP ) ) THEN
                                CALL SBIT ( IBLSEL_G(1,J1), INT2(J2), INT2(1) )
                                CALL SBIT ( IBLSEL_G(1,J2), INT2(J1), INT2(1) )
                                U_BAS = U_BAS + 1
                              ELSE
                                CALL SBIT ( IBLSEL_G(1,J1), INT2(J2), INT2(0) )
                                CALL SBIT ( IBLSEL_G(1,J2), INT2(J1), INT2(0) )
                           END IF
!
                           IF ( BTEST ( BAS_USE(L_BAS), PX_GS__DTP ) .OR. &
     &                          BTEST ( BAS_USE(L_BAS), PX__DTP    ) .OR. &
     &                          BTEST ( BAS_USE(L_BAS), PS__DTP    )      ) THEN
!
                                CALL SBIT ( IBLSEL_P(1,J1), INT2(J2), INT2(1) )
                                CALL SBIT ( IBLSEL_P(1,J2), INT2(J1), INT2(1) )
                              ELSE
                                CALL SBIT ( IBLSEL_P(1,J1), INT2(J2), INT2(0) )
                                CALL SBIT ( IBLSEL_P(1,J2), INT2(J1), INT2(0) )
                           END IF
                         ELSE 
!
! ------------------------ Single band case
!
                           IF ( BTEST ( BAS_USE(L_BAS), GX__DTP ) ) THEN
                                CALL SBIT ( IBLSEL_G(1,J1), INT2(J2), INT2(1) )
                                CALL SBIT ( IBLSEL_G(1,J2), INT2(J1), INT2(1) )
                                U_BAS = U_BAS + 1
                              ELSE
                                CALL SBIT ( IBLSEL_G(1,J1), INT2(J2), INT2(0) )
                                CALL SBIT ( IBLSEL_G(1,J2), INT2(J1), INT2(0) )
                           END IF
!
                           IF ( BTEST ( BAS_USE(L_BAS), PX__DTP ) ) THEN
                                CALL SBIT ( IBLSEL_P(1,J1), INT2(J2), INT2(1) )
                                CALL SBIT ( IBLSEL_P(1,J2), INT2(J1), INT2(1) )
                              ELSE
                                CALL SBIT ( IBLSEL_P(1,J1), INT2(J2), INT2(0) )
                                CALL SBIT ( IBLSEL_P(1,J2), INT2(J1), INT2(0) )
                           END IF
                      END IF
!
                      IF ( BTEST ( BSCL_EST(L_BAS), INT4(IDATYP) ) ) THEN
                           CALL SBIT ( ICLOCK(1,J1), INT2(J2), INT2(1) )
                           CALL SBIT ( ICLOCK(1,J2), INT2(J1), INT2(1) )
                           LOGBCL = .TRUE.
                         ELSE
                           CALL SBIT ( ICLOCK(1,J1), INT2(J2), INT2(0) )
                           CALL SBIT ( ICLOCK(1,J2), INT2(J1), INT2(0) )
                      END IF
!
                      BAS_CLK_SIGMA = BAS_CLK__SIG__DEF
                      IF ( BAS_CLK_SIGMA > 0.0D0 ) THEN
                           KBSL_CONST = .TRUE.
                         ELSE
                           KBSL_CONST = .FALSE.
                      END IF
 420               CONTINUE
              END IF
 410       CONTINUE
      END IF
!
      CALL ERR_PASS      ( IUER, IER )
      CALL GVH_INQ_LCODE ( GVH, 'SOU_USE ', DESCR, CLASS, TYP, DIMS, &
     &                     NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                     ADR_DATA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9114, IUER, 'GETDB_PARAM', 'Error in '// &
     &        'inquiring lcode SOU_USE ' )
           RETURN
      END IF
!
      IF ( CLASS .NE. 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'SOU_USE ', 1, 1, 4*N_SOU, DIMS(1), DIMS(2), &
     &                       SOU_USE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9115, IUER, 'GETDB_PARAM', 'Error in '// &
     &              'getting "SOU_USE " lcode' )
                RETURN
           END IF
         ELSE
           SOU_USE = -1
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_INQ_LCODE ( GVH, 'SOCO_EST', DESCR, CLASS, TYP, DIMS, &
     &                          NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                          ADR_DATA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9116, IUER, 'GETDB_PARAM', 'Error in '// &
     &        'inquiring lcode BAS_USE ' )
           RETURN
      END IF
      IF ( CLASS .NE. 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'SOCO_EST', 1, 1, 4*2*N_SOU, &
     &                      DIMS(1), DIMS(2), SOCO_EST, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9117, IUER, 'GETDB_PARAM', 'Error in '// &
     &              'getting "SOCO_EST" lcode' )
                RETURN
           END IF
         ELSE
           SOCO_EST = 0
      END IF
      CALL SET_STABIT ( INT2(2) )
!
! --- Set source usage flag status
!
      DO 430 J3=1,N_SOU
         IF ( FL_SOU_USE_DB_IGNORE ) THEN
!
! ----------- User requested to ignore source usage flags.
! ----------- Let us grant this request by setting the usage flag
! ----------- into state "to use"
!
              SOU_USE(J3) = IBSET ( SOU_USE(J3), INT4(IDATYP) )
          END IF
!
          IF ( BTEST ( SOU_USE(J3), INT4(IDATYP) ) ) THEN
               CALL SBIT ( ISRSEL(1), INT2(J3), INT2(1) )
             ELSE
               CALL SBIT ( ISRSEL(1), INT2(J3), INT2(0) )
          END IF
!
          IF ( BTEST ( SOCO_EST(1,J3), INT4(IDATYP) ) ) THEN
               CALL SBIT ( LSTAR(1,1), INT2(J3), INT2(1) )
          END IF
!
          IF ( BTEST ( SOCO_EST(2,J3), INT4(IDATYP) ) ) THEN
               CALL SBIT ( LSTAR(1,2), INT2(J3), INT2(1) )
          END IF
 430  CONTINUE
!
      IF ( FL_PARAM ) THEN
           ALLOCATE ( DGCL_EST(N_STA,0:SLV__MAX_SOLTYP-1) )
           ALLOCATE (  CLO_CNS(N_STA,0:SLV__MAX_SOLTYP-1) )
           ALLOCATE (  ATM_CNS(N_STA,0:SLV__MAX_SOLTYP-1) )
           ALLOCATE ( TLOF_CNS(N_STA,0:SLV__MAX_SOLTYP-1) )
           ALLOCATE ( TLRT_CNS(N_STA,0:SLV__MAX_SOLTYP-1) )
           ALLOCATE ( STPS_CNS(N_STA,0:SLV__MAX_SOLTYP-1) )
           ALLOCATE ( SOCO_CNS(N_SOU,0:SLV__MAX_SOLTYP-1) )
           ALLOCATE ( BSCL_CNS(N_BAS,0:SLV__MAX_SOLTYP-1)  )
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'DGCL_EST', 1, 1, 2*N_STA*SLV__MAX_SOLTYP, &
     &                       DIMS(1), DIMS(2), DGCL_EST, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9118, IUER, 'GETDB_PARAM', 'Error in '// &
     &              'getting "DGCL_EST" lcode' )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_INQ_LCODE ( GVH, 'STPS_EST', DESCR, CLASS, TYP, DIMS, &
     &                          NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                          ADR_DATA, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9119, IUER, 'GETDB_PARAM', 'Error in '// &
     &              'inquiring lcode STPS_EST' )
                RETURN
            END IF
!
           IF ( CLASS > 0 ) THEN
                CALL ERR_PASS ( IUER, IER )
                CALL GVH_GLCODE ( GVH, 'STPS_EST', 1, 1, 4*3*N_STA, &
     &                            DIMS(1), DIMS(2), STPS_EST, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 9120, IUER, 'GETDB_PARAM', 'Error in '// &
     &                   'getting "STPS_EST" lcode' )
                     RETURN
                END IF
              ELSE
                STPS_EST = 0
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_INQ_LCODE ( GVH, 'EOP_EST ', DESCR, CLASS, TYP, DIMS, &
     &                          NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                          ADR_DATA, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9121, IUER, 'GETDB_PARAM', 'Error in '// &
     &              'inquiring lcode EOP_EST' )
                RETURN
           END IF
           IF ( CLASS > 0 ) THEN
                CALL ERR_PASS ( IUER, IER )
                CALL GVH_GLCODE ( GVH, 'EOP_EST ', 1, 1, 4*11, DIMS(1), DIMS(2), &
     &                            EOP_EST, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 9122, IUER, 'GETDB_PARAM', 'Error in '// &
     &                   'getting "EOP_EST " lcode' )
                     RETURN
                END IF
              ELSE
                EOP_EST = 0
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'CLO_INTR', 1, 1, 8*SLV__MAX_SOLTYP, &
     &                       DIMS(1), DIMS(2), CLO_INTR, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9123, IUER, 'GETDB_PARAM', 'Error in '// &
     &              'getting "CLO_INTR" lcode' )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'ATM_INTR', 1, 1, 8*SLV__MAX_SOLTYP, &
     &                       DIMS(1), DIMS(2), ATM_INTR, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9124, IUER, 'GETDB_PARAM', 'Error in '// &
     &              'getting "ATM_INTR" lcode' )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_INQ_LCODE ( GVH, 'TIL_INTR', DESCR, CLASS, TYP, DIMS, &
     &                          NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                          ADR_DATA, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9125, IUER, 'GETDB_PARAM', 'Error in '// &
     &              'inquiring lcode TIL_INTR' )
                RETURN
           END IF
           IF ( CLASS > 0 ) THEN
                CALL ERR_PASS ( IUER, IER )
                CALL GVH_GLCODE ( GVH, 'TIL_INTR', 1, 1, 8*SLV__MAX_SOLTYP, &
     &                            DIMS(1), DIMS(2), TIL_INTR, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 9126, IUER, 'GETDB_PARAM', 'Error in '// &
     &                   'getting "TIL_INTR" lcode' )
                     RETURN
                END IF
              ELSE
                TIL_INTR = 0.0D0
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'CLO_CNS ', 1, 1, 8*N_STA*SLV__MAX_SOLTYP, &
     &                       DIMS(1), DIMS(2), CLO_CNS, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9127, IUER, 'GETDB_PARAM', 'Error in '// &
     &              'getting "CLO_CNS " lcode' )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'ATM_CNS ', 1, 1, 8*N_STA*SLV__MAX_SOLTYP, &
     &                       DIMS(1), DIMS(2), ATM_CNS, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9128, IUER, 'GETDB_PARAM', 'Error in '// &
     &              'getting "ATM_CNS " lcode' )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_INQ_LCODE ( GVH, 'TLOF_CNS', DESCR, CLASS, TYP, DIMS, &
     &                          NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                          ADR_DATA, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9129, IUER, 'GETDB_PARAM', 'Error in '// &
     &              'inquiring lcode TLOF_CNS' )
                RETURN
           END IF
           IF ( CLASS > 0 ) THEN
                CALL ERR_PASS ( IUER, IER )
                CALL GVH_GLCODE ( GVH, 'TLOF_CNS', 1, 1, 8*N_STA*SLV__MAX_SOLTYP, &
     &                            DIMS(1), DIMS(2), TLOF_CNS, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 9130, IUER, 'GETDB_PARAM', 'Error in '// &
     &                   'getting "TLOF_CNS" lcode' )
                     RETURN
                END IF
              ELSE
                TLOF_CNS = 0.0D0
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_INQ_LCODE ( GVH, 'TLRT_CNS', DESCR, CLASS, TYP, DIMS, &
     &                          NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                          ADR_DATA, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9131, IUER, 'GETDB_PARAM', 'Error in '// &
     &              'inquiring lcode EOP_EST' )
                RETURN
           END IF
           IF ( CLASS > 0 ) THEN
                CALL ERR_PASS ( IUER, IER )
                CALL GVH_GLCODE ( GVH, 'TLRT_CNS', 1, 1, 8*N_STA*SLV__MAX_SOLTYP, &
     &                            DIMS(1), DIMS(2), TLRT_CNS, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 9132, IUER, 'GETDB_PARAM', 'Error in '// &
     &                   'getting "TLRT_CNS" lcode' )
                     RETURN
                END IF
              ELSE
                TLRT_CNS = 0.0D0
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_INQ_LCODE ( GVH, 'STPS_CNS ', DESCR, CLASS, TYP, DIMS, &
     &                          NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                          ADR_DATA, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9133, IUER, 'GETDB_PARAM', 'Error in '// &
     &              'inquiring lcode STPS_CNS' )
                RETURN
           END IF
           IF ( CLASS > 0 ) THEN
                CALL ERR_PASS ( IUER, IER )
                CALL GVH_GLCODE ( GVH, 'STPS_CNS', 1, 1, 8*N_STA*SLV__MAX_SOLTYP, &
     &                            DIMS(1), DIMS(2), STPS_CNS, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 9134, IUER, 'GETDB_PARAM', 'Error in '// &
     &                   'getting "STPS_CNS" lcode' )
                     RETURN
                END IF
              ELSE
                STPS_CNS = 0.0D0
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_INQ_LCODE ( GVH, 'SOCO_CNS', DESCR, CLASS, TYP, DIMS, &
     &                          NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                          ADR_DATA, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9135, IUER, 'GETDB_PARAM', 'Error in '// &
     &              'inquiring lcode SOCO_CNS' )
                RETURN
           END IF
           IF ( CLASS > 0 ) THEN
                CALL ERR_PASS ( IUER, IER )
                CALL GVH_GLCODE ( GVH, 'SOCO_CNS', 1, 1, 8*N_SOU*SLV__MAX_SOLTYP, &
     &                            DIMS(1), DIMS(2), SOCO_CNS, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 9136, IUER, 'GETDB_PARAM', 'Error in '// &
     &                   'getting "SOCO_CNS" lcode' )
                     RETURN
                END IF
              ELSE
                SOCO_CNS = 0.0D0
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_INQ_LCODE ( GVH, 'BSCL_CNS', DESCR, CLASS, TYP, DIMS, &
     &                          NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                          ADR_DATA, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9137, IUER, 'GETDB_PARAM', 'Error in '// &
     &              'inquiring lcode BSCL_CNS' )
                RETURN
           END IF
           IF ( CLASS > 0 ) THEN
                CALL ERR_PASS ( IUER, IER )
                CALL GVH_GLCODE ( GVH, 'BSCL_CNS', 1, 1, 8*N_BAS*SLV__MAX_SOLTYP, &
     &                            DIMS(1), DIMS(2), BSCL_CNS, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 9138, IUER, 'GETDB_PARAM', 'Error in '// &
     &                   'getting "BSCL_CNS" lcode' )
                     RETURN
                END IF
              ELSE
                BSCL_CNS = 0.0D0
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'STA_USE ', 1, 1, 4*N_STA, DIMS(1), DIMS(2), &
     &                       STA_USE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9139, IUER, 'GETDB_PARAM', 'Error in '// &
     &              'getting "STA_USE " lcode' )
                RETURN
           END IF
!
           BMODE_AT  = .FALSE.
           BMODE_CL  = .FALSE.
           CLOCK_INTERVAL = CLO_INTR(IDATYP)/3600.0D0
           ATMOS_INTERVAL = ATM_INTR(IDATYP)/3600.0D0
           GRAD_INTERVAL  = TIL_INTR(IDATYP)/3600.0D0
!
           L_BAS = 0
           DO 440 J4=1,N_STA
              SACNST(J4) = ATM_CNS(J4,IDATYP)/1.D-12*3600.0D0
              SCCNST(J4) = CLO_CNS(J4,IDATYP)/1.D-14
              IF ( SACNST(J4) > 0.0D0 ) THEN
                   CALL SBIT ( CONSTRAINT_BITS, INT2(2), INT2(1) )
              END IF
              IF ( SCCNST(J4) > 0.0D0 ) THEN
                   CALL SBIT ( CONSTRAINT_BITS, INT2(3), INT2(1) )
              END IF
              GRADCONS(1) = TLRT_CNS(J4,IDATYP)/MM__TO__SEC*86400.0D0
              GRADCONS(2) = TLOF_CNS(J4,IDATYP)/MM__TO__SEC
              IF ( GRADCONS(1) > 0.0D0  .OR.  GRADCONS(2) > 0 ) THEN
                   CALL SBIT ( CONSTRAINT_BITS, INT2(8), INT2(1) )
              END IF
!
              IF ( BTEST ( STPS_EST(1,J4), INT4(IDATYP) ) ) THEN
                   CALL SBIT ( LSITEC(1,1), INT2(J4), INT2(1) )
              ENDIF
!
              IF ( BTEST ( STPS_EST(2,J4), INT4(IDATYP) ) ) THEN
                   CALL SBIT ( LSITEC(1,2), INT2(J4), INT2(1) )
              ENDIF
!
              IF ( BTEST ( STPS_EST(3,J4), INT4(IDATYP) ) ) THEN
                   CALL SBIT ( LSITEC(1,3), INT2(J4), INT2(1) )
              ENDIF
!
              IF ( NUM_CLRF > 0 ) THEN
                   STA_NAM = ISITN_CHR(J4)
                   CALL VTD_NAME_REPAIR ( STA_NAM )
                   IP = LTM_DIF ( 0, NUM_CLRF, STA_CLRF, STA_NAM )
                   IF ( IP > 0 ) THEN
                        CALL SBIT ( CLOCK_REF_BITS, INT2(J4), INT2(1) )
                        BM_REF_CL = J4
                        GOTO 440
                   END IF
              END IF
              CALL INCLK ( INT2(J4), JD_UTC_BEG - 1.0D0/1440.0D0, KLOC_I2, TRUE__L2 )
              IF ( KLOC_I2 .GT. 0 ) LCLK(KLOC_I2) = 0
              CALL INATM ( INT2(J4), JD_UTC_BEG - 1.0D0/1440.0D0, JATM_I2 )
              CALL OFST_STA ( INT2(J4), JD_UTC_BEG, INT4(DGCL_EST(J4,IDATYP)) )
              IF ( J4 < N_STA ) THEN
                   DO 450 J5=J4+1,N_STA
                      L_BAS = L_BAS + 1
                      BAS_CLK_SIGMA = BAS_CLK__SIG__DEF
                      IF ( BAS_CLK_SIGMA > 0.0D0 ) THEN
                           KBSL_CONST = .TRUE.
                         ELSE
                           KBSL_CONST = .FALSE.
                      END IF
 450               CONTINUE
              END IF
 440       CONTINUE
!
           IF ( NUM_CLBR > MAX4_BRK ) NUM_CLBR = MAX4_BRK
           DO 460 J6=1,NUM_CLBR
              CALL VTD_NAME_REPAIR ( STA_CLBR(J6) )
              IND_STA = LTM_DIF ( 0, N_STA, ISITN_CHR, STA_CLBR(J6) )
              IF ( IND_STA .LE. 0 ) THEN
                   CALL LIST_TO_LINE ( N_STA, ISITN_CHR, ', ', OUT )
                   CALL ERR_LOG ( 9140, IUER, 'GETDB_PARAM', 'Cannot find '// &
     &                 'clock reference station '//STA_CLBR(J6)// &
     &                 ' among participated stations: '//OUT )
                   RETURN
              END IF
!
              JD_CLBR(J6) = MJD_SEC_TO_JD ( MJD_CLBR(J6), UTC_CLBR(J6) )
              TJD = JD_CLBR(J6)
              NUM_TRYS = 0
              FL_CONT = .TRUE.
              DO WHILE ( FL_CONT  .AND.  NUM_TRYS < 32 )
                 NUM_TRYS = NUM_TRYS + 1
                 DO 470 J7=ICLSTR(IND_STA)+1,ICLSTR(IND_STA)+NUMCLK(IND_STA)
                    IF ( DABS(TJD-FJDCL(J7) ) .LT. 1.D0/86400.D0 ) THEN
                         IF ( NUM_TRYS > 32 ) THEN
                              CALL ERR_LOG ( 9141, IUER, 'GETDB_PARAM', &
     &                            'Trap of internal control' )
                              RETURN
                           ELSE
                             TJD = TJD + 0.2D0/86400.D0
                         ENDIF
                      ELSE
                         FL_CONT = .FALSE.
                    ENDIF
 470             CONTINUE
              ENDDO
              CALL INCLK ( INT2(IND_STA), TJD, KLOC_I2, FALSE__L2 )
              DO JCLOCK_I2 = 1, NUMCLK(IND_STA)
                 K_I2 = JCLOCK_I2 + ICLSTR(IND_STA)
                 IF ( KBIT( ICLSTA(1,K_I2), INT2(IND_STA) ) ) THEN
                      DO IORD_I2 = 1,3
                         CALL SBIT (  LCLK(K_I2), IORD_I2, INT2(1) )
                      END DO
                  END IF
              END DO
 460       CONTINUE
!
           IF ( CLO_INTR(IDATYP) > 0.2D0 ) THEN
                CALL AUTC_MULT_REF ( 'A', CLOCK_REF_BITS, &
     &                               INT2(CLOCK_INTERVAL*60.0D0), FAIL_L2 )
                IF ( .NOT. FAIL_L2 ) BMODE_CL = .TRUE.
           END IF
           IF ( ATM_INTR(IDATYP) > 0.2D0 ) THEN
                CALL AUTA ( INT2(ATMOS_INTERVAL*60.0D0), FAIL_L2 )
                IF ( .NOT. FAIL_L2 ) BMODE_AT = .TRUE.
           END IF
           IF ( TIL_INTR(IDATYP) > 0.2D0 ) THEN
                CALL AUTGRAD ( GRAD_INTERVAL*60.0D0, FAIL_L2 )
           END IF
!
           EOP_STYLE(1) = EOP__POLY
           EOP_STYLE(2) = EOP__POLY
           IF ( BTEST ( EOP_EST( 1), INT4(IDATYP) ) ) CALL IROTF(1,1,1,LROT)
           IF ( BTEST ( EOP_EST( 2), INT4(IDATYP) ) ) CALL IROTF(1,2,1,LROT)
           IF ( BTEST ( EOP_EST( 4), INT4(IDATYP) ) ) CALL IROTF(1,1,2,LROT)
           IF ( BTEST ( EOP_EST( 5), INT4(IDATYP) ) ) CALL IROTF(1,2,2,LROT)
           IF ( BTEST ( EOP_EST( 7), INT4(IDATYP) ) ) CALL IROTF(1,1,3,LROT)
           IF ( BTEST ( EOP_EST( 8), INT4(IDATYP) ) ) CALL IROTF(1,2,3,LROT)
           IF ( BTEST ( EOP_EST( 3), INT4(IDATYP) ) ) CALL IROTF(1,3,1,LROT)
           IF ( BTEST ( EOP_EST( 6), INT4(IDATYP) ) ) CALL IROTF(1,3,2,LROT)
           IF ( BTEST ( EOP_EST( 9), INT4(IDATYP) ) ) CALL IROTF(1,3,3,LROT)
           IF ( BTEST ( EOP_EST(10), INT4(IDATYP) ) ) CALL SBIT ( LNUT, INT2(1), INT2(1) )
           IF ( BTEST ( EOP_EST(11), INT4(IDATYP) ) ) CALL SBIT ( LNUT, INT2(2), INT2(1) )
!
! -------- Put a bunch of variables to glbc3
!
           CALL META_VAR_EXCHANGE ( 1, N_BAS, N_STA, N_SOU, &
     &                              CLO_INTR, ATM_INTR, TIL_INTR, &
     &                              EOP_CNS, CLO_CNS, ATM_CNS,  &
     &                              TLOF_CNS, TLRT_CNS, STPS_CNS, &
     &                              SOCO_CNS, DGCL_EST, BSCL_CNS, &
     &                              RW_BAS_DEL, RW_BAS_RAT, RW_BAS_NAM )
!
           DEALLOCATE ( DGCL_EST )
           DEALLOCATE (  CLO_CNS )
           DEALLOCATE (  ATM_CNS )
           DEALLOCATE ( TLOF_CNS )
           DEALLOCATE ( TLRT_CNS )
           DEALLOCATE ( STPS_CNS )
           DEALLOCATE ( SOCO_CNS )
           DEALLOCATE ( BSCL_CNS )
         ELSE ! not. fl_param
!
! -------- Default parameterization
!
           ALLOCATE ( CLO_CNS(N_STA,0:SLV__MAX_SOLTYP-1)  )
           ALLOCATE ( ATM_CNS(N_STA,0:SLV__MAX_SOLTYP-1)  )
           ALLOCATE ( TLOF_CNS(N_STA,0:SLV__MAX_SOLTYP-1) )
           ALLOCATE ( TLRT_CNS(N_STA,0:SLV__MAX_SOLTYP-1) )
           ALLOCATE ( STPS_CNS(N_STA,0:SLV__MAX_SOLTYP-1) )
           ALLOCATE ( SOCO_CNS(N_STA,0:SLV__MAX_SOLTYP-1) )
           ALLOCATE ( BSCL_CNS(N_BAS,0:SLV__MAX_SOLTYP-1) )
           ALLOCATE ( DGCL_EST(N_BAS,0:SLV__MAX_SOLTYP-1) )
!
! -------- These variables are allocated an initialized to prevent crash
!
           TLOF_CNS = 0.0D0
           TLRT_CNS = 0.0D0
           STPS_CNS = 0.0D0
           SOCO_CNS = 0.0D0
           BSCL_CNS = 0.0D0
           DGCL_EST = 0.0D0
!
           BMODE_AT  = .FALSE.
           BMODE_CL  = .FALSE.
           CALL SBIT ( CONSTRAINT_BITS, INT2(3), INT2(1) ) ! The constraint defaults on.
           CALL SBIT ( CONSTRAINT_BITS, INT2(2), INT2(1) ) ! The constraint is turned on.
!
           CALL ERR_PASS   ( IUER, IER )
           CALL CONSTRAINT_DEFAULT ( ATM_CNST, CLO_CNST, POL_CNST, &
     &                               UT1_CNST, GRR_CNST, GRO_CNST, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9142, IUER, 'GETDB_PARAM', 'Error in an attempt '// &
     &              'to setup default constraints sigmas' )
                RETURN
           END IF
           GRADCONS(1) = GRR_CNST ! Constant of constraints on gradient rate
           GRADCONS(2) = GRO_CNST ! Constant of constraints on gradient offset
           SEOCNST(1)  = POL_CNST
           SEOCNST(2)  = UT1_CNST
!
           IER = 0
           CALL GVH_GLCODE ( GVH, 'CLO_CNS ', 1, 1, 8*N_STA*SLV__MAX_SOLTYP, &
     &                       DIMS(1), DIMS(2), CLO_CNS, IER )
           IF ( IER == 0 ) THEN
                FL_CLO_CNS = .TRUE.
              ELSE
                FL_CLO_CNS = .FALSE.
           END IF
!
           IER = 0
           CALL GVH_GLCODE ( GVH, 'ATM_CNS ', 1, 1, 8*N_STA*SLV__MAX_SOLTYP, &
     &                       DIMS(1), DIMS(2), ATM_CNS, IER )
           IF ( IER == 0 ) THEN
                FL_ATM_CNS = .TRUE.
              ELSE
                FL_ATM_CNS = .FALSE.
           END IF
!
           DO 480 J8=1,N_STA
              IF ( FL_ATM_CNS ) THEN
                   SACNST(J8) = ATM_CNS(J8,IDATYP)/1.0D-12*3600.0D0
                 ELSE
                   SACNST(J8) = ATM_CNST
              END IF
              IF ( FL_CLO_CNS ) THEN
                   SCCNST(J8) = CLO_CNS(J8,IDATYP)/1.0D-14
                 ELSE
                   SCCNST(J8) = CLO_CNST
              END IF
              IF ( NUM_CLRF > 0 ) THEN
                   IP = LTM_DIF ( 0, NUM_CLRF, STA_CLRF, ISITN_CHR(J8) )
                   IF ( IP > 0 ) THEN
                        CALL SBIT ( CLOCK_REF_BITS, INT2(J8), INT2(1) )
                        GOTO 480
                   END IF
              END IF
              CALL INCLK ( INT2(J8), JD_UTC_BEG - 1.0D0/1440.0D0, KLOC_I2, TRUE__L2 )
              IF ( KLOC_I2 .GT. 0 ) LCLK(KLOC_I2) = 0
              CALL INATM ( INT2(J8), JD_UTC_BEG - 1.0D0/1440.0D0, JATM_I2 )
              CALL OFST_STA ( INT2(J8), JD_UTC_BEG, 2 )
 480       CONTINUE
!
           IF ( NUM_CLBR > MAX4_BRK ) NUM_CLBR = MAX4_BRK
           DO 490 J9=1,NUM_CLBR
              IND_STA = LTM_DIF ( 0, N_STA, ISITN_CHR, STA_CLBR(J9) )
              IF ( IND_STA .LE. 0 ) THEN
                   WRITE ( 6, * ) ' NUM_CLBR = ', NUM_CLBR, &
     &                            ' J9=', J9, ' STA_CLBR = ' , STA_CLBR(J9)
                   GOTO 490
              END IF
              JD_CLBR(J9) = MJD_SEC_TO_JD ( MJD_CLBR(J9), UTC_CLBR(J9) )
              TJD = JD_CLBR(J9)
              NUM_TRYS = 0
              FL_CONT = .TRUE.
              DO WHILE ( FL_CONT  .AND.  NUM_TRYS < 32 )
                 NUM_TRYS = NUM_TRYS + 1
                 DO 4100 J10=ICLSTR(IND_STA)+1,ICLSTR(IND_STA)+NUMCLK(IND_STA)
                    IF ( DABS(TJD-FJDCL(J10) ) .LT. 1.D0/86400.D0 ) THEN
                         IF ( NUM_TRYS > 32 ) THEN
                              CALL ERR_LOG ( 9143, IUER, 'GETDB_PARAM', &
     &                            'Trap of internal control' )
                              RETURN
                           ELSE
                             TJD = TJD + 0.2D0/86400.D0
                         ENDIF
                      ELSE
                         FL_CONT = .FALSE.
                    ENDIF
 4100            CONTINUE
              ENDDO
!
              CALL INCLK ( INT2(IND_STA), TJD, KLOC_I2, FALSE__L2 )
              DO JCLOCK_I2 = 1, NUMCLK(IND_STA)
                 K_I2 = JCLOCK_I2 + ICLSTR(IND_STA)
                 IF ( KBIT( ICLSTA(1,K_I2), INT2(IND_STA) ) ) THEN
                      DO IORD_I2 = 1,3
                         CALL SBIT (  LCLK(K_I2), IORD_I2, INT2(1) )
                      END DO
                  END IF
              END DO
 490       CONTINUE
!
           IER = 0
           CALL GVH_GLCODE ( GVH, 'CLO_INTR', 1, 1, 8*SLV__MAX_SOLTYP, &
     &                       DIMS(1), DIMS(2), CLO_INTR, IER )
           IF ( IER == 0 ) THEN
                CLOCK_INTERVAL = CLO_INTR(IDATYP)/3600.0D0
                IF ( CLO_INTR(IDATYP) > 0.5D0 ) THEN
                     CALL AUTC_MULT_REF ( 'A', CLOCK_REF_BITS, &
     &                                    INT2(CLOCK_INTERVAL*60.0D0), FAIL_L2 )
                     IF ( .NOT. FAIL_L2 ) BMODE_CL = .TRUE.
                END IF
           END IF
!
           IER = 0
           CALL GVH_GLCODE ( GVH, 'ATM_INTR', 1, 1, 8*SLV__MAX_SOLTYP, &
     &                       DIMS(1), DIMS(2), ATM_INTR, IER )
           IF ( IER == 0 ) THEN
                ATMOS_INTERVAL = ATM_INTR(IDATYP)/3600.0D0
                IF ( ATM_INTR(IDATYP) > 0.2D0 ) THEN
                     CALL AUTA ( INT2(ATMOS_INTERVAL*60.0D0), FAIL_L2 )
                     IF ( .NOT. FAIL_L2 ) BMODE_AT = .TRUE.
                END IF
           END IF
!
           CALL META_VAR_EXCHANGE ( 0, N_BAS, N_STA, N_SOU, &
     &                                 CLO_INTR, ATM_INTR, TIL_INTR, &
     &                                 EOP_CNS, CLO_CNS, ATM_CNS,  &
     &                                 TLOF_CNS, TLRT_CNS, STPS_CNS, &
     &                                 SOCO_CNS, DGCL_EST, BSCL_CNS, &
     &                                 RW_BAS_DEL, RW_BAS_RAT, RW_BAS_NAM )
!
           DEALLOCATE ( DGCL_EST )
           DEALLOCATE (  CLO_CNS )
           DEALLOCATE (  ATM_CNS )
           DEALLOCATE ( TLOF_CNS )
           DEALLOCATE ( TLRT_CNS )
           DEALLOCATE ( STPS_CNS )
           DEALLOCATE ( SOCO_CNS )
           DEALLOCATE ( BSCL_CNS )
      END IF ! fl_param
!
      UT1INB(1) = 2400000.5D0 + MJD_BEG - MAX_EROT_VALUES/2 + 1
      UT1INB(2) = 1.0D0
      UT1INB(3) = MAX_EROT_VALUES
!
      WOBINB(1) = UT1INB(1)
      WOBINB(2) = UT1INB(2)
      WOBINB(3) = UT1INB(3)
!
! --- Computation of EOP
!
      IF ( (MJD_END - MJD_BEG)*86400.0D0 + (TAI_END - TAI_BEG) .LT. 4.0D0*86400.0D0 ) THEN
           JD_STEP = 1.0D0
         ELSE
           JD_STEP = 2.0D0
      END IF
      DO 4110 J11=1,MAX_EROT_VALUES
         MJD_TAB = UT1INB(1) + (J11-1)*IDNINT(JD_STEP) - 2400000.5D0
         JDPTB(J11) = UT1INB(1) + (J11-1)*JD_STEP
         CALL ERR_PASS ( IUER, IER )
         CALL VTD_MOMENT ( 'EOP_ONLY', MJD_TAB, 0.0D0, VTD, IER )
         IF ( IER .NE. 0 ) THEN
              STR1 = MJDSEC_TO_DATE ( IDNINT(UT1INB(1)                   - 2400000.5D0), 0.0D0, -2 )
              STR2 = MJDSEC_TO_DATE ( IDNINT(UT1INB(1) + MAX_EROT_VALUES - 2400000.5D0), 0.0D0, -2 )
              CALL ERR_LOG ( 9144, IUER, 'GETDB_PARAM', 'Error in an '// &
     &            'attempt to compute interpolated value of UT1 and polar '// &
     &            'motion  at epoch '//MJDSEC_TO_DATE ( MJD_TAB, 0.0D0, -3 )// &
     &            ' from the range [ '//STR1(1:19)//', '//STR2(1:19)//' ]' )
              RETURN
         END IF
         UT1PTB(J11) = -VTD%MOM%UT1_M_TAI
         WOBXXB(J11) = VTD%MOM%XPL/MAS__TO__RAD
         WOBYYB(J11) = VTD%MOM%YPL/MAS__TO__RAD
 4110 CONTINUE
!
      INTERPOLATION_UT1 = 4
      INTERPOLATION_PM  = 4
!
      NROT = 1
      TROT(1) = ( MJD_SEC_TO_JD ( MJD_BEG, TAI_BEG ) + &
     &            MJD_SEC_TO_JD ( MJD_END, TAI_END )   )/2.0D0
      EOP_STYLE(1) = EOP__POLY
      EOP_STYLE(2) = EOP__POLY
!
      CALL ERR_PASS ( IUER, IER )
      CALL MAKE_SPLINE ( 3, INT4(MAX_EROT_VALUES), JDPTB, UT1PTB, 0.0D0, &
     &                   0.D0, SPL_UT1, WORK_ARR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9145, IUER, 'GETDB_PARAM', 'Trap of intenral control' )
           RETURN
      END IF
!
! --- Now compute average apriori nutation and average difference between
! --- the apriori nutation and the reference nutation
!
      CALL TAI_TO_TDB ( MJD_BEG, TAI_BEG, TDB_BEG )
      STEP_NAV = ( (MJD_END - MJD_BEG)*86400.0D0 + (TAI_END - TAI_BEG) )/ &
     &           (M_NAV-1)
      TARG_TDB_BEG = (MJD_BEG - J2000__MJD - 0.5D0)*86400.0D0 + TDB_BEG
!
      NUTPSI_AVE = 0.0D0
      NUTEPS_AVE = 0.0D0
      NUTPSI_DIF = 0.0D0
      NUTEPS_DIF = 0.0D0
!
      DO 4120 J12=1,M_NAV
         TARG_TDB = TARG_TDB_BEG + (J12-1)*STEP_NAV
         JD_EP = 2400000.5D0 + MJD_BEG + TAI_BEG/86400.0D0 + &
     &           (J12-1)*STEP_NAV/86400.0D0
         IXC = IXMN8 ( INT4(MAX_EROT_VALUES), JDPTB, JD_EP )
         IF ( IXC .LE. 1  .OR.  IXC .GE. MAX_EROT_VALUES ) THEN
              WRITE ( 6, * ) ' J12=',J12,' IXC=',IXC, ' JD_EP = ', JD_EP
              WRITE ( 6, * ) ' STEP_NAV = ', STEP_NAV
              WRITE ( 6, * ) ' MJD_BEG  = ', MJD_BEG, ' MJD_END= ', MJD_END
              WRITE ( 6, * ) ' TAI_BEG  = ', TAI_BEG, ' TAI_END= ', TAI_END
              WRITE ( 6, * ) ' DUR= ', (MJD_END - MJD_BEG)*86400.0D0 + (TAI_END - TAI_BEG)
              WRITE ( 6, * ) ' JDPTB= ', JDPTB(1:MAX_EROT_VALUES)
              CALL ERR_LOG ( 9146, IUER, 'GETDB_PARAM', 'Trap of intenral '// &
     &                      'control' )
              RETURN
         END IF
!
         TAI_M_UT1 = FSPL8 ( JD_EP, INT4(MAX_EROT_VALUES), JDPTB, UT1PTB, &
     &                       IXC, SPL_UT1 )
         UT1_M_TDB = -TAI_M_UT1 + TAI_BEG - TDB_BEG
!
! ------ This is a controversial conversion aimed to have compatibitliy with
! ------ Calc/Solve.
!
         TARG_TDB = TARG_TDB - UT1_M_TDB ! ???????????????
!
         IF ( VTD%CONF%NUT_EXP == NUT__WAHR1980 ) THEN
              CALL HEO_WAHR1980 ( 4, TARG_TDB, UT1_M_TDB, E1, E2, DPSI, DEPS, &
     &                            E1_RATE, E2_RATE, DPSI_RATE, DEPS_RATE, &
     &                            CROSS_NUT_E3  )
            ELSE IF ( VTD%CONF%NUT_EXP == NUT__IERS1996 ) THEN
              CALL HEO_IERS1996 ( 4, TARG_TDB, UT1_M_TDB, E1, E2, DPSI, DEPS, &
     &                            E1_RATE, E2_RATE, DPSI_RATE, DEPS_RATE, &
     &                            CROSS_NUT_E3  )
            ELSE IF ( VTD%CONF%NUT_EXP == NUT__REN2000 ) THEN
              CALL HEO_REN2000  ( 4, TARG_TDB, UT1_M_TDB, E1, E2, DPSI, DEPS, &
     &                            E1_RATE, E2_RATE, DPSI_RATE, DEPS_RATE, &
     &                            CROSS_NUT_E3 )
            ELSE IF ( VTD%CONF%NUT_EXP == NUT__MHB2000 ) THEN
              CALL HEO_MHB2000  ( 4, TARG_TDB, UT1_M_TDB, E1, E2, DPSI, DEPS, &
     &                            E1_RATE, E2_RATE, DPSI_RATE, DEPS_RATE, &
     &                            CROSS_NUT_E3 )
            ELSE IF ( VTD%CONF%NUT_EXP == NUT__MHB2000_TRANSF ) THEN
              CALL HEO_MHB2000_TRANSF ( 4, TARG_TDB, UT1_M_TDB, E1, E2, &
     &                                  DPSI, DEPS, E1_RATE, E2_RATE, &
     &                                  DPSI_RATE, DEPS_RATE, CROSS_NUT_E3  )
            ELSE IF ( VTD%CONF%NUT_EXP == VTD__NERS ) THEN
              TIM_TAI = TAI_BEG + (J12-1)*STEP_NAV + (MJD_BEG - J2000__MJD)*86400.0D0
              CALL ERR_PASS ( IUER, IER )
              CALL NERS_GET_EOP ( VTD%NERS, TIM_TAI, 'nutr', M_PAR, L_PAR, PARS, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 9147, IUER, 'GETDB_PARAM', 'Error in '// &
     &                 'getting nutation angles using NERS' )
                   RETURN
              END IF
              DPSI = PARS(1)
              DEPS = PARS(2)
              DPSI_RATE = PARS(3)
              DEPS_RATE = PARS(4)
              CALL HEO_MHB2000  ( 4, TARG_TDB, UT1_M_TDB, E1, E2, DPSI, DEPS, &
     &                            E1_RATE, E2_RATE, DPSI_RATE, DEPS_RATE, &
     &                            CROSS_NUT_E3 )
            ELSE IF ( VTD%CONF%NUT_EXP == NUT__UNDF ) THEN
              VTD%CONF%NUT_EXP = VTD__UNDF
              E1           = 0.0D0
              E2           = 0.0D0
              DPSI         = 0.0D0
              DEPS         = 0.0D0
              E1_RATE      = 0.0D0
              E2_RATE      = 0.0D0
              DPSI_RATE    = 0.0D0
              DEPS_RATE    = 0.0D0
              CROSS_NUT_E3 = 0.0D0
            ELSE
              CALL CLRCH ( STR )
              CALL INCH  ( VTD%CONF%NUT_EXP, STR )
              CALL ERR_LOG ( 9148, IUER, 'GETDB_PARAM', 'Unknown nutation '// &
     &            'code '//STR )
              RETURN
         END IF
!
         IF ( VTD%CONF%NUT_GDS .EQ. NUT__GDS_YES ) THEN
!
! ----------- Compute contribution due to geodesic nutation to
! ----------- E1,E2 and DPSI,DEPS
!
              CALL NUT_GEODESIC ( MJD_BEG, TAI_BEG + (J12-1)*STEP_NAV, &
     &                            E1_GDS, E2_GDS, DPSI_GDS, DEPS_GDS )
              E1 = E1 + E1_GDS
              E2 = E2 + E2_GDS
              DPSI = DPSI + DPSI_GDS
              DEPS = DEPS + DEPS_GDS
         END IF
!
         NUTPSI_AVE = NUTPSI_AVE + DPSI/M_NAV
         NUTEPS_AVE = NUTEPS_AVE + DEPS/M_NAV
!
         CALL HEO_WAHR1980 ( 4, TARG_TDB, UT1_M_TDB, E1, E2, DPSI_WAHR, &
     &                       DEPS_WAHR, E1_RATE, E2_RATE, &
     &                       DPSI_RATE, DEPS_RATE, CROSS_NUT_E3 )
         NUTPSI_DIF = NUTPSI_DIF + (DPSI - DPSI_WAHR)/M_NAV
         NUTEPS_DIF = NUTEPS_DIF + (DEPS - DEPS_WAHR)/M_NAV
 4120 CONTINUE
!
! --- Update counters related to parameters
!
      CALL PARCN ()
!
      CALL ERR_PASS      ( IUER, IER )
      CALL GVH_INQ_LCODE ( GVH, 'FRTYPFIT', DESCR, CLASS, TYP, DIMS, &
     &                     NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                     ADR_DATA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9150, IUER, 'GETDB_PARAM', 'Error in '// &
     &         'inquiring lcode FRTYPFIT' )
           RETURN
      END IF
      IF ( CLASS > 0 ) THEN
           CALL ERR_PASS      ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'FRTYPFIT', 1, 1, 4, &
     &                       DIMS(1), DIMS(2), FRTYPFIT, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9151, IUER, 'GETDB_PARAM', 'Error in '// &
     &              'getting "FRTYPFIT" lcode' )
                RETURN
           END IF
        ELSE
           CALL ERR_PASS      ( IUER, IER )
           CALL GVH_INQ_LCODE ( GVH, 'PIMA_CNT', DESCR, CLASS, TYP, DIMS, &
     &                         NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                         ADR_DATA, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9152, IUER, 'GETDB_PARAM', 'Error in '// &
     &              'inquiring lcode PIMA_CNT' )
                RETURN
           END IF
           IF ( CLASS > 0 ) THEN
                FRTYPFIT = PHS_GRP_PIMA__FTP
              ELSE 
                FRTYPFIT = PHS_GRP_HOPS__FTP
           END IF
      END IF
!
      VSITEC_TIM_REF_MJD = VTD%STA(1)%MJD_REF + VTD%STA(1)%TAI_REF/86400.0D0 + 2400000.5D0
      CALL USE_COMMON ( 'OWC' )
      CALL USE_PARFIL ( 'OWC' )
!
! --- Initialization some fields in GLBC4
!
      PART_APPLIED = 0
      RDUMMY = 1.0D0
      CDUMMY = '    '
      CALL FLYBY_INIT ( 'NONE', 'NONE', 'NONE', 'NONE', 'NONE', &
     &                  'NONE',  TIME0, 'NONE', CDUMMY, CDUMMY, &
     &                   CDUMMY, CDUMMY, RDUMMY, 'NONE', 'NONE', &
     &                  'NONE', 'NONE' )
      CALL SHFEOP_INT ( 'NONE' )
      HFEOPF_CHR = 'NONE'
      IF ( .NOT. FL_BATCH ) THEN
           CALL CLRCH ( USER_PROG_NAME  )
           CALL CLRCH ( USER_PROG_BUFF  )
           CALL CLRCH ( USER_PART_PROG  )
           CALL CLRCH ( USER_CONST_PROG )
           CALL CLRCH ( MERGCGM )
           ARCPE_WORKED = .FALSE.
           CRES_WORKED  = .FALSE.
           KUSER_PART   = .FALSE.
           KUSER_CONST  = .FALSE.
           NUM_USER_PART = 0
!
           IOCGM = 0
           CALL CLRCH ( ARCDIR(1) )
           CALL CLRCH ( ARCDIR(2) )
           CALL CLRCH ( ARCDIR(3) )
           ICONT = 0
           ISOLU = 0
           INAMCG = ' '
           CALL CLRCH ( FINAM_HEO )
           CALL CLRCH ( NAME_HEO )
           L_HEO    = 0
           ADR_HEO  = 0
           L_BSP    = 0
           ADR_BSP  = 0
           STAT_HEO = HEO__UNDF
           HEO_EPOCH_SEC = 0
!
           L_HPE = 0
           ADR_HPE = 0
           L_SPE = 0
           ADR_SPE = 0
           N_POSVAR = 0
!
           DEFCMP = 0
           DEFVEL = 0
           DEFSRC = 0
           ISTASP = 0
           ISRCSP = 0
           SNR_MIN_X = 0.0D0
           SNR_MIN_S = 0.0D0
           IOS_EST   = IOS__UNDF
           IOS_SIG   = IOS__SIG_DEF
           CALL CLRCH ( EXT_ERR_FIL )
      END IF
!
      CALL USE_GLBFIL_4 ( 'OW' )
      CALL USE_GLBFIL_3 ( 'W' )
      CALL USE_GLBFIL   ( 'WC' )
!
! --- Do a final pass to make sure all glbfil variables important to the
! --- interactive mode are set properly.  (This may reset some previously
! --- set variables, because this is an effort to set problem variables
! --- that have been set in some places, but not in others.)
!
      IF ( .NOT. FL_BATCH ) THEN
           CALL INIT_GLB_CM4 ( 'NONE' )
           CALL USE_GLBFIL_4 ( 'OR' )
           NUT_USE_CODE = NUT__PSE
           CALL USE_GLBFIL_4 ( 'WC' )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GETDB_PARAM  !#!#
