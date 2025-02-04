      SUBROUTINE  UPTDB_SOLSETUP ( GVH, N_BAS, NUM_CLRF, STA_CLRF, &
     &                             NUM_CLBR, STA_CLBR, MJD_CLBR, UTC_CLBR, &
     &                             L_ACM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine UPTDB_SOLSETUP 
! *                                                                      *
! * ### 05-DEC-2005  UPTDB_SOLSETUP  v1.7 (c) L. Petrov 03-FEB-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'erm.i'
      INCLUDE   'socom.i'
      INCLUDE   'socom_plus.i'
      INCLUDE   'gvh.i'
      INCLUDE   'prfil.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'glbc3.i'
      TYPE     ( GVH__STRU ) :: GVH
      INTEGER*4  N_BAS, NUM_CLRF, NUM_CLBR, L_ACM, IUER
      INTEGER*4  M_CLBR
      PARAMETER  ( M_CLBR = MAX_ARC_STA*MAX4_BRK )
      CHARACTER  C_STA(MAX_ARC_STA)*8, STA_CLRF(MAX_ARC_STA)*8, &
     &           STA_CLBR(MAX4_BRK)*8, STAT_ACM(M_ACM)*8
      CHARACTER  STR*128
      REAL*8     UTC_CLBR(MAX4_BRK), CLOOF_ACM(M_ACM), CLODR_ACM(M_ACM)
      REAL*8     DEF_DEL_WEI, DEF_RAT_WEI
      PARAMETER  ( DEF_DEL_WEI =  8.0D-12 )
      PARAMETER  ( DEF_RAT_WEI = 50.0D-15 )
      INTEGER*4  MJD_CLBR(MAX4_BRK)
      INTEGER*2  IERR_I2, IPAR_I2
      CHARACTER  RW_BAS_NAM_OLD(2,MAX_ARC_BSL)*8
      CHARACTER, ALLOCATABLE :: C_BAS(:)*16
      INTEGER*2, ALLOCATABLE :: DGCL_EST(:,:)
      INTEGER*4, ALLOCATABLE :: BSCL_EST(:),     BAS_USE(:)
      REAL*8,    ALLOCATABLE :: CLO_CNS(:,:),    ATM_CNS(:,:),  &
     &                          TLOF_CNS(:,:),   TLRT_CNS(:,:), &
     &                          STPS_CNS(:,:),   SOCO_CNS(:,:), &
     &                          BSCL_CNS(:,:), &
     &                          RW_DEL(:,:),     RW_RAT(:,:)
      REAL*8     BSCL_CNS_OLD(0:SLV__MAX_SOLTYP-1,MAX_ARC_BSL), &
     &           RW_DEL_OLD(0:SLV__MAX_SOLTYP-1,MAX_ARC_BSL),  & 
     &           RW_RAT_OLD(0:SLV__MAX_SOLTYP-1,MAX_ARC_BSL)
      REAL*8     CLO_INTR(0:SLV__MAX_SOLTYP-1), &
     &           ATM_INTR(0:SLV__MAX_SOLTYP-1), &
     &           TIL_INTR(0:SLV__MAX_SOLTYP-1), &
     &           EOP_CNS(11,0:SLV__MAX_SOLTYP-1)
      REAL*8     VAL1, VAL2, VAL3, VAL4
      ADDRESS__TYPE  ADR_DATA
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, L_BAS, &
     &           EOP_EST(11), STPS_EST(3,MAX_ARC_STA), &
     &           SOCO_EST(2,MAX_ARC_SRC), STA_USE(MAX_ARC_SRC), &
     &           SOU_USE(MAX_ARC_SRC), NUM_BAS_RW_OLD, IND_BAS_RW_OLD, &
     &           IER
      LOGICAL*4  FL_CLOCK_EST 
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*2, EXTERNAL :: IROTT
      LOGICAL*2, EXTERNAL :: KBIT
      LOGICAL*4, EXTERNAL :: CHECK_STABIT, DATYP_INQ, IS_R8_NAN
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      ALLOCATE ( DGCL_EST(NUMSTA,0:SLV__MAX_SOLTYP-1) )
      ALLOCATE ( BSCL_EST(N_BAS)                      )
      ALLOCATE (  BAS_USE(N_BAS)                      )
      ALLOCATE (  CLO_CNS(NUMSTA,0:SLV__MAX_SOLTYP-1) )
      ALLOCATE (  ATM_CNS(NUMSTA,0:SLV__MAX_SOLTYP-1) )
      ALLOCATE ( TLOF_CNS(NUMSTA,0:SLV__MAX_SOLTYP-1) )
      ALLOCATE ( TLRT_CNS(NUMSTA,0:SLV__MAX_SOLTYP-1) )
      ALLOCATE ( STPS_CNS(NUMSTA,0:SLV__MAX_SOLTYP-1) )
      ALLOCATE ( SOCO_CNS(NUMSTR,0:SLV__MAX_SOLTYP-1) )
      ALLOCATE ( BSCL_CNS(N_BAS,0:SLV__MAX_SOLTYP-1)  )
      ALLOCATE ( C_BAS(N_BAS)                         )
      ALLOCATE ( RW_DEL(0:SLV__MAX_SOLTYP-1,N_BAS)    )
      ALLOCATE ( RW_RAT(0:SLV__MAX_SOLTYP-1,N_BAS)    )
!
! --- Collect information about clock reference station(s) and clock
! --- break(s)
!
      L_BAS  = 0
      CALL NOUT_I4 ( 3*INT4(MAX_ARC_STA), STPS_EST )
      CALL NOUT_I4 ( 2*INT4(MAX_ARC_SRC), SOCO_EST )
      CALL NOUT_I4 ( N_BAS,               BSCL_EST )
      CALL NOUT_R8 ( INT4(NUMSTA)*SLV__MAX_SOLTYP, CLO_CNS  )
      CALL NOUT_R8 ( INT4(NUMSTA)*SLV__MAX_SOLTYP, ATM_CNS  )
      CALL NOUT_R8 ( INT4(NUMSTA)*SLV__MAX_SOLTYP, TLOF_CNS )
      CALL NOUT_R8 ( INT4(NUMSTA)*SLV__MAX_SOLTYP, TLRT_CNS )
      CALL NOUT_R8 ( 11*SLV__MAX_SOLTYP,           EOP_CNS  )
      CALL NOUT_R8 ( INT4(NUMSTA)*SLV__MAX_SOLTYP, STPS_CNS )
      CALL NOUT_R8 ( INT4(NUMSTR)*SLV__MAX_SOLTYP, SOCO_CNS )
      CALL NOUT_R8 ( N_BAS*SLV__MAX_SOLTYP,        BSCL_CNS )
      CALL NOUT_I4 ( INT4(NUMSTA), STA_USE  )
      CALL NOUT_I4 ( INT4(NUMSTR), SOU_USE  )
      CALL NOUT_I4 ( N_BAS,        BAS_USE  )
!
      CALL NOUT_R8 ( SLV__MAX_SOLTYP*INT4(MAX_ARC_BSL), BSCL_CNS_OLD )
      CALL NOUT_R8 ( SLV__MAX_SOLTYP*INT4(MAX_ARC_BSL), RW_DEL_OLD   )
      CALL NOUT_R8 ( SLV__MAX_SOLTYP*INT4(MAX_ARC_BSL), RW_RAT_OLD   )
!
      CALL USE_GLBFIL_3 ( 'ORC' )
      IF ( META_N_STA .NE. NUMSTA ) THEN
           WRITE ( 6, * ) ' META_N_STA = ', META_N_STA, ' NUMSTA = ', NUMSTA
!@           CALL ERR_LOG ( 6611, IUER, 'UPTDB_SOLSETUP', 'Trap of internal '// &
!@     &         'control: META_N_STA is not equal to NUMSTA' )
!@           RETURN 
      END IF
      IF ( META_N_SOU .NE. NUMSTR ) THEN
           WRITE ( 6, * ) ' META_N_SOU = ', META_N_SOU, ' NUMSTR = ', NUMSTR
!@           CALL ERR_LOG ( 6612, IUER, 'UPTDB_SOLSETUP', 'Trap of internal '// &
!@     &         'control: META_N_SOU is not equal to NUMSTR' )
!@           RETURN 
      END IF
!
      IF ( META_N_STA > 0 .AND. &
     &     META_N_BAS > 0 .AND. &
     &     META_N_SOU > 0       ) THEN
           CALL META_VAR_EXCHANGE ( 2, META_N_BAS, META_N_STA, META_N_SOU, &
     &                                 CLO_INTR, ATM_INTR, TIL_INTR, &
     &                                 EOP_CNS, CLO_CNS, ATM_CNS,  &
     &                                 TLOF_CNS, TLRT_CNS, STPS_CNS, &
     &                                 SOCO_CNS, DGCL_EST, BSCL_CNS_OLD, &
     &                                 RW_DEL_OLD, RW_RAT_OLD, RW_BAS_NAM_OLD )
      END IF
      NUM_BAS_RW_OLD = META_N_BAS
!
      DO 410 J1=1,NUMSTA
         C_STA(J1) = ISITN_CHR(J1)
         CALL VTD_NAME_REPAIR ( C_STA(J1) )
!
         DGCL_EST(J1,IDATYP) = NPL_CLO
         DGCL_EST(J1,SLV__MAX_SOLTYP-1) = DGCL_EST(J1,IDATYP) 
         CLO_INTR(IDATYP) = CLOCK_INTERVAL*3600.0D0
         CLO_INTR(SLV__MAX_SOLTYP-1) = CLO_INTR(IDATYP) 
         ATM_INTR(IDATYP) = ATMOS_INTERVAL*3600.0D0
         ATM_INTR(SLV__MAX_SOLTYP-1) = ATM_INTR(IDATYP) 
         TIL_INTR(IDATYP) = GRAD_INTERVAL*3600.0D0
         TIL_INTR(SLV__MAX_SOLTYP-1) = TIL_INTR(IDATYP) 
!
         IF ( KBIT(LSITEC(1,1), INT2(J1)) ) THEN
              STPS_EST(1,J1) = IBSET ( STPS_EST(1,J1), INT4(IDATYP) )
              STPS_EST(1,J1) = IBSET ( STPS_EST(1,J1), SLV__MAX_SOLTYP-1 )
         END IF
         IF ( KBIT(LSITEC(1,2), INT2(J1)) ) THEN
              STPS_EST(2,J1) = IBSET ( STPS_EST(2,J1), INT4(IDATYP) )
              STPS_EST(2,J1) = IBSET ( STPS_EST(2,J1), SLV__MAX_SOLTYP-1 )
         END IF
         IF ( KBIT(LSITEC(1,3), INT2(J1)) ) THEN
              STPS_EST(3,J1) = IBSET ( STPS_EST(3,J1), INT4(IDATYP) )
              STPS_EST(3,J1) = IBSET ( STPS_EST(3,J1), SLV__MAX_SOLTYP-1 )
         END IF
         CLO_CNS(J1,IDATYP) = SCCNST(J1)*1.D-14
         CLO_CNS(J1,SLV__MAX_SOLTYP-1) = SCCNST(J1)*1.D-14
         ATM_CNS(J1,IDATYP) = SACNST(J1)*1.D-12/3600.0D0
         ATM_CNS(J1,SLV__MAX_SOLTYP-1) = SACNST(J1)*1.D-12/3600.0D0
         IF ( KBIT( CONSTRAINT_BITS, INT2(8) ) ) THEN 
                    TLOF_CNS(J1,IDATYP) = GRADCONS(2)*MM__TO__SEC
                    TLOF_CNS(J1,SLV__MAX_SOLTYP-1) = TLOF_CNS(J1,IDATYP) 
                    TLRT_CNS(J1,IDATYP) = GRADCONS(1)*MM__TO__SEC/86400.0D0
                    TLRT_CNS(J1,SLV__MAX_SOLTYP-1) = TLRT_CNS(J1,IDATYP) 
         END IF
         STPS_CNS(J1,IDATYP) = STA_WEA_SIGMA
         STPS_CNS(J1,SLV__MAX_SOLTYP-1) = STPS_CNS(J1,IDATYP) 
!
         DGCL_EST(J1,IDATYP) = NPL_CLO
         DGCL_EST(J1,SLV__MAX_SOLTYP-1) = DGCL_EST(J1,IDATYP) 
         CLO_INTR(IDATYP) = CLOCK_INTERVAL*3600.0D0
         CLO_INTR(SLV__MAX_SOLTYP-1) = CLO_INTR(IDATYP) 
         ATM_INTR(IDATYP) = ATMOS_INTERVAL*3600.0D0
         ATM_INTR(SLV__MAX_SOLTYP-1) = ATM_INTR(IDATYP) 
         TIL_INTR(IDATYP) = GRAD_INTERVAL*3600.0D0
         TIL_INTR(SLV__MAX_SOLTYP-1) = TIL_INTR(IDATYP) 
!
         IF ( IDATYP == GRPRAT__DTP .OR. &
     &        IDATYP == SNBRAT__DTP .OR. &
     &        IDATYP == GRPONL__DTP .OR. &
     &        IDATYP == SNBONL__DTP .OR. &
     &        IDATYP == RATONL__DTP .OR. &
     &        IDATYP ==  G_GXS__DTP .OR. &
     &        IDATYP ==    GX__DTP  .OR. &
     &        IDATYP ==    GS__DTP  .OR. &
     &        IDATYP == SNG_X__DTP  .OR. &
     &        IDATYP == SNG_S__DTP       ) THEN
              IF ( KBIT ( STABIT_G, INT2(J1) ) ) THEN
                   STA_USE(J1) = IBSET ( STA_USE(J1), IDATYP )
              END IF
         END IF
!
         IF ( IDATYP == PHSRAT__DTP .OR. &
     &        IDATYP == PHSONL__DTP .OR. &
     &        IDATYP == PX_GXS__DTP .OR. &
     &        IDATYP == PS_GXS__DTP .OR. &
     &        IDATYP ==  PX_GX__DTP .OR. &
     &        IDATYP ==  PX_GS__DTP .OR. &
     &        IDATYP ==  PS_GX__DTP .OR. &
     &        IDATYP ==  PS_GS__DTP .OR. &
     &        IDATYP ==  P_PXS__DTP .OR. &
     &        IDATYP ==     PX__DTP .OR. &
     &        IDATYP ==     PS__DTP      ) THEN
              IF ( KBIT ( STABIT_P, INT2(J1) ) ) THEN
                   STA_USE(J1) = IBSET ( STA_USE(J1), IDATYP )
              END IF
         END IF
!
         IF ( J1 < NUMSTA ) THEN
              DO 430 J3=J1+1,NUMSTA
                 C_STA(J3) = ISITN_CHR(J3)
                 CALL VTD_NAME_REPAIR ( C_STA(J3) )
                 L_BAS = L_BAS + 1
                 IND_BAS_RW_OLD = 0
                 IF ( NUM_BAS_RW_OLD > 0 ) THEN
                      DO 440 J4=1,NUM_BAS_RW_OLD
                         CALL VTD_NAME_REPAIR ( RW_BAS_NAM_OLD(1,J4) )
                         CALL VTD_NAME_REPAIR ( RW_BAS_NAM_OLD(2,J4) )
                         IF ( RW_BAS_NAM_OLD(1,J4) == C_STA(J1) .AND. &
     &                        RW_BAS_NAM_OLD(2,J4) == C_STA(J3)       ) THEN
                              IND_BAS_RW_OLD = J4
                         END IF
                         IF ( RW_BAS_NAM_OLD(1,J4) == C_STA(J3) .AND. &
     &                        RW_BAS_NAM_OLD(2,J4) == C_STA(J1)       ) THEN
                              IND_BAS_RW_OLD = J4
                         END IF
 440                  CONTINUE 
                 END IF
!
                 IPAR_I2 = 0
                 IF ( L_BAS == 1 ) IPAR_I2 = 1
                 CALL GETCARD ( INT2(1), 'REWT', IPAR_I2, STR, IERR_I2 )
                 C_BAS(L_BAS) = STR(6:13)//STR(15:22)
                 CALL VTD_NAME_REPAIR ( C_BAS(L_BAS)(1:8)  )
                 CALL VTD_NAME_REPAIR ( C_BAS(L_BAS)(9:16) )
                 IF ( STR(34:42) == '*********'  ) STR(34:42) = '     0.00'
                 IF ( STR(34:42) == '*****    '  ) STR(34:42) = '     0.00'
                 IF ( STR(44:52) == '*********'  ) STR(44:52) = '     0.00'
                 IF ( STR(44:52) == '*****    '  ) STR(44:52) = '     0.00'
                 IF ( STR(49:52) == ' NaN' ) STR(49:52) = '0.00'
                 IF ( STR(59:62) == ' NaN' ) STR(59:62) = '0.00'
                 READ ( UNIT=STR(23:32), FMT='(F10.5)' ) VAL1
                 READ ( UNIT=STR(33:42), FMT='(F10.5)' ) VAL2
                 READ ( UNIT=STR(43:52), FMT='(F10.5)' ) VAL3
                 READ ( UNIT=STR(53:62), FMT='(F10.5)' ) VAL4
                 IF ( IS_R8_NAN(VAL1) ) VAL1 = 1.D7
                 IF ( IS_R8_NAN(VAL2) ) VAL2 = 1.D7
                 IF ( IS_R8_NAN(VAL3) ) VAL3 = 1.D7
                 IF ( IS_R8_NAN(VAL4) ) VAL4 = 1.D7
                 IF ( VAL1 >  1.D6  ) VAL1 =  1.D7
                 IF ( VAL1 < -1.D6  ) VAL1 = -1.D7
                 IF ( VAL2 >  1.D6  ) VAL2 =  1.D7
                 IF ( VAL2 < -1.D6  ) VAL2 = -1.D7
                 IF ( VAL3 >  1.D6  ) VAL3 =  1.D7
                 IF ( VAL3 < -1.D6  ) VAL3 = -1.D7
                 IF ( VAL4 >  1.D6  ) VAL4 =  1.D7
                 IF ( VAL4 < -1.D6  ) VAL4 = -1.D7
!
                 DO 450 J5=0,SLV__MAX_SOLTYP-1
                    IF ( IND_BAS_RW_OLD > 0 ) THEN
                         RW_DEL(J5,L_BAS) = RW_DEL_OLD(J5,IND_BAS_RW_OLD) 
                         RW_RAT(J5,L_BAS) = RW_RAT_OLD(J5,IND_BAS_RW_OLD) 
                    END IF
!
                    IF ( J5 == IDATYP            .OR. &
     &                   J5 == SLV__MAX_SOLTYP-1      ) THEN
                         IF ( KBIT(ICLOCK(1,J1), INT2(J3)) .OR. &
     &                        KBIT(ICLOCK(1,J3), INT2(J1))      ) THEN
                              BSCL_EST(L_BAS) = IBSET ( BSCL_EST(L_BAS), J5 ) 
                              IF ( KBSL_CONST ) THEN
                                   BSCL_CNS(L_BAS,J5) = BAS_CLK_SIGMA
                              END IF
                         END IF
                         IF ( DATYP_INQ ( IDATYP, PX__DTP    ) .OR. &
     &                        DATYP_INQ ( IDATYP, PS__DTP    ) .OR. &
     &                        DATYP_INQ ( IDATYP, PX_GX__DTP ) .OR. &
     &                        DATYP_INQ ( IDATYP, PX_GS__DTP ) .OR. &
     &                        DATYP_INQ ( IDATYP, PS_GX__DTP ) .OR. &
     &                        DATYP_INQ ( IDATYP, PS_GS__DTP ) .OR. &
     &                        DATYP_INQ ( IDATYP, P_PXS__DTP )      ) THEN
!
! --------------------------- Phase delay soltions
!
                              RW_DEL(J5,L_BAS) = VAL3*1.D-12
                              RW_RAT(J5,L_BAS) = VAL4*1.D-15
                           ELSE 
!
! --------------------------- Group delay solutions
!
                              RW_DEL(J5,L_BAS) = VAL1*1.D-12
                              RW_RAT(J5,L_BAS) = VAL2*1.D-15
                         END IF
                    END IF
                    IF ( J5 == GRPRAT__DTP .OR. &
     &                   J5 == SNBRAT__DTP .OR. &
     &                   J5 == GRPONL__DTP .OR. &
     &                   J5 == SNBONL__DTP .OR. &
     &                   J5 == RATONL__DTP .OR. &
     &                   J5 ==  G_GXS__DTP .OR. &
     &                   J5 ==    GX__DTP  .OR. &
     &                   J5 ==    GS__DTP  .OR. &
     &                   J5 == SNG_X__DTP  .OR. &
     &                   J5 == SNG_S__DTP  .OR. &
     &                   J5 == FUSED__DTP        ) THEN
!
                         IF ( KBIT ( IBLSEL_G(1,J1), INT2(J3) ) ) THEN
                              BAS_USE(L_BAS) = IBSET ( BAS_USE(L_BAS), J5 ) 
                         END IF
                    END IF
                    IF ( J5 == PHSRAT__DTP .OR. &
     &                   J5 == PHSONL__DTP .OR. &
     &                   J5 == PX_GXS__DTP .OR. &
     &                   J5 == PS_GXS__DTP .OR. &
     &                   J5 ==  PX_GX__DTP .OR. &
     &                   J5 ==  PX_GS__DTP .OR. &
     &                   J5 ==  PS_GX__DTP .OR. &
     &                   J5 ==  PS_GS__DTP .OR. &
     &                   J5 ==  P_PXS__DTP .OR. &
     &                   J5 ==     PX__DTP .OR. &
     &                   J5 ==     PS__DTP      ) THEN
                         IF ( KBIT ( IBLSEL_P(1,J1), INT2(J3) ) ) THEN
                              BAS_USE(L_BAS) = IBSET ( BAS_USE(L_BAS), J5 ) 
                         END IF
                    END IF
                    IF ( J5 == SLV__MAX_SOLTYP-1 ) THEN
                         IF ( KBIT ( IBLSEL_G(1,J1), INT2(J3) ) ) THEN
                              BAS_USE(L_BAS) = IBSET ( BAS_USE(L_BAS), J5 ) 
                         END IF
                    END IF
 450             CONTINUE 
 430          CONTINUE 
         END IF
 410  CONTINUE 
!
      CALL NOUT_I2 ( 11, EOP_EST )
      IF ( EOP_STYLE(1) .EQ. EOP__POLY ) THEN
           IF ( IROTT(1,1,1,LROT) == 1 ) THEN
                EOP_EST(1) = IBSET ( EOP_EST(1), INT4(IDATYP)      )
                EOP_EST(1) = IBSET ( EOP_EST(1), SLV__MAX_SOLTYP-1 )
           END IF
           IF ( IROTT(1,2,1,LROT) == 1 ) THEN
                EOP_EST(2) = IBSET ( EOP_EST(2), INT4(IDATYP)      )
                EOP_EST(2) = IBSET ( EOP_EST(2), SLV__MAX_SOLTYP-1 )
           END IF
           IF ( IROTT(1,1,2,LROT) == 1 ) THEN
                EOP_EST(4) = IBSET ( EOP_EST(4), INT4(IDATYP)      )
                EOP_EST(4) = IBSET ( EOP_EST(4), SLV__MAX_SOLTYP-1 )
           END IF
           IF ( IROTT(1,2,2,LROT) == 1 ) THEN
                EOP_EST(5) = IBSET ( EOP_EST(5), INT4(IDATYP)      )
                EOP_EST(5) = IBSET ( EOP_EST(5), SLV__MAX_SOLTYP-1 )
           END IF
           IF ( IROTT(1,1,3,LROT) == 1 ) THEN
                EOP_EST(7) = IBSET ( EOP_EST(7), INT4(IDATYP)      )
                EOP_EST(7) = IBSET ( EOP_EST(7), SLV__MAX_SOLTYP-1 )
           END IF
           IF ( IROTT(1,2,3,LROT) == 1 ) THEN
                EOP_EST(8) = IBSET ( EOP_EST(8), INT4(IDATYP)      )
                EOP_EST(8) = IBSET ( EOP_EST(8), SLV__MAX_SOLTYP-1 )
           END IF
      END IF
!
      IF ( EOP_STYLE(2) .EQ. EOP__POLY ) THEN
           IF ( IROTT(1,3,1,LROT) == 1 ) THEN
                EOP_EST(3) = IBSET ( EOP_EST(3), INT4(IDATYP)      )
                EOP_EST(3) = IBSET ( EOP_EST(3), SLV__MAX_SOLTYP-1 )
           END IF
           IF ( IROTT(1,3,2,LROT) == 1 ) THEN
                EOP_EST(6) = IBSET ( EOP_EST(6), INT4(IDATYP)      )
                EOP_EST(6) = IBSET ( EOP_EST(6), SLV__MAX_SOLTYP-1 )
           END IF
           IF ( IROTT(1,3,3,LROT) == 1 ) THEN
                EOP_EST(9) = IBSET ( EOP_EST(9), INT4(IDATYP)      )
                EOP_EST(9) = IBSET ( EOP_EST(9), SLV__MAX_SOLTYP-1 )
           END IF
      END IF
!
      IF ( KBIT(LNUT,1) ) THEN
           EOP_EST(10) = IBSET ( EOP_EST(10), INT4(IDATYP)      )
           EOP_EST(10) = IBSET ( EOP_EST(10), SLV__MAX_SOLTYP-1 )
      END IF
!
      IF ( KBIT(LNUT,2) ) THEN
           EOP_EST(11) = IBSET ( EOP_EST(11), INT4(IDATYP)      )
           EOP_EST(11) = IBSET ( EOP_EST(11), SLV__MAX_SOLTYP-1 )
      END IF
!
      DO 460 J6=1,NUMSTR
         IF ( KSRC_CONST ) THEN
              SOCO_CNS(J6,IDATYP) = SRC_COO_SIGMA
              SOCO_CNS(J6,SLV__MAX_SOLTYP-1) = SOCO_CNS(J6,IDATYP) 
         END IF
         IF ( KBIT(ISRSEL(1), INT2(J6)) ) THEN
              SOU_USE(J6) = IBSET ( SOU_USE(J6), INT4(IDATYP) ) 
              SOU_USE(J6) = IBSET ( SOU_USE(J6), SLV__MAX_SOLTYP-1 ) 
            ELSE 
              SOU_USE(J6) = IBCLR ( SOU_USE(J6), INT4(IDATYP) ) 
              SOU_USE(J6) = IBCLR ( SOU_USE(J6), SLV__MAX_SOLTYP-1 ) 
         END IF
!
         IF ( KBIT(LSTAR(1,1), INT2(J6)) ) THEN 
              SOCO_EST(1,J6) = IBSET ( SOCO_EST(1,J6), INT4(IDATYP) )
              SOCO_EST(1,J6) = IBSET ( SOCO_EST(1,J6), SLV__MAX_SOLTYP-1 )
         END IF         
         IF ( KBIT(LSTAR(1,2), INT2(J6)) ) THEN 
              SOCO_EST(2,J6) = IBSET ( SOCO_EST(2,J6), INT4(IDATYP) )
              SOCO_EST(2,J6) = IBSET ( SOCO_EST(2,J6), SLV__MAX_SOLTYP-1 )
         END IF         
 460  CONTINUE 
!
      DO 480 J8=0,SLV__MAX_SOLTYP-1
         EOP_CNS(1,IDATYP) = EOPCONS(1)*MAS__TO__RAD
         EOP_CNS(1,SLV__MAX_SOLTYP-1) = EOP_CNS(1,IDATYP) 
         EOP_CNS(2,IDATYP) = EOPCONS(2)*MAS__TO__RAD
         EOP_CNS(2,SLV__MAX_SOLTYP-1) = EOP_CNS(2,IDATYP) 
         EOP_CNS(3,IDATYP) = EOPCONS(3)*MSEC__TO__RAD
         EOP_CNS(3,SLV__MAX_SOLTYP-1) = EOP_CNS(3,IDATYP) 
         EOP_CNS(4,IDATYP) = EOPCONS(1)*MAS__TO__RAD/86400.0D0
         EOP_CNS(4,SLV__MAX_SOLTYP-1) = EOP_CNS(4,IDATYP) 
         EOP_CNS(5,IDATYP) = EOPCONS(2)*MAS__TO__RAD/86400.0D0
         EOP_CNS(5,SLV__MAX_SOLTYP-1) = EOP_CNS(5,IDATYP) 
         EOP_CNS(6,IDATYP) = EOPCONS(3)*MSEC__TO__RAD/86400.0D0
         EOP_CNS(6,SLV__MAX_SOLTYP-1) = EOP_CNS(6,IDATYP) 
 480  CONTINUE 
!
! --- Put into the database information about clock reference stations
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'NUM_CLRF', 1, 0, NUM_CLRF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6611, IUER, 'UPTDB_SOLSETUP', 'Error '// &
     &         'in putting "NUM_CLRF" lcode' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'STA_CLRF', 1, 0, STA_CLRF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6612, IUER, 'UPTDB_SOLSETUP', 'Error '// &
     &         'in putting "STA_CLRF" lcode' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'NUM_CLBR', 1, 0, NUM_CLBR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6613, IUER, 'UPTDB_SOLSETUP', 'Error '// &
     &         'in putting "NUM_CLBR" lcode' )
           RETURN 
      END IF
!
      IF ( NUM_CLBR > 0 ) THEN
!
! -------- Put into the database information about clock breaks
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PLCODE ( GVH, 'STA_CLBR', 1, 0, STA_CLBR, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6614, IUER, 'UPTDB_SOLSETUP', 'Error '// &
     &              'in putting "STA_CLBR" lcode' )
                RETURN 
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PLCODE ( GVH, 'MJD_CLBR', 1, 0, MJD_CLBR, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6615, IUER, 'UPTDB_SOLSETUP', 'Error '// &
     &              'in putting "MJD_CLBR" lcode' )
                RETURN 
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PLCODE ( GVH, 'UTC_CLBR', 1, 0, UTC_CLBR, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6616, IUER, 'UPTDB_SOLSETUP', 'Error '// &
     &              'in putting "UTC_CLBR" lcode' )
                RETURN 
           END IF
         ELSE 
      END IF
!
      IF ( L_ACM > 0 ) THEN
           DO 490 J9=1,L_ACM
              IPAR_I2 = 0
              IF ( J9 == 1 ) IPAR_I2 = 1
              CALL GETCARD ( INT2(1), 'ACM ', IPAR_I2, STR, IERR_I2 )
              STAT_ACM(J9) = STR(15:22)
              READ ( UNIT=STR(25:46), FMT='(F22.16)' ) CLOOF_ACM(J9)
              READ ( UNIT=STR(49:70), FMT='(F22.16)' ) CLODR_ACM(J9)
 490       CONTINUE 
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PLCODE ( GVH, 'STAT_ACM', 1, 0, %REF(STAT_ACM), IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6617, IUER, 'UPTDB_SOLSETUP', 'Error in '// &
     &              'putting "STAT_ACM" lcode' )
                RETURN 
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PLCODE ( GVH, 'CLOOFACM', 1, 0, CLOOF_ACM, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6618, IUER, 'UPTDB_SOLSETUP', 'Error in '// &
     &              'putting "CLOOFACM" lcode' )
                RETURN 
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PLCODE ( GVH, 'CLODRACM', 1, 0, CLODR_ACM, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6619, IUER, 'UPTDB_SOLSETUP', 'Error in '// &
     &              'putting "CLODRACM" lcode' )
                RETURN 
           END IF
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'DATYP   ', 1, 0, IDATYP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6620, IUER, 'UPTDB_SOLSETUP', 'Error in '// &
     &         'putting "DATYP   " lcode' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'SUPMET  ', 1, 0, SUPMET, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6621, IUER, 'UPTDB_SOLSETUP', 'Error in '// &
     &         'putting "SUPMET  " lcode' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'DGCL_EST', 1, 0, DGCL_EST, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6622, IUER, 'UPTDB_SOLSETUP', 'Error in '// &
     &         'putting "DGCL_EST" lcode' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'CLO_INTR', 1, 0, CLO_INTR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6623, IUER, 'UPTDB_SOLSETUP', 'Error in '// &
     &         'putting "CLO_INTR" lcode' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'ATM_INTR', 1, 0, ATM_INTR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6624, IUER, 'UPTDB_SOLSETUP', 'Error in '// &
     &         'putting "ATM_INTR" lcode' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'TIL_INTR', 1, 0, TIL_INTR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6625, IUER, 'UPTDB_SOLSETUP', 'Error in '// &
     &         'putting "TIL_INTR" lcode' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'EOP_EST ', 1, 0, EOP_EST, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6626, IUER, 'UPTDB_SOLSETUP', 'Error in '// &
     &         'putting "EOP_EST" lcode' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'STPS_EST', 1, 0, STPS_EST, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6627, IUER, 'UPTDB_SOLSETUP', 'Error in '// &
     &         'putting "STPS_EST" lcode' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'SOCO_EST', 1, 0, SOCO_EST, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6628, IUER, 'UPTDB_SOLSETUP', 'Error in '// &
     &         'putting "SOCO_EST" lcode' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'BSCL_EST', 1, 0, BSCL_EST, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6629, IUER, 'UPTDB_SOLSETUP', 'Error in '// &
     &         'putting "BSCL_EST" lcode' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'CLO_CNS ', 1, 0, CLO_CNS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6630, IUER, 'UPTDB_SOLSETUP', 'Error in '// &
     &         'putting "CLO_CNS " lcode' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'ATM_CNS ', 1, 0, ATM_CNS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6631, IUER, 'UPTDB_SOLSETUP', 'Error in '// &
     &         'putting "ATM_CNS " lcode' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'TLOF_CNS', 1, 0, TLOF_CNS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6632, IUER, 'UPTDB_SOLSETUP', 'Error in '// &
     &         'putting "TLOF_CNS" lcode' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'TLRT_CNS', 1, 0, TLRT_CNS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6633, IUER, 'UPTDB_SOLSETUP', 'Error in '// &
     &         'putting "TLRT_CNS" lcode' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'EOP_CNS ', 1, 0, EOP_CNS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6634, IUER, 'UPTDB_SOLSETUP', 'Error in '// &
     &         'putting "EOP_CNS " lcode' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'STPS_CNS', 1, 0, STPS_CNS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6635, IUER, 'UPTDB_SOLSETUP', 'Error in '// &
     &         'putting "STPS_CNS" lcode' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'SOCO_CNS', 1, 0, SOCO_CNS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6636, IUER, 'UPTDB_SOLSETUP', 'Error in '// &
     &         'putting "SOCO_CNS" lcode' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'BSCL_CNS', 1, 0, BSCL_CNS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6637, IUER, 'UPTDB_SOLSETUP', 'Error in '// &
     &         'putting "BSCL_CNS" lcode' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'STA_USE ', 1, 0, STA_USE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6638, IUER, 'UPTDB_SOLSETUP', 'Error in '// &
     &         'putting "STA_USE" lcode' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'SOU_USE ', 1, 0, SOU_USE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6639, IUER, 'UPTDB_SOLSETUP', 'Error in '// &
     &         'putting "SOU_USE" lcode' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'BAS_USE ', 1, 0, BAS_USE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6640, IUER, 'UPTDB_SOLSETUP', 'Error in '// &
     &         'putting "BAS_USE" lcode' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'RWBASNAM', 1, 0, %REF(C_BAS), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6641, IUER, 'UPTDB_SOLSETUP', 'Error in '// &
     &         'putting "RWBASNAM" lcode' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'RWDELVAL', 1, 0, RW_DEL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6642, IUER, 'UPTDB_SOLSETUP', 'Error in '// &
     &         'putting "RWDELVAL" lcode' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'RWRATVAL', 1, 0, RW_RAT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6643, IUER, 'UPTDB_SOLSETUP', 'Error in '// &
     &              'putting "RWRATVAL" lcode' )
           RETURN 
      END IF
!
      DEALLOCATE ( DGCL_EST )
      DEALLOCATE ( BSCL_EST )
      DEALLOCATE (  CLO_CNS )
      DEALLOCATE (  ATM_CNS )
      DEALLOCATE ( TLOF_CNS )
      DEALLOCATE ( TLRT_CNS )
      DEALLOCATE ( STPS_CNS )
      DEALLOCATE ( SOCO_CNS )
      DEALLOCATE ( BSCL_CNS )
      DEALLOCATE ( BAS_USE  )
      DEALLOCATE ( C_BAS    )
      DEALLOCATE ( RW_DEL   )
      DEALLOCATE ( RW_RAT   )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  UPTDB_SOLSETUP  !#!#
