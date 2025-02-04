      SUBROUTINE GETDB_FILL_NAMFIL ( GVH, EXP_NAME, EXP_VERSION, MJD_BEG, &
     &                               TAI_BEG, MJD_END, TAI_END, &
     &                               L_ACM, STAT_ACM, CLOOF_ACM, CLODR_ACM, &
     &                               L_CAL, CAL, RW_BAS_NAM, RW_BAS_DEL, &
     &                               RW_BAS_RAT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GETDB_FILL_NAMFIL
! *                                                                      *
! * ## 21-NOV-2005  GETDB_FILL_NAMFIL  v1.3 (c) L. Petrov 19-DEC-2023 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'gvh.i'
      INCLUDE   'socom.i'
      INCLUDE   'prfil.i'
      TYPE     ( GVH__STRU ) :: GVH
      TYPE     ( CAL__TYPE ) :: CAL(M__CAL)
      CHARACTER  EXP_NAME*(*), STAT_ACM(M_ACM)*8, LCODE_CAL(M__CAL)*8
      INTEGER*4  MJD_UTC_BEG, MJD_UTC_END, MJD_BEG, MJD_END, EXP_VERSION, &
     &           L_ACM, L_CAL, IUER
      REAL*8     CLOOF_ACM(M_ACM), CLODR_ACM(M_ACM)
!
      INTEGER*4  MCARD
      PARAMETER  ( MCARD = 32*1024 )
      CHARACTER  BUF(MCARD)*70, STR*80, STR1*80
      CHARACTER  RW_BAS_NAM(2,MAX_ARC_BSL)*8
      REAL*8     RW_BAS_DEL(0:SLV__MAX_SOLTYP-1,MAX_ARC_BSL), &
     &           RW_BAS_RAT(0:SLV__MAX_SOLTYP-1,MAX_ARC_BSL)
      REAL*8     UTC_BEG, UTC_END, TAI_BEG, TAI_END
      REAL*8,    ALLOCATABLE :: PRES_ARR(:,:), TEMP_ARR(:,:), REHU_ARR(:,:)
      INTEGER*4, ALLOCATABLE :: OBS_TAB(:,:)
      REAL*8     ARR_R8(512), ATMPR(2), TEMPC(2), RELHU(2)
      INTEGER*4  ION_STS(MAX_ARC_STA), &
     &           ICAL_AV(MAX_ARC_STA), &
     &           ICAL_AP(MAX_ARC_STA)
      REAL*8     PRES_MIN, PRES_MAX, PRES_AV, PRES_RMS, &
     &           TEMP_MIN, TEMP_MAX, TEMP_AV, TEMP_RMS, &
     &           REHU_MIN, REHU_MAX, REHU_AV, REHU_RMS
      INTEGER*2  CABLE_APL(MAX_ARC_STA), N_CALIB
      LOGICAL*4  FL_CAL, FL_CAB, FL_ACM
      INTEGER*4  NOBS_STA(MAX_ARC_STA), TEC_STS_STA(MAX_ARC_STA)
      ADDRESS__TYPE  ADR_DATA
      CHARACTER  CAL_NAME(M__CAL)*8, DESCR*256
      INTEGER*4  CAL_STS(MAX_ARC_STA*M__CAL)
      REAL*8       RW_DEL_MIN, RW_DEL_MAX, RW_RAT_MIN, RW_RAT_MAX
      PARAMETER  ( RW_DEL_MIN = 1.0D-13 ) 
      PARAMETER  ( RW_DEL_MAX = 1.0D-6  ) 
      PARAMETER  ( RW_RAT_MIN = 1.0D-15 ) 
      PARAMETER  ( RW_RAT_MAX = 1.0D-9  ) 
      INTEGER*4  MBUF, DIMS(2), J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, &
     &           J11, J12, J13, J14, J15, IB, IC, IP, NZ, CLASS, TYP, &
     &           NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, NUM_BAS_RW, IER
      TYPE     ( CAL__INFO__TYPE ) :: CAL_INFO(M__CAL)
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*4, EXTERNAL :: IS_R8_NAN 
      REAL*8,    EXTERNAL :: MJD_SEC_TO_JD
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, MAX_LIST_R8, MIN_LIST_R8
!
! --- Get MJD/UTC time tag for beginning the session
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'MJD_OBS ', 1, 0, 4, DIMS(1), DIMS(2), &
     &                  MJD_UTC_BEG, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9011, IUER, 'GETDB_FILL_NAMFIL', 'Error in '// &
     &         'getting lcode MJD_OBS' )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'UTC_OBS ', 1, 0, 8, DIMS(1), DIMS(2), &
     &                  UTC_BEG, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9012, IUER, 'GETDB_FILL_NAMFIL', 'Error in '// &
     &         'getting lcode UTC_OBS' )
           RETURN
      END IF
!
! --- Get MJD/UTC time tag for the end of the session
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'MJD_OBS ', NUMOBS, 0, 4, DIMS(1), DIMS(2), &
     &                  MJD_UTC_END, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9013, IUER, 'GETDB_FILL_NAMFIL', 'Error in '// &
     &         'getting last lcode MJD_OBS' )
           RETURN
      END IF
      IF ( NUMOBS > MAX_OBS ) THEN
           CALL CLRCH ( STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( NUMOBS,  STR  )  
           CALL INCH  ( MAX_OBS, STR1 )
           CALL ERR_LOG ( 9014, IUER, 'GETDB_FILL_NAMFIL', 'Experiment '// &
     &          TRIM(EXP_NAME)//' has too many observations: '//TRIM(STR)// &
     &          ' more than '//STR1 )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'UTC_OBS ', NUMOBS, 0, 8, DIMS(1), DIMS(2), &
     &                  UTC_END, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9015, IUER, 'GETDB_FILL_NAMFIL', 'Error in '// &
     &         'getting last lcode UTC_OBS' )
           RETURN
      END IF
!
      CALL ERR_PASS      ( IUER, IER )
      CALL GVH_INQ_LCODE ( GVH, 'RWBASNAM', DESCR, CLASS, TYP, DIMS, &
     &                     NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, ADR_DATA, &
     &                     IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9016, IUER, 'GETDB_FILL_NAMFIL', 'Error in '// &
     &         'inquiring lcode RWBASNAM' )
           RETURN
      END IF
      NUM_BAS_RW = DIMS(2)
!
      CALL ERR_PASS      ( IUER, IER )
      CALL GVH_INQ_LCODE ( GVH, 'STAT_ACM', DESCR, CLASS, TYP, DIMS, &
     &                     NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, ADR_DATA, &
     &                     IER )
      IF ( CLASS == 0 ) THEN
           FL_ACM = .FALSE.
           L_ACM = 0
         ELSE
           FL_ACM = .TRUE.
      END IF
!
      IF ( NUM_BAS_RW > 0 ) THEN
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'DATYP   ', 1, 0, 2, DIMS(1), DIMS(2), &
     &                       IDATYP, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9017, IUER, 'GETDB_FILL_NAMFIL', 'Error in '// &
     &              'getting lcode DATYP ' )
                RETURN
           END IF
!
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'RWBASNAM', 1, 0, 16*NUM_BAS_RW, &
     &                       DIMS(1), DIMS(2), RW_BAS_NAM, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9018, IUER, 'GETDB_FILL_NAMFIL', 'Error in '// &
     &              'getting lcode BAS_NAM_RW' )
                RETURN
           END IF
!
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'RWDELVAL', 1, 0, &
     &                       8*NUM_BAS_RW*SLV__MAX_SOLTYP, DIMS(1), DIMS(2), &
     &                       RW_BAS_DEL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9019, IUER, 'GETDB_FILL_NAMFIL', 'Error in '// &
     &              'getting lcode RW_BAS_DEL' )
                RETURN
           END IF
!
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'RWRATVAL', 1, 0, &
     &                       8*NUM_BAS_RW*SLV__MAX_SOLTYP, DIMS(1), DIMS(2), &
     &                       RW_BAS_RAT, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9020, IUER, 'GETDB_FILL_NAMFIL', 'Error in '// &
     &              'getting lcode RW_BAS_RAT' )
                RETURN
           END IF
      END IF
!
      CALL ERR_PASS      ( IUER, IER )
      CALL GVH_INQ_LCODE ( GVH, 'CABL_SGN', DESCR, CLASS, TYP, DIMS, &
     &                     NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, ADR_DATA, &
     &                     IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9021, IUER, 'GETDB_FILL_NAMFIL', 'Error in '// &
     &         'inquiring lcode CABL_SGN' )
           RETURN
      END IF
      IF ( CLASS == 0 ) THEN
           FL_CAB = .FALSE.
         ELSE
           FL_CAB = .TRUE.
      END IF
!
      CALL ERR_PASS      ( IUER, IER )
      CALL GVH_INQ_LCODE ( GVH, 'N_CALIB ', DESCR, CLASS, TYP, DIMS, &
     &                     NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, ADR_DATA, &
     &                     IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9022, IUER, 'GETDB_FILL_NAMFIL', 'Error in '// &
     &         'inquiring lcode N_CALIB ' )
           RETURN
      END IF
      IF ( CLASS == 0 ) THEN
           FL_CAL = .FALSE.
         ELSE
           FL_CAL = .TRUE.
      END IF
!
      L_CAL = 0
      IF ( FL_CAL ) THEN
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'N_CALIB ', 1, 0, 2, DIMS(1), DIMS(2), &
     &                       N_CALIB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9023, IUER, 'GETDB_FILL_NAMFIL', 'Error in '// &
     &              'getting "N_CALIB" lcode' )
                RETURN
           END IF
           L_CAL = N_CALIB
!
           IF ( L_CAL > 0 ) THEN
                CALL ERR_PASS   ( IUER, IER )
                CALL GVH_GLCODE ( GVH, 'CAL_NAME', 1, 0, 8*L_CAL, DIMS(1), &
     &                            DIMS(2), CAL_NAME, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 9024, IUER, 'GETDB_FILL_NAMFIL', 'Error in '// &
     &                   'getting "CAL_NAME" lcode' )
                     RETURN
                END IF
!
                CALL ERR_PASS   ( IUER, IER )
                CALL GVH_GLCODE ( GVH, 'CAL_INFO', 1, 0, L_CAL*SIZEOF(CAL_INFO(1)), &
     &                            DIMS(1), DIMS(2), CAL_INFO, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 9025, IUER, 'GETDB_FILL_NAMFIL', 'Error in '// &
     &                   'getting "CAL_INFO" lcode' )
                     RETURN
                END IF
!
                CALL ERR_PASS      ( IUER, IER )
                CALL GVH_INQ_LCODE ( GVH, 'CAL_STS ', DESCR, CLASS, TYP, DIMS, &
     &                               NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, ADR_DATA, &
     &                               IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 9022, IUER, 'GETDB_FILL_NAMFIL', 'Error in '// &
     &              'inquiring lcode N_CALIB ' )
                    RETURN
                END IF
!
                IF ( CLASS .NE. 0 ) THEN
                     CALL ERR_PASS   ( IUER, IER )
                     CALL GVH_GLCODE ( GVH, 'CAL_STS ', 1, 0, 4*L_CAL*INT4(NUMSTA), &
     &                                 DIMS(1), DIMS(2), CAL_STS, IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL ERR_LOG ( 9026, IUER, 'GETDB_FILL_NAMFIL', 'Error in '// &
     &                        'getting "CAL_STS " lcode' )
                          RETURN
                     END IF
                   ELSE
                     CAL_STS = 0
                END IF
          END IF
      END IF
!
      TAI_BEG = UTC_BEG - UTC_M_TAI
      IF ( TAI_BEG > 86400.0D0 ) THEN
           TAI_BEG = TAI_BEG - 86400.0D0
           MJD_BEG = MJD_UTC_BEG + 1
         ELSE
           MJD_BEG = MJD_UTC_BEG
      END IF
!
      TAI_END = UTC_END - UTC_M_TAI
      IF ( TAI_END > 86400.0D0 ) THEN
           TAI_END = TAI_END - 86400.0D0
           MJD_END = MJD_UTC_END + 1
         ELSE
           MJD_END = MJD_UTC_END
      END IF
!
      DO 410 J1=1,MCARD
         CALL CLRCH ( BUF(J1) )
 410  CONTINUE
      BUF(1) = 'POIN   1   0   2   0   0   0   0   0   0   0   0   0   0   0   0   0  '
      BUF(2)(1:49) = 'INIT    1              0          1  000000000 X-'
      DBNAME_CH  = EXP_NAME(1:10)
      DBNAME_VER = EXP_VERSION
      BUF(2)(11:20) = DBNAME_CH
      WRITE ( UNIT=BUF(2)(38:46), FMT='(I9)' ) NUMOBS
      WRITE ( UNIT=BUF(2)(22:24), FMT='(I3)' ) EXP_VERSION
      BUF(3)(1:5) = 'IONO '
!
      IC = 3
      IB = 0
      DO 420 J2=1,NUMSTA-1
         DO 430 J3=J2+1,NUMSTA
            IC = IC + 1
            IB = IB + 1
            BUF(IC) = 'REWT                        0.00      0.00      0.00      0.00        '
            BUF(IC)(6:13)  = ISITN_CHR(J2)
            BUF(IC)(14:14) = '-'
            BUF(IC)(15:22) = ISITN_CHR(J3)
            IB = 0
            IF ( NUM_BAS_RW > 0 ) THEN
!
! -------------- Search the baseline name. NB: since station(s) may be deselected,
! -------------- the baseline order may be not in the order of stations
!
                 DO 440 J4=1,NUM_BAS_RW
                    IF ( RW_BAS_NAM(1,J4) == ISITN_CHR(J2) .AND. &
     &                   RW_BAS_NAM(2,J4) == ISITN_CHR(J3)       ) THEN
                         IB = J4
                         GOTO 440
                    END IF
!
                    IF ( RW_BAS_NAM(1,J4) == ISITN_CHR(J3) .AND. &
     &                   RW_BAS_NAM(2,J4) == ISITN_CHR(J2)       ) THEN
                         IB = J4
                         GOTO 440
                    END IF
 440             CONTINUE
            END IF
!
            IF ( IB > 0 ) THEN
                 BUF(IC)(6:13)  = RW_BAS_NAM(1,IB)
                 BUF(IC)(14:14) = '-'
                 BUF(IC)(15:22) = RW_BAS_NAM(2,IB)
!
! -------------- Check validity of G_GXS weights
!
                 IF ( IS_R8_NAN ( RW_BAS_DEL(G_GXS__DTP,IB) ) ) THEN
                      RW_BAS_DEL(G_GXS__DTP,IB) = RW_DEL_MIN 
                 END IF
                 IF ( IS_R8_NAN ( RW_BAS_RAT(G_GXS__DTP,IB) ) ) THEN
                      RW_BAS_RAT(G_GXS__DTP,IB) = RW_RAT_MIN 
                 END IF
!
                 IF ( RW_BAS_DEL(G_GXS__DTP,IB) < RW_DEL_MIN ) THEN
                      RW_BAS_DEL(G_GXS__DTP,IB) = RW_DEL_MIN 
                 END IF
                 IF ( RW_BAS_DEL(G_GXS__DTP,IB) > RW_DEL_MAX ) THEN
                      RW_BAS_DEL(G_GXS__DTP,IB) = RW_DEL_MAX 
                 END IF
!
                 IF ( RW_BAS_RAT(G_GXS__DTP,IB) < RW_RAT_MIN ) THEN
                      RW_BAS_RAT(G_GXS__DTP,IB) = RW_RAT_MIN 
                 END IF
                 IF ( RW_BAS_RAT(G_GXS__DTP,IB) > RW_RAT_MAX ) THEN
                      RW_BAS_RAT(G_GXS__DTP,IB) = RW_RAT_MAX 
                 END IF
!
! -------------- Check validity of weights which corresponds to IDATYP solution
!
                 IF ( IS_R8_NAN ( RW_BAS_DEL(IDATYP,IB) ) ) THEN
                      RW_BAS_DEL(IDATYP,IB) = RW_DEL_MIN 
                 END IF
                 IF ( IS_R8_NAN ( RW_BAS_RAT(IDATYP,IB) ) ) THEN
                      RW_BAS_RAT(IDATYP,IB) = RW_RAT_MIN 
                 END IF
!
                 IF ( RW_BAS_DEL(IDATYP,IB) < RW_DEL_MIN ) THEN
                      RW_BAS_DEL(IDATYP,IB) = RW_DEL_MIN 
                 END IF
                 IF ( RW_BAS_DEL(IDATYP,IB) > RW_DEL_MAX ) THEN
                      RW_BAS_DEL(IDATYP,IB) = RW_DEL_MAX 
                 END IF
!
                 IF ( RW_BAS_RAT(IDATYP,IB) < RW_RAT_MIN ) THEN
                      RW_BAS_RAT(IDATYP,IB) = RW_RAT_MIN 
                 END IF
                 IF ( RW_BAS_RAT(IDATYP,IB) > RW_RAT_MAX ) THEN
                      RW_BAS_RAT(IDATYP,IB) = RW_RAT_MAX 
                 END IF
!
! -------------- Check validity of G_GXS weights
!
                 IF ( IS_R8_NAN ( RW_BAS_DEL(PX_GS__DTP,IB) ) ) THEN
                      RW_BAS_DEL(PX_GS__DTP,IB) = RW_DEL_MIN 
                 END IF
                 IF ( IS_R8_NAN ( RW_BAS_RAT(PX_GS__DTP,IB) ) ) THEN
                      RW_BAS_RAT(PX_GS__DTP,IB) = RW_RAT_MIN 
                 END IF
!
! -------------- Sanitizing the vairables before writing them in the namfil
! 
                 IF ( DABS(RW_BAS_DEL(G_GXS__DTP,IB)) > 9.99D-7  ) RW_BAS_DEL(G_GXS__DTP,IB) = 9.999D-7
                 IF ( DABS(RW_BAS_RAT(G_GXS__DTP,IB)) > 9.99D-10 ) RW_BAS_RAT(G_GXS__DTP,IB) = 9.999D-10 
                 IF ( DABS(RW_BAS_DEL(PX_GS__DTP,IB)) > 9.99D-7  ) RW_BAS_DEL(PX_GS__DTP,IB) = 9.999D-7
                 IF ( DABS(RW_BAS_RAT(PX_GS__DTP,IB)) > 9.99D-10 ) RW_BAS_RAT(PX_GS__DTP,IB) = 9.999D-10 
!
                 WRITE ( BUF(IC)(24:32), FMT='(F9.2)' ) &
     &                   RW_BAS_DEL(G_GXS__DTP,IB)*1.0D12
                 WRITE ( BUF(IC)(34:42), FMT='(F9.2)' ) &
                         RW_BAS_RAT(G_GXS__DTP,IB)*1.0D15
                 WRITE ( BUF(IC)(44:52), FMT='(F9.2)' ) &
     &                   RW_BAS_DEL(PX_GS__DTP,IB)*1.0D12
                 WRITE ( BUF(IC)(54:62), FMT='(F9.2)' ) &
                         RW_BAS_RAT(G_GXS__DTP,IB)*1.0D15
!
! -------------- Then set values in accordance with the current solution type
!
                 IF ( IDATYP == GRPRAT__DTP .OR. &
     &                IDATYP == SNBRAT__DTP .OR. &
     &                IDATYP == GRPONL__DTP .OR. &
     &                IDATYP == SNBONL__DTP .OR. &
     &                IDATYP == RATONL__DTP .OR. &
     &                IDATYP ==  G_GXS__DTP .OR. &
     &                IDATYP ==    GX__DTP  .OR. &
     &                IDATYP ==    GS__DTP  .OR. &
     &                IDATYP == SNG_X__DTP  .OR. &
     &                IDATYP == SNG_S__DTP  .OR. &
     &                IDATYP == FUSED__DTP       ) THEN
                      WRITE ( BUF(IC)(24:32), FMT='(F9.2)' ) &
     &                        RW_BAS_DEL(IDATYP,IB)*1.0D12
                      WRITE ( BUF(IC)(34:42), FMT='(F9.2)' ) &
                              RW_BAS_RAT(IDATYP,IB)*1.0D15
                 END IF
!
                 IF ( IDATYP == PHSRAT__DTP .OR. &
     &                IDATYP == PHSONL__DTP .OR. &
     &                IDATYP == PX_GXS__DTP .OR. &
     &                IDATYP == PS_GXS__DTP .OR. &
     &                IDATYP ==  PX_GX__DTP .OR. &
     &                IDATYP ==  PX_GS__DTP .OR. &
     &                IDATYP ==  PS_GX__DTP .OR. &
     &                IDATYP ==  PS_GS__DTP .OR. &
     &                IDATYP ==  P_PXS__DTP .OR. &
     &                IDATYP ==     PX__DTP .OR. &
     &                IDATYP ==     PS__DTP      ) THEN
                      WRITE ( BUF(IC)(44:52), FMT='(F9.2)' ) &
     &                        RW_BAS_DEL(IDATYP,IB)*1.0D12
                 END IF
            END IF
 430     CONTINUE
 420  CONTINUE
!
      IC = IC + 1
      BUF(IC)(1:5)  = 'SPAN '
      WRITE ( UNIT=BUF(IC)(14:25), FMT='(F12.4)' ) &
     &        MJD_SEC_TO_JD ( MJD_BEG, TAI_BEG )
      WRITE ( UNIT=BUF(IC)(34:45), FMT='(F12.4)' ) &
     &        MJD_SEC_TO_JD ( MJD_END, TAI_END )
!
      IC = IC + 1
      BUF(IC)(1:33) = 'CONT       0      0      0      0'
      IC = IC + 1
      BUF(IC)(1:13) = 'PART VTD_NMFW'
!
      IF ( FL_CAB ) THEN
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'CABL_SGN', 1, 0, 2*INT4(NUMSTA), &
     &                       DIMS(1), DIMS(2), CABLE_SIGN, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9027, IUER, 'GETDB_FILL_NAMFIL', 'Error in '// &
     &              'getting lcode CABL_SGN' )
                RETURN
           END IF
      END IF
!
      CALL ERR_PASS      ( IUER, IER )
      CALL GVH_INQ_LCODE ( GVH, 'TEC_STS ', DESCR, CLASS, TYP, DIMS, &
     &                     NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, ADR_DATA, &
     &                     IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9028, IUER, 'GETDB_FILL_OBORG', 'Error in '// &
     &         'inquiring lcode TEC_STS ' )
           RETURN
      END IF
      IF ( CLASS .NE. 0 ) THEN
           CALL ERR_PASS   ( 0, IER )
           CALL GVH_GLCODE ( GVH, 'TEC_STS ', 0, 0, 4*INT4(NUMSTA), &
     &                       DIMS(1), DIMS(2), TEC_STS_STA, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_PASS ( IUER, IER )
                CALL ERR_LOG ( 9029, IUER, 'GETDB_FILL_NAMFIL', 'Error in '// &
     &              'getting lcode TEC_STS ' )
                TEC_STS_STA = 0
                RETURN
           END IF
        ELSE
           TEC_STS_STA = 0
      END IF
      CALL NOUT ( M__CAL*SIZEOF(CAL(1)), CAL )
!
! --- Add a special calibration: "user cal"
!
      L_CAL = L_CAL + 1
      CAL_NAME(L_CAL)       = 'user cal'
      CAL_INFO(L_CAL)%CLASS = GVH__STA
      CAL_INFO(L_CAL)%MODE  = CAL__DEL
      CAL(L_CAL)%APPLIED    = .FALSE.
!
      DO 450 J5=1,NUMSTA
         IC = IC + 1
!
! ------ Set status of the external ionosphere calibration
!
         ION_STS(J5) = 0
         IF ( BTEST ( TEC_STS_STA(J5), 0 ) ) THEN
              CALL SBIT ( ION_STS(J5), INT2(1), INT2(1) )
         END IF
!
         IF ( BTEST ( TEC_STS_STA(J5), 1 ) ) THEN
              CALL SBIT ( ION_STS(J5), INT2(4),  INT2(1) )
         END IF
         BUF(IC)(1:13) = 'CALS '//ISITN_CHR(J5)
         ICAL_AV(J5) = 0
         ICAL_AP(J5) = 0
         IF ( L_CAL > 0 ) THEN
              DO 460 J6=1,MIN(L_CAL,8)
                 IP = J5 + (J6-1)*NUMSTA
                 IF ( J5 == 1 ) THEN
                      CAL(J6)%NAME = CAL_NAME(J6)
                      CAL(J6)%INFO%CLASS = CAL_INFO(J6)%CLASS
                      CAL(J6)%INFO%MODE  = CAL_INFO(J6)%MODE
                      CAL(J6)%APPLIED    = CAL_STS(IP)
                 END IF
                 ICAL_AV(J5) = IBSET ( ICAL_AV(J5), J6-1 )
                 IF ( BTEST ( CAL_STS(IP), INT4(IDATYP) ) ) THEN
                      ICAL_AP(J5) = IBSET ( ICAL_AP(J5), J6-1 )
                 END IF
 460          CONTINUE
         END IF
         WRITE ( UNIT=BUF(IC)(16:21), FMT='(I6)' ) ION_STS(J5)
         WRITE ( UNIT=BUF(IC)(23:28), FMT='(I6)' ) ICAL_AV(J5)
         WRITE ( UNIT=BUF(IC)(30:35), FMT='(I6)' ) ICAL_AP(J5)
 450  CONTINUE
!
      DO 470 J7=1,NUMSTA
         IC = IC + 1
         BUF(IC) = 'FCLS '//ISITN_CHR(J7)// &
     &           '       0       0       0       0       0       0       0 '
 470  CONTINUE
!
      DO 480 J8=1,NUMSTA
         IC = IC + 1
         BUF(IC)(1:13) = 'SITE '//ISITN_CHR(J8)
 480  CONTINUE
      IC = IC + 1
      BUF(IC)(1:46) = 'BARO None     No barometric calibrations used.'
      IC = IC + 1
      BUF(IC)(1:5) = 'CALN '
!
      IF ( L_CAL > 0 ) THEN
           DO 490 J9=1,MIN(8,L_CAL)
              BUF(IC)(6+(J9-1)*8:13+(J9-1)*8) = CAL_NAME(J9)
 490       CONTINUE
      END IF
!
      DO 4100 J10=1,14
         IC = IC + 1
         BUF(IC) = 'FCLN -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* '
 4100 CONTINUE
!
      IC = IC + 1
      BUF(IC) = 'CLZN -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* '
      IC = IC + 1
      BUF(IC) = 'CNTN -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* '
      IC = IC + 1
      BUF(IC) = 'CNTN -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* '
      IC = IC + 1
      BUF(IC)(1:5) = 'CALI '
      IF ( L_CAL > 0 ) THEN
           DO 4110 J11=1,MIN(L_CAL,8)
              BUF(IC)(6+(J11-1)*8:13+(J11-1)*8) = CAL_NAME(J11)
 4110      CONTINUE
      END IF
      IC = IC + 1
      BUF(IC) = 'CNTI -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* '
      IC = IC + 1
      BUF(IC) = 'CNTI -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* '
      IC = IC + 1
      BUF(IC) = 'MCAL -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* '
      IC = IC + 1
      BUF(IC) = 'MCLI -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* '
      IC = IC + 1
      BUF(IC)(1:24) = 'CLCT       0   0   0   0'
      CALL INCH   ( L_CAL, BUF(IC)(6:8) )
      CALL CHASHR        ( BUF(IC)(6:8) )
      IC = IC + 1
      BUF(IC)(1:28) = 'CZPT  0  0  0  0  0  0  0  0'
      IC = IC + 1
      BUF(IC)(1:28) = 'MDAT  00******************************************'
      WRITE ( UNIT=BUF(IC)(7:8), FMT='(I2)' ) NUMSTA
!
      ALLOCATE ( OBS_TAB(3,NUMOBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*3*NUMOBS, STR )
           CALL ERR_LOG ( 9030, IUER, 'GETDB_FILL_NAMFIL', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for observation table' )
           RETURN
      END IF
      ALLOCATE ( TEMP_ARR(NUMOBS,NUMSTA) )
      ALLOCATE ( PRES_ARR(NUMOBS,NUMSTA) )
      ALLOCATE ( REHU_ARR(NUMOBS,NUMSTA) )
!
      CALL NOUT_R8 ( INT4(NUMSTA)*NUMOBS,      TEMP_ARR )
      CALL NOUT_R8 ( INT4(NUMSTA)*NUMOBS,      PRES_ARR )
      CALL NOUT_R8 ( INT4(NUMSTA)*NUMOBS,      REHU_ARR )
      CALL NOUT_I4 ( INT4(MAX_ARC_STA),        NOBS_STA )
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'OBS_TAB ', 1, 0, 4*3*NUMOBS, DIMS(1), DIMS(2), &
     &                  OBS_TAB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9031, IUER, 'GETDB_FILL_NAMFIL', 'Error in '// &
     &         'getting lcode OBS_TAB' )
           DEALLOCATE ( OBS_TAB  )
           DEALLOCATE ( TEMP_ARR )
           DEALLOCATE ( PRES_ARR )
           DEALLOCATE ( REHU_ARR )
           RETURN
      END IF
!
      CALL ERR_PASS      ( IUER, IER )
      CALL GVH_INQ_LCODE ( GVH, 'ATM_PRES', DESCR, CLASS, TYP, DIMS, &
     &                     NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, ADR_DATA, &
     &                     IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9032, IUER, 'GETDB_FILL_NAMIL', 'Error in '// &
     &         'inquiring lcode ATM_PRES' )
           RETURN
      END IF
!
      DO 4120 J12=1,NUMOBS
!
! ------ Get air pressure
!
         IF ( CLASS .NE. 0 ) THEN
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'ATM_PRES', J12, 1, 8, DIMS(1), DIMS(2), &
     &                          ARR_R8, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 9033, IUER, 'GETDB_FILL_NAMFIL', 'Error in '// &
     &                 'getting lcode AIR_TEMP for the first station' )
                   RETURN
              END IF
              ATMPR(1) = ARR_R8(1)*1.D-2
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'ATM_PRES', J12, 2, 8, DIMS(1), DIMS(2), &
     &                          ARR_R8, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 9034, IUER, 'GETDB_FILL_NAMFIL', 'Error in '// &
     &                 'getting lcode AIR_TEMP for the second station' )
                   RETURN
              END IF
              ATMPR(2) = ARR_R8(1)*1.D-2
!
! ----------- Get air temperature
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'AIR_TEMP', J12, 1, 8, DIMS(1), DIMS(2), &
     &                          ARR_R8, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 9035, IUER, 'GETDB_FILL_NAMFIL', 'Error in '// &
     &                 'getting lcode AIR_TEMP for the first station' )
                   RETURN
              END IF
              TEMPC(1) = ARR_R8(1) - 273.16D0
              IF ( TEMPC(1) .LE. -900.0 ) TEMPC(1) = -999.00
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'AIR_TEMP', J12, 2, 8, DIMS(1), DIMS(2), &
     &                          ARR_R8, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 9036, IUER, 'GETDB_FILL_NAMFIL', 'Error in '// &
     &                 'getting lcode AIR_TEMP for the second station' )
                   RETURN
              END IF
              TEMPC(2) = ARR_R8(1) - 273.16D0
              IF ( TEMPC(2) .LE. -900.0 ) TEMPC(2) = -999.00
!
! ----------- Get relative humidity
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'REL_HUMD', J12, 1, 8, DIMS(1), DIMS(2), &
     &                          ARR_R8, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 9037, IUER, 'GETDB_FILL_NAMFIL', 'Error in '// &
     &                 'getting lcode REL_HIMI for the first station' )
                   RETURN
              END IF
              RELHU(1) = ARR_R8(1) ! *1.D-2
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'REL_HUMD', J12, 2, 8, DIMS(1), DIMS(2), &
     &                          ARR_R8, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 9038, IUER, 'GETDB_FILL_NAMFIL', 'Error in '// &
     &                 'getting lcode REL_HUMI for the second station' )
                   RETURN
              END IF
              RELHU(2) = ARR_R8(1) ! *1.D-2
            ELSE
!
! ----------- Dummy atmospheric parameters
!
              ATMPR(1) = -999.0
              ATMPR(2) = -999.0
              TEMPC(1) = -999.0
              TEMPC(2) = -999.0
              RELHU(1) = -999.0
              RELHU(2) = -999.0
         END IF
!
         IF ( ATMPR(1) > 0.0D0 ) THEN
              NOBS_STA(OBS_TAB(2,J12)) = NOBS_STA(OBS_TAB(2,J12)) + 1
              PRES_ARR(NOBS_STA(OBS_TAB(2,J12)),OBS_TAB(2,J12)) = ATMPR(1)
              TEMP_ARR(NOBS_STA(OBS_TAB(2,J12)),OBS_TAB(2,J12)) = TEMPC(1)
              REHU_ARR(NOBS_STA(OBS_TAB(2,J12)),OBS_TAB(2,J12)) = RELHU(1)
         END IF
!
         IF ( ATMPR(2) > 0.0D0 ) THEN
              NOBS_STA(OBS_TAB(3,J12)) = NOBS_STA(OBS_TAB(3,J12)) + 1
              PRES_ARR(NOBS_STA(OBS_TAB(3,J12)),OBS_TAB(3,J12)) = ATMPR(2)
              TEMP_ARR(NOBS_STA(OBS_TAB(3,J12)),OBS_TAB(3,J12)) = TEMPC(2)
              REHU_ARR(NOBS_STA(OBS_TAB(3,J12)),OBS_TAB(3,J12)) = RELHU(2)
         END IF
 4120 CONTINUE
!
      DO 4130 J13=1,NUMSTA
         IF ( NOBS_STA(J13) > 2 ) THEN
              CALL DISP8 ( NOBS_STA(J13), PRES_ARR(1,J13), %VAL(0), &
     &                     PRES_AV, PRES_RMS, NZ, -3 )
              CALL DISP8 ( NOBS_STA(J13), TEMP_ARR(1,J13), %VAL(0), &
     &                     TEMP_AV, TEMP_RMS, NZ, -3 )
              CALL DISP8 ( NOBS_STA(J13), REHU_ARR(1,J13), %VAL(0), &
     &                     REHU_AV, REHU_RMS, NZ, -3 )
              PRES_MIN = PRES_ARR(MIN_LIST_R8(NOBS_STA(J13), PRES_ARR(1,J13)),J13)
              TEMP_MIN = TEMP_ARR(MIN_LIST_R8(NOBS_STA(J13), TEMP_ARR(1,J13)),J13)
              REHU_MIN = REHU_ARR(MIN_LIST_R8(NOBS_STA(J13), REHU_ARR(1,J13)),J13)
              PRES_MAX = PRES_ARR(MAX_LIST_R8(NOBS_STA(J13), PRES_ARR(1,J13)),J13)
              TEMP_MAX = TEMP_ARR(MAX_LIST_R8(NOBS_STA(J13), TEMP_ARR(1,J13)),J13)
              REHU_MAX = REHU_ARR(MAX_LIST_R8(NOBS_STA(J13), REHU_ARR(1,J13)),J13)
            ELSE
              PRES_MIN = -999.0
              PRES_MAX = -999.0
              PRES_AV  = -999.0
              PRES_RMS =   0.0
!
              TEMP_MIN = -999.0
              TEMP_MAX = -999.0
              TEMP_AV  = -999.0
              TEMP_RMS =    0.0
!
              REHU_MIN = -999.0
              REHU_MAX = -999.0
              REHU_AV  = -999.0
              REHU_RMS =    0.0
         END IF
         IC = IC + 1
         BUF(IC)(1:13) = 'MDAT '//ISITN_CHR(J13)
         WRITE ( UNIT=BUF(IC)(16:21), FMT='(F6.1)' ) TEMP_MIN
         WRITE ( UNIT=BUF(IC)(23:28), FMT='(F6.1)' ) TEMP_MAX
         WRITE ( UNIT=BUF(IC)(30:35), FMT='(F6.1)' ) TEMP_AV
         WRITE ( UNIT=BUF(IC)(37:42), FMT='(F6.1)' ) TEMP_RMS
         WRITE ( UNIT=BUF(IC)(44:49), FMT='(F6.1)' ) PRES_MIN
         WRITE ( UNIT=BUF(IC)(51:56), FMT='(F6.1)' ) PRES_MAX
         WRITE ( UNIT=BUF(IC)(58:63), FMT='(F6.1)' ) PRES_AV
         WRITE ( UNIT=BUF(IC)(65:70), FMT='(F6.1)' ) PRES_RMS
 4130 CONTINUE
!
      IF ( FL_ACM ) THEN
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'STAT_ACM', 1, 0, 4*8, DIMS(1), DIMS(2), &
     &                       STAT_ACM, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9039, IUER, 'GETDB_FILL_NAMFIL', 'Error in '// &
     &                 'getting lcode STAT_ACM' )
                RETURN
           END IF
!
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'CLOOFACM', 1, 0, 4*8, DIMS(1), DIMS(2), &
     &                       CLOOF_ACM, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9040, IUER, 'GETDB_FILL_NAMFIL', 'Error in '// &
     &                 'getting lcode CLOOF_ACM' )
                RETURN
           END IF
!
           CALL ERR_PASS   ( IUER, IER )
           CALL GVH_GLCODE ( GVH, 'CLODRACM', 1, 0, 4*8, DIMS(1), DIMS(2), &
     &                       CLODR_ACM, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9041, IUER, 'GETDB_FILL_NAMFIL', 'Error in '// &
     &                 'getting lcode CLODRACM' )
                RETURN
           END IF
!
           L_ACM  = 0
           DO 4140 J14=1,M_ACM
!
! ----------- Sanitizing the vairables before writing them in the namfil
!
              IF ( IS_R8_NAN ( CLOOF_ACM(J14) ) ) CLOOF_ACM(J14) = 0.0D0
              IF ( IS_R8_NAN ( CLODR_ACM(J14) ) ) CLODR_ACM(J14) = 0.0D0
              IF ( DABS(CLOOF_ACM(J14)) > 1.0D4   ) CLOOF_ACM(J14) = 0.0D0
              IF ( DABS(CLODR_ACM(J14)) > 1.0D4   ) CLODR_ACM(J14) = 0.0D0
              IF ( DABS(CLOOF_ACM(J14)) < 1.0D-18 ) CLOOF_ACM(J14) = 0.0D0
              IF ( DABS(CLODR_ACM(J14)) > 1.0D-18 ) CLODR_ACM(J14) = 0.0D0
!
              IC = IC + 1
              IF ( ILEN(STAT_ACM(J14)) > 0 ) L_ACM = L_ACM + 1
              CALL CLRCH ( BUF(IC) )
              WRITE ( BUF(IC), '("ACM  ",I2,1X,I2,1X,I2,1X,A8,1X,D23.15,1X,D23.15)') &
     &                M_ACM, L_ACM, J14, STAT_ACM(J14), CLOOF_ACM(J14), &
     &                CLODR_ACM(J14)
 4140      CONTINUE
      END IF
!
      WRITE ( UNIT=BUF(1)(9:12),  FMT='(I4)' ) MIN(9999,IC)
      WRITE ( UNIT=BUF(1)(61:68), FMT='(I8)' ) IC
      CALL OPENNAMFIL()
      DO 4150 J15=1,IC
         WRITE ( UNIT=UNITNAM, REC=J15 ) BUF(J15)
 4150 CONTINUE
      CLOSE ( UNIT=UNITNAM )
      CALL OPENNAMFIL()
!
      DEALLOCATE ( OBS_TAB  )
      DEALLOCATE ( TEMP_ARR )
      DEALLOCATE ( PRES_ARR )
      DEALLOCATE ( REHU_ARR )
!
      CALL GETENVAR ( 'GETDB_SUPMET_CHECK', STR )
      IF ( STR == 'YES' ) THEN
           if ( supmet == supmet__pre98 )  write ( 23, * ) 'GETDB_FILL_NAMFIL(730): '//exp_name(1:10)//'  SUPMET PRE98'
           if ( supmet == supmet__pre91 )  write ( 23, * ) 'GETDB_FILL_NAMFIL(730): '//exp_name(1:10)//'  SUPMET PRE91'
           if ( supmet == supmet__comb1 )  write ( 23, * ) 'GETDB_FILL_NAMFIL(730): '//exp_name(1:10)//'  SUPMET COMB1'
           if ( supmet == supmet__sngba )  write ( 23, * ) 'GETDB_FILL_NAMFIL(730): '//exp_name(1:10)//'  SUPMET SNGBA'
           if ( supmet == supmet__meta  )  write ( 23, * ) 'GETDB_FILL_NAMFIL(730): '//exp_name(1:10)//'  SUPMET META '
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GETDB_FILL_NAMFIL  !#!#
