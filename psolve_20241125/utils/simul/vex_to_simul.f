      SUBROUTINE VEX_TO_SIMUL ( VEX, VTD, NERS, SIMUL, LT, RENAME_TAB, &
     &                          IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VEX_TO_SIMUL
! *                                                                      *
! * ### 09-JUN-2020   VEX_TO_SIMUL  v1.3 (c)  L. Petrov  10-SEP-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vex.i'
      INCLUDE   'vtd.i'
      INCLUDE   'simul.i'
      TYPE     ( VEX_TYPE    ) :: VEX
      TYPE     ( VTD__TYPE   ) :: VTD
      TYPE     ( SIMUL__TYPE ) :: SIMUL
      TYPE     ( NERS__TYPE  ) :: NERS
      TYPE     ( SIMUL__TYPE ) :: SIMUL_OLD
      TYPE     ( SIMUL_SORT__TYPE ) :: SIM_SORT(SIM__MOBS)
      INTEGER*4  LT, IVRB, IUER
      REAL*8     START_MIN, STOP_MAX
      CHARACTER  RENAME_TAB(LT)*(*)
      CHARACTER  STR*128
      REAL*8     UTC_OBS, UTC_1ST_OBS, UTC_LAST_OBS, START_OFFSET, STOP_OFFSET, &
     &           TOT_UTC_OBS, TIM_TAI
      REAL*8     AZ(2), EL(2), HA(2), AZ_RATE(2), EL_RATE(2), HA_RATE(2)
      REAL*8     EL_MIN
      REAL*8     UTC_M_TAI, TAI_START, TAI_STOP
      LOGICAL*1  FL_STA(SIM__MSTA), FL_SOU(SIM__MSOU), FL_SCA(SIM__MSCA), FL_BAS_STA(SIM__MSTA)
      PARAMETER  ( EL_MIN = 5.0D0*DEG__TO__RAD )
      INTEGER*4  MIND
      PARAMETER  ( MIND = 32 )
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, J14, J15, J16, &
     &           STA_IND(VEX__MSTA), LIND, IND(2,MIND), &
     &           XREF_SCA(SIM__MSCA), XREF_SOU(SIM__MSOU), XREF_STA(SIM__MSTA), &
     &           MJD_OBS, MJD_1ST_OBS, MJD_LAST_OBS, K_SOU, K_STA, K_SCA, IER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, TIM_TO_DATE*30
      INTEGER*4, EXTERNAL :: SIM_COMPAR_OBS
!
      SIMUL%NOBS = 0
      SIMUL%OBSTAB = 0
      SIMUL%NOBS_STA = 0
      SIMUL%NSCA = VEX%N_SCA
      SIMUL%NSTA = VEX%N_STA
      SIMUL%NSOU = VEX%N_SOU
      SIMUL%NBND = 1
!
      SIMUL%MJD_BEG = VEX%MJD_START 
      SIMUL%MJD_END = VEX%MJD_STOP
      SIMUL%UTC_BEG = VEX%UTC_START 
      SIMUL%UTC_END = VEX%UTC_STOP
      TOT_UTC_OBS   = (SIMUL%MJD_BEG - J2000__MJD)*86400.0D0 + SIMUL%UTC_BEG 
!
      IF ( SIMUL%MJD_BEG > J2000__MJD  .AND. SIMUL%MJD_END > J2000__MJD ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL NERS_GET_UTCMTAI ( NERS, TOT_UTC_OBS, SIMUL%UTC_MTAI, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1511, IUER, 'VEX_TO_SIMUL', 'Error in '// &
     &              'gettting UTC minus TAI function for the first observation' )
                RETURN
           END IF
      END IF
!
      FL_SOU = .FALSE.
      FL_STA = .FALSE.
      FL_SCA = .FALSE.
      K_SOU  = 0
      K_STA  = 0
      K_SCA  = 0
      XREF_STA = 0
      XREF_SCA = 0
      XREF_SOU = 0
!
      SIMUL%SCA_IND = 0
      DO 410 J1=1,VEX%N_STA
         SIMUL%STA_NAM(J1) = VEX%STA(J1)%SITE_NAME
         SIMUL%STA_ID(J1)  = VEX%STA(J1)%SITE_ID
         SIMUL%STA_COO(1:3,J1) = VEX%STA(J1)%SITE_POS(1:3)
         SIMUL%SAMPLE_RATE(J1) = VEX%FRQ(1)%SAMPLE_RATE
         SIMUL%BITS_SAMPLE(J1) = VEX%FRQ(1)%BIT_SAMPLE
         IF ( IVRB .GE. 3 ) THEN
              WRITE ( 6, * ) 'VEX_TO_SIMUL: I_STA= ', INT2(J1), ' Sta_nam: ', SIMUL%STA_NAM(J1) 
         END IF
  410 CONTINUE 
!
      DO 420 J2=1,VEX%N_SOU
         SIMUL%SOU_NAM(J2)   = VEX%SOU(J2)%NAME
         SIMUL%SOU_COO(1,J2) = VEX%SOU(J2)%RA
         SIMUL%SOU_COO(2,J2) = VEX%SOU(J2)%DEC
         IF ( IVRB .GE. 3 ) THEN
              WRITE ( 6, * ) 'VEX_TO_SIMUL: I_SOU= ', INT2(J2), ' Sou_nam: ', SIMUL%SOU_NAM(J2), ' ra: ', VEX%SOU(J2)%RA, ' dec: ', VEX%SOU(J2)%DEC ! %%%%%%%%%%%%
         END IF
 420  CONTINUE 
!
      DO 430 J3=1,VEX%N_SCA
         STA_IND = 0
         IF ( IVRB .GE. 3 ) THEN
              STR = MJDSEC_TO_DATE ( VEX%SCA(J3)%MJD, VEX%SCA(J3)%UTC, IER )
              WRITE ( 6, * ) 'VEX_TO_SIMUL: I_SCA= ', INT2(J3), ' Sou_ind: ', VEX%SCA(J3)%IND_SOU, &
     &                       ' Sou_nam: ', SIMUL%SOU_NAM(VEX%SCA(J3)%IND_SOU), &
     &                       ' UTC_Date: ', STR(1:24)
         END IF
         START_MIN = -1.D12
         STOP_MAX  =  1.D12
         CALL CLRCH ( STR )
         STR(1:2) = 'No'
         CALL INCH   ( J3,    STR(3:7) )
         CALL CHASHR (        STR(3:7) )
         CALL BLANK_TO_ZERO ( STR(3:7) )
         DO 440 J4=1,VEX%SCA(J3)%N_STA
            START_MIN = MAX ( START_MIN, VEX%SCA(J3)%START_OFFSET(J4) )
            STOP_MAX  = MIN ( STOP_MAX,  VEX%SCA(J3)%START_OFFSET(J4) + VEX%SCA(J3)%SCAN_DUR(J4)     )
 440     CONTINUE 
!
         MJD_OBS = VEX%SCA(J3)%MJD
         UTC_OBS = VEX%SCA(J3)%UTC + IDINT ( (START_MIN + STOP_MAX)/2.0D0 )
         IF ( UTC_OBS .GE. 86400.0D0 ) THEN
              MJD_OBS = MJD_OBS + 1
              UTC_OBS = UTC_OBS - 86400.0D0
         END IF
         IF ( J3 == 1 ) THEN
              MJD_1ST_OBS = MJD_OBS
              UTC_1ST_OBS = UTC_OBS
!
              IF ( SIMUL%MJD_BEG < J2000__MJD  .AND. SIMUL%MJD_END < J2000__MJD ) THEN
                   TOT_UTC_OBS   = (MJD_1ST_OBS - J2000__MJD)*86400.0D0 + UTC_1ST_OBS 
                   CALL ERR_PASS ( IUER, IER )
                   CALL NERS_GET_UTCMTAI ( NERS, TOT_UTC_OBS, SIMUL%UTC_MTAI, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 1512, IUER, 'VEX_TO_SIMUL', 'Error in '// &
     &                      'gettting UTC minus TAI function for the first observation' )
                        RETURN
                   END IF
              END IF
         END IF
         IF ( J3 == VEX%N_SCA ) THEN
              MJD_LAST_OBS = MJD_OBS
              UTC_LAST_OBS = UTC_OBS
         END IF
!
         TIM_TAI = (MJD_OBS - J2000__MJD)*86400.0D0 + UTC_OBS - SIMUL%UTC_MTAI
         FL_BAS_STA = .FALSE.
         IF ( IVRB .GE. 3 ) THEN
              WRITE ( 6, * ) 'VEX_TO_SIMUL-142: I_SCA= ', INT2(J3), ' NOBS = ', SIMUL%NOBS, ' N_STA= ', VEX%SCA(J3)%N_STA ! %%%
         END IF
         DO 450 J5=1,VEX%SCA(J3)%N_STA-1
            CALL ERR_PASS ( IUER, IER )
            CALL NERS_AZELHA_COMP ( NERS, TIM_TAI, SIMUL%STA_COO(1,VEX%SCA(J3)%IND_STA(J5)), &
     &                              SIMUL%SOU_COO(1,VEX%SCA(J3)%IND_SOU), &
     &                              SIMUL%SOU_COO(2,VEX%SCA(J3)%IND_SOU), &
     &                             'radio', AZ(1), EL(1), HA(1), AZ_RATE(1), &
     &                              EL_RATE(1), HA_RATE(1), IER )
            IF ( IER .NE. 0 ) THEN
                 WRITE ( 6, * ) 'SIMUL%UTC_MTAI= ', SIMUL%UTC_MTAI
                 WRITE ( 6, * ) 'MJD_OBS= ', MJD_OBS, ' UTC_OBS= ', UTC_OBS
                 CALL ERR_LOG ( 1513, IUER, 'VEX_TO_SIMUL', 'Error in '// &
     &               'computing azimuth and elevation' )
                 RETURN 
            END IF
            IF ( EL(1) < EL_MIN ) GOTO 450
!
            DO 460 J6=J5+1,VEX%SCA(J3)%N_STA
               CALL ERR_PASS ( IUER, IER )
               CALL NERS_AZELHA_COMP ( NERS, TIM_TAI, SIMUL%STA_COO(1,VEX%SCA(J3)%IND_STA(J6)), &
     &                                 SIMUL%SOU_COO(1,VEX%SCA(J3)%IND_SOU), &
     &                                 SIMUL%SOU_COO(2,VEX%SCA(J3)%IND_SOU), &
     &                                'radio', AZ(2), EL(2), HA(2), AZ_RATE(2), &
     &                                 EL_RATE(2), HA_RATE(2), IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 1514, IUER, 'VEX_TO_SIMUL', 'Error in '// &
     &                  'computing azimuth and elevation' )
                    RETURN 
               END IF
               IF ( EL(2) < EL_MIN ) GOTO 460
!
               START_OFFSET = MAX ( VEX%SCA(J3)%START_OFFSET(J5), VEX%SCA(J3)%START_OFFSET(J6) )
               STOP_OFFSET  = MIN ( VEX%SCA(J3)%START_OFFSET(J5) + VEX%SCA(J3)%SCAN_DUR(J5),   &
     &                              VEX%SCA(J3)%START_OFFSET(J6) + VEX%SCA(J3)%SCAN_DUR(J6)    )
!
! ------------ Check whethe the scan is too short
!
               IF ( STOP_OFFSET - START_OFFSET < SCAN_DUR_MIN ) GOTO 460
!
               SIMUL%NOBS = SIMUL%NOBS + 1
               SIMUL%MJD_OBS(SIMUL%NOBS)   = MJD_OBS
               SIMUL%UTC_OBS(SIMUL%NOBS)   = UTC_OBS
               SIMUL%SOU_IND(SIMUL%NOBS)   = VEX%SCA(J3)%IND_SOU
               SIMUL%STA_IND(1,SIMUL%NOBS) = VEX%SCA(J3)%IND_STA(J5)
               SIMUL%STA_IND(2,SIMUL%NOBS) = VEX%SCA(J3)%IND_STA(J6)
               CALL CLRCH ( SIMUL%SCAN_NAME(SIMUL%NOBS) )
               SIMUL%SCA_IND(SIMUL%NOBS)   = J3
               SIMUL%SCAN_NAME(SIMUL%NOBS) = STR(1:7)
               SIMUL%SCAN_DUR(SIMUL%NOBS)  = STOP_OFFSET - START_OFFSET
               SIMUL%QUALCODE(SIMUL%NOBS)  = '9'
!
               SIMUL%EL(1,SIMUL%NOBS) = EL(1)
               SIMUL%EL(2,SIMUL%NOBS) = EL(2)
               SIMUL%AZ(1,SIMUL%NOBS) = AZ(1)
               SIMUL%AZ(2,SIMUL%NOBS) = AZ(2)
               IF ( .NOT. FL_SCA(J3) ) THEN
                    K_SCA = K_SCA + 1
                    XREF_SCA(J3) = K_SCA
                    FL_SCA(J3) = .TRUE.
               END IF
               IF ( .NOT. FL_SOU(VEX%SCA(J3)%IND_SOU) ) THEN
                    K_SOU = K_SOU + 1
                    XREF_SOU(VEX%SCA(J3)%IND_SOU) = K_SOU
                    FL_SOU(VEX%SCA(J3)%IND_SOU) = .TRUE.
               END IF 
               IF ( .NOT. FL_STA(VEX%SCA(J3)%IND_STA(J5)) ) THEN
                    K_STA = K_STA + 1
                    XREF_STA(VEX%SCA(J3)%IND_STA(J5)) = K_STA 
                    FL_STA(VEX%SCA(J3)%IND_STA(J5)) = .TRUE.
               END IF
               IF ( .NOT. FL_STA(VEX%SCA(J3)%IND_STA(J6)) ) THEN
                    K_STA = K_STA + 1
                    XREF_STA(VEX%SCA(J3)%IND_STA(J6)) = K_STA 
                    FL_STA(VEX%SCA(J3)%IND_STA(J6)) = .TRUE.
               END IF
               IF ( .NOT. FL_BAS_STA(VEX%SCA(J3)%IND_STA(J5)) ) THEN
                    SIMUL%NOBS_STA(VEX%SCA(J3)%IND_STA(J5)) = SIMUL%NOBS_STA(VEX%SCA(J3)%IND_STA(J6))  + 1
               END IF
               IF ( .NOT. FL_BAS_STA(VEX%SCA(J3)%IND_STA(J6)) ) THEN
                    SIMUL%NOBS_STA(VEX%SCA(J3)%IND_STA(J6)) = SIMUL%NOBS_STA(VEX%SCA(J3)%IND_STA(J6))  + 1
               END IF
               SIMUL%OBSTAB(1,SIMUL%NOBS) = J3
               SIMUL%OBSTAB(2,SIMUL%NOBS) = VEX%SCA(J3)%IND_STA(J5)
               SIMUL%OBSTAB(3,SIMUL%NOBS) = VEX%SCA(J3)%IND_STA(J6)
 460        CONTINUE 
 450     CONTINUE 
         IF ( IVRB .GE. 3 ) THEN
              WRITE ( 6, * ) 'VEX_TO_SIMUL-225: I_SCA= ', INT2(J3), ' NOBS = ', SIMUL%NOBS
         END IF
 430  CONTINUE 
!
      IF ( K_SCA < SIMUL%NSCA  .OR.  K_STA < SIMUL%NSTA  .OR.  K_SOU < SIMUL%NSOU  ) THEN
           SIMUL_OLD = SIMUL
           SIMUL%NSCA = K_SCA
           DO 470 J7=1,SIMUL%NOBS
              SIMUL%OBSTAB(1,J7) = XREF_SCA(SIMUL_OLD%OBSTAB(1,J7))
              SIMUL%OBSTAB(2,J7) = XREF_STA(SIMUL_OLD%STA_IND(1,J7))
              SIMUL%OBSTAB(3,J7) = XREF_STA(SIMUL_OLD%STA_IND(2,J7))
              SIMUL%STA_IND(1,J7) = XREF_STA(SIMUL_OLD%STA_IND(1,J7))
              SIMUL%STA_IND(2,J7) = XREF_STA(SIMUL_OLD%STA_IND(2,J7))
 470       CONTINUE 
!
           SIMUL%NSOU = K_SOU
           DO 480 J8=1,SIMUL_OLD%NSOU
              IF ( XREF_SOU(J8) > 0 ) THEN
                   SIMUL%SOU_NAM(XREF_SOU(J8))   = SIMUL_OLD%SOU_NAM(J8)
                   SIMUL%SOU_COO(1,XREF_SOU(J8)) = SIMUL_OLD%SOU_COO(1,J8)
                   SIMUL%SOU_COO(2,XREF_SOU(J8)) = SIMUL_OLD%SOU_COO(2,J8)
              END IF
 480      CONTINUE 
          DO 490 J9=1,SIMUL%NOBS
              SIMUL%SOU_IND(J9)   = XREF_SOU(SIMUL_OLD%SOU_IND(J9))
 490      CONTINUE 
!
           SIMUL%NSTA = K_STA
           DO 4100 J10=1,SIMUL_OLD%NSTA
              IF ( XREF_STA(J10) > 0 ) THEN
                   SIMUL%STA_NAM(XREF_STA(J10))     = SIMUL_OLD%STA_NAM(J10)
                   SIMUL%STA_ID(XREF_STA(J10))      = SIMUL_OLD%STA_ID(J10)
                   SIMUL%STA_COO(1,XREF_STA(J10))   = SIMUL_OLD%STA_COO(1,J10)
                   SIMUL%STA_COO(2,XREF_STA(J10))   = SIMUL_OLD%STA_COO(2,J10)
                   SIMUL%STA_COO(3,XREF_STA(J10))   = SIMUL_OLD%STA_COO(3,J10)
                   SIMUL%SAMPLE_RATE(XREF_STA(J10)) = SIMUL_OLD%SAMPLE_RATE(J10)
                   SIMUL%BITS_SAMPLE(XREF_STA(J10)) = SIMUL_OLD%BITS_SAMPLE(J10)
                   SIMUL%NOBS_STA(XREF_STA(J10))    = SIMUL%NOBS_STA(J10)
              END IF
 4100      CONTINUE 
      END IF
!
      DO 4110 J11=1,SIMUL%NOBS
         SIM_SORT(J11)%OBS_IND = J11
         SIM_SORT(J11)%MJD = SIMUL%MJD_OBS(J11)
         SIM_SORT(J11)%UTC = SIMUL%UTC_OBS(J11)
         SIM_SORT(J11)%STA_IND(1) = SIMUL%STA_IND(1,J11)
         SIM_SORT(J11)%STA_IND(2) = SIMUL%STA_IND(2,J11)
 4110 CONTINUE 
!
! --- Sort SIMUL object first over time, second over the first station index,
! --- third over the second stations index
!
      CALL FOR_QSORT ( SIM_SORT, SIMUL%NOBS, SIZEOF(SIM_SORT(1)), SIM_COMPAR_OBS )
      SIMUL_OLD = SIMUL
      DO 4120 J12=1,SIMUL%NOBS
         SIMUL%SCAN_NAME(J12)   = SIMUL_OLD%SCAN_NAME(SIM_SORT(J12)%OBS_IND)
         SIMUL%OBSTAB(1:3,J12)  = SIMUL_OLD%OBSTAB(1:3,SIM_SORT(J12)%OBS_IND)
         SIMUL%MJD_OBS(J12)     = SIMUL_OLD%MJD_OBS(SIM_SORT(J12)%OBS_IND)
         SIMUL%UTC_OBS(J12)     = SIMUL_OLD%UTC_OBS(SIM_SORT(J12)%OBS_IND)
         SIMUL%SOU_IND(J12)     = SIMUL_OLD%SOU_IND(SIM_SORT(J12)%OBS_IND)
         SIMUL%SCA_IND(J12)     = SIMUL_OLD%SCA_IND(SIM_SORT(J12)%OBS_IND)
         SIMUL%STA_IND(1:2,J12) = SIMUL_OLD%STA_IND(1:2,SIM_SORT(J12)%OBS_IND)
         SIMUL%SCAN_DUR(J12)    = SIMUL_OLD%SCAN_DUR(SIM_SORT(J12)%OBS_IND)
         SIMUL%GR_DEL(1:SIM__MBND,J12) = SIMUL_OLD%GR_DEL(1:SIM__MBND,SIM_SORT(J12)%OBS_IND)
         SIMUL%PH_RAT(1:SIM__MBND,J12) = SIMUL_OLD%PH_RAT(1:SIM__MBND,SIM_SORT(J12)%OBS_IND)
         SIMUL%GR_DEL_ERR(1:SIM__MBND,J12) = SIMUL_OLD%GR_DEL_ERR(1:SIM__MBND,SIM_SORT(J12)%OBS_IND)
         SIMUL%PH_RAT_ERR(1:SIM__MBND,J12) = SIMUL_OLD%PH_RAT_ERR(1:SIM__MBND,SIM_SORT(J12)%OBS_IND)
         SIMUL%SNR(1:SIM__MBND,J12) = SIMUL_OLD%SNR(1:SIM__MBND,SIM_SORT(J12)%OBS_IND)
         SIMUL%AMP(1:SIM__MBND,J12) = SIMUL_OLD%AMP(1:SIM__MBND,SIM_SORT(J12)%OBS_IND)
         SIMUL%NOI(1:SIM__MBND,J12) = SIMUL_OLD%NOI(1:SIM__MBND,SIM_SORT(J12)%OBS_IND)
         SIMUL%EFF_FREQ(1:3,1:SIM__MBND,J12) = SIMUL_OLD%EFF_FREQ(1:3,1:SIM__MBND,SIM_SORT(J12)%OBS_IND)
         SIMUL%REF_FREQ(1:SIM__MBND,J12)     = SIMUL_OLD%REF_FREQ(1:SIM__MBND,SIM_SORT(J12)%OBS_IND)
         SIMUL%EL(1:2,J12)   = SIMUL_OLD%EL(1:2,SIM_SORT(J12)%OBS_IND)
         SIMUL%AZ(1:2,J12)   = SIMUL_OLD%AZ(1:2,SIM_SORT(J12)%OBS_IND)
         SIMUL%ZPD(1:2,J12)  = SIMUL_OLD%ZPD(1:2,SIM_SORT(J12)%OBS_IND)
         SIMUL%SPD(1:2,J12)  = SIMUL_OLD%SPD(1:2,SIM_SORT(J12)%OBS_IND)
         SIMUL%AUTO_SUP(J12) = SIMUL_OLD%AUTO_SUP(SIM_SORT(J12)%OBS_IND)
         SIMUL%USER_SUP(J12) = SIMUL_OLD%USER_SUP(SIM_SORT(J12)%OBS_IND)
         SIMUL%USER_REC(J12) = SIMUL_OLD%USER_REC(SIM_SORT(J12)%OBS_IND)
         SIMUL%QUALCODE(J12) = SIMUL_OLD%QUALCODE(SIM_SORT(J12)%OBS_IND)
 4120 CONTINUE 
!
      CALL CLRCH ( SIMUL%EXPER_NAME  )
      CALL CLRCH ( SIMUL%EXPER_DESCR )
      CALL CLRCH ( SIMUL%REVISION    )
      CALL CLRCH ( SIMUL%PI_NAME     )
      CALL CLRCH ( SIMUL%REVISION    )
      SIMUL%EXPER_NAME  = VEX%EXPER_NAME 
      SIMUL%EXPER_DESCR = VEX%EXPER_DESCR
      CALL TRAN ( 12, SIMUL%EXPER_NAME, SIMUL%EXPER_NAME )
      SIMUL%PI_NAME    = VEX%CONTACT_NAME
      SIMUL%REVISION   = VEX%REVISION  
!
      IF ( VEX%MJD_START < J2000__MJD  .OR.  VEX%MJD_START < J2000__MJD ) THEN
           SIMUL%MJD_BEG = MJD_1ST_OBS
           SIMUL%MJD_END = MJD_LAST_OBS
           SIMUL%UTC_BEG = UTC_1ST_OBS
           SIMUL%UTC_END = UTC_LAST_OBS
      END IF
!
      IF ( LT > 0 ) THEN
           DO 4130 J13=1,LT
              IF ( RENAME_TAB(J13)(1:1) == '#' ) GOTO 4130
              CALL EXWORD ( RENAME_TAB(J13), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IER )
              IF ( LIND < 1 ) GOTO 4130
              DO 4140 J14=1,LIND
                 DO 4150 J15=1,SIMUL%NSTA
                    IF ( TRIM(SIMUL%STA_NAM(J15)) == RENAME_TAB(J13)(IND(1,J14):IND(2,J14)) ) THEN
                         SIMUL%STA_NAM(J15) = RENAME_TAB(J13)(IND(1,1):IND(2,1)) 
                    END IF
 4150            CONTINUE 
!
                 DO 4160 J16=1,SIMUL%NSOU
                    IF ( TRIM(SIMUL%SOU_NAM(J16)) == RENAME_TAB(J13)(IND(1,J14):IND(2,J14)) ) THEN
                         SIMUL%SOU_NAM(J16) = RENAME_TAB(J13)(IND(1,1):IND(2,1)) 
                    END IF
 4160            CONTINUE 
 4140         CONTINUE 
 4130      CONTINUE 
      END IF
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL NERS_GET_UTCMTAI ( NERS, VEX%UTC_START, UTC_M_TAI, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1515, IUER, 'VEX_TO_SIMUL', 'Error in an attempt '// &
     &         'to get UTC minus TAI for the session start time' ) 
           RETURN
      END IF
      TAI_START = VEX%UTC_START - UTC_M_TAI
!
      CALL NERS_GET_UTCMTAI ( NERS, VEX%UTC_STOP, UTC_M_TAI, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1516, IUER, 'VEX_TO_SIMUL', 'Error in an attempt '// &
     &         'to get UTC minus TAI for the session start time' ) 
           RETURN
      END IF
      TAI_STOP = VEX%UTC_STOP - UTC_M_TAI
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL VTD_LOAD ( VTD, VEX%N_STA, VEX%C_STA, VEX%N_SOU, VEX%C_SOU, &
     &                VEX%MJD_START, TAI_START, &
     &                VEX%MJD_STOP,  TAI_STOP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1517, IUER, 'VEX_TO_SIMUL', 'Error in an attempt '// &
     &         'to load the data into VTD data structure' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VEX_TO_SIMUL  !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION   SIM_COMPAR_OBS ( SIM_SORT1, SIM_SORT2 )
! ************************************************************************
! *                                                                      *
! *   Routine for sorting abridged SIMUL objects. Observations are       *
! *   sorted first in chronoloical order, second according the first     *
! *   station index, third according the second station index.           *
! *                                                                      *
! *  ### 10-SEP-2021               v1.0 (c)  L. Petrov  10-SEP-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'simul.i'
      TYPE     ( SIMUL_SORT__TYPE ) :: SIM_SORT1, SIM_SORT2
      REAL*8     TIM_EPS
      PARAMETER  ( TIM_EPS = 1.D-9 )
      INTEGER*4  SIM_COMPAR_OBS
!
      IF ( (SIM_SORT1%MJD - SIM_SORT2%MJD)*86400.0D0 + &
     &     (SIM_SORT1%UTC - SIM_SORT2%UTC)           >  TIM_EPS ) THEN
           SIM_COMPAR_OBS =  1
         ELSE IF ( (SIM_SORT1%MJD - SIM_SORT2%MJD)*86400.0D0 + &
     &             (SIM_SORT1%UTC - SIM_SORT2%UTC)   < -TIM_EPS ) THEN
           SIM_COMPAR_OBS = -1
         ELSE
           IF ( SIM_SORT1%STA_IND(1) > SIM_SORT2%STA_IND(1) ) THEN
                SIM_COMPAR_OBS =  1
              ELSE IF ( SIM_SORT1%STA_IND(1) < SIM_SORT2%STA_IND(1) ) THEN
                SIM_COMPAR_OBS = -1
              ELSE
                IF ( SIM_SORT1%STA_IND(2) > SIM_SORT2%STA_IND(2) ) THEN
                     SIM_COMPAR_OBS =  1
                   ELSE IF ( SIM_SORT1%STA_IND(1) < SIM_SORT2%STA_IND(2) ) THEN
                     SIM_COMPAR_OBS = -1
                   ELSE
                     SIM_COMPAR_OBS =  0
                END IF
           END IF
      END IF
!
      RETURN
      END  FUNCTION  SIM_COMPAR_OBS  !#!#
