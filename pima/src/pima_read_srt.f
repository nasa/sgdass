      SUBROUTINE PIMA_READ_SRT ( PIM, SNR_DETECTION_2, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_READ_SRT
! *                                                                      *
! * ### 15-JAN-2010   PIMA_READ_SRT   v1.1 (c) L. Petrov 15-FEB-2010 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      TYPE     ( VTD__TYPE  ) :: VTD
      REAL*8     SNR_DETECTION_2
      INTEGER*4  IUER
      CHARACTER, ALLOCATABLE :: BUF(:)*1024
      LOGICAL*4  LEX, FL_FOUND
      CHARACTER  STR*128, SCA_NAM*10, SCA_NAM_ARR(PIM__MSCA)*10, STA(2)*8, &
     &           SOU_NAM*8, SRT_DAT*24, REG*5
      PARAMETER  ( REG = CHAR(32)//CHAR(0)//CHAR(9)//'/>' )
      INTEGER*4    EXTRA__LINES
      PARAMETER  ( EXTRA__LINES = 512 )
      REAL*8       USER_FLAG__MIN
      PARAMETER  ( USER_FLAG__MIN = 0.1 )
      INTEGER*4  MIND
      PARAMETER  ( MIND = 32 )
      REAL*8     SRT_OFF, SNR_1, SNR_2, TIM_BEG_OBS, TIM_END_OBS, TAI_SRT, &
     &           VAR, TIM_BEG_MOD_OBS, TIM_END_MOD_OBS, TIM_1ST_AP
      LOGICAL*1, ALLOCATABLE :: FL_USE(:), FL_SRT(:)
      INTEGER*4, ALLOCATABLE :: SCA_IND_ARR(:)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           J13, J14, TIM_IND_MIN, TIM_IND_MAX, UV_IND, MOD_IND_BEG, &
     &           MOD_IND_END, MJD_SRT, NBUF, IND_SCA, STA_IND, N_AP, KOBS, IP, &
     &           SCA_IND, IDAY, MJD_YR, IND_STA(2), IND_SOU, IFMT, &
     &           LIND, IND(2,MIND), MAX_NUM_OBS, FRG_IND, IER
      CHARACTER  MJDSEC_TO_DATE*30, MJDSEC_TO_VEX*22
      INTEGER*4, EXTERNAL :: ADD_CLIST, ILEN, I_LEN, LTM_DIF, MAX_I4
!
      IF ( PIM%CONF%FRIB_OBS_STATUS  == PIMA__OBS_NO ) THEN
           PIM%L_MKDB = 0
           CALL ERR_LOG ( 0, IUER )
           RETURN
         ELSE IF ( PIM%CONF%FRIB_OBS_STATUS  == PIMA__OBS_ALL .AND. &
     &             PIM%CONF%FRIB_NOBS        == -1                  ) THEN
           PIM%CONF%FRIB_NOBS = PIM%NOBS
           IF ( ASSOCIATED ( PIM%CONF%FRIB_OBS ) ) THEN
                DEALLOCATE ( PIM%CONF%FRIB_OBS )
           END IF
!
           ALLOCATE ( PIM%CONF%FRIB_OBS(PIM%CONF%FRIB_NOBS), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 4*PIM%CONF%FRIB_NOBS, STR )
                CALL ERR_LOG ( 7613, IUER, 'PIMA_READ_SRT', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes '//&
     &              'of dynamic memory for the list of observations' )
                RETURN
           END IF
!
           DO 410 J1=1,PIM%CONF%FRIB_NOBS
              PIM%CONF%FRIB_OBS(J1) = J1
 410       CONTINUE
      END IF
!
      ALLOCATE ( BUF(PIM%NOBS+EXTRA__LINES), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( (PIM%NOBS+EXTRA__LINES)*LEN(BUF(1)), STR )
           CALL ERR_LOG ( 7614, IUER, 'PIMA_READ_SRT', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes '//&
     &         'of dynamic memory for the buffer for the file '// &
     &         PIM%CONF%MKDB_SRT_FILE )
           RETURN
      END IF
!
      ALLOCATE ( SCA_IND_ARR(PIM%NOBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*PIM%NOBS, STR )
           CALL ERR_LOG ( 7615, IUER, 'PIMA_READ_SRT', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for the buffer for the file '//PIM%CONF%MKDB_SRT_FILE )
           RETURN
      END IF
      CALL NOUT_I4 ( PIM%NOBS, SCA_IND_ARR )
!
      ALLOCATE ( FL_USE(PIM%NOBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( PIM%NOBS, STR )
           CALL ERR_LOG ( 7615, IUER, 'PIMA_READ_SRT', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for the FL_USE array' )
           RETURN
      END IF
!
      ALLOCATE ( FL_SRT(PIM%NOBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( PIM%NOBS, STR )
           CALL ERR_LOG ( 7615, IUER, 'PIMA_READ_SRT', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for the FL_SRT array' )
           RETURN
      END IF
!
      DO 420 J2=1,PIM%NOBS
         PIM%OBS(J2)%SRT_OFFSET = -1.D20
         FL_USE(J2) = .FALSE.
         DO 430 J3=1,PIM%CONF%FRIB_NOBS
            IF ( J2 == PIM%CONF%FRIB_OBS(J3) ) FL_USE(J2) = .TRUE.
 430     CONTINUE
         FL_SRT(J2) = .FALSE.
         IF ( PIM%CONF%MKDB_SRT_TYPE == PIMA__MKDB_SRT_FRT ) THEN
              PIM%OBS(J2)%SRT_OFFSET = PIM%OBS(J2)%FRT_OFFSET(1)
         END IF
 420  CONTINUE
!
      DO 440 J4=1,PIM__MSCA
         IF ( ALLOCATED ( PIM%SCADB(J4)%OBS_IND ) ) THEN
              DEALLOCATE ( PIM%SCADB(J4)%OBS_IND )
         END IF
         CALL NOUT ( SIZEOF(PIM%SCADB(J4)), PIM%SCADB(J4) )
         IF ( PIM%CONF%MKDB_SRT_TYPE == PIMA__MKDB_FILE ) THEN
              PIM%SCADB(J4)%TIM_BEG =  1.D20
              PIM%SCADB(J4)%TIM_END = -1.D20
            ELSE IF ( PIM%CONF%MKDB_SRT_TYPE == PIMA__MKDB_SRT_FRT ) THEN
              PIM%SCADB(J4)%TIM_BEG =  1.D20
              PIM%SCADB(J4)%TIM_END = -1.D20
         END IF
!
         IF ( PIM%CONF%MKDB_SRT_TYPE == PIMA__MKDB_SRT_FRT ) THEN
              PIM%SCADB(J4)%NAME = PIM%SCA(J4)%SCAN_NAME
              PIM%SCADB(J4)%NOBS = 0
              DO 550 J5=1,PIM%NOBS
                 IF ( .NOT. FL_USE(J5) ) GOTO 550
                 IF ( PIM%OBS(J5)%SCA_IND == J4 ) THEN
                      PIM%SCADB(J4)%NOBS = PIM%SCADB(J4)%NOBS + 1
                 END IF
 550          CONTINUE 
!
              IF ( PIM%SCADB(J4)%NOBS > 0 ) THEN
                   ALLOCATE ( PIM%SCADB(J4)%OBS_IND(PIM%SCADB(J4)%NOBS), STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 7619, IUER, 'PIMA_READ_SRT', 'Failure to '// &
     &                      'allocate dynamic memory for PIM%SCADB%OBS_IND' )
                       RETURN
                   END IF
                   CALL NOUT_I4 ( PIM%SCADB(J4)%NOBS, PIM%SCADB(J4)%OBS_IND )
!
                   KOBS = 0
                   DO 560 J6=1,PIM%NOBS
                      IF ( .NOT. FL_USE(J6) ) GOTO 560
                      IF ( PIM%OBS(J6)%SCA_IND == J4 ) THEN
                           KOBS = KOBS + 1
                           PIM%SCADB(J4)%OBS_IND(KOBS) = J6
                           PIM%SCADB(J4)%SOU_IND = PIM%OBS(J6)%SOU_IND
                      END IF
 560               CONTINUE 
              END IF
!
              IF ( J4 .GE. PIM%NSCA ) THEN
                   PIM%L_MKDB = PIM%NSCA 
                   CALL ERR_LOG ( 0, IUER )
                   RETURN
              END IF
         END IF
 440  CONTINUE
!
      INQUIRE ( FILE=PIM%CONF%MKDB_SRT_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 7611, IUER, 'PIMA_READ_SRT', 'Cannot '// &
     &          'find input MKDB.SRT file '// &
     &           PIM%CONF%MKDB_SRT_FILE(1:I_LEN(PIM%CONF%MKDB_SRT_FILE))// &
     &          ' specified in the control file '//PIM%CONF_FILE )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( PIM%CONF%MKDB_SRT_FILE, PIM%NOBS+EXTRA__LINES, BUF, &
     &                NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7616, IUER, 'PIMA_READ_SRT', 'Failure in an '// &
     &         'attempt to read file '//PIM%CONF%MKDB_SRT_FILE )
           RETURN
      END IF
!
      IF ( BUF(1)(1:LEN(PIMA__TOTAL_LABEL)) == PIMA__TOTAL_LABEL ) THEN
           IFMT = 1
         ELSE IF ( INDEX ( BUF(1), 'Residuals from Solve' ) > 0 .AND. &
     &             INDEX ( BUF(1), 'Format version of 2010.02.14' ) > 0 ) THEN
           IFMT = 2
         ELSE 
           CALL ERR_LOG ( 7616, IUER, 'PIMA_READ_SRT', 'Unrecognized format '// &
     &         'of the input SRT file '// &
     &         PIM%CONF%MKDB_SRT_FILE(1:I_LEN(PIM%CONF%MKDB_SRT_FILE))// &
     &         ' -- the first line does not contain recognizable magic' )
           RETURN 
      END IF
!
      PIM%L_MKDB = 0
      DO 450 J5=1,NBUF
         IF ( BUF(J5)(1:1)  == '#' ) GOTO 450
         IF ( ILEN(BUF(J5)) ==  0  ) GOTO 450
         CALL EXWORD ( BUF(J5), MIND, LIND, IND, REG, IER )
         IF ( ( IFMT == 1 .AND. &
     &               ( BUF(J5)(8:8)     == '|'    .AND. &
     &                 BUF(J5)(19:22)   == 'Sca:' .AND. &
     &                 BUF(J5)(53:56)   == 'Sou:' .AND. &
     &                 BUF(J5)(92:95)   == 'SNR:' .AND. &
     &                 BUF(J5)(114:117) == 'SRT:'       ) &
     &        ) .OR.                                    & 
     &        ( IFMT == 2 .AND. LIND .GE. 5 .AND.       &
     &          INDEX ( BUF(J5), 'Residuals' ) == 0     &
     &        )                                         &
     &                                                  ) THEN
!
              CALL CLRCH  ( SRT_DAT )
              IF ( IFMT == 1 ) THEN
                   SCA_NAM = BUF(J5)(42:51)
                   STA(1)  = BUF(J5)(72:79)
                   STA(2)  = BUF(J5)(83:90)
                   SOU_NAM = BUF(J5)(58:65)
                   SRT_DAT = BUF(J5)(119:142) 
                ELSE IF ( IFMT == 2 ) THEN
                   STA(1)  = BUF(J5)(IND(1,2):IND(2,2))
                   STA(2)  = BUF(J5)(IND(1,3):IND(2,3))
                   SOU_NAM = BUF(J5)(IND(1,4):IND(2,4))
                   SRT_DAT = BUF(J5)(IND(1,5):IND(2,5))
              END IF
              IND_STA(1) = LTM_DIF ( 0, PIM%NSTA, PIM%C_STA, STA(1)  )
              IND_STA(2) = LTM_DIF ( 0, PIM%NSTA, PIM%C_STA, STA(2)  )
              IND_SOU    = LTM_DIF ( 0, PIM%NSOU, PIM%C_SOU, SOU_NAM )
              IF ( IND_STA(1) .LE. 0  .OR.  IND_STA(2) .LE. 0 ) GOTO 450
              IF ( IND_SOU .LE. 0 ) GOTO 450
!
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( SRT_DAT, MJD_SRT, TAI_SRT, IER )
              IF ( IER .NE. 0 ) THEN
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J5, STR )
                        CALL ERR_LOG ( 7617, IUER, 'PIMA_READ_SRT', &
     &                      'Failure in reading SRT offset from the line '// &
     &                       STR(1:I_LEN(STR))//' file '// &
     &                       PIM%CONF%MKDB_SRT_FILE(1:I_LEN(PIM%CONF%MKDB_SRT_FILE))// &
     &                       ' -- cannot decode field '//SRT_DAT )
                        RETURN
                   END IF
              END IF
              IF ( IFMT == 2 ) THEN
                   TAI_SRT = TAI_SRT - PIM%UTC_MTAI
              END IF
!
              DO 460 J6=1,PIM%NOBS
                 IF ( .NOT. FL_USE(J6) ) GOTO 460
                 IF ( FL_SRT(J6)       ) GOTO 460
                 IF ( IFMT == 2 ) THEN 
!
! ------------------- We disable scan name matching in this mode
!
                      SCA_NAM = PIM%SCA(PIM%OBS(J6)%SCA_IND)%SCAN_NAME 
                 END IF 
                 IF ( PIM%SCA(PIM%OBS(J6)%SCA_IND)%SCAN_NAME == SCA_NAM  .AND. &
     &                ( ( PIM%OBS(J6)%STA_IND(1) == IND_STA(1)   .AND. &
     &                    PIM%OBS(J6)%STA_IND(2) == IND_STA(2) )  .OR. &
     &                  ( PIM%OBS(J6)%STA_IND(1) == IND_STA(2)   .AND. &
     &                    PIM%OBS(J6)%STA_IND(2) == IND_STA(1) )       ) .AND. &
     &                PIM%OBS(J6)%SOU_IND == IND_SOU                           ) THEN
!
! ------------------- Get the range of apriori model validity
!
                      DO 470 J7=1,2
                         IF ( PIM%OBS(J6)%MOD_IND_BEG(J7) > 0 ) THEN
                              STA_IND = PIM%OBS(J6)%STA_IND(J7)
                              MOD_IND_BEG = PIM%OBS(J6)%MOD_IND_BEG(J7)
                              MOD_IND_END = PIM%OBS(J6)%MOD_IND_END(J7)
                              IF ( J7 == 1 ) THEN
                                   TIM_BEG_MOD_OBS = PIM%STA(STA_IND)%MOD(MOD_IND_BEG)%TIM_BEG
                                   TIM_END_MOD_OBS = PIM%STA(STA_IND)%MOD(MOD_IND_END)%TIM_END
                                 ELSE IF ( J7 == 2 ) THEN
                                   TIM_BEG_MOD_OBS = MAX ( TIM_BEG_MOD_OBS, &
     &                                     PIM%STA(STA_IND)%MOD(MOD_IND_BEG)%TIM_BEG )
                                   TIM_END_MOD_OBS = MIN ( TIM_END_MOD_OBS, &
     &                                     PIM%STA(STA_IND)%MOD(MOD_IND_END)%TIM_END )
                              END IF
                         END IF
 470                  CONTINUE
!
! ------------------- Bypass observations for which the scan reference time 
! ------------------- is not in the range of model validity
!
                      IF ( (TAI_SRT - PIM%TAI_0) + (MJD_SRT - PIM%MJD_0)*86400.0D0 .GE. TIM_BEG_MOD_OBS .AND. &
     &                     (TAI_SRT - PIM%TAI_0) + (MJD_SRT - PIM%MJD_0)*86400.0D0 .LE. TIM_END_MOD_OBS       ) THEN
                           CONTINUE 
                         ELSE
                           GOTO 460
                      END IF
!
                      IF ( PIM%CONF%FRG_USE == PIMA__COMBINE ) THEN
!
! ------------------------ If the frequency group is combined, we search for the first 
! ------------------------ frequency group that is not empty, i.e. has accumulation
! ------------------------ periods
!
                           DO 480 J8=PIM%CONF%FRG_LIST(1),PIM%CONF%FRG_LIST(2)
                              FRG_IND = PIM%OBS(J6)%REF_FRG_INDS(J8)
                              IF ( FRG_IND == 0 ) GOTO 480
                              UV_IND = PIM%OBS(J6)%UV_IND(1,FRG_IND)
                              IF ( UV_IND == 0 ) GOTO 480
                              GOTO 880
 480                       CONTINUE 
 880                       CONTINUE 
                         ELSE
!
! ------------------------ Normal case: the frequency group index is fixed
!
                          FRG_IND = PIM%OBS(J6)%REF_FRG_INDS(PIM%CONF%FRQ_GRP) 
                      END IF
                      IF ( FRG_IND == 0 ) THEN
!
! ------------------------ FRG_IND is a pathological case: ther are no data in the 
! ------------------------ current frequency group. In order to prevent a crash, let
! ------------------------ us set default frequency group (1)
!
                           FRG_IND = 1
                      END IF
!
                      IF ( PIM%NFRG == 1 ) THEN
                           TIM_1ST_AP = PIM%TIM_R8(PIM%OBS(J6)%TIM_BEG_IND)
                         ELSE
                           TIM_1ST_AP = PIM%TIM_R8( PIM%UV_IND(PIM%OBS(J6)%UV_IND(1,FRG_IND))%TIM_IND )
                      END IF
!
                      PIM%OBS(J6)%SRT_OFFSET = (MJD_SRT - PIM%MJD_0)*86400.0D0 + &
     &                                         (TAI_SRT - PIM%TAI_0) - &
     &                                         TIM_1ST_AP
                      FL_SRT(J6) = .TRUE.
!
                      IND_SCA = ADD_CLIST ( PIM__MSCA, PIM%L_MKDB, SCA_NAM_ARR, &
     &                                      SCA_NAM, -2 )
                      SCA_IND_ARR(J6) = IND_SCA
                      PIM%SCADB(IND_SCA)%NOBS    = PIM%SCADB(IND_SCA)%NOBS + 1
                      PIM%SCADB(IND_SCA)%SOU_IND = PIM%OBS(J6)%SOU_IND
                      PIM%SCADB(IND_SCA)%MJD_SRT = MJD_SRT
                      PIM%SCADB(IND_SCA)%TAI_SRT = TAI_SRT
!
! ------------------- Find TIM_BEG_OBS and TIM_END_OBS --
! ------------------- time tag of the first and last valid AP at this observation
!
                      TIM_BEG_OBS =  1.D20
                      TIM_END_OBS = -1.D20
                      TIM_IND_MIN = PIM__MEPC + 1
                      TIM_IND_MAX = -1
                      N_AP = 0
!
! ------------------- Check all epochs
!
                      MAX_NUM_OBS = MAX_I4 ( PIM__MUVS, PIM%OBS(J6)%NUM_EPC )
                      DO 490 J9=1,MAX_NUM_OBS
                         DO 4100 J10=1,PIM%OBS(J6)%NUVS
                            UV_IND = PIM%OBS(J6)%UV_IND(J9,J10)
                            IF ( UV_IND > 0 ) GOTO 8100
 4100                     CONTINUE
 8100                    CONTINUE
                         IF ( PIM%CONF%CORR_FLAG_MIN .GE. -2 ) THEN
                              IF ( PIM%OBS(J6)%CORR_FLAG(J9,PIM%CONF%FRQ_GRP) .LE. &
     &                             PIM%CONF%CORR_FLAG_MIN         ) THEN
                                   GOTO 490
                              END IF
                         END IF
                         IF ( ILEN(PIM%CONF%TIME_FLAG_FILE) > 0   .AND. &
     &                        ASSOCIATED ( PIM%OBS(J6)%USER_FLAG )      ) THEN
                              IF ( PIM%OBS(J6)%USER_FLAG(J9) < USER_FLAG__MIN ) THEN
                                   GOTO 490
                              END IF
                         END IF
                         N_AP = N_AP + 1
!
                         TIM_IND_MIN = MIN ( TIM_IND_MIN, PIM%UV_IND(UV_IND)%TIM_IND )
                         TIM_IND_MAX = MAX ( TIM_IND_MAX, PIM%UV_IND(UV_IND)%TIM_IND )
!
                         IF ( PIM%TIM_R8(PIM%UV_IND(UV_IND)%TIM_IND) < TIM_BEG_OBS ) THEN
                              TIM_BEG_OBS = PIM%TIM_R8(PIM%UV_IND(UV_IND)%TIM_IND)
                         END IF
                         IF ( PIM%TIM_R8(PIM%UV_IND(UV_IND)%TIM_IND) > TIM_END_OBS ) THEN
                              TIM_END_OBS = PIM%TIM_R8(PIM%UV_IND(UV_IND)%TIM_IND)
                         END IF
 490                  CONTINUE
!
! ------------------- Check time epochs of the model. Epochs of the model sets
! ------------------- a constriaint on observation-wide TIM_BEG/TIM_END
!
                      IF ( TIM_BEG_MOD_OBS > TIM_BEG_OBS ) THEN
                           TIM_BEG_OBS = TIM_BEG_MOD_OBS 
                      END IF
                      IF ( TIM_END_MOD_OBS < TIM_BEG_OBS ) THEN
                           TIM_END_OBS = TIM_END_MOD_OBS 
                      END IF
!
                      PIM%SCADB(IND_SCA)%TIM_BEG = MIN ( PIM%SCADB(IND_SCA)%TIM_BEG, &
     &                                                   TIM_BEG_OBS )
                      PIM%SCADB(IND_SCA)%TIM_END = MAX ( PIM%SCADB(IND_SCA)%TIM_END, &
     &                                                   TIM_END_OBS )
                 END IF
 460          CONTINUE
         END IF
 450  CONTINUE
!
      DO 4120 J12=1,PIM%NOBS
!
! ------ We check, whether this observation is marked as eligible
! ------ for fringing. If not, we bypass it
!
         IF ( .NOT. FL_USE(J12) ) GOTO 4120
!
! ------ Bypass scans with no fringes at both bands
!
         IF ( BTEST ( PIM%OBS(J12)%FRI_STS(1), REA__PIM ) ) THEN
              IF ( BTEST ( PIM%OBS(J12)%FRI_STS(1), FAI__PIM ) ) THEN
                   SNR_1 = 0.0D0
                 ELSE
                   SNR_1 = PIM%OBS(J12)%AMPL(PIMA__DRF,1)/PIM%OBS(J12)%NOISE(1)
              END IF
            ELSE
              SNR_1 = 0.0D0
         END IF
         IF ( BTEST ( PIM%OBS(J12)%FRI_STS(2), REA__PIM ) ) THEN
              IF ( BTEST ( PIM%OBS(J12)%FRI_STS(2), FAI__PIM ) ) THEN
                   SNR_2 = 0.0D0
                 ELSE
                   SNR_2 = PIM%OBS(J12)%AMPL(PIMA__DRF,2)/PIM%OBS(J12)%NOISE(2)
              END IF
              IF ( SNR_1 < PIM%CONF%FRIB_SNR_DETECTION  .AND. &
     &             SNR_2 < SNR_DETECTION_2                    ) THEN
                   GOTO 4120
              END IF
            ELSE
              IF ( SNR_1 < PIM%CONF%FRIB_SNR_DETECTION ) THEN
                   GOTO 4120
              END IF
         END IF
         IF ( SCA_IND_ARR(J12) == 0 ) THEN
              IF ( PIM%CONF%WARNING ) THEN
                   WRITE ( 6, 111 ) J12, SNR_1, SNR_2
 111               FORMAT ( 'PIMA_READ_SRT: No scan was associated with ', &
     &                      'observation ', I7, ' SNR_1: ', F9.2, &
     &                      ' SNR_2: ', F9.2 )
              END IF
              IF ( PIM%CONF%CHECK_SEVERITY .GE. 2 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J12, STR )
                   CALL ERR_LOG ( 7618, IUER, 'PIMA_READ_SRT', 'No scan was '// &
     &                 'associated with observation '//STR )
                   RETURN
              END IF
         END IF
 4120 CONTINUE
!
      DO 4130 J13=1,PIM%L_MKDB
         ALLOCATE ( PIM%SCADB(J13)%OBS_IND(PIM%SCADB(J13)%NOBS), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7619, IUER, 'PIMA_READ_SRT', 'Failure to '// &
     &            'allocate dynamic memory for PIM%SCADB%OBS_IND' )
              RETURN
         END IF
         CALL NOUT_I4 ( PIM%SCADB(J13)%NOBS, PIM%SCADB(J13)%OBS_IND )
!
         KOBS = 0
         SCA_IND = 0
         DO 4140 J14=1,PIM%NOBS
            IF ( SCA_IND_ARR(J14) == J13 .AND. FL_USE(J14) ) THEN
                 KOBS = KOBS + 1
                 PIM%SCADB(J13)%OBS_IND(KOBS) = J14
                 SCA_IND = PIM%OBS(J14)%SCA_IND
            END IF
 4140    CONTINUE
!
         STR = MJDSEC_TO_VEX ( PIM%SCADB(J13)%MJD_SRT, PIM%SCADB(J13)%TAI_SRT, -2 )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              WRITE ( UNIT=STR(1:22), FMT='(1PD22.15)' ) PIM%SCADB(J13)%TAI_SRT
              CALL ERR_LOG ( 7747, IUER, 'PIMA_READ_SRT', 'Trap of '// &
     &            'internal control: PIM%SCADB(J13)%TAI_SRT = '//STR )
              RETURN
         END IF
!
         CALL CLRCH ( PIM%SCADB(J13)%NAME )
         PIM%SCADB(J13)%NAME = STR(6:8)//'_'//STR(10:11)//STR(13:14)//STR(16:17)
         IF ( J13 > 1 ) THEN
              IF ( PIM%SCADB(J13)%NAME(1:10) == PIM%SCADB(J13-1)%NAME(1:10) ) THEN
                   IF ( PIM%SCADB(J13-1)%NAME(12:12) == '_' ) THEN
                        IP = 0
                      ELSE
                        IP = ICHAR( PIM%SCADB(J13-1)%NAME(12:12) ) - 96
                   END IF
                   PIM%SCADB(J13)%NAME = PIM%SCADB(J13)%NAME(1:10)// &
     &                                   CHAR(96+IP+1)//PIM%SCADB(J13)%NAME(12:)
              END IF
         END IF
!
         CALL CLRCH   ( STR )
         CALL INCH    ( SCA_IND, STR )
         CALL CHASHR  ( STR(1:4) )
         CALL BLANK_TO_ZERO ( STR(1:4) )
         PIM%SCADB(J13)%NAME = PIM%SCADB(J13)%NAME(1:I_LEN(PIM%SCADB(J13)%NAME))// &
     &                             '_'//STR(1:4)
 4130 CONTINUE
!
      DEALLOCATE ( FL_SRT )
      DEALLOCATE ( FL_USE )
      DEALLOCATE ( SCA_IND_ARR )
      DEALLOCATE ( BUF )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PIMA_READ_SRT  !#!
