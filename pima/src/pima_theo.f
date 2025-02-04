      SUBROUTINE PIMA_THEO ( PIM, VTD, MODE_TIM, MODE_REF, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_THEO
! *                                                                      *
! *  ### 30-JAN-2006   PIMA_THEO   v1.9 (c)  L. Petrov  03-MAY-2020 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE     ) :: PIM
      TYPE     ( VTD__TYPE      ) :: VTD
      TYPE     ( VTD__OBS_TYPE  ) :: OBS_TYP
      INTEGER*4  IUER
      CHARACTER  MODE_TIM*(*), MODE_REF*(*)
      REAL*8     DER_DEL(VTD__NDER), DER_RAT(VTD__NDER), AZ(2), ELEV(2), &
     &           UVW(3), TIM_ARG, FEED1_ADD, FEED2_ADD, TIM_1ST_AP
      LOGICAL*1  FL_FEED_REVERSE, FL_FEED1_ADD, FL_FEED2_ADD
      CHARACTER  STR*128, STR1*128
      INTEGER*4  J1, J2, J3, J4, FRG_IND, IND_1ST, IND_2ND, UV_IND, L_NOD, &
     &           L_DEG, NZO_REF, IER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      FL_FEED_REVERSE = .FALSE.
      CALL GETENVAR ( 'PIMAVAR_FEED_REVERSE', STR )
      IF ( STR(1:3) == 'YES' .OR.  STR(1:3) == 'yes' ) FL_FEED_REVERSE = .TRUE.
!
      FL_FEED1_ADD = .FALSE.
      CALL GETENVAR ( 'PIMAVAR_FEED1_ADD_DEG', STR )
      IF ( ILEN(STR) > 0 ) THEN
           IF ( INDEX ( STR, '.' ) < 1 ) STR = TRIM(STR)//'.0'
           READ ( UNIT=STR, FMT='(F10.5)' ) FEED1_ADD
           FEED1_ADD = FEED1_ADD*DEG__TO__RAD
           WRITE ( 6, '(A)' ) 'PIMA_THEO: extra feed1 angle in deg: '//TRIM(STR)
           FL_FEED1_ADD = .TRUE.
      END IF
!
      FL_FEED2_ADD = .FALSE.
      CALL GETENVAR ( 'PIMAVAR_FEED2_ADD_DEG', STR )
      IF ( ILEN(STR) > 0 ) THEN
           IF ( INDEX ( STR, '.' ) < 1 ) STR = TRIM(STR)//'.0'
           READ ( UNIT=STR, FMT='(F10.5)' ) FEED2_ADD
           FEED2_ADD = FEED2_ADD*DEG__TO__RAD
           WRITE ( 6, '(A)' ) 'PIMA_THEO: extra feed2 angle in deg: '//TRIM(STR)
           FL_FEED2_ADD = .TRUE.
      END IF
!
! --- Initlialize VTD object
!
      IF ( VTD%STATUS == VTD__LOAD ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_QUIT ( VTD,  IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 3711, IUER, 'PIMA_THEO', 'Error in an '// &
     &              'attempt to release memory in the VTD oibject' )
                RETURN 
           END IF
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_INIT ( VTD,  IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3712, IUER, 'PIMA_THEO', 'Error in an attempt to '// &
     &         'initialize VTD oibject' )
           RETURN 
      END IF
!
! --- Read and parse configuration file
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_CONF ( PIM%CONF%VTD_CONFIG_FILE, VTD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3713, IUER, 'PIMA_THEO', 'Error in an attempt '// &
     &         'to read configuration file '//PIM%CONF%VTD_CONFIG_FILE )
           RETURN 
      END IF
!
! --- Load catalogues, ephemerides, EOP series and other data files
!
      PIM%C_STA(PIM%NSTA+1) = 'GEOCENTR' 
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_LOAD ( VTD, PIM%NSTA+1, PIM%C_STA, PIM%NSOU, PIM%C_SOU, &
     &                PIM%MJD_0, PIM%TAI_0 - PIM__MSCL, PIM%MJD_0, &
     &                PIM%TAI_0 + PIM%TIM_R8(PIM%NEPC) + PIM__MSCL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3714, IUER, 'PIMA_THEO', 'Error in an '// &
     &         'attempt to load the data into VTD data structure' )
           RETURN 
      END IF
!
! --- Disable automatic NERS update during run
!
      VTD%NERS%CNF%AGE_FCS = 1.D15
      VTD%NERS%CNF%AGE_SPL = 1.D15
!
      IF ( ILEN(PIM%CONF%EPHEMERIDES_FILE) > 0 ) THEN
!
! -------- Read the ephemeride of the orbiting station
!
           PIM%NZO%FILNZO = PIM%CONF%EPHEMERIDES_FILE
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_READ_NZO ( PIM%NZO%FILNZO, PIM__MNZO, PIM%NZO%L_NZO, &
     &                         PIM%NZO%MJD_ARR, PIM%NZO%TIM_ARR, PIM%NZO%POS_ARR, &
     &                         PIM%NZO%VEL_ARR, PIM%NZO%NZO_NAME, PIM%NZO%OBJ_TYPE, &
     &                         PIM%NZO%CENTER_NAME, PIM%NZO%REF_NAME, &
     &                         PIM%NZO%TIM_CODE, PIM%NZO%COO_CODE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 3715, IUER, 'PIMA_THEO', 'Error in an '// &
     &              'attempt to read NZO data into VTD data structure' )
                RETURN 
           END IF
           IF ( PIM%NZO%CENTER_NAME == 'EARTH BARYCENTER' ) THEN
                NZO_REF = VTD__EME
              ELSE 
                CALL ERR_LOG ( 3716, IUER, 'PIMA_THEO', 'Unsupported '// &
     &              'coordinate center name: '//PIM%NZO%CENTER_NAME )
                RETURN 
           END IF
!
! -------- Expand the orbiting station ephemeride into the B-spline basis
!
           L_NOD = MIN ( PIM%NZO%L_NZO/PIMA__NZO_NOT_SCAL, VTD__M_NOD )
           L_DEG = 3
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_LOAD_OBJ_NZO ( PIM%NZO%NZO_NAME, VTD__ES, VTD__OR, &
     &                             NZO_REF, PIM%NZO%TIM_CODE, &
     &                             VTD, PIM%NZO%L_NZO, PIM%NZO%MJD_ARR, &
     &                             PIM%NZO%TIM_ARR, PIM%NZO%POS_ARR, &
     &                             L_NOD, L_DEG, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 3717, IUER, 'PIMA_THEO', 'Error in an '// &
     &              'attempt to read NZO data into VTD data structure' )
                RETURN 
           END IF
      END IF
!
      OBS_TYP%PLRZ       = 'RR'     
      OBS_TYP%FRQ_REF(1) = PIM%FREQ_ARR(1,MIN(PIM%CONF%BEG_FRQ,PIM%NFRQ),MIN(PIM%CONF%FRQ_GRP,PIM%NFRG))
      OBS_TYP%N_BND      = 1
      OBS_TYP%DELAY_TYPE = VTD__ML__DTP
      OBS_TYP%FRQ_ION_EFF(1) = PIM%FREQ_ARR(1,MIN(PIM%CONF%BEG_FRQ,PIM%NFRQ),MIN(PIM%CONF%FRQ_GRP,PIM%NFRG))
      OBS_TYP%STATUS     = VTD__BND 
!
      DO 410 J1=1,PIM%NOBS
         OBS_TYP%DELAY_TYPE  = VTD__ML__DTP
         CALL ERR_PASS ( IUER, IER )
!
         IF ( PIM%CONF%FRG_USE == PIMA__COMBINE ) THEN
!
! ----------- If the frequency group is combined, we search for the first 
! ----------- frequency group that is not empty, i.e. has accumulation
! ----------- periods
!
              DO 420 J2=PIM%CONF%FRG_LIST(1),PIM%CONF%FRG_LIST(2)
                 FRG_IND = PIM%OBS(J1)%REF_FRG_INDS(J2)
                 IF ( FRG_IND == 0 ) GOTO 420
                 UV_IND  = PIM%OBS(J1)%UV_IND(1,FRG_IND)
                 IF ( UV_IND == 0 ) GOTO 420
                 GOTO 820
 420          CONTINUE 
 820          CONTINUE 
           ELSE
!
! ---------- Normal case: the frequency group index is fixed
!
             FRG_IND = 1
         END IF
         IF ( FRG_IND == 0 ) THEN
!
! ----------- This observations does not have an associated frequncy ID.
! ----------- Skip it, but first put something unusual in the output 
! ----------- arrays
!
              PIM%OBS(J1)%THE_GR_DEL = -1.0D0
              PIM%OBS(J1)%THE_PH_DEL = -1.0D0
              PIM%OBS(J1)%THE_RATE   = -1.0D0
              PIM%OBS(J1)%UVW        = -1.0D0
              DER_DEL                = -1.0D0
              DER_RAT                = -1.0D0
              PIM%OBS(J1)%FEED_ANG(1)      = -1.0D0
              PIM%OBS(J1)%FEED_ANG(2)      = -1.0D0
              PIM%OBS(J1)%FEED_ANG_RATE(1) = -1.0D0
              PIM%OBS(J1)%FEED_ANG_RATE(2) = -1.0D0
              GOTO 410
         END IF
!
         IF ( PIM%OBS(J1)%TIM_BEG_IND < 1        .OR. &
     &        PIM%OBS(J1)%TIM_BEG_IND > PIM%NEPC      ) THEN
!
              WRITE ( 6, * ) 'J1= ', J1, &
     &                       ' PIM%OBS(J1)%TIM_BEG_IND = ', PIM%OBS(J1)%TIM_BEG_IND, &
     &                       ' PIM%NEPC= ', PIM%NEPC
              CALL ERR_LOG ( 3718, IUER, 'PIMA_THEO', 'Trap of internal '// &
     &             'control. Apparently, file '// &
     &              PIM%CONF%EXPER_DIR(1:I_LEN(PIM%CONF%EXPER_DIR))//'/'// &
     &              PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//' is '// &
     &             'damaged. You may need re-load the experiment' )
              RETURN 
         END IF
!
         IF ( PIM%NFRG == 1 ) THEN
              TIM_1ST_AP = PIM%TIM_R8(PIM%OBS(J1)%TIM_BEG_IND)
            ELSE
              TIM_1ST_AP = PIM%TIM_R8( PIM%UV_IND(PIM%OBS(J1)%UV_IND(1,FRG_IND))%TIM_IND )
         END IF
         IF ( MODE_TIM == 'OBS_BEG' ) THEN
              TIM_ARG = PIM%TAI_0 + TIM_1ST_AP
            ELSE IF ( MODE_TIM == 'OBS_SRT' ) THEN
!
! ----------- i.e. SRT_OFFSET is within a maximum scan length
!
              IF ( DABS(PIM%OBS(J1)%SRT_OFFSET) < PIM__MSCL ) THEN
                   TIM_ARG = PIM%TAI_0 + TIM_1ST_AP + PIM%OBS(J1)%SRT_OFFSET
                 ELSE 
                   TIM_ARG = PIM%TAI_0 + TIM_1ST_AP 
              END IF
            ELSE 
              CALL ERR_LOG ( 3720, IUER, 'PIMA_THEO', 'Unknown time '// &
     &            'mode '//MODE_TIM(1:I_LEN(MODE_TIM))//' -- one of '// &
     &            'OBS_BEG or OBS_SRT were expected' )
              RETURN 
         END IF
!
         IF ( MODE_REF == '1ST_STA' ) THEN
              IF ( PIM%C_STA(PIM%OBS(J1)%STA_IND(1)) < &
     &             PIM%C_STA(PIM%OBS(J1)%STA_IND(2))   ) THEN
                   IND_1ST = 1
                   IND_2ND = 2
                 ELSE 
                   IND_1ST = 2
                   IND_2ND = 1
              END IF
            ELSE 
              IND_1ST = 1
              IND_2ND = 2
         END IF
!
         CALL VTD_DELAY ( PIM%C_SOU(PIM%OBS(J1)%SOU_IND),    &
     &                 PIM%C_STA(PIM%OBS(J1)%STA_IND(IND_1ST)), &
     &                 PIM%C_STA(PIM%OBS(J1)%STA_IND(IND_2ND)), &
     &                 PIM%MJD_0, TIM_ARG, OBS_TYP, VTD, &
     &                 PIM%OBS(J1)%THE_GR_DEL, PIM%OBS(J1)%THE_RATE, &
     &                 DER_DEL, DER_RAT, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 3721, IUER, 'PIMA_THEO', 'Error in an '// &
     &            'attempt to compute theoretical path delay for '// &
     &            'observation '//STR )
              RETURN 
         END IF
         IF ( PIM%C_STA(PIM%OBS(J1)%STA_IND(1)) > &
     &        PIM%C_STA(PIM%OBS(J1)%STA_IND(2))   ) THEN
              PIM%OBS(J1)%THE_GR_DEL = -PIM%OBS(J1)%THE_GR_DEL
              PIM%OBS(J1)%THE_RATE   = -PIM%OBS(J1)%THE_RATE   
         END IF         
!
         OBS_TYP%DELAY_TYPE  = VTD__PL__DTP
         CALL ERR_PASS ( IUER, IER )
         CALL VTD_DELAY ( PIM%C_SOU(PIM%OBS(J1)%SOU_IND),    &
     &                 PIM%C_STA(PIM%OBS(J1)%STA_IND(1)), &
     &                 PIM%C_STA(PIM%OBS(J1)%STA_IND(2)), &
     &                 PIM%MJD_0, TIM_ARG, &
     &                 OBS_TYP, VTD, PIM%OBS(J1)%THE_PH_DEL, &
     &                 PIM%OBS(J1)%THE_RATE, DER_DEL, DER_RAT, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 3722, IUER, 'PIMA_THEO', 'Error in an '// &
     &            'attempt to compute theoretical path delay for '// &
     &            'observation '//STR )
              RETURN 
         END IF
         IF ( PIM%C_STA(PIM%OBS(J1)%STA_IND(1)) > &
     &        PIM%C_STA(PIM%OBS(J1)%STA_IND(2))   ) THEN
              PIM%OBS(J1)%THE_PH_DEL = -PIM%OBS(J1)%THE_PH_DEL
              PIM%OBS(J1)%THE_RATE   = -PIM%OBS(J1)%THE_RATE
         END IF         
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 7 ) THEN
              STR  = MJDSEC_TO_DATE ( PIM%MJD_0, TIM_ARG, -2 )
              WRITE ( 6, 210 ) J1, STR(1:30), PIM%C_STA(PIM%OBS(J1)%STA_IND(1)), &
     &                                        PIM%C_STA(PIM%OBS(J1)%STA_IND(2)), &
     &                                        PIM%C_SOU(PIM%OBS(J1)%SOU_IND), &
     &                                        PIM%OBS(J1)%THE_GR_DEL, &
     &                                        PIM%OBS(J1)%THE_RATE
 210          FORMAT ( 'PIMA_THEO-237 ind_obs: ', I6, ' tai: ', A, ' Bas: ', A, ' / ', A, &
     &                 ' Sou: ', A, ' Gr_del: ', 1PD20.12, ' Gr_rat: ', 1PD20.12 )
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL VTD_GET_AZEL ( PIM%C_SOU(PIM%OBS(J1)%SOU_IND),    &
     &                 PIM%C_STA(PIM%OBS(J1)%STA_IND(1)), &
     &                 PIM%C_STA(PIM%OBS(J1)%STA_IND(2)), &
     &                 PIM%MJD_0, TIM_ARG, VTD, AZ, ELEV, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 3723, IUER, 'PIMA_THEO', 'Error in an '// &
     &            'attempt to compute azimuth and elevation delay for '// &
     &            'observation '//STR )
              RETURN 
         END IF
         PIM%OBS(J1)%AZ   = AZ
         PIM%OBS(J1)%ELEV = ELEV
!
         IF ( PIM%CONF%BEG_FRQ > PIM%NFRQ ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( PIM%CONF%BEG_FRQ, STR )
              CALL CLRCH ( STR )
              CALL INCH  ( PIM%NFRQ, STR )
              CALL ERR_LOG ( 3724, IUER, 'PIMA_THEO', 'Wrong parameter '// &
     &            'BEG_FRQ: '//TRIM(STR)//' -- it exceeds the number of '// &
     &            'IFs in this experiment: '//STR1 )
              RETURN 
         END IF
!
         IF ( PIM%CONF%END_FRQ > PIM%NFRQ ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( PIM%CONF%END_FRQ, STR )
              CALL CLRCH ( STR1 )
              CALL INCH  ( PIM%NFRQ, STR1 )
              CALL ERR_LOG ( 3725, IUER, 'PIMA_THEO', 'Wrong parameter '// &
     &            'END_FRQ: '//TRIM(STR)//' -- it exceeds the number of '// &
     &            'IFs in this experiment: '//STR1 )
              RETURN 
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL VTD_GET_UVW ( PIM%C_SOU(PIM%OBS(J1)%SOU_IND),    &
     &                      PIM%C_STA(PIM%OBS(J1)%STA_IND(1)), &
     &                      PIM%C_STA(PIM%OBS(J1)%STA_IND(2)), &
     &                      PIM%MJD_0, TIM_ARG, &
     &                      PIM%FREQ_ARR(1,MIN(PIM%CONF%BEG_FRQ,PIM%NFRQ),MIN(PIM%CONF%FRQ_GRP,PIM%NFRG)), &
     &                      VTD, PIM%OBS(J1)%UVW, IER )

         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 3726, IUER, 'PIMA_THEO', 'Error in an '// &
     &            'attempt to extract UVW projections of the baseline '// &
     &            'vector for observation '//STR )
              RETURN 
         END IF
!
         PIM%OBS(J1)%FEED_ANG(1) = DER_DEL(VTD__FEED1)
         PIM%OBS(J1)%FEED_ANG(2) = DER_DEL(VTD__FEED2)
         PIM%OBS(J1)%FEED_ANG_RATE(1) = DER_RAT(VTD__FEED1)
         PIM%OBS(J1)%FEED_ANG_RATE(2) = DER_RAT(VTD__FEED1)
!
         IF ( FL_FEED_REVERSE ) THEN
              PIM%OBS(J1)%FEED_ANG(1) = DER_DEL(VTD__FEED2)
              PIM%OBS(J1)%FEED_ANG(2) = DER_DEL(VTD__FEED1)
         END IF
         IF ( FL_FEED1_ADD ) PIM%OBS(J1)%FEED_ANG(1) = PIM%OBS(J1)%FEED_ANG(1) + FEED1_ADD
         IF ( FL_FEED2_ADD ) PIM%OBS(J1)%FEED_ANG(2) = PIM%OBS(J1)%FEED_ANG(2) + FEED2_ADD
 410  CONTINUE 
!
! --- Copy antenna diameter
!
      DO 440 J4=1,PIM%NSTA
         PIM%STA(J4)%ANT_DIAM = VTD%STA(J4)%ANT_DIAM
 440  CONTINUE 
!
      PIM%THE_STATUS = PIMA__LOADED    
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_THEO !#!  
