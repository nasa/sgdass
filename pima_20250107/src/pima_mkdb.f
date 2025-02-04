      SUBROUTINE PIMA_MKDB ( PIM, VTD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_MKDB
! *                                                                      *
! *  ### 02-JUL-2009   PIMA_MKDB   v1.15 (c) L. Petrov  30-MAY-2020 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'pima_local.i'
      INCLUDE   'gvh.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      TYPE     ( PIMA__TYPE ) :: PIM_2ND
      TYPE     ( VTD__TYPE  ) :: VTD
      TYPE     ( GVH__STRU  ) :: GVH
      INTEGER*4  IUER
      REAL*8     SNR_1, SNR_2, TIM_SCA_FACT, SNR_DETECTION_2, TIM_SRT_LAST
      INTEGER*4  M_HEA
      PARAMETER  ( M_HEA = 128 )
      CHARACTER  STR*128, FILHEA*128, HEA(M_HEA)*256, SRT_DATE_STR*32, &
     &           STR_BAS*16, PIMA_FRI_USED_VERS*24
      INTEGER*4  J0, J1, J2, J3, J4, J5, J6, J7, J8, FRG_IND, &
     &           IND_OBS, KOBS, IS, OBS_DB_IND, IND_OBS_LAST, &
     &           SWAP_STA, IND_OBS_2ND, IND_SCA, IND_FRA, N_MIS, NH, IER
      INTEGER*4  NOBS_STA(PIM__MSTA), NOBS_STA_SRT(PIM__MSTA), NUMB_OBS, &
     &           NUMB_SCA, NUMB_SOU, NUMB_STA, NUMB_BAS, &
     &           OBS_TAB(3,PIM__MOBS), L_OPT, STA_OBS(PIM__MSTA), &
     &           IND_STA(2), LUN, L_STA_CAB, UV_STA_ORDER 
      REAL*8     TIM_EPS
      PARAMETER  ( TIM_EPS = 1.D-7 )
      CHARACTER  PIMAVAR_ZERO_FRINGE_RES*32
      CHARACTER  C_SOU(PIM__MSOU)*8,  C_SOU_SRT(PIM__MSOU)*8,  &
     &           C_STA(PIM__MSTA)*8,  C_STA_SRT(PIM__MSTA)*8,  &
     &           C_BAS(PIM__MBAS)*16, C_BAS_SRT(PIM__MBAS)*16, &
     &           C_SCA(PIM__MSCA)*16, C_STA_CAB(PIM__MSTA)*8,  &
     &           KEYWORD(PIM__MOPT)*80, VALUE(PIM__MOPT)*80, STA_NAM(2)*8
      LOGICAL*1  FL_ERR, FL_MKDB_NOSORT, LEX
!
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_TZ_CDATE*26
      INTEGER*4, EXTERNAL :: ADD_CLIST, ILEN, I_LEN, LTM_DIF, GET_UNIT
!
      CALL GETENVAR ( 'PIMAVAR_ZERO_FRINGE_RES', PIMAVAR_ZERO_FRINGE_RES )
      FL_MKDB_NOSORT = .FALSE.
      CALL GETENVAR ( 'PIMAVAR_MKDB_NOSORT', STR )
      CALL TRAN ( 11, STR, STR )
      IF ( STR(1:3) == 'YES' ) FL_MKDB_NOSORT = .TRUE.
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_FRI_READ ( PIM, 1, PIMA_FRI_USED_VERS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7711, IUER, 'PIMA_MKDB', 'Error in an attempt to '// &
     &         'read results of fringing from the fringe file '// &
     &          PIM%CONF%FRINGE_FILE )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_RESID_REA ( PIM, 1, PIM%CONF%FRIRES_FILE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7712, IUER, 'PIMA_MKDB', 'Error in an attempt to '// &
     &         'read fringe residual file '//PIM%CONF%FRIRES_FILE )
           RETURN
      END IF
!
      IF ( ILEN(PIM%CONF%MKDB_2ND_BAND_FILE) > 0 ) THEN
!
! -------- Initialize the second PIM object
!
           CALL PIMA_INIT ( PIM_2ND )
!
           PIM_2ND%CONF%ACT_CODE = PIM%CONF%ACT_CODE
           L_OPT = 0
           CALL ERR_PASS  ( IUER, IER )
           CALL PIMA_CONF ( PIM%CONF%MKDB_2ND_BAND_FILE, PIM_2ND, L_OPT, &
     &                      KEYWORD, VALUE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7713, IUER, 'PIMA_MKDB', 'Error in an '// &
     &              'attempt to parse configuration file for the 2nd '// &
     &              'band control file '//PIM%CONF%MKDB_2ND_BAND_FILE )
                RETURN
           END IF
           SNR_DETECTION_2 = PIM%CONF%FRIB_SNR_DETECTION
!
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_FRI_READ ( PIM, 2, PIMA_FRI_USED_VERS, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7714, IUER, 'PIMA_MKDB', 'Error in an '// &
     &              'attempt to read results of fringing from the '// &
     &              '2nd band control file '//PIM%CONF%MKDB_2ND_BAND_FILE )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_RESID_REA ( PIM, 2, PIM_2ND%CONF%FRIRES_FILE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7715, IUER, 'PIMA_MKDB', 'Error in an '// &
     &               'attempt to read the 2-nd fringe residual file '// &
     &                PIM_2ND%CONF%FRIRES_FILE )
                RETURN
           END IF
         ELSE
           SNR_DETECTION_2 = 0.0D0
      END IF
!
      IF ( PIM%CONF%MKDB_SRT_TYPE == PIMA__MKDB_MID_SCAN ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_CREATE_SRT ( PIM, SNR_DETECTION_2, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7716, IUER, 'PIMA_MKDB', 'Error in '// &
     &              'an attempt to create the list of the output '// &
     &              'database scans' )
                RETURN
           END IF
         ELSE IF ( PIM%CONF%MKDB_SRT_TYPE == PIMA__MKDB_FILE .OR. &
     &             PIM%CONF%MKDB_SRT_TYPE == PIMA__MKDB_SRT_FRT ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_READ_SRT ( PIM, SNR_DETECTION_2, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7717, IUER, 'PIMA_MKDB', 'Error in '// &
     &              'an attempt to create the list of the output '// &
     &              'database scans' )
                RETURN
           END IF
           CONTINUE 
      END IF
!
      IF ( PIM%CONF%MKDB_OUTPUT_TYPE == PIMA__MKDB_AMPL ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_AMPL_OUT ( PIM, PIM_2ND, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7718, IUER, 'PIMA_MKDB', 'Error in an '// &
     &               'attempt to create the output file with calibrated '// &
     &               'fringe amplitude' )
                RETURN
           END IF
           CALL ERR_LOG ( 7719, IUER )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_WEA_INTRP ( PIM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7720, IUER, 'PIMA_MKDB', 'Error in an attempt to '// &
     &         'create the list of the output database scans' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_INSERT_CAB ( PIM, L_STA_CAB, C_STA_CAB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7721, IUER, 'PIMA_MKDB', 'Error in an '// &
     &         'attempt to insert cable calibration into the PIM%OBS '// &
     &         'data structure' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_THEO ( PIM, VTD, 'OBS_SRT', '1ST_STA', IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7722, IUER, 'PIMA_MKDB', 'Error in an attempt '// &
     &         'to compute theoretical path delays' )
           RETURN
      END IF
!
      CALL NOUT_I4 ( PIM__MSTA,   NOBS_STA )
      CALL NOUT_I4 ( 3*PIM__MOBS, OBS_TAB  )
      NUMB_OBS = 0
      NUMB_SCA = 0
      NUMB_BAS = 0
      NUMB_STA = 0
      NUMB_SOU = 0
!
      IF ( PIM%CONF%MKDB_OUTPUT_TYPE == PIMA__MKDB_GVF ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_GVH_INIT ( PIM, GVH, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7723, IUER, 'PIMA_MKDB', 'Error in '// &
     &              'an attempt to initialize GVH data structure')
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_GVH_HIST ( PIM, GVH, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7724, IUER, 'PIMA_MKDB', 'Error in '// &
     &              'an attempt to write the database history' )
                RETURN
           END IF
         ELSE IF ( PIM%CONF%MKDB_OUTPUT_TYPE == PIMA__MKDB_TEXT ) THEN
           LUN = GET_UNIT()
           OPEN ( UNIT=LUN, FILE=PIM%CONF%MKDB_OUTPUT_NAME, STATUS='UNKNOWN', &
     &            IOSTAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IER, STR )
                CALL ERR_LOG ( 7725, IUER, 'PIMA_MKDB', 'Error '// &
     &               STR(1:I_LEN(STR))//' an attempt to output MKDB file '// &
     &               PIM%CONF%MKDB_OUTPUT_NAME )
                RETURN
           END IF
      END IF
!
      KOBS = 0
      IF ( PIM%CONF%MKDB_OUTPUT_TYPE == PIMA__MKDB_TEXT ) THEN
           WRITE ( LUN, '(A)' ) PIMA__TOTAL_LABEL      
           WRITE ( LUN, '(A)' ) '#'
           WRITE ( LUN, '(A)' ) '#  Created by '//PIMA__LABEL
           WRITE ( LUN, '(A)' ) '#  Created on '//GET_TZ_CDATE()
           WRITE ( LUN, '(A)' ) '#  Fine search algorithm '//PIM%CONF%FRIB_SEARCH_TYPE
           WRITE ( LUN, '(A)' ) '#'
           FILHEA = PIMA__ROOT//'/share/pima/mktxt_header.txt'
           INQUIRE ( FILE=FILHEA, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 7726, IUER, 'PIMA_MKDB', 'Trap of internal '// &
     &              'control: cannot find a file with mktxt header '// &
     &               FILHEA )
                RETURN
           END IF
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT  ( FILHEA, M_HEA, HEA, NH, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7727, IUER, 'PIMA_MKDB', 'Failure in reading '// &
     &               'file with mktxt header '//FILHEA )
                RETURN
           END IF
!
           DO 400 J0=1,NH
              WRITE ( LUN, '(A)' ) TRIM(HEA(J0))
 400       CONTINUE 
      END IF
      TIM_SRT_LAST = -1.D6
      FL_ERR = .FALSE.
      N_MIS = 0
      IND_OBS_LAST = 0
      DO 410 J1=1,PIM%L_MKDB
         CALL NOUT_I4 ( PIM__MSTA, STA_OBS )
         DO 420 J2=1,PIM%SCADB(J1)%NOBS
            IND_OBS = PIM%SCADB(J1)%OBS_IND(J2)
            IF ( IND_OBS == 0 ) THEN
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 8 ) THEN
                      WRITE ( 6, * ) 'PIMA_MKDB J1= ', INT2(J1), &
     &                               ' Bypassing obs ', J2, ' because scadb has obs_ind=0'
                 END IF
                 GOTO 420
            END IF
!
! --------- Bypass an observation that was not read
!
            IF ( ILEN(PIM%CONF%MKDB_2ND_BAND_FILE) == 0 ) THEN
                 IF ( .NOT. BTEST ( PIM%OBS(IND_OBS)%FRI_STS(1), REA__PIM ) ) THEN
                      PIM%USE_OBS(IND_OBS) = .FALSE.
                 END IF
               ELSE 
                 IF ( .NOT. BTEST ( PIM%OBS(IND_OBS)%FRI_STS(1), REA__PIM ) .AND. &
     &                .NOT. BTEST ( PIM%OBS(IND_OBS)%FRI_STS(2), REA__PIM )       ) THEN
!
! ------------------- The observation was not read in both bands
!
                      PIM%USE_OBS(IND_OBS) = .FALSE.
                 END IF
            END IF
            IF ( PIM%CONF%DEBUG_LEVEL .GE. 8 ) THEN
                 WRITE ( 6, 212 ) IND_OBS, PIM%USE_OBS(IND_OBS), &
     &                            PIM%OBS(IND_OBS)%FRI_STS(1), &
     &                            PIM%OBS(IND_OBS)%FRI_STS(2)
 212             FORMAT ( 'PIMA_MKDB Ind_obs: ', I6, ' use: ', L1, ' Fri_sts: ', B16, 2X, B16 )
                 CALL FLUSH ( 6 )
            END IF
!
! --------- Bypass a deselected observation
!
            IF ( .NOT. PIM%USE_OBS(IND_OBS) ) THEN
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 8 ) THEN
                      WRITE ( 6, * ) 'PIMA_MKDB J1= ', INT2(J1), &
     &                               ' Bypassing obs ', J2, ' because use_obs = .False. '// &
     &                               ' rea__pim= ', BTEST ( PIM%OBS(IND_OBS)%FRI_STS(1), REA__PIM ) 
                 END IF
                 GOTO 420
            END IF
            IND_OBS_2ND = PIM%OBS(IND_OBS)%IND_OBS_2ND
            IF ( IND_OBS_2ND == 0 ) IND_OBS_2ND = IND_OBS
            IF ( PIMAVAR_ZERO_FRINGE_RES == 'yes' ) THEN
                 PIM%OBS(IND_OBS)%RES_MB_DEL = 0.0D0
                 PIM%OBS(IND_OBS)%RES_GR_RAT = 0.0D0
            END IF
!
            SNR_1 = PIM%OBS(IND_OBS)%AMPL(PIMA__DRF,1)/PIM%OBS(IND_OBS)%NOISE(1)
            IF ( ILEN(PIM%CONF%MKDB_2ND_BAND_FILE) > 0 ) THEN
                 SNR_2 = PIM%OBS(IND_OBS_2ND)%AMPL(PIMA__DRF,2)/PIM%OBS(IND_OBS_2ND)%NOISE(2)
               ELSE 
                 SNR_2 = 0.0D0
                 PIM%OBS(IND_OBS_2ND)%FRT_OFFSET(2) = 0.0D0
                 PIM%OBS(IND_OBS)%SCAN_DURA(2) = 0.0D0
            END IF
!
            IF ( PIM%NFRG == 1 ) THEN
                 SRT_DATE_STR = MJDSEC_TO_DATE ( PIM%MJD_0, &  
     &                          PIM%TAI_0 + PIM%TIM_R8(PIM%OBS(IND_OBS)%TIM_BEG_IND) + &
     &                          PIM%OBS(IND_OBS)%SRT_OFFSET, -2 )
                 FRG_IND = 1
               ELSE
                 FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)
                 IF ( FRG_IND == 0 ) THEN
!
! ------------------- FRG_IND is a pathological case: there are no data in the 
! ------------------- current frequency group. In order to prevent a crash, let
! ------------------- us set default frequency group (1)
!
                     FRG_IND = 1
                 END IF
!
                 SRT_DATE_STR = MJDSEC_TO_DATE ( PIM%MJD_0, &  
     &                          PIM%TAI_0 + PIM%TIM_R8( PIM%UV_IND(PIM%OBS(IND_OBS)%UV_IND(1,FRG_IND))%TIM_IND ) + &
     &                          PIM%OBS(IND_OBS)%SRT_OFFSET, -2 )
            END IF 
            IF ( PIM%OBS(IND_OBS)%STA_IND(1) < PIM%OBS(IND_OBS)%STA_IND(2) ) THEN
                 UV_STA_ORDER = 1
              ELSE
                 UV_STA_ORDER = -1
            END IF
!
            IF ( PIM%CONF%MKDB_FILTER == PIMA__ONLY_DET ) THEN
!
! -------------- Deselect the observation that has no detection at any band
!
                 IF ( ILEN(PIM%CONF%MKDB_2ND_BAND_FILE) > 0 ) THEN
                      IF ( SNR_1 < PIM%CONF%FRIB_SNR_DETECTION  .AND. &
     &                     SNR_2 < SNR_DETECTION_2                    ) THEN
                           IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                                 WRITE ( 6, '(A,I6,A)' ) 'PIMA_MKDB Obs ', &
     &                                 IND_OBS, &
     &                                 ' was rejected because was '// &
     &                                 'not detected at both bands'
                           END IF
                           PIM%SCADB(J1)%OBS_IND(J2) = 0
                           GOTO 420
                      END IF
                    ELSE
                      IF ( SNR_1 < PIM%CONF%FRIB_SNR_DETECTION ) THEN
                           IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                                 WRITE ( 6, '(A,I6,A)' ) 'PIMA_MKDB Obs ', &
     &                                 IND_OBS, &
     &                                 ' was rejected because was '// &
     &                                 'not detected at the second band '
                           END IF
                           PIM%SCADB(J1)%OBS_IND(J2) = 0
                           GOTO 420
                      END IF
                 END IF
            END IF
!
            IF ( PIM%TAI_0 + PIM%TIM_R8( PIM%UV_IND(PIM%OBS(IND_OBS)%UV_IND(1,FRG_IND))%TIM_IND ) + &
     &                PIM%OBS(IND_OBS)%SRT_OFFSET < TIM_SRT_LAST - 2.D0*TIM_EPS ) THEN
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
                      WRITE ( 6, 213 ) IND_OBS, PIM%TAI_0 + PIM%TIM_R8( PIM%UV_IND(PIM%OBS(IND_OBS)%UV_IND(1,FRG_IND))%TIM_IND ) + &
     &                                 PIM%OBS(IND_OBS)%SRT_OFFSET, TIM_SRT_LAST, &
     &                                 PIM%OBS(IND_OBS)%FRT_OFFSET(1), PIM%OBS(IND_OBS)%SRT_OFFSET, &
     &                                 IND_OBS_LAST
 213                  FORMAT ( 'PIMA_MKDB Obs ', I6, ' was rejected because it ', &
     &                         'was followed in a wrong time order '/ &
     &                         ' Tim_last: ', F10.3, ' tim_srt_last: ', F10.3, &
     &                         ' FRT_offset: ', F7.3, ' SRT_offset: ', F7.3, &
     &                         ' Ind_obs_last= ', I6 )
                 END IF
                 PIM%SCADB(J1)%OBS_IND(J2) = 0
                 N_MIS = N_MIS + 1
                 GOTO 420
            END IF
!
! --------- Compute apriori path delay and the total path delay.
! --------- Convert "geocenter" referenced delays to the 1st station
! --------- referenced delays
!
            KOBS = KOBS + 1
            TIM_SRT_LAST = PIM%TAI_0 + PIM%TIM_R8( PIM%UV_IND(PIM%OBS(IND_OBS)%UV_IND(1,FRG_IND))%TIM_IND ) + &
     &                     PIM%OBS(IND_OBS)%SRT_OFFSET
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!   write ( 6, * ) 'PIMA_MKDB-363  ind_obs= ', ind_obs, ' tim_srt_last= ', sngl(tim_srt_last), ' tii= ', sngl(pim%tai_0 + pim%tim_r8( pim%uv_ind(pim%obs(ind_obs)%uv_ind(1,frg_ind))%tim_ind )), ' tof= ', sngl(pim%obs(ind_obs)%srt_offset), ' ampl= ', PIM%OBS(IND_OBS)%AMPL(PIMA__LSQ,1:2), ' noi= ', PIM%OBS(IND_OBS)%NOISE(1:2) ; call flush ( 6 ) ! %%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            IND_OBS_LAST = IND_OBS
!
            IF ( PIM%NMOD > 0 ) THEN
                 CALL ERR_PASS ( IUER, IER )
                 CALL PIMA_APR_DELAY ( PIM, VTD, IND_OBS, 1, 'OBS_SRT', &
     &                                 '1ST_STA', IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_PASS ( IUER, IER )
                      CALL ERR_LOG ( 7728, IER, 'PIMA_MKDB', 'Error in '// &
     &                    'an attempt to compute the apriori path delay' )
                      FL_ERR = .TRUE.
!!                      GOTO 420
                 END IF
!
                 IF ( PIM%NMOD > 0 .AND. &
     &                ILEN(PIM%CONF%MKDB_2ND_BAND_FILE) > 0 ) THEN
                      IND_OBS_2ND = PIM%OBS(PIM%SCADB(J1)%OBS_IND(J2))%IND_OBS_2ND
                      IF ( IND_OBS_2ND == 0 ) IND_OBS_2ND = PIM%SCADB(J1)%OBS_IND(J2)
!
                      CALL ERR_PASS ( IUER, IER )
                      CALL PIMA_APR_DELAY ( PIM, VTD, IND_OBS_2ND, 2, 'OBS_SRT', &
     &                                     '1ST_STA', IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 7729, IUER, 'PIMA_MKDB', 'Error in '// &
     &                         'an attempt to compute the apriori path delay for '// &
     &                         ' the 2nd band' )
                           FL_ERR = .TRUE.
!!                           GOTO 420
                      END IF
                 END IF
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 11 ) THEN
                      WRITE ( 6, 216 ) IND_OBS, &
     &                                 PIM%OBS(IND_OBS)%APR_GR_DEL(1), &
     &                                 PIM%OBS(IND_OBS)%APR_GR_DEL(2)
 216                  FORMAT ( 'PIMA_MKDB(303) IND_OBS: ', I6, &
     &                         ' APR_GR_DEL: ', 1PD19.12, 1X, 1PD19.12 )
                 END IF
               ELSE 
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
                      WRITE ( 6, * ) 'PIMA_MKDB: No apriori model was found'
                 END IF
            END IF
!
            IF ( PIMAVAR_ZERO_FRINGE_RES == 'yes' ) THEN
                 PIM%OBS(IND_OBS)%TOT_MB_DEL(1,1) = PIM%OBS(IND_OBS)%APR_GR_DEL(1)
                 PIM%OBS(IND_OBS)%TOT_MB_DEL(2,1) = PIM%OBS(IND_OBS)%APR_GR_DEL(1)
                 PIM%OBS(IND_OBS)%TOT_MB_DEL(3,1) = PIM%OBS(IND_OBS)%APR_GR_DEL(1)
                 PIM%OBS(IND_OBS)%TOT_MB_DEL(4,1) = PIM%OBS(IND_OBS)%APR_GR_DEL(1)
                 PIM%OBS(IND_OBS)%TOT_GR_RAT      = PIM%OBS(IND_OBS)%APR_RAT(1)
            END IF
!
            IF ( PIM%CONF%MKDB_OUTPUT_TYPE == PIMA__MKDB_TEXT ) THEN
                 NUMB_OBS = NUMB_OBS + 1
                 IF ( .NOT. FL_MKDB_NOSORT ) THEN
                      IF ( PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)) < &
     &                     PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2))   ) THEN
                           STA_NAM(1) = PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1))
                           STA_NAM(2) = PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2))
                         ELSE
                           STA_NAM(1) = PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2))
                           STA_NAM(2) = PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1))
                      END IF
                    ELSE
                      STA_NAM(1) = PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1))
                      STA_NAM(2) = PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2))
                 END IF
!
                 IF ( (PIM%OBS(IND_OBS)%TIM_END - PIM%OBS(IND_OBS)%TIM_BEG) > &
     &                PIM%OBS(IND_OBS)%AP_LEN ) THEN
                      TIM_SCA_FACT = &
     &                  (   PIM%TIM_R8(PIM%UV_IND(PIM%OBS(IND_OBS)%UV_IND(1,FRG_IND))%TIM_IND) &
     &                    + PIM%OBS(IND_OBS)%SRT_OFFSET &
     &                    - PIM%OBS(IND_OBS)%TIM_BEG &
     &                  )/ &
     &                  (PIM%OBS(IND_OBS)%TIM_END - PIM%OBS(IND_OBS)%TIM_BEG)
                    ELSE
                      TIM_SCA_FACT = 0.0
                 END IF
!
! -------------- Replace huge values set during initialization with zeroes
!
                 DO 430 J3=1,PIM__MFRA
                    IF ( DABS(PIM%OBS(IND_OBS)%TOT_MB_DEL(J3,1))     > 1.0D0  ) PIM%OBS(IND_OBS)%TOT_MB_DEL(J3,1)     = 0.0D0
                    IF ( DABS(PIM%OBS(IND_OBS)%TOT_PH_RAT(J3,1))     > 1.0D0  ) PIM%OBS(IND_OBS)%TOT_PH_RAT(J3,1)     = 0.0D0
                    IF ( DABS(PIM%OBS(IND_OBS)%TOT_PHS(J3,1))        > 1.0D5  ) PIM%OBS(IND_OBS)%TOT_PHS(J3,1)        = 0.0D0
                    IF ( DABS(PIM%OBS(IND_OBS)%TOT_PHS_GC(J3,1))     > 1.0D5  ) PIM%OBS(IND_OBS)%TOT_PHS_GC(J3,1)     = 0.0D0
                    IF ( DABS(PIM%OBS(IND_OBS)%RES_PHS(J3,1))        > 1.0D5  ) PIM%OBS(IND_OBS)%RES_PHS(J3,1)        = 0.0D0
                    IF ( DABS(PIM%OBS(IND_OBS)%RES_PH_RAT(J3,1))     > 1.0    ) PIM%OBS(IND_OBS)%RES_PH_RAT(J3,1)     = 0.0D0
                    IF ( DABS(PIM%OBS(IND_OBS_2ND)%TOT_MB_DEL(J3,2)) > 1.0D0  ) PIM%OBS(IND_OBS_2ND)%TOT_MB_DEL(J3,2) = 0.0D0
                    IF ( DABS(PIM%OBS(IND_OBS_2ND)%TOT_PH_RAT(J3,2)) > 1.0D0  ) PIM%OBS(IND_OBS_2ND)%TOT_PH_RAT(J3,2) = 0.0D0
                    IF ( DABS(PIM%OBS(IND_OBS_2ND)%TOT_PHS(J3,2))    > 1.0D5  ) PIM%OBS(IND_OBS_2ND)%TOT_PHS(J3,2)    = 0.0D0
                    IF ( DABS(PIM%OBS(IND_OBS_2ND)%TOT_PHS_GC(J3,2)) > 1.0D5  ) PIM%OBS(IND_OBS_2ND)%TOT_PHS_GC(J3,2) = 0.0D0
                    IF ( DABS(PIM%OBS(IND_OBS_2ND)%RES_PHS(J3,2))    > 1.0D5  ) PIM%OBS(IND_OBS_2ND)%RES_PHS(J3,2)    = 0.0D0
                    IF ( DABS(PIM%OBS(IND_OBS_2ND)%RES_PH_RAT(J3,2)) > 1.0    ) PIM%OBS(IND_OBS_2ND)%RES_PH_RAT(J3,2) = 0.0D0
 430             CONTINUE 
!
                 IF ( DABS(PIM%OBS(IND_OBS)%TOT_SB_DEL(1))     > 1.0D0  ) PIM%OBS(IND_OBS)%TOT_SB_DEL(1) = 0.0D0
                 IF ( DABS(PIM%OBS(IND_OBS)%REF_FREQ(1))       > 1.0D15 ) PIM%OBS(IND_OBS)%REF_FREQ(1)   = 0.0D0
                 IF ( DABS(PIM%OBS(IND_OBS)%RES_GR_RAT(1))     > 1.0    ) PIM%OBS(IND_OBS)%RES_GR_RAT(1) = 0.0D0
                 IF ( DABS(PIM%OBS(IND_OBS_2ND)%TOT_SB_DEL(2)) > 1.0D0  ) PIM%OBS(IND_OBS_2ND)%TOT_SB_DEL(2) = 0.0D0
                 IF ( DABS(PIM%OBS(IND_OBS_2ND)%REF_FREQ(2))   > 1.0D15 ) PIM%OBS(IND_OBS_2ND)%REF_FREQ(2)   = 0.0D0
                 IF ( DABS(PIM%OBS(IND_OBS_2ND)%RES_GR_RAT(2)) > 1.0    ) PIM%OBS(IND_OBS_2ND)%RES_GR_RAT(2) = 0.0D0
                 IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_DRF ) THEN
                      IND_FRA = PIMA__DRF 
                    ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_LSQ ) THEN
                      IND_FRA = PIMA__LSQ 
                    ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_MUL ) THEN
                      IND_FRA = PIMA__MUL 
                    ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_ADD ) THEN
                      IND_FRA = PIMA__ADD
                 END IF
!
                 IND_SCA = PIM%OBS(IND_OBS)%SCA_IND
!%       write ( 6, * ) 'PIMA_MKDB-456 ind_obs= ', int2(ind_obs), int2(ind_obs_2nd), ' res_del = ', PIM%OBS(IND_OBS)%RES_MB_DEL(IND_FRA,1), PIM%OBS(IND_OBS_2ND)%RES_MB_DEL(IND_FRA,1)
                 WRITE ( LUN, 220 ) KOBS, PIM%SCADB(J1)%OBS_IND(J2), &
     &                              PIM%SCADB(J1)%NAME, &
     &                              PIM%SCA(IND_SCA)%SCAN_NAME, &
     &                              PIM%C_SOU(PIM%SCADB(J1)%SOU_IND), &
     &                              STA_NAM, &
     &                              SNR_1, SNR_2, SRT_DATE_STR(1:24),     &
     &                              PIM%TIM_R8( PIM%UV_IND(PIM%OBS(IND_OBS)%UV_IND(1,FRG_IND))%TIM_IND), &
     &                              PIM%OBS(IND_OBS)%SRT_OFFSET,          &
     &                              PIM%OBS(IND_OBS)%FRT_OFFSET(1),       &
     &                              PIM%OBS(IND_OBS_2ND)%FRT_OFFSET(2),   &
     &                              PIM%OBS(IND_OBS)%THE_GR_DEL,          &
     &                              PIM%OBS(IND_OBS)%THE_RATE,            &
     &                              PIM%OBS(IND_OBS)%APR_GR_DEL(1),       &
     &                              PIM%OBS(IND_OBS_2ND)%APR_GR_DEL(2),          &
     &                              PIM%OBS(IND_OBS)%APR_RAT(1),                 &
     &                              PIM%OBS(IND_OBS_2ND)%APR_RAT(2),             &
     &                              PIM%OBS(IND_OBS)%TOT_MB_DEL(IND_FRA,1),      &
     &                              PIM%OBS(IND_OBS_2ND)%TOT_MB_DEL(IND_FRA,2),  &
     &                              PIM%OBS(IND_OBS)%TOT_SB_DEL(1),              &
     &                              PIM%OBS(IND_OBS_2ND)%TOT_SB_DEL(2),          &
     &                              PIM%OBS(IND_OBS)%TOT_PH_RAT(IND_FRA,1),      &
     &                              PIM%OBS(IND_OBS_2ND)%TOT_PH_RAT(IND_FRA,2),  &
     &                              PIM%OBS(IND_OBS)%REF_FREQ(1),                &
     &                              PIM%OBS(IND_OBS_2ND)%REF_FREQ(2),            &
     &                              PIM%OBS(IND_OBS)%TOT_PHS(IND_FRA,1),         &
     &                              PIM%OBS(IND_OBS_2ND)%TOT_PHS(IND_FRA,2),     &
     &                              PIM%OBS(IND_OBS)%TOT_PHS_GC(IND_FRA,1),      &
     &                              PIM%OBS(IND_OBS_2ND)%TOT_PHS_GC(IND_FRA,2),  &
     &                              PIM%OBS(IND_OBS)%RES_PHS(IND_FRA,1),         &
     &                              PIM%OBS(IND_OBS_2ND)%RES_PHS(IND_FRA,2),     &
     &                              PIM%OBS(IND_OBS)%RES_MB_DEL(IND_FRA,1),      &
     &                              PIM%OBS(IND_OBS_2ND)%RES_MB_DEL(IND_FRA,2),  &
     &                              PIM%OBS(IND_OBS)%RES_PH_RAT(IND_FRA,1),      &
     &                              PIM%OBS(IND_OBS_2ND)%RES_PH_RAT(IND_FRA,2),  &
     &                              PIM%OBS(IND_OBS)%RES_GR_RAT(1),        &
     &                              PIM%OBS(IND_OBS_2ND)%RES_GR_RAT(2),    &
     &                              TIM_SCA_FACT, UV_STA_ORDER,            &
     &                              PIM%OBS(IND_OBS)%SCAN_DURA(1),         & 
     &                              PIM%OBS(IND_OBS_2ND)%SCAN_DURA(2),     &
     &                              PIM%OBS(IND_OBS)%ELEV(1)/DEG__TO__RAD, &
     &                              PIM%OBS(IND_OBS)%ELEV(2)/DEG__TO__RAD, &
     &                              PIM%OBS(IND_OBS)%AZ(1)/DEG__TO__RAD,   &
     &                              PIM%OBS(IND_OBS)%AZ(2)/DEG__TO__RAD, &
     &                              PIM%OBS(IND_OBS)%MB_DEL_ERR(IND_FRA,1), &
     &                              PIM%OBS(IND_OBS_2ND)%MB_DEL_ERR(IND_FRA,2), &
     &                              PIM%OBS(IND_OBS)%PH_RAT_ERR(IND_FRA,1), &
     &                              PIM%OBS(IND_OBS_2ND)%PH_RAT_ERR(IND_FRA,2), &
     &                              PIM%OBS(IND_OBS)%EFF_FRQ(1,1), &
     &                              PIM%OBS(IND_OBS)%EFF_FRQ(2,1), &
     &                              PIM%OBS(IND_OBS_2ND)%EFF_FRQ(1,2), &
     &                              PIM%OBS(IND_OBS_2ND)%EFF_FRQ(2,2), &
     &                              PIM%OBS(IND_OBS)%FEED_ANG(1),  &
     &                              PIM%OBS(IND_OBS)%FEED_ANG(2),  &
     &                              PIM%OBS(IND_OBS)%UVW(1),       &
     &                              PIM%OBS(IND_OBS)%UVW(2),       &
     &                              PIM%OBS(IND_OBS)%UVW(3),       &
     &                              PIM%OBS(IND_OBS)%CABLE,        &
     &                              PIM%OBS(IND_OBS)%FRI_STS(1),   &
     &                              PIM%OBS(IND_OBS_2ND)%FRI_STS(2)
 220             FORMAT ( 1X, I5, ' | ', I5,' )  Sca: ', A16, 2X, A10, &
     &                    ' Sou: ',A, &
     &                    ' Sta: ',A, ' / ', A, ' SNR: ', F7.2, 2X, F7.2, &
     &                    ' SRT: ', A, ' TIM_BEG = ', 0PF10.3, &
     &                    ' SRT_off = ', 0PF16.9, &
     &                    ' FRT_off_1 = ', 0PF16.9, &
     &                    ' FRT_off_2 = ', 0PF16.9, &
     &                    ' The_gr: ',       1PD20.12, ' The_rat: ', 1PD20.12, &
     &                    ' Apr_gr_1: ',     1PD20.12, ' Apr_gr_2: ', 1PD20.12, &
     &                    ' Apr_rat_1: ',    1PD20.12, ' Apr_rat_2: ', 1PD20.12, &
     &                    ' Tot_gr_1: ',     1PD20.12, ' Tot_gr_2: ',     1PD20.12, &
     &                    ' Tot_sb_1: ',     1PD20.12, ' Tot_sb_2: ',     1PD20.12, &
     &                    ' Tot_rat_1: ',    1PD20.12, ' Tot_rat_2: ',    1PD20.12, &
     &                    ' Ref_frq_1: ',    1PD20.12, ' Ref_frq_2: ',    1PD20.12, &
     &                    ' Tot_phs_1: ',    0PF9.6,   ' Tot_phs_2: ',    0PF9.6,   &
     &                    ' Tot_phs_gc_1: ', 0PF9.6,   ' Tot_phs_gc_2: ', 0PF9.6,   &
     &                    ' Res_phs_gc_1: ', 0PF9.6,   ' Res_phs_gc_2: ', 0PF9.6,   &
     &                    ' Res_gr_del_1: ', 1PD20.12, ' Res_gr_del_2: ', 1PD20.12, &
     &                    ' Res_pr_1: ',     1PD20.12, ' Re_pr_2: ',      1PD20.12, &
     &                    ' Res_gr_1:  ',    1PD20.12, ' Res_gr_2: ',     1PD20.12, &
     &                    ' Tim_sca_fact: ', 0PF9.6, 2X, 'UV_STA_ORD: ', I2, &
     &                    ' Ef_dur1: ', F8.4, ' Ef_dur2: ', F8.4, &
     &                    ' Elev_1: ', F5.2, ' Elev_2: ', F5.2,   &
     &                    ' Azim_1: ', F7.2, ' Azim_2: ', F7.2,   &
     &                    ' Err_gr_1: ', 1PD11.4, ' Err_gr_2: ', 1PD11.4, &
     &                    ' Err_pr_1: ', 1PD11.4, ' Err_pr_2: ', 1PD11.4, &
     &                    ' Eff_frq_grp_1: ', 1PD13.6, ' Eff_frq_phs_1: ', 1PD13.6, &
     &                    ' Eff_frq_grp_2: ', 1PD13.6, ' Eff_frq_phs_2: ', 1PD13.6, &
     &                    ' Feed_ang_1: ', 0PF9.6, ' Feed_ang_2: ', 0PF9.6, &
     &                    ' UVW_1: ', 1PD15.8, ' UVW_2: ', 1PD15.8, ' UVW_3: ', 1PD15.8, &
     &                    ' Cable_1: ', 1PD12.5, ' Cable_2: ', 1PD12.5, &
     &                    ' Fri_sts_1: ', B16, ' Fri_sts_2: ', B16  )
              ELSE IF ( PIM%CONF%MKDB_OUTPUT_TYPE == PIMA__MKDB_GVF ) THEN
                 NUMB_OBS = NUMB_OBS + 1
                 OBS_TAB(1,NUMB_OBS) = ADD_CLIST ( PIM__MSCA, NUMB_SCA, C_SCA, &
                                                   PIM%SCADB(J1)%NAME, -2 )
                 OBS_TAB(2,NUMB_OBS) = ADD_CLIST ( PIM__MSTA, NUMB_STA, C_STA, &
     &                                 PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)), &
     &                                 -2 )
                 OBS_TAB(3,NUMB_OBS) = ADD_CLIST ( PIM__MSTA, NUMB_STA, C_STA, &
     &                                 PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)), &
     &                                 -2 )
                 IS = ADD_CLIST ( PIM__MSOU, NUMB_SOU, C_SOU, &
     &                            PIM%C_SOU(PIM%SCADB(J1)%SOU_IND), -2 )
                 IF ( .NOT. FL_MKDB_NOSORT ) THEN
                       IF ( PIM%OBS(IND_OBS)%STA_IND(1) < &
     &                      PIM%OBS(IND_OBS)%STA_IND(2)   ) THEN
                          ELSE
                            STR_BAS = PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2))// &
     &                                PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1))
                            SWAP_STA = OBS_TAB(2,NUMB_OBS)
                            OBS_TAB(2,NUMB_OBS) = OBS_TAB(3,NUMB_OBS)
                            OBS_TAB(3,NUMB_OBS) = SWAP_STA
                       END IF
                     ELSE 
                       STR_BAS = PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1))// &
     &                           PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2))
                 END IF
                 IS = ADD_CLIST ( PIM__MBAS, NUMB_BAS, C_BAS, STR_BAS, -2 )
                 IF ( STA_OBS(OBS_TAB(2,NUMB_OBS)) == 0 ) THEN
                      NOBS_STA(OBS_TAB(2,NUMB_OBS)) = NOBS_STA(OBS_TAB(2,NUMB_OBS)) + 1
                      STA_OBS(OBS_TAB(2,NUMB_OBS)) = 1
                 END IF
                 IF ( STA_OBS(OBS_TAB(3,NUMB_OBS)) == 0 ) THEN
                      NOBS_STA(OBS_TAB(3,NUMB_OBS)) = NOBS_STA(OBS_TAB(3,NUMB_OBS)) + 1
                      STA_OBS(OBS_TAB(3,NUMB_OBS)) = 1
                 END IF
            END IF
 420     CONTINUE
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
              WRITE ( 6, 210 ) J1, PIM%SCADB(J1)%NAME, &
    &                          PIM%C_SOU(PIM%SCADB(J1)%SOU_IND), &
    &                          PIM%SCADB(J1)%TAI_SRT, PIM%SCADB(J1)%NOBS, &
     &                         NUMB_OBS
 210          FORMAT ( I4,') ', A, 2X, A, 2X, F10.3, ' Sca_Nobs: ', I4, &
     &                 ' Numb_obs: ', I6 )
         END IF
 410  CONTINUE
      IF ( FL_ERR ) THEN
           IF ( PIM%CONF%CHECK_SEVERITY > 1 ) THEN
                CALL ERR_LOG ( 7730, IUER, 'PIMA_MKDB', 'Error in '// &
     &               'an attempt to compute total path delays for experiment '// &
     &               PIM%CONF%SESS_CODE )
                RETURN
              ELSE 
                IER = -1
                CALL ERR_LOG ( 7730, IER, 'PIMA_MKDB', 'Error in '// &
     &               'an attempt to compute total path delays for experiment '// &
     &               TRIM(PIM%CONF%SESS_CODE)//' Nevertheless, continue' )
           END IF
      END IF
!
      IF ( NUMB_OBS == 0 ) THEN
           CALL ERR_LOG ( 7731, IUER, 'PIMA_MKDB', 'Trap of internal control: '// &
     &         'no useable observations were found for experiment '// &
     &          PIM%CONF%SESS_CODE )
           RETURN 
      END IF
!
      IF ( PIM%CONF%MKDB_OUTPUT_TYPE == PIMA__MKDB_GVF ) THEN
!
! -------- Sort source, station and baseline lists
!
           
           C_SOU_SRT = C_SOU
           C_STA_SRT = C_STA
           C_BAS_SRT = C_BAS
           NOBS_STA_SRT = NOBS_STA
           IF ( .NOT. FL_MKDB_NOSORT ) THEN
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                      WRITE ( 6, * ) 'PIMA_MKDB: no sorting of stations, sources, baselines' 
                 END IF
                 CALL SORT_FAST_CH ( NUMB_SOU, C_SOU_SRT )
                 CALL SORT_FAST_CH ( NUMB_STA, C_STA_SRT )
                 CALL SORT_FAST_CH ( NUMB_BAS, C_BAS_SRT )
!
! -------------- Update NOBS_STA table, since station indexes have changed after sorting 
!
                 DO 450 J5=1,NUMB_STA
                    NOBS_STA(J5) = NOBS_STA_SRT ( LTM_DIF ( 0, NUMB_STA, C_STA, &
     &                                                      C_STA_SRT(J5) ) )
 450             CONTINUE
!
! -------------- Update OBS_TAB for sorting the station list
!
                 DO 460 J6=1,NUMB_OBS
                    IND_STA(1) = LTM_DIF ( 0, NUMB_STA, C_STA_SRT, C_STA(OBS_TAB(2,J6)) )
                    IND_STA(2) = LTM_DIF ( 0, NUMB_STA, C_STA_SRT, C_STA(OBS_TAB(3,J6)) )
                    OBS_TAB(2,J6) = IND_STA(1)
                    OBS_TAB(3,J6) = IND_STA(2)
 460             CONTINUE
!
                 C_SOU = C_SOU_SRT
                 C_STA = C_STA_SRT
                 C_BAS = C_BAS_SRT
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_GVH_SESS ( PIM, PIM_2ND, GVH, NUMB_OBS, NUMB_STA, &
     &                          NUMB_SOU, NUMB_SCA, NUMB_BAS, NOBS_STA, &
     &                          OBS_TAB, C_STA, C_SOU, C_SCA, C_BAS, &
     &                          L_STA_CAB, C_STA_CAB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7731, IUER, 'PIMA_MKDB', 'Error in '// &
     &              'writing session-wide parameters to the GVH data '// &
     &              'structure' )
                RETURN
           END IF
!
           OBS_DB_IND = 0
           DO 470 J7=1,PIM%L_MKDB
              DO 480 J8=1,PIM%SCADB(J7)%NOBS
                 IND_OBS = PIM%SCADB(J7)%OBS_IND(J8)
                 IF ( IND_OBS == 0 ) GOTO 480
                 IF ( .NOT. PIM%USE_OBS(IND_OBS) ) GOTO 480
                 OBS_DB_IND = OBS_DB_IND + 1
                 CALL ERR_PASS ( IUER, IER )
                 CALL PIMA_GVH_OBS ( PIM, PIM_2ND, GVH, NUMB_STA, &
     &                               NUMB_SOU, NUMB_SCA, NUMB_BAS, &
     &                               C_STA, C_SOU, C_SCA, C_BAS, &
     &                               OBS_TAB(1,OBS_DB_IND), IND_OBS, &
     &                               OBS_DB_IND, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( IND_OBS, STR )
                      CALL ERR_LOG ( 7732, IUER, 'PIMA_MKDB', 'Error in '// &
     &                    'writing observation-wide parameters to the '// &
     &                    'GVH data structure for observation '//STR )
                      RETURN
                 END IF
 480          CONTINUE
 470       CONTINUE
!
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_GVH_WRI ( PIM, GVH, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7733, IUER, 'PIMA_MKDB', 'Error in '// &
     &              'an attempt to write the output database files in '// &
     &              'GVF format' )
                RETURN
           END IF
        ELSE IF ( PIM%CONF%MKDB_OUTPUT_TYPE == PIMA__MKDB_TEXT ) THEN
           CLOSE ( UNIT=LUN )
           IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                WRITE ( 6, '(A)' ) 'Output file: '// &
     &                PIM%CONF%MKDB_OUTPUT_NAME(1:I_LEN(PIM%CONF%MKDB_OUTPUT_NAME))
           END IF
      END IF
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
!
! -------- Last messages
!
           IF ( N_MIS > 0 ) THEN
                WRITE ( 6, 230 ) N_MIS
 230            FORMAT ( '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!' / &
     &                   '!!! ', I6, ' Observations were not put in the database!!!'/ &
     &                   '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!' )
           END IF
!
           WRITE ( 6, 240 ) PIM%NOBS, NUMB_OBS
 240       FORMAT ( 'Number of observations in the experiment: ', I6/ &
     &              'Number of observations in the database:   ', I6  )
      END IF
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
           WRITE ( 6, * ) ' END OF PIMA_MKDB'
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_MKDB  !#!#
