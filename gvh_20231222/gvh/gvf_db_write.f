      SUBROUTINE GVF_DB_WRITE ( MODE, GVF_DB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GVF_DB_WRITE
! *                                                                      *
! *  ### 14-OCT-2007  GVF_DB_WRITE  v2.5 (c) L. Petrov  31-OCT-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      INCLUDE   'gvf_db.i'
      INCLUDE   'gvh_solve.i'
      TYPE     ( GVF_DB__TYPE ) :: GVF_DB
      INTEGER*4  MODE, IUER
      CHARACTER  STR*128
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_TZ_CDATE*26
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      IF ( MODE == 1 ) THEN
           WRITE ( 6, '(A)' ) 'DB_NAME:  '//GVF_DB%DB_NAME
           WRITE ( 6, '(A)' ) 'EXP_NAME: '//GVF_DB%SES%EXP_NAME
           WRITE ( 6, '(A)' ) 'EXP_NAME: '//GVF_DB%SES%EXP_NAME(1:I_LEN(GVF_DB%SES%EXP_NAME))
           WRITE ( 6, '(A)' ) 'MK3_DBNM: '//GVF_DB%SES%MK3_DBNM
           WRITE ( 6, '(A,I2)' ) 'NUMB_STA: ', GVF_DB%SES%NUMB_STA
           DO 410 J1=1,GVF_DB%SES%NUMB_STA
              WRITE ( 6, 110 ) J1, GVF_DB%STA(J1)%SITNAMES, &
     &                             GVF_DB%STA(J1)%SIT_COOR
 110          FORMAT ( 'Sta: ', I2,') ',A, 2X, 3(F13.4,1X) )
 410       CONTINUE
           WRITE ( 6, '(A,I3)' ) 'NUMB_SOU: ', GVF_DB%SES%NUMB_SOU
!
           DO 420 J2=1,GVF_DB%SES%NUMB_SOU
              CALL RH_TAT ( GVF_DB%SOU(J2)%SOU_COOR(1), 6, STR(1:15),    -2 )
              CALL RG_TAT ( GVF_DB%SOU(J2)%SOU_COOR(2), 5, STR(101:115), -2 )
              IF ( STR(101:101) == ' ' ) STR(101:101) = '+'
              WRITE ( 6, 120 ) J2, GVF_DB%SOU(J2)%SRCNAMES, &
     &                         STR(1:15), STR(101:115)
 120          FORMAT ( 'Sou: ', I3,') ',A, 2X, A15, 1X, A15 )
 420       CONTINUE
         ELSE IF ( MODE == 21 ) THEN
           WRITE ( 6, '(A,I2)' ) '# gvf_db Output. Mode: ', MODE
           WRITE ( 6, '(A)' ) 'DB_NAME:  '//GVF_DB%DB_NAME
           WRITE ( 6, '(A)' ) 'MK3_DBNM: '//GVF_DB%SES%MK3_DBNM
           WRITE ( 6, '(A,I6)' ) 'NUMB_OBS: ', GVF_DB%SES%NUMB_OBS
           DO 4210 J2=1,GVF_DB%SES%NUMB_OBS
              STR = MJDSEC_TO_DATE ( GVF_DB%OBS(J2)%MJD_OBS,  &
     &                               GVF_DB%OBS(J2)%UTC_OBS-GVF_DB%SES%UTC_MTAI, &
     &                               -2 )
              WRITE ( 6, 1210 ) J2, &
     &                         GVF_DB%OBS(J2)%SCANNAME, &
     &                         GVF_DB%SOU(GVF_DB%OBS(J2)%SOU_IND)%SRCNAMES, &
     &                         GVF_DB%STA(GVF_DB%OBS(J2)%STA_IND(1))%SITNAMES, &
     &                         GVF_DB%STA(GVF_DB%OBS(J2)%STA_IND(2))%SITNAMES, &
     &                         STR(1:21), &
     &                         GVF_DB%OBS(J2)%AUTO_SUP, &
     &                         GVF_DB%OBS(J2)%USER_SUP, &
     &                         GVF_DB%OBS(J2)%USER_REC,  &
     &                         GVF_DB%OBS(J2)%QUALCODE(1), &
     &                         GVF_DB%OBS(J2)%QUALCODE(2)
 1210         FORMAT ( 'Obs: ', I6, 1X, A, 1X, A, 1X, A, '/', A, 1X, A, &
     &                 1X, I11, 1X, I11, 1X, I11, 2X, A, '/',A )
 4210       CONTINUE
         ELSE IF ( MODE == 2 ) THEN
           WRITE ( 6, '(A,I2)' ) '# gvf_db Output. Mode: ', MODE
           WRITE ( 6, '(A)' ) 'DB_NAME:  '//GVF_DB%DB_NAME
           WRITE ( 6, '(A)' ) 'MK3_DBNM: '//GVF_DB%SES%MK3_DBNM
           WRITE ( 6, '(A,I6)' ) 'NUMB_OBS: ', GVF_DB%SES%NUMB_OBS
           DO 430 J3=1,GVF_DB%SES%NUMB_OBS
              WRITE ( 6, 130 ) J3, &
     &                         GVF_DB%SOU(GVF_DB%OBS(J3)%SOU_IND)%SRCNAMES, &
     &                         GVF_DB%STA(GVF_DB%OBS(J3)%STA_IND(1))%SITNAMES, &
     &                         GVF_DB%STA(GVF_DB%OBS(J3)%STA_IND(2))%SITNAMES, &
     &                         GVF_DB%OBS(J3)%MJD_OBS,  &
     &                         GVF_DB%OBS(J3)%UTC_OBS,  &
     &                         GVF_DB%OBS(J3)%AUTO_SUP, &
     &                         GVF_DB%OBS(J3)%USER_SUP, &
     &                         GVF_DB%OBS(J3)%USER_REC, &
     &                         GVF_DB%OBS(J3)%N_GRAMB,  &
     &                         GVF_DB%OBS(J3)%GDAMBSP
 130          FORMAT ( 'Obs: ', I6, 1X, A, 1X, A, '/', A, 1X, I5, 1X, F8.2, 1X, &
     &                 I11, 1X, I11, 1X, I11, 2X, I6, 1X, I6, 1X, 1PD15.8, &
     &                 1X, 1PD15.8 )
 430       CONTINUE
         ELSE IF ( MODE == 3 ) THEN
           WRITE ( 6, '(A,I2)' ) '# gvf_db Output. Mode: ', MODE
           WRITE ( 6, '(A)' ) '#'
           WRITE ( 6, '(A)' ) '#   4-11   I8    Observation ID'
           WRITE ( 6, '(A)' ) '#  13-20   A8    Source name'
           WRITE ( 6, '(A)' ) '#  13-20   A8    Source name'
           WRITE ( 6, '(A)' ) '#  22-38   A17   Baseline name'
           WRITE ( 6, '(A)' ) '#  40-44   I4    MJD date'
           WRITE ( 6, '(A)' ) '#  46-53   F8.2  Frinte reference time in TAI'
           WRITE ( 6, '(A)' ) '#  55-65   I12   Autosuppression flag'
           WRITE ( 6, '(A)' ) '#  67-77   I12   Supression code  (even stands for "used" observation)'
           WRITE ( 6, '(A)' ) '#  79-89   I12   User recovery code.'
           WRITE ( 6, '(A)' ) '#  92-97   I6    Group delay ambiguity counter'
           WRITE ( 6, '(A)' ) '# 100-113  D14   Group delay ambiguity spacing in sec'
           WRITE ( 6, '(A)' ) '# 115-130  D16   Group delay'
           WRITE ( 6, '(A)' ) '# 132-146  D15   Group delay formal error'
           WRITE ( 6, '(A)' ) '# 148-162  D15   Single band delay'
           WRITE ( 6, '(A)' ) '# 165-178  D14   Single band delay error'
           WRITE ( 6, '(A)' ) '# 180-187  F8.5  Fringe phase at the reference frequency'
           WRITE ( 6, '(A)' ) '# 189-196  F8.5  SNR'
           WRITE ( 6, '(A)' ) '#'
!
           WRITE ( 6, '(A)' ) 'DB_NAME:  '//GVF_DB%DB_NAME
           WRITE ( 6, '(A)' ) 'MK3_DBNM: '//GVF_DB%SES%MK3_DBNM
           WRITE ( 6, '(A,I6)' ) 'NUMB_OBS: ', GVF_DB%SES%NUMB_OBS
           DO 440 J4=1,GVF_DB%SES%NUMB_OBS
              WRITE ( 6, 140 ) J4, &
     &                         GVF_DB%SOU(GVF_DB%OBS(J4)%SOU_IND)%SRCNAMES, &
     &                         GVF_DB%STA(GVF_DB%OBS(J4)%STA_IND(1))%SITNAMES, &
     &                         GVF_DB%STA(GVF_DB%OBS(J4)%STA_IND(2))%SITNAMES, &
     &                         GVF_DB%OBS(J4)%MJD_OBS,  &
     &                         GVF_DB%OBS(J4)%UTC_OBS,  &
     &                         GVF_DB%OBS(J4)%AUTO_SUP, &
     &                         GVF_DB%OBS(J4)%USER_SUP, &
     &                         GVF_DB%OBS(J4)%USER_REC, &
     &                         GVF_DB%OBS(J4)%N_GRAMB(1),  &
     &                         GVF_DB%OBS(J4)%GDAMBSP(1),  &
     &                         GVF_DB%OBS(J4)%GR_DELAY(1), &
     &                         GVF_DB%OBS(J4)%GRDELERR(1), &
     &                         GVF_DB%OBS(J4)%SB_DELAY(1), &
     &                         GVF_DB%OBS(J4)%SBDELERR(1), &
     &                         GVF_DB%OBS(J4)%TOTPHASE(1), &
     &                         GVF_DB%OBS(J4)%SNRATIO(1)
 140          FORMAT ( 'Obs: ', I6, 1X, A, 1X, A, '/', A, 1X, I5, 1X, F8.2, 1X, &
     &                 I11, 1X, I11, 1X, I11, 2X, I6, 1X, 1PD15.8, &
     &                 1X, 1PD15.8, 2X, 1PD15.8, 1X, 1PD15.8, 1X, 1PD15.8, &
     &                 1X, 0PF8.5, 1X, 0PF8.3 )
 440       CONTINUE
         ELSE IF ( MODE == 4 ) THEN
           DO 450 J5=1,GVF_DB%SES%NUMB_OBS
              WRITE ( 6, 150 ) J5, &
     &                         GVF_DB%SOU(GVF_DB%OBS(J5)%SOU_IND)%SRCNAMES, &
     &                         GVF_DB%STA(GVF_DB%OBS(J5)%STA_IND(1))%SITNAMES, &
     &                         GVF_DB%STA(GVF_DB%OBS(J5)%STA_IND(2))%SITNAMES, &
     &                         GVF_DB%OBS(J5)%SCANNAME, &
     &                         GVF_DB%OBS(J5)%MJD_OBS,  &
     &                         GVF_DB%OBS(J5)%UTC_OBS 
 150          FORMAT ( 'Obs: ', I6, 1X, A, 1X, A, '/', A, 1X, A, 1X, I5, &
     &                  1X, F8.2 )
 450       CONTINUE 
         ELSE IF ( MODE == 5 ) THEN
           WRITE ( 6, '(A,I2)' ) '# gvf_db Output. Mode: ', MODE
           WRITE ( 6, '(A)' ) '#'
           WRITE ( 6, '(A)' ) 'DB_NAME:  '//GVF_DB%DB_NAME
           WRITE ( 6, '(A)' ) 'MK3_DBNM: '//GVF_DB%SES%MK3_DBNM
           WRITE ( 6, '(A,I6)' ) 'NUMB_OBS: ', GVF_DB%SES%NUMB_OBS
           WRITE ( 6, '(A)' ) '#'
           WRITE ( 6, '(A)' ) '#   Obs_ind Source   Baseline          Scan name        MJD   UTC          '// &
     &                        'USER_SUP  Elev_1   Elev_2    Azim_1   Azim_2    Sc_dur  Sc_dur     '// &
     &                        'SNR_1    SNR_2  Fr_Ampl  Fr_Ampl      U-coord      V-coord    '// &
     &                        'Tsys_1X Tsys_2X  Tsys_1S Tsys_2S  Gain_1X Gain_2X  Gain_1S Gain_2S Pind_obs'
           WRITE ( 6, '(A)' ) '#'
!
           DO 460 J6=1,GVF_DB%SES%NUMB_OBS
              IF ( GVF_DB%OBS(J6)%FRN_AMPL(1) < 0.0 ) GVF_DB%OBS(J6)%FRN_AMPL(1) = 0.0
              IF ( GVF_DB%OBS(J6)%FRN_AMPL(2) < 0.0 ) GVF_DB%OBS(J6)%FRN_AMPL(2) = 0.0
              IF ( GVF_DB%OBS(J6)%TSYS(1,1) > 99999.9 ) GVF_DB%OBS(J6)%TSYS(1,1) = 0.0
              IF ( GVF_DB%OBS(J6)%TSYS(2,1) > 99999.9 ) GVF_DB%OBS(J6)%TSYS(2,1) = 0.0
              IF ( GVF_DB%OBS(J6)%TSYS(1,2) > 99999.9 ) GVF_DB%OBS(J6)%TSYS(1,2) = 0.0
              IF ( GVF_DB%OBS(J6)%TSYS(2,2) > 99999.9 ) GVF_DB%OBS(J6)%TSYS(2,2) = 0.0
              WRITE ( 6, 160 ) J6, &
     &                         GVF_DB%SOU(GVF_DB%OBS(J6)%SOU_IND)%SRCNAMES, &
     &                         GVF_DB%STA(GVF_DB%OBS(J6)%STA_IND(1))%SITNAMES, &
     &                         GVF_DB%STA(GVF_DB%OBS(J6)%STA_IND(2))%SITNAMES, &
     &                         GVF_DB%OBS(J6)%SCANNAME,  &
     &                         GVF_DB%OBS(J6)%MJD_OBS,   &
     &                         GVF_DB%OBS(J6)%UTC_OBS,   &
     &                         GVF_DB%OBS(J6)%USER_SUP,  &
     &                         GVF_DB%OBS(J6)%ELEV,      &
     &                         GVF_DB%OBS(J6)%AZIMUTH,   &
     &                         GVF_DB%OBS(J6)%SCAN_DUR,  &
     &                         GVF_DB%OBS(J6)%SNRATIO,   &
     &                         GVF_DB%OBS(J6)%FRN_AMPL,  &
     &                         GVF_DB%OBS(J6)%UV_COOR,   &
     &                         GVF_DB%OBS(J6)%TSYS(1,1), &
     &                         GVF_DB%OBS(J6)%TSYS(2,1), &
     &                         GVF_DB%OBS(J6)%TSYS(1,2), &
     &                         GVF_DB%OBS(J6)%TSYS(2,2), &
     &                         GVF_DB%OBS(J6)%GAIN(1,1), &
     &                         GVF_DB%OBS(J6)%GAIN(1,2), &
     &                         GVF_DB%OBS(J6)%GAIN(2,1), &
     &                         GVF_DB%OBS(J6)%GAIN(2,2), &
     &                         GVF_DB%OBS(J6)%PIND_OBS
 160          FORMAT ( 'Obs: ', I6, 1X, A, 1X, A, '/', A, 1X, A, 1X, I5, &
     &                  1X, F8.2, 2X, 1X, I11, &
     &                  2(F8.5, 1X), 1X, 2(F8.5, 1X), &
     &                  1X, 2(F7.2, 1X), 1X, &
     &                  2(F8.2, 1X), 1X, 2(F8.6, 1X), 1X, &
     &                  2(F12.2, 1X), 1X, 2(F7.1, 1X),  1X, 2(F7.1, 1X), &
     &                  1X, 2(F7.5, 1X),  1X, 2(F7.5, 1X), 2X, I6 )
 460       CONTINUE 
         ELSE IF ( MODE == 10 ) THEN
!
! -------- List observations that are excluded from the single-band X solution
!
           WRITE ( 6, '(A,I2)' ) '# gvf_db Output. Mode: ', MODE
           WRITE ( 6, '(A)' ) '# '
           WRITE ( 6, '(A)' ) '# Exclude observations for band  '//GVF_DB%SES%BAND_NAM(1)
           WRITE ( 6, '(A)' ) '# '
           WRITE ( 6, '(A)' ) '# Experiment   '//GVF_DB%SES%EXP_NAME
           WRITE ( 6, '(A)' ) '# Generated on '//GET_TZ_CDATE()
           WRITE ( 6, '(A)' ) '#'
           DO 470 J7=1,GVF_DB%SES%NUMB_OBS
              IF ( BTEST ( GVF_DB%OBS(J7)%AUTO_SUP, NOFX__SPS ) .OR. &
     &             BTEST ( GVF_DB%OBS(J7)%AUTO_SUP, BQCX__SPS ) .OR. &
     &             BTEST ( GVF_DB%OBS(J7)%AUTO_SUP, DECM__SPS ) .OR. &
     &             BTEST ( GVF_DB%OBS(J7)%AUTO_SUP, DSBS__SPS ) .OR. &
     &             BTEST ( GVF_DB%OBS(J7)%AUTO_SUP, DSSO__SPS ) .OR. &
     &             BTEST ( GVF_DB%OBS(J7)%AUTO_SUP, CUEL__SPS ) .OR. &
     &             BTEST ( GVF_DB%OBS(J7)%USER_SUP, GX__DTP_PAR )    ) THEN
!
                   WRITE ( 6, '(I6)' ) GVF_DB%OBS(J7)%PIND_OBS
              END IF
 470       CONTINUE 
         ELSE IF ( MODE == 20 ) THEN
!
! -------- List observations that are excluded from the S-band solution
!
           IF ( GVF_DB%SES%NUM_BAND < 2 ) THEN
                CALL ERR_LOG ( 7701, IUER, 'GVF_FB_WRITE', 'Mode 20 '// &
     &              'is not applicable for experiment '// &
     &               GVF_DB%SES%EXP_NAME(1:I_LEN(GVF_DB%SES%EXP_NAME))// &
     &              ' since it has only one band' )
                RETURN 
           END IF
           WRITE ( 6, '(A,I2)' ) '# gvf_db Output. Mode: ', MODE
           WRITE ( 6, '(A)' ) '# '
           WRITE ( 6, '(A)' ) '# Exclude observations for band '//GVF_DB%SES%BAND_NAM(2)
           WRITE ( 6, '(A)' ) '# '
           WRITE ( 6, '(A)' ) '# Experiment   '//GVF_DB%SES%EXP_NAME
           WRITE ( 6, '(A)' ) '# Generated on '//GET_TZ_CDATE()
           WRITE ( 6, '(A)' ) '#'
           DO 480 J8=1,GVF_DB%SES%NUMB_OBS
!%%
!%              write ( 6, 210 ) j8, btest ( gvf_db%obs(j8)%auto_sup, nofs__sps ), &
!%     &                             btest ( gvf_db%obs(j8)%auto_sup, bqcs__sps ), &
!%     &                             btest ( gvf_db%obs(j8)%auto_sup, decm__sps ), &
!%     &                             btest ( gvf_db%obs(j8)%auto_sup, dsbs__sps ), &
!%     &                             btest ( gvf_db%obs(j8)%auto_sup, dsso__sps ), &
!%     &                             btest ( gvf_db%obs(j8)%auto_sup, cuel__sps ), &
!%     &                             btest ( gvf_db%obs(j8)%user_sup, gs__dtp_par )
!% 210          format ( 'GVF_DB_WRITE-238 j8= ', i5, 2x, 7(l1,1x) )
! %%
              IF ( BTEST ( GVF_DB%OBS(J8)%AUTO_SUP, NOFS__SPS ) .OR. &
     &             BTEST ( GVF_DB%OBS(J8)%AUTO_SUP, BQCS__SPS ) .OR. &
     &             BTEST ( GVF_DB%OBS(J8)%AUTO_SUP, DECM__SPS ) .OR. &
     &             BTEST ( GVF_DB%OBS(J8)%AUTO_SUP, DSBS__SPS ) .OR. &
     &             BTEST ( GVF_DB%OBS(J8)%AUTO_SUP, DSSO__SPS ) .OR. &
     &             BTEST ( GVF_DB%OBS(J8)%AUTO_SUP, CUEL__SPS ) .OR. &
     &             BTEST ( GVF_DB%OBS(J8)%USER_SUP, GS__DTP_PAR )    ) THEN
!
                   WRITE ( 6, '(I6)' ) GVF_DB%OBS(J8)%PIND_OBS
              END IF
 480       CONTINUE 
         ELSE IF ( MODE == 30 ) THEN
!
! -------- List observations that are excluded from the dual-band XS solution
!
           IF ( GVF_DB%SES%NUM_BAND < 2 ) THEN
                CALL ERR_LOG ( 7702, IUER, 'GVF_FB_WRITE', 'Mode 30 '// &
     &              'is not applicable for experiment '// &
     &               GVF_DB%SES%EXP_NAME(1:I_LEN(GVF_DB%SES%EXP_NAME))// &
     &              ' since it has only one band' )
                RETURN 
           END IF
           WRITE ( 6, '(A,I2)' ) '# gvf_db Output. Mode: ', MODE
           WRITE ( 6, '(A)' ) '# '
           WRITE ( 6, '(A)' ) '# Exclude observations for dual band observables '//GVF_DB%SES%BAND_NAM(1)//'/'//GVF_DB%SES%BAND_NAM(2)
           WRITE ( 6, '(A)' ) '# '
           WRITE ( 6, '(A)' ) '# Experiment   '//GVF_DB%SES%EXP_NAME
           WRITE ( 6, '(A)' ) '# Generated on '//GET_TZ_CDATE()
           WRITE ( 6, '(A)' ) '#'
           DO 490 J9=1,GVF_DB%SES%NUMB_OBS
              IF ( BTEST ( GVF_DB%OBS(J9)%AUTO_SUP, NOFS__SPS ) .OR. &
     &             BTEST ( GVF_DB%OBS(J9)%AUTO_SUP, BQCS__SPS ) .OR. &
     &             BTEST ( GVF_DB%OBS(J9)%AUTO_SUP, DECM__SPS ) .OR. &
     &             BTEST ( GVF_DB%OBS(J9)%AUTO_SUP, DSBS__SPS ) .OR. &
     &             BTEST ( GVF_DB%OBS(J9)%AUTO_SUP, DSSO__SPS ) .OR. &
     &             BTEST ( GVF_DB%OBS(J9)%AUTO_SUP, CUEL__SPS ) .OR. &
     &             BTEST ( GVF_DB%OBS(J9)%USER_SUP, GXS__DTP_PAR )    ) THEN
!
                   WRITE ( 6, '(I6)' ) GVF_DB%OBS(J9)%PIND_OBS
              END IF
 490       CONTINUE 
         ELSE
           CALL CLRCH ( STR )
           CALL INCH  ( MODE, STR )
           CALL ERR_LOG ( 7703, IUER, 'GVF_DB_WRITE', 'Unknown mode: '//STR )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GVF_DB_WRITE  !#!
