      SUBROUTINE ANC_COMPUTE_TATM_OPA ( FILIN, FILOUT, SPD_URL, TMP_DIR, &
     &                                  IVRB, ANC, NERS, SPD_DEL, L_PRV, &
     &                                  C_PRV, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine ANC_COMPUTE_TATM_OPA
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * # 11-AUG-2025 ANC_COMPUTE_TATM_OPA v1.0 (d) L. Petrov 15-AUG-2025 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'atp.i'
      INCLUDE   'spd.i'
      INCLUDE   'ners.i'
      INCLUDE   'ners_local.i'
      TYPE     ( ANC__TYP      ) :: ANC      
      TYPE     ( NERS__TYPE    ) :: NERS
      TYPE     ( SPD_DEL__TYPE ) :: SPD_DEL
      INTEGER*4  IVRB, M_PRV, L_PRV, IUER
      CHARACTER  FILIN*(*), FILOUT*(*)
      CHARACTER  SPD_URL*(*), TMP_DIR*(*), C_PRV(ANC__MPRV)*(*)
      INTEGER*4  MIND, MSPD
      PARAMETER  ( MIND = 32 )
      PARAMETER  ( MSPD = 128*1024 )
      CHARACTER  SPD_DIR_FILE*128, PID_STR*8, DOWNLOAD_COMMAND*256, &
     &           DECOMPRESS_COMMAND*256, C_SPD(MSPD)*48, CU_SPD(MSPD)*128, &
     &           ANC_BEG_DATE*16, ANC_END_DATE*16, SPD_INP_URL*128, &
     &           SPD_OUT_FILE*128, STR*256, STR1*128, STR2*128, STR3*128
      CHARACTER, ALLOCATABLE :: BUF(:)*256
      REAL*8     TIM, TAI_TSYS_BEG, TAI_TSYS_END, EL_USED, MAP
      REAL*4     ARGS(4)
      REAL*8     EPS
      REAL*8     TSS1, TSS2, TSS3, TSF1, TSF2, TSF3
      PARAMETER  ( EPS = 1.D-5 )
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, &
     &           MJD_TSYS_BEG, MJD_TSYS_END, PID, IDAY, IB, IL, IP, IS, &
     &           SPD_BEG_IND, SPD_END_IND, NBUF, LIND, IND(2,MIND), &
     &           L_SPD, LU_SPD, DIMS(4), INDS(4), N_FIL, IER, IV_PRV
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*31, GET_CDATE*19
      REAL*4,    EXTERNAL :: VAL_3D_BSPL4, VAL_4D_BSPL4
      REAL*8,    EXTERNAL :: DEL_ISA
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IXMN4, LTM_DIF, GETPID, SYSTEM
!
      PID = GETPID()
      CALL INCH   ( PID, PID_STR )
      CALL CHASHR ( PID_STR ) 
      CALL BLANK_TO_ZERO ( PID_STR )
!
! --- Parse file
!
      IF ( IVRB > 2 ) THEN
         CALL CPU_TIME ( TSS1 )
      END IF
! ---      
      CALL ERR_PASS  ( IUER, IER )
      CALL ANC_PARSE ( FILIN, ANC, NERS, IER )
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 4111, IUER, 'ANC_COMPUTE_TATM_OPA',             &
     &           'Error in an attempt to parse the file with '//        &
     &           'antenna calibration format '//TRIM(FILIN) )
           RETURN 
      END IF
! ---
      IF ( IVRB > 2 ) THEN
         CALL CPU_TIME ( TSF1 )
         WRITE( 6, 1001 ) TSF1-TSS1, TRIM(FILIN)
      END IF
! ---
      MJD_TSYS_BEG = ANC%MJD_TTO
      TAI_TSYS_BEG = ANC%TAI_TTO + ANC%TTO(1)%TIM
      IDAY = TAI_TSYS_BEG/86400.0D0
      TAI_TSYS_BEG = TAI_TSYS_BEG - IDAY*86400.0D0
      MJD_TSYS_BEG = MJD_TSYS_BEG + IDAY
      ANC_BEG_DATE = MJDSEC_TO_DATE ( MJD_TSYS_BEG, TAI_TSYS_BEG, IER )
! ---
      MJD_TSYS_END = ANC%MJD_TTO
      TAI_TSYS_END = ANC%TAI_TTO + ANC%TTO(ANC%NUM_EPO_TTO)%TIM
      IDAY = TAI_TSYS_END/86400.0D0
      TAI_TSYS_END = TAI_TSYS_END - IDAY*86400.0D0
      MJD_TSYS_END = MJD_TSYS_END + IDAY
      ANC_END_DATE = MJDSEC_TO_DATE ( MJD_TSYS_END, TAI_TSYS_END, IER )
! ---
      ANC_BEG_DATE = ANC_BEG_DATE(1:4)//ANC_BEG_DATE(6:7)//             &
     &               ANC_BEG_DATE(9:10)//'_'//ANC_BEG_DATE(12:13)//     &
     &               ANC_BEG_DATE(15:16)
      ANC_END_DATE = ANC_END_DATE(1:4)//ANC_END_DATE(6:7)//             &
     &               ANC_END_DATE(9:10)//'_'//ANC_END_DATE(12:13)//     &
     &               ANC_END_DATE(15:16)
!
! --- 
!
      IF ( IVRB .GE. 3 ) THEN
         write ( 6, * ) 'mjd_tsys_beg = ', mjd_tsys_beg,                &
     &                  ' tai_tsys_beg= ', tai_tsys_beg,                &
     &                  ' beg_date= ', ANC_BEG_DATE
         write ( 6, * ) 'mjd_tsys_end = ', mjd_tsys_end,                &
     &                  ' tai_tsys_beg= ', tai_tsys_end,                &
     &                  ' end_date= ', ANC_END_DATE
      END IF
!
      SPD_DIR_FILE = TRIM(TMP_DIR)//'/spd__'//PID_STR//'.txt'
!
      DOWNLOAD_COMMAND = 'wget --retry-connrefused --tries=8 '//        &
     &                   '--timeout=10 --waitretry=2 -c -q -O '//       &
     &                   TRIM(SPD_DIR_FILE)//' '//SPD_URL
      IF ( IVRB .GE. 2 ) THEN
         WRITE ( 6, * ) 'ANC_COMPUTE_TATM_OPA: about to execute '//     &
     &                   TRIM(DOWNLOAD_COMMAND)
      END IF
!
      ALLOCATE ( BUF(ANC__MBUF) )
      IS = SYSTEM ( TRIM(DOWNLOAD_COMMAND)//CHAR(0) ) 
      IF ( IS .NE. 0 ) THEN
         DOWNLOAD_COMMAND = 'wget --retry-connrefused --tries=1 '//     &
     &                    '--timeout=2 --waitretry=1 -O /dev/null -o '  &
     &                    //TRIM(SPD_DIR_FILE)//' '//SPD_URL
         IS = SYSTEM   ( TRIM(DOWNLOAD_COMMAND)//CHAR(0) ) 
         CALL ERR_PASS ( IUER, IER )
         CALL RD_TEXT  ( SPD_DIR_FILE, ANC__MBUF, BUF, NBUF, IER )
         DO 410 J1=1,NBUF
            WRITE ( 6, '(A)' ) TRIM(BUF(J1))
 410     CONTINUE
         CALL ERR_LOG ( 4112, IUER, 'ANC_COMPUTE_TATM_OPA',             &
     &           'Error in getting contnents of directory '//SPD_URL )
         RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( SPD_DIR_FILE, ANC__MBUF, BUF, NBUF, IER )
      L_SPD = 0 
      DO 420 J2=1,NBUF
         CALL EXWORD ( BUF(J2), MIND, LIND, IND, ' "'//CHAR(9), IER )
         IF ( INDEX ( BUF(J2), '</td><td><a href="' ) > 0 .AND.         &
     &        INDEX ( BUF(J2), '?C' )                 < 1 .AND.         &
     &        LIND .GE. 12                                      ) THEN
            L_SPD = L_SPD + 1
            IF ( L_SPD > MSPD ) THEN
               CALL CLRCH ( STR ) 
               CALL INCH  ( MSPD, STR )
               CALL ERR_LOG ( 4113, IUER, 'ANC_COMPUTE_TATM_OPA',   &
     &                 'Trap of internal control: too many files'// &
     &                 ' in '//TRIM(SPD_URL)//' -- more than '//    &
     &                 TRIM(STR)//' -- please increase '//          &
     &                 'parameter MSPD' )
               RETURN 
            END IF
            C_SPD(L_SPD) = BUF(J2)(IND(1,12):IND(2,12))               
         END IF
 420  CONTINUE 
      CALL UNLINK ( TRIM(SPD_DIR_FILE)//CHAR(0) )
!     
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, * ) 'ANC_COMPUTE_TATM_OPA: L_SPD = ', L_SPD
      END IF
!
      IF ( L_SPD < 4 ) THEN
         CALL CLRCH ( STR )
         CALL INCH  ( L_SPD, STR )
         CALL ERR_LOG ( 4112, IUER, 'ANC_COMPUTE_TATM_OPA',             &
     &          'Too few spd files were found at '//TRIM(SPD_URL)//     &
     &          ' -- '//STR )
         RETURN 
      END IF
!
      CALL SORT_FAST_CH ( L_SPD, C_SPD )
      SPD_BEG_IND = 0
      SPD_END_IND = 0
      DO 430 J3=1,L_SPD
         IL = ILEN(C_SPD(J3))
         IP = INDEX ( C_SPD(J3), '.spd.bz2' )
         IF ( IP > 0 ) THEN
            IF ( C_SPD(J3)(IL-20:IL-8) .LE. ANC_BEG_DATE ) THEN
               SPD_BEG_IND = J3
            END IF
            IF ( C_SPD(J3)(IL-20:IL-8) .GE. ANC_END_DATE ) THEN
               IF ( SPD_END_IND < 1 ) THEN              
                  SPD_END_IND = J3
               END IF
            END IF
         END IF
 430  CONTINUE
!
      IF ( SPD_BEG_IND == 0 ) THEN
         CALL ERR_LOG ( 4113, IUER, 'ANC_COMPUTE_TATM_OPA',             &
     &           'The epoch of the telemety file '//                    &
     &           TRIM(ANC_BEG_DATE)//' is too early. '//                &
     &           'The earliest epoch in found at '//TRIM(SPD_URL)//     &
     &           ' is '//C_SPD(1) )
         RETURN
      END IF
      IF ( SPD_END_IND == 0 ) THEN
         CALL ERR_LOG ( 4113, IUER, 'ANC_COMPUTE_TATM_OPA',             &
     &           'The epoch of the telemety file '//                    &
     &           TRIM(ANC_END_DATE)//' is too late. '//                 &
     &           'The latest epoch in found at '//TRIM(SPD_URL)//       &
     &           ' is '//C_SPD(L_SPD) )
         RETURN
      END IF
      SPD_BEG_IND = MAX ( 1,     SPD_BEG_IND - ATP__SPD_MARGIN )
      SPD_END_IND = MIN ( L_SPD, SPD_END_IND + ATP__SPD_MARGIN )
      IF ( IVRB .GE. 2 ) THEN
         WRITE ( 6, * ) 'ANC_ISERT_TATM: SPD_BEG_IND= ', SPD_BEG_IND,   &
     &                  ' SPD_END_IND= ', SPD_END_IND
         WRITE ( 6, * ) 'ANC_ISERT_TATM: C_SPD dates: ',                &
     &                  TRIM(C_SPD(SPD_BEG_IND)), ' ',                  &
     &                  TRIM(C_SPD(SPD_END_IND))
      END IF
!
      LU_SPD = 0
      DO 440 J4=SPD_BEG_IND,SPD_END_IND
         SPD_INP_URL  = TRIM(SPD_URL)//'/'//C_SPD(J4)
         SPD_OUT_FILE = TRIM(TMP_DIR)//'/'//C_SPD(J4)
         DOWNLOAD_COMMAND = 'wget --retry-connrefused --tries=8 '//     &
     &                      '--timeout=10 --waitretry=2 -c -q -O '//    &
     &                      TRIM(SPD_OUT_FILE)//' '//SPD_INP_URL
         IF ( IVRB .GE. 2 ) THEN
            WRITE ( 6, * ) 'ANC_COMPUTE_TATM_OPA: J4= ', J4,            &
     &                     ' about to execute '//TRIM(DOWNLOAD_COMMAND)
         END IF
!
         IS = SYSTEM ( TRIM(DOWNLOAD_COMMAND)//CHAR(0) ) 
         IF ( IS .NE. 0 ) THEN
            DOWNLOAD_COMMAND = 'wget --retry-connrefused --tries=1 '//  &
     &                         '--timeout=2 --waitretry=2 -c -O '//     &
     &                         TRIM(SPD_OUT_FILE)//' '//SPD_INP_URL
            IS = SYSTEM   ( TRIM(DOWNLOAD_COMMAND)//CHAR(0) ) 
            CALL ERR_PASS ( IUER, IER )
            CALL RD_TEXT  ( SPD_DIR_FILE, ANC__MBUF, BUF, NBUF, IER )
            DO 450 J5=1,NBUF
               WRITE ( 6, '(A)' ) TRIM(BUF(J1))
 450        CONTINUE
            CALL ERR_LOG ( 4114, IUER, 'ANC_COMPUTE_TATM_OPA',          &
     &              'Error in an attempt to download file with '//      &
     &              'atmospheric brightness temperature and '//         &
     &              'atmospheric opacity '//SPD_INP_URL )
            RETURN 
         END IF
         DECOMPRESS_COMMAND = 'lbzip2 -fd '//SPD_OUT_FILE
         IS = SYSTEM  ( TRIM(DECOMPRESS_COMMAND)//CHAR(0) )
         IF ( IS .NE. 0 ) THEN
            CALL ERR_LOG ( 4115, IUER, 'ANC_COMPUTE_TATM_OPA',          &
     &              'Error in decompression of the downloaded '//       &
     &              'file with Tatm and atmospheric opacity '//         &
     &              TRIM(SPD_OUT_FILE)//' with command '//              &
     &              DECOMPRESS_COMMAND )
            RETURN
         END IF
         LU_SPD = LU_SPD + 1
         CU_SPD(LU_SPD) = SPD_OUT_FILE
         IP = INDEX ( SPD_OUT_FILE, '.bz2' )
         IF ( IP > 0 ) THEN
            CALL CLRCH ( CU_SPD(LU_SPD)(IP:) )
         END IF
 440  CONTINUE
      IF ( IVRB .GE. 1 ) THEN
         WRITE ( 6, * ) 'ANC_COMPUTE_TATM_OPA: downloaded ', LU_SPD,    &
     &                  ' files with Tatm and opacity'
      END IF
!
      MJD_TSYS_BEG = ANC%MJD_TTO
      TAI_TSYS_BEG = ANC%TAI_TTO + ANC%TTO(1)%TIM
!
! --- Computing the interpolation polynomial
!
      IF ( IVRB .GE. 2 ) THEN
         WRITE ( 6, * ) 'ANC_COMPUTE_TATM_OPA: '//                      &
     &                  'started interpolation of Tatm and opacity'
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL SPD_RES_INTRP ( TMP_DIR, 1, ANC%STA_NAM, MJD_TSYS_BEG,       &
     &                     TAI_TSYS_BEG, MJD_TSYS_END, TAI_TSYS_END,    &
     &                     SPD_DEL, 'azel', IVRB, IER )
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 4116, IUER, 'ANC_COMPUTE_TATM_OPA',             &
     &           'Error in interpolation of atmospheric opacity '//     &
     &           'and brightness temperature' )
         RETURN 
      END IF
      IF ( IVRB .GE. 1 ) THEN
         WRITE ( 6, * ) 'ANC_COMPUTE_TATM_OPA: Computed '//           &
     &                  'interpolation polynomials for '//ANC%STA_NAM
      END IF
!
      DEALLOCATE ( BUF )
      IF ( SPD_DEL%MODE_OPA_TAT == SPD__NAZ ) THEN
         DIMS(1) = SPD_DEL%N_FRQ
         DIMS(2) = SPD_DEL%ELV%N_EL
         DIMS(3) = SPD_DEL%AZM%N_AZ
         DIMS(4) = SPD_DEL%N_TIM
         SPD_DEL%MODE_OPA_TAT = SPD__NAZ
      ELSE IF ( SPD_DEL%MODE_OPA_TAT == SPD__1AZ ) THEN
         DIMS(1) = SPD_DEL%N_FRQ
         DIMS(2) = SPD_DEL%ELV%N_EL
         DIMS(3) = SPD_DEL%N_TIM
         DIMS(4) = 1
         SPD_DEL%MODE_OPA_TAT = SPD__1AZ
      ELSE
         CALL CLRCH ( STR )
         CALL INCH  ( SPD_DEL%MODE_OPA_TAT, STR )
         CALL ERR_LOG ( 4117, IUER, 'ANC_COMPUTE_TATM_OPA',             &
     &           'Trap of internal control: wrong value of '//          &
     &           'SPD_DEL%MODE_OPA_TAT: '//STR )
         RETURN
      END IF
!
      DO 460 J6=1,ANC%NUM_EPO_TTO
         IF ( .NOT. ASSOCIATED ( ANC%TTO(J6)%TATM ) ) THEN
            ALLOCATE ( ANC%TTO(J6)%TATM(ANC%NUM_TPS), STAT=IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 4118, IUER, 'ANC_COMPUTE_TATM_OPA',       &
     &                 'Error in allocating memory for the '//          &
     &                 'ANC%TTO(J6)%TATM while processing ' //          &
     &                 'antenna calibration file '//TRIM(ANC%FILIN) )
               RETURN
            END IF
         END IF
!
         IF ( .NOT. ASSOCIATED ( ANC%TTO(J6)%OPA ) ) THEN
            ALLOCATE ( ANC%TTO(J6)%OPA(ANC%NUM_TPS), STAT=IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 4119, IUER, 'ANC_COMPUTE_TATM_OPA',       &
     &                 'Error in allocating memory for the '//          &
     &                 'ANC%TTO(J6)%OPA while processing ' //           &
     &                 'antenna calibration file '//TRIM(ANC%FILIN) )
               RETURN
            END IF
         END IF
!
         TIM = (ANC%MJD_TTO - SPD_DEL%MJD_OBS_FIRST)*86400.0D0 +        &
     &         (ANC%TAI_TTO + ANC%TTO(J6)%TIM - SPD_DEL%TAI_OBS_FIRST)
         INDS(3) = IXMN4 ( SPD_DEL%N_TIM, SPD_DEL%TIM_ARR, SNGL(TIM) )
         IF ( INDS(3) < 1 ) THEN
            CALL CLRCH ( STR  )
            CALL CLRCH ( STR1 )
            CALL CLRCH ( STR2 )
            STR  = MJDSEC_TO_DATE ( ANC%MJD_TTO,                        &
     &                              ANC%TAI_TTO + ANC%TTO(J6)%TIM, IER )
            STR1 = MJDSEC_TO_DATE ( SPD_DEL%MJD_OBS_FIRST,              &
     &                              SPD_DEL%TAI_OBS_FIRST, IER  )
            STR2 = MJDSEC_TO_DATE ( SPD_DEL%MJD_OBS_FIRST,              &
     &                              SPD_DEL%TAI_OBS_FIRST +             &
     &                              SPD_DEL%TIM_ARR(SPD_DEL%N_TIM), IER)
            CALL ERR_LOG ( 4120, IUER, 'ANC_COMPUTE_TATM_OPA',          &
     &              'Trap of internal control: time '//TRIM(STR)//      &
     &              ' is out of range ['//                              &
     &              TRIM(STR1)//', '//TRIM(STR2)//']' )
            RETURN 
         END IF
         EL_USED = MIN ( MAX ( ANC%TTO(J6)%EL,                          &
     &                         SPD__DEL_MIN_DEG*DEG__TO__RAD + EPS),    &
     &                   P2I - EPS )
         MAP = DEL_ISA ( EL_USED )/ DEL_ISA ( P2I )
         INDS(2) = IXMN4 ( INT(SPD_DEL%ELV%N_EL,KIND=4),                &
     &                     SPD_DEL%MAP_ARR, SNGL(MAP)    )
         IF ( INDS(2) < 1 ) THEN
              CALL CLRCH ( STR  )
              CALL CLRCH ( STR1 )
              CALL CLRCH ( STR2 )
              WRITE ( UNIT=STR, FMT='(F8.4)' ) EL_USED
              WRITE ( UNIT=STR1, FMT='(F8.4)' ) SPD__DEL_MIN_DEG*DEG__TO__RAD
              WRITE ( UNIT=STR2, FMT='(F8.4)' ) P2I 
              CALL ERR_LOG ( 4120, IUER, 'ANC_COMPUTE_TATM_OPA',          &
     &                'Trap of internal control: time '//TRIM(STR)//      &
     &                ' is out of range ['//                              &
     &                TRIM(STR1)//', '//TRIM(STR2)//']' )
              RETURN 
         END IF 
!
         DO 470 J7=1,ANC%NUM_TPS
            INDS(1) = IXMN4 ( SPD_DEL%N_FRQ, SPD_DEL%FRQ_ARR,           &
     &                        SNGL(ANC%TPS(J7)%SKY_FRQ)         )
            IF ( INDS(1) < 1 ) THEN
                 CALL CLRCH ( STR  )
                 CALL CLRCH ( STR1 )
                 CALL CLRCH ( STR2 )
                 WRITE ( UNIT=STR(1:13),  FMT='(1PD13.6)' ) ANC%TPS(J7)%SKY_FRQ
                 WRITE ( UNIT=STR1(1:13), FMT='(1PD13.6)' ) SPD_DEL%FRQ_ARR(1)
                 WRITE ( UNIT=STR2(1:13), FMT='(1PD13.6)' ) SPD_DEL%FRQ_ARR(SPD_DEL%N_FRQ)
                 CALL INCH ( J1, STR3 )
                 CALL ERR_LOG ( 4121, IUER, 'ANC_COMPUTE_TATM_OPA',       &
     &                   'Trap of internal control: the frequency'//      &
     &                   TRIM(STR)//' is of of range ['//                 &
     &                   TRIM(STR2)//', '//TRIM(STR3)//'] Hz'  )
                 RETURN
            END IF
!
            ARGS(1) = ANC%TPS(J7)%SKY_FRQ
            ARGS(2) = MAP
            ARGS(3) = TIM
!
            ANC%TTO(J6)%OPA(J7) = VAL_3D_BSPL4 ( ARGS, SPD__MDEG, DIMS, &
     &                                 INDS, SPD_DEL%FRQ_ARR,           &
     &                                 SPD_DEL%MAP_ARR,                 &
     &                                 SPD_DEL%TIM_ARR,                 &
     &                                 SPD_DEL%OPA(1-SPD__MDEG,         &
     &                                             1-SPD__MDEG,         &
     &                                             1-SPD__MDEG,1) )
            ANC%TTO(J6)%TATM(J7) = VAL_3D_BSPL4 ( ARGS, SPD__MDEG,      &
     &                                 DIMS, INDS, SPD_DEL%FRQ_ARR,     &
     &                                 SPD_DEL%MAP_ARR,                 &
     &                                 SPD_DEL%TIM_ARR,                 &
     &                                 SPD_DEL%TAT(1-SPD__MDEG,         &
     &                                             1-SPD__MDEG,         &
     &                                             1-SPD__MDEG,1) )
            IF ( IVRB .GE. 4 ) THEN
                 write ( 6, * ) 'ANC_COMPUTE_TATM_OPA j6= ', j6,          &
     &                          ' j7= ', int2(j7), ' tim= ', sngl(tim),   &
     &                          ' el= ', sngl(el_used),                   &
     &                          ' frq= ', sngl(ANC%TPS(J7)%SKY_FRQ),      &
     &                          ' opa= ', sngl(ANC%TTO(J6)%OPA(J7)),      &
     &                          ' tatm= ', sngl(ANC%TTO(J6)%TATM(J7)) ! %%%%%%%%%
            END IF
 470     CONTINUE
 460  CONTINUE 
      IF ( IVRB .GE. 1 ) THEN
         WRITE ( 6, * ) 'ANC_COMPUTE_TATM_OPA: '//                      &
     &                  'Computed atmospheric opacity and Tatm for '//  &
     &                  ANC%STA_NAM
      END IF
!
      L_PRV = 0
      L_PRV = L_PRV + 1 ; C_PRV(L_PRV) =                                &
     &                         'GENERATOR:      '//ANC_COMPUTE_TATM_OPA__LABEL
      CALL GET_COMMAND ( STR ) 
      L_PRV = L_PRV + 1 ; C_PRV(L_PRV) =                                &
     &                         'COMMAND_LINE:   '//TRIM(STR)
      L_PRV = L_PRV + 1 ; C_PRV(L_PRV) =                                &
     &                         'CREATED_VARS:   TATM OPA'
      L_PRV = L_PRV + 1 ; C_PRV(L_PRV) =                                &
     &                         'CREATED_DATE:   '//GET_CDATE()
      CALL CLRCH ( STR )
      CALL INCH  ( SPD_END_IND-SPD_BEG_IND+2, STR )
      L_PRV = L_PRV + 1 ; C_PRV(L_PRV) =                                &
     &                     'NUM_FILES:      '//STR
      L_PRV = L_PRV + 1 ; C_PRV(L_PRV) =                                &
     &                     'DATA_TYPE:      1 ancilliary telemetry data'
      L_PRV = L_PRV + 1 ; C_PRV(L_PRV) =                                &
     &                     'DATA_FILE:      '//ANC%FILIN
      L_PRV = L_PRV + 1 ; C_PRV(L_PRV) =                                &
     &                     'DATA_CREATED:   unknown'
      N_FIL = 1
      DO 480 J8=SPD_BEG_IND,SPD_END_IND
         N_FIL = N_FIL + 1
         CALL CLRCH ( STR )
         CALL INCH  ( N_FIL, STR )
         L_PRV = L_PRV + 1 ; C_PRV(L_PRV) =                             &
     &                                    'DATA_TYPE:      '//          &
     &                                    TRIM(STR)//' slant path delay'
         L_PRV = L_PRV + 1 ; C_PRV(L_PRV) =                             &
     &                                    'DATA_FILE:      '//          &
     &                                    TRIM(STR)//' '//C_SPD(J8)
         L_PRV = L_PRV + 1 ; C_PRV(L_PRV) =                             &
     &                                    'DATA_CREATED:   '//          &
     &                                    TRIM(STR)//' unknown'
 480  CONTINUE 
      CALL CLRCH ( STR )
      CALL INCH  ( SPD_DEL%MOD%N_LINES + SPD_DEL%MET%N_LINES + 5,  STR )
      L_PRV = L_PRV + 1 ;     C_PRV(L_PRV) = 'NUM_COMMENTS:   '//STR
      IB = 1
!
      DO 490 J9=1,SPD_DEL%MOD%N_LINES
         CALL LIB$MOVC3 ( 80, SPD_DEL%MOD%TEXT(IB:), STR )
         IB = IB + SPD__M_TXT+1
         L_PRV = L_PRV + 1 ;  C_PRV(L_PRV) = 'COMMENT:        '//STR
 490  CONTINUE 
      L_PRV = L_PRV + 1 ;     C_PRV(L_PRV) = 'COMMENT:        .'
      IB = 1
! ---
      DO 4100 J10=1,SPD_DEL%MET%N_LINES
         CALL LIB$MOVC3 ( 80, SPD_DEL%MET%TEXT(IB:), STR )
         IB = IB + SPD__M_TXT+1
         L_PRV = L_PRV + 1 ;  C_PRV(L_PRV) = 'COMMENT:        '//STR
 4100 CONTINUE 
      L_PRV = L_PRV + 1 ; C_PRV(L_PRV) = 'COMMENT:        .'
      L_PRV = L_PRV + 1 ; C_PRV(L_PRV) =                                &
     &                          'COMMENT:        '//                    &
     &                          'Computed atmospheric opacity and '//   &
     &                          'atmosphere brightness temperature '//  &
     &                          'by integrating'
      L_PRV = L_PRV + 1 ; C_PRV(L_PRV) =                                &
     &                          'COMMENT:        '//                    &
     &                          'equations of propagation in the '//    &
     &                          'heterogeneous media with the '//       &
     &                          'refractivity'
      L_PRV = L_PRV + 1 ; C_PRV(L_PRV) =                                &
     &                          'COMMENT:        '//                    &
     &                          'index derived from the output of '//   &
     &                          'numerical weather models'
!     
      IF ( IVRB .GE. 2 ) THEN
           DO 4110 J11=1,L_PRV
              WRITE ( 6, * ) 'ANC_COMPUTE_TATM_OPA: provenance: '//TRIM(C_PRV(J11))
 4110      CONTINUE 
      END IF
!
!%NH%!      ANC%NUM_OPA  = ANC%NUM_EPO_TTO
!%NH%!      ANC%NUM_TATM = ANC%NUM_EPO_TTO
      ANC%NUM_OPA   =  ANC%NUM_TSYS
      ANC%NUM_TATM  =  ANC%NUM_TSYS
!
! --- Write updated anc file to file
!
      IF ( IVRB > 2 ) THEN
           CALL CPU_TIME ( TSS2 )
      END IF
 1001 FORMAT ( F12.5, ' sec to parse ', A )

      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  ANC_COMPUTE_TATM_OPA  !#!#
