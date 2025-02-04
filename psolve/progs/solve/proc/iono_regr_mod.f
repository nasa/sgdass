      SUBROUTINE IONO_REGR_MOD ( EXP_NAME, GIM_INFO_DIR, VTD, &
     &                           GIM_ADDW_DIR, GIM_DEL_DIR, &
     &                           GIM_NOI_DIR, ISEED, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine IONO_REGR_MOD
! *                                                                      *
! *  ### 08-APR-2022  IONO_REGR_MOD v1.0 (c)  L. Petrov 08-DEC-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'iono_solve.i'
      INCLUDE   'vtd.i'
      TYPE     ( IONO_DEL__TYPE ) :: IONO(M_OBS)
      TYPE     ( VTD__TYPE      ) :: VTD
      CHARACTER  EXP_NAME*(*), GIM_INFO_DIR*(*), GIM_ADDW_DIR*(*), &
     &           GIM_DEL_DIR*(*), GIM_NOI_DIR*(*)
      INTEGER*4  ISEED, IVRB, IUER
      CHARACTER  C_STA(M_STA)*8, C_BAS(M_BAS)*17, STR*128, &
     &           FIL_IONO*128, FIL_WEI*128, FIL_DEL*128, FIL_NOI*128
      CHARACTER  GEN_IER__LABEL*46
      INTEGER*4  MP
      PARAMETER  ( MP = 64*1024 )
      REAL*8     DUR, AVR_BAS(MP), RMS_BAS(MP), ME_BAS(MP), TAI_CEN, &
     &           IONO_VAL, IONO_FREQ, ME_VAL, IONO_FRQ_AVR
      REAL*8     RMS_MOD_FUDGE, MAX_SIG
      INTEGER*4  J1, J2, J3, J4, KP, L_STA, L_BAS, I_STA(2), I_BAS, NOBS, &
     &           MJD_CEN, EXP_VERS, IDAY, IER
      INTEGER*4, EXTERNAL :: ADD_CLIST, ILEN, I_LEN, LTM_DIF
      REAL*8,    EXTERNAL :: GET_IONO_MOD_RES_RMS, RGAUSS
!
      RMS_MOD_FUDGE =  1.19D0
      MAX_SIG       = 10.00D9
!
! --- Read informaion about the ionospheric path delay from this VLBI experiment
! --- and from GNSS maps
!
      FIL_IONO = TRIM(GIM_INFO_DIR)//'/'//TRIM(EXP_NAME)//'_iono.txt'
      CALL ERR_PASS ( IUER, IER )
      CALL IONO_READ_INFO ( FIL_IONO, IONO__ABND, M_OBS, NOBS, IONO, EXP_NAME, &
     &                      EXP_VERS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7311, IUER, 'IONO_REGR_MOD', 'Error in parsing '// &
     &         'pSolve iono information file '//FIL_IONO )
           RETURN
      END IF
!
      IONO%ISEED = ISEED
!
! --- Collect station list 
!
      L_STA = 0
      DO 410 J1=1,NOBS
         IER = 0
         I_STA(1) = ADD_CLIST ( M_STA, L_STA, C_STA, IONO(J1)%STA(1), IER )
         I_STA(2) = ADD_CLIST ( M_STA, L_STA, C_STA, IONO(J1)%STA(2), IER )
 410  CONTINUE 
      L_BAS = (L_STA*(L_STA-1))/2
      CALL SORT_CH ( L_STA, C_STA ) 
      DUR = (IONO(NOBS)%MJD*86400.0D0 + IONO(NOBS)%TAI) - (IONO(1)%MJD*86400.0D0 + IONO(1)%TAI)
      TAI_CEN = IONO(1)%TAI + DUR/2
      IDAY = TAI_CEN/86400.0D0
      TAI_CEN = TAI_CEN - IDAY*86400.0D0
      MJD_CEN = IONO(1)%MJD + IDAY
!
      CALL ERR_PASS ( IUER, IER )
      CALL COMP_IONO_ERR_REGR ( MJD_CEN, TAI_CEN, L_STA, L_BAS, C_STA, C_BAS, &
     &                          AVR_BAS, RMS_BAS, ME_BAS, &
     &                          VTD, ISEED, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7312, IUER, 'IONO_REGR_MOD', 'Error in computation '// &
     &         'of ionosphere statistics for database '//EXP_NAME )
           RETURN
      END IF
!
      IF ( IVRB .GE. 1 ) THEN
           DO 430 J3=1,L_BAS
              WRITE ( 6, 110 ) EXP_NAME, C_BAS(J3), &
     &                         1.D12*AVR_BAS(J3), 1.D12*RMS_BAS(J3), ME_BAS(J3)
 110          FORMAT ( 'Exp: ', A, ' Bas: ', A, ' Avr_iono: ', F8.1, &
     &                 ' ps  Rms_iono: ', F8.1, ' me: ', F5.3 )
 430       CONTINUE 
      END IF
!
      IONO_FRQ_AVR = 0.0D0
      KP = 0
!
      DO 440 J4=1,NOBS
         IF ( IONO(J4)%FREQ_EFF(1) > 3.0D0*IONO__FREQ_MIN ) THEN
              ME_VAL = (IONO(J4)%IONO_MAP(1) + IONO(J4)%IONO_MAP(2) )/2.0D0
              I_BAS = LTM_DIF ( 1, L_BAS, C_BAS, IONO(J4)%STA(1)//'/'//IONO(J4)%STA(2) )
              IF ( I_BAS < 1 ) THEN
!
! ---------------- In rare cases one needs swap station order
!
                   I_BAS = LTM_DIF ( 1, L_BAS, C_BAS, IONO(J4)%STA(2)//'/'//IONO(J4)%STA(1) )
              END IF
              IF ( IONO(J4)%FREQ_EFF(1) > 42.0D9 .AND. IONO(J4)%FREQ_EFF(2) > 20.D9 ) THEN
!
! ---------------- Case of K/Q observations. We take the second frequency
!
                   IONO_FREQ = IONO(J4)%FREQ_EFF(2)
                 ELSE
                   IONO_FREQ = IONO(J4)%FREQ_EFF(1)
              END IF
              IONO(J4)%ADD_IONO_VAL = IONO(J4)%IONO_G*(IONO__FREQ_REF/IONO_FREQ)**2
              IONO(J4)%ADD_IONO_SIG = RMS_MOD_FUDGE* &
     &                                ME_VAL* &
     &                                GET_IONO_MOD_RES_RMS( RMS_BAS(I_BAS) )* &
     &                                (IONO__FREQ_REF/IONO_FREQ)**2
              IONO(J4)%IONO_ZEN(1)  = RGAUSS ( ISEED, IONO(J4)%ADD_IONO_SIG/DSQRT(2.0D0)/ME_VAL )
              IONO(J4)%IONO_ZEN(2)  = RGAUSS ( ISEED, IONO(J4)%ADD_IONO_SIG/DSQRT(2.0D0)/ME_VAL )
              IONO(J4)%IONO_D       = IONO(J4)%IONO_ZEN(2)*IONO(J4)%IONO_MAP(2) - &
     &                                IONO(J4)%IONO_ZEN(1)*IONO(J4)%IONO_MAP(1) 
              IONO_FRQ_AVR = IONO_FRQ_AVR + IONO_FREQ 
              KP = KP + 1
            ELSE
              IONO(J4)%ADD_IONO_VAL = 0.0D0
              IONO(J4)%ADD_IONO_VAL = MAX_SIG
         END IF
 440  CONTINUE 
      IF ( KP > 0 ) THEN
           IONO_FRQ_AVR = IONO_FRQ_AVR/KP
      END IF
!
      FIL_DEL = TRIM(GIM_DEL_DIR)//'/'//TRIM(EXP_NAME)//'.del'
      FIL_WEI = TRIM(GIM_ADDW_DIR)//'/'//TRIM(EXP_NAME)//'.addw'
      FIL_NOI = TRIM(GIM_NOI_DIR)//'/'//TRIM(EXP_NAME)//'.noi'
!    
      CALL ERR_PASS ( IUER, IER )
      CALL IONO_WRI ( NOBS, ION__WEI, GEN_IER__LABEL, IONO_FRQ_AVR, &
     &                EXP_NAME, EXP_VERS, IONO, FIL_WEI, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7313, IUER, 'IONO_REGR_MOD', 'Error in '// &
     &         'an attenpt to write additive weights due to the '// &
     &         'ionosphere into file '//FIL_WEI )
           RETURN
      END IF
!    
      CALL ERR_PASS ( IUER, IER )
      CALL IONO_WRI ( NOBS, ION__DEL, GEN_IER__LABEL, IONO_FRQ_AVR, &
     &                EXP_NAME, EXP_VERS, IONO, FIL_DEL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7314, IUER, 'IONO_REGR_MOD', 'Error in '// &
     &         'an attept to write additive ionospheric delays '// &
     &         'into file '//FIL_DEL )
           RETURN
      END IF
!    
      CALL ERR_PASS ( IUER, IER )
      CALL IONO_WRI ( NOBS, ION__NOI, GEN_IER__LABEL, IONO_FRQ_AVR, &
     &                EXP_NAME, EXP_VERS, IONO, FIL_NOI, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7315, IUER, 'IONO_REGR_MOD', 'Error in '// &
     &         'an attempt to write additive noise due to the inosphere '// &
     &         'into file '//FIL_NOI )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  IONO_REGR_MOD  !#!#
