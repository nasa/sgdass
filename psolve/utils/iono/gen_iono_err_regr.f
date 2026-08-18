      PROGRAM    GEN_IONO_ERR_REGR
! ************************************************************************
! *                                                                      *
! *   Program GEN_IONO_ERR_REGR 
! *                                                                      *
! * # 08-APR-2022 GEN_IONO_ERR_REGR_MAIN v2.0 (c) L. Petrov 02-OCT-2022 #*
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'iono_from_solve.i'
      TYPE     ( IONO__TYPE     ) :: IONO(M_OBS)
      TYPE     ( IONO__EST_TYPE ) :: IONO_EST
      CHARACTER  FIL_IONO*128, FIL_ADD*128, FIL_WEI*128, FIL_NOI*128, &
     &           EXP_NAME*10, C_STA(M_STA)*8, C_BAS(M_BAS)*17, &
     &           STR*128, VTD_CONF_FILE*128
      CHARACTER  GEN_IER__LABEL*46, REG_MOD*4
      PARAMETER  ( GEN_IER__LABEL = 'gen_iono_err_regr   1.02 Version of 2022.09.26' )
      INTEGER*4  MP
      PARAMETER  ( MP = 64*1024 )
      REAL*8     DUR, AVR_BAS(MP), RMS_BAS(MP), ME_BAS(MP), TAI_CEN, &
     &           IONO_VAL, IONO_FREQ, ME_VAL, IONO_FRQ_AVR, IONO_SCL
      REAL*8     RMS_MOD_FUDGE, MAX_SIG
      INTEGER*4  J1, J2, J3, J4, KP, L_STA, L_BAS, I_STA(2), I_BAS, NOBS, &
     &           MJD_CEN, EXP_VERS, IDAY, ISEED, DEBUG_LEVEL, IVRB, IUER 
      INTEGER*4, EXTERNAL :: ADD_CLIST, ILEN, I_LEN, LTM_DIF
      REAL*8,    EXTERNAL :: GET_MOD_RES_RMS, RGAUSS
!
      RMS_MOD_FUDGE =  1.19D0
      MAX_SIG       = 10.00D9
      IONO_SCL      = 0.850D0
!! IONO_SCL      = 1.000
      REG_MOD       = 'spl'
!
      IVRB = 1
      VTD_CONF_FILE = '/cont/iono.vtd'
      ISEED = 1239141
      CALL CLRCH ( FIL_NOI )
!
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: gen_iono_err_regr iono_file fil_del fil_addw [seed] [fil_noi]'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FIL_IONO )
           CALL GETARG ( 2, FIL_ADD )
           CALL GETARG ( 3, FIL_WEI )
           IF ( IARGC() .GE. 4 )  THEN
                CALL GETARG ( 4, STR )
                CALL CHIN   ( STR, ISEED )                
           END IF
           IF ( IARGC() .GE. 5 )  THEN
                CALL GETARG ( 5, FIL_NOI )
           END IF
      END IF
!
      IUER = -1
      CALL LOAD_IONO_SOLVE ( FIL_IONO, IONO__ABND, M_OBS, NOBS, IONO, IONO_EST, &
     &                       EXP_NAME, EXP_VERS, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 7001, IUER, 'GEN_IONO_ERR_REGR', 'Error in parsing '// &
     &         'pSolve iono output file '//FIL_IONO )
           CALL EXIT ( 1 )
      END IF
      IONO%ISEED = ISEED
!
! --- Collect station list 
!
      L_STA = 0
      DO 410 J1=1,NOBS
         IUER = -1
         I_STA(1) = ADD_CLIST ( M_STA, L_STA, C_STA, IONO(J1)%STA(1), IUER )
         I_STA(2) = ADD_CLIST ( M_STA, L_STA, C_STA, IONO(J1)%STA(2), IUER )
 410  CONTINUE 
      L_BAS = (L_STA*(L_STA-1))/2
      CALL SORT_CH ( L_STA, C_STA ) 
      DUR = (IONO(NOBS)%MJD*86400.0D0 + IONO(NOBS)%TAI) - (IONO(1)%MJD*86400.0D0 + IONO(1)%TAI)
      TAI_CEN = IONO(1)%TAI + DUR/2
      IDAY = TAI_CEN/86400.0D0
      TAI_CEN = TAI_CEN - IDAY*86400.0D0
      MJD_CEN = IONO(1)%MJD + IDAY
!
      IF ( DEBUG_LEVEL .GE. 1 ) THEN
           AVR_BAS(1) = 0.0
           RMS_BAS(1) = 0.0
           ME_BAS(1)  = 0.0
           KP = 0
           DO 420 J2=1,NOBS    
              IF ( IONO(J2)%STA(1) == 'HARTRAO ' .AND. IONO(J2)%STA(2) == 'NYALES20' ) THEN
                   ME_VAL   = (IONO(J2)%IONO_MAP(1) + IONO(J2)%IONO_MAP(2))/2.0D0
                   IONO_VAL = IONO_SCL*IONO(J2)%IONO_G/ME_VAL
                   AVR_BAS(1) = AVR_BAS(1) + IONO_VAL
                   RMS_BAS(1) = RMS_BAS(1) + IONO_VAL**2
                   ME_BAS(1)  = ME_BAS(1)  + ME_VAL
                   KP = KP + 1
              END IF
 420       CONTINUE 
!
           IF ( KP > 0  ) THEN
                AVR_BAS(1) = AVR_BAS(1)/KP
                RMS_BAS(1) = DSQRT ( RMS_BAS(1)/KP - AVR_BAS(1)**2 )
                ME_BAS(1)  = ME_BAS(1)/KP
                WRITE ( 6, * ) 'AVR= ', SNGL(1.D12*AVR_BAS(1)), SNGL(1.D12*RMS_BAS(1)), &
     &                         ' ME= ', SNGL(ME_BAS(1)), ' KP= ', INT2(KP)
           END IF
           WRITE ( 6, * ) 'L_STA= ', L_STA, ' L_BAS= ', L_BAS, &
     &                    ' MJD_CEN= ', MJD_CEN, ' TAI_CEN= ', TAI_CEN, ' DUR= ', DUR
      END IF
!
      IUER = -1
      CALL COMP_IONO_ERR_REGR ( MJD_CEN, TAI_CEN, L_STA, L_BAS, C_STA, C_BAS, &
     &                         AVR_BAS, RMS_BAS, ME_BAS, &
     &                         VTD_CONF_FILE, ISEED, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 7002, IUER, 'GEN_IONO_ERR_REGR', 'Error in computation '// &
     &         'of ionosphere statistics' )
           CALL EXIT ( 1 )
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
              IONO(J4)%ADD_IONO_VAL = IONO_SCL*IONO(J4)%IONO_G*(IONO__FREQ_REF/IONO_FREQ)**2
              IONO(J4)%ADD_IONO_SIG = RMS_MOD_FUDGE* &
     &                                ME_VAL* &
     &                                GET_MOD_RES_RMS( REG_MOD, RMS_BAS(I_BAS) )* &
     &                                (IONO__FREQ_REF/IONO_FREQ)**2
              IONO(J4)%IONO_ZEN(1)  = RGAUSS ( ISEED, IONO(J4)%ADD_IONO_SIG/DSQRT(2.0D0)/ME_VAL )
              IONO(J4)%IONO_ZEN(2)  = RGAUSS ( ISEED, IONO(J4)%ADD_IONO_SIG/DSQRT(2.0D0)/ME_VAL )
              IONO(J4)%IONO_D       = IONO(J4)%IONO_ZEN(2)*IONO(J4)%IONO_MAP(2) - &
     &                                IONO(J4)%IONO_ZEN(1)*IONO(J4)%IONO_MAP(1) 
     &                                         
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
      IUER = -1
      CALL WRI_ADD_IONO_WEI ( NOBS, 2, GEN_IER__LABEL, IONO_FRQ_AVR, &
     &                        EXP_NAME, EXP_VERS, IONO, FIL_WEI, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!    
      IUER = -1
      CALL WRI_ADD_IONO_WEI ( NOBS, 12, GEN_IER__LABEL, IONO_FRQ_AVR, &
     &                        EXP_NAME, EXP_VERS, IONO, FIL_ADD, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!    
      IF ( ILEN(FIL_NOI) > 0 ) THEN
           IUER = -1
           CALL WRI_ADD_IONO_WEI ( NOBS, 22, GEN_IER__LABEL, IONO_FRQ_AVR, &
     &                             EXP_NAME, EXP_VERS, IONO, FIL_NOI, IUER )
      END IF
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      END  PROGRAM    GEN_IONO_ERR_REGR  !#!#
