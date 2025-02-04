      PROGRAM    C04_TO_ERP
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'getpar.i'
      CHARACTER  FILIN*128, FILOUT*128
      INTEGER*4  MP, MB_LEAP
      PARAMETER  ( MP = 32768, MB_LEAP = 512 )
      INTEGER*4  NHEAD_BUF, NSES
      REAL*8     JD1(MP), XR1_VAL(MP), YR1_VAL(MP), UR1_VAL(MP), &
     &                    XR1_ERR(MP), YR1_ERR(MP), UR1_ERR(MP)
      REAL*8     T8(MP), X8(MP), E8(MP), W8(MP), JD_EOP, JD_LAST, DR, SH, &
     &           UTC, TAI, WW, WRMS
      REAL*8     X1(MP), X2(MP)
      REAL*8     D1, DN, COEF1(MP), COEF2(MP), WORK(MP)
      REAL*8     JD_C04(MP), YR_C04(MP), &
     &           XP_C04(MP), YP_C04(MP), U1_C04(MP), DPSI_C04(MP), DEPS_C04(MP), &
     &           XP_ERR(MP), YP_ERR(MP), U1_ERR(MP), DPSI_ERR(MP), DEPS_ERR(MP)
      REAL*8     TIM1(MP),       TIM2(MP), &
     &           EOP1_VAL(MP,3), EOP2_VAL(MP,3), &
     &           EOP1_ERR(MP,3), EOP2_ERR(MP,3)
      CHARACTER  CH_FLAG(MP), BUF_LEAP(MB_LEAP)*256, URL_C04*128, FILERP*128, &
     &           EXTFMT*12, URLEXT2*8
      CHARACTER  FINAM_ERP*128, C04_FILE*128
      REAL*8     TIM_BEG, TIM_END
      INTEGER*4  N_ERP, N_C04, NUMARG, IP1, IP2, J1, J2, J3, J4, MJD, IER, IUER
      REAL*8,    EXTERNAL :: FSPL8 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IXMN8 
!
      EXTFMT  = 'IERS_C04'
      URLEXT2 = ' '
      URL_C04 = 'ftp://hpiers.obspm.fr/iers/eop/eopc04_14/eopc04.62-now'
      FILERP  = 'eopc04.erp'
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, * ) 'Usage c04_to_erp fil_c04 fil_erp'
           CALL EXIT ( 1 ) 
         ELSE 
           CALL GETARG ( 1, FILIN  ) 
           CALL GETARG ( 2, FILOUT ) 
      END IF
!
      IUER = -1
      CALL RD_IERS_C04 ( FILIN, MP, N_C04, JD_C04, XP_C04, XP_ERR, YP_C04, &
     &                   YP_ERR, U1_C04, U1_ERR, DPSI_C04, DPSI_ERR, &
     &                   DEPS_C04, DEPS_ERR, CH_FLAG, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      IUER = -1
      CALL WRI_ERP ( FILOUT, EXTFMT, URL_C04, URLEXT2, FILERP, N_C04, &
     &               JD_C04, XP_C04, YP_C04, U1_C04, XP_ERR, YP_ERR, U1_ERR, &
     &               'I', .FALSE., .FALSE., 0.0D0, 0.0D0, &
     &               0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      WRITE ( 6, * ) 'Output file: '//FILOUT(1:I_LEN(FILOUT))
      END  !#!  
