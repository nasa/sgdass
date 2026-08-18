      PROGRAM    DIF_C04_USNO
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'getpar.i'
      INTEGER*4    M_DEG, M_ERM, MP, MSES, MHEAD
      PARAMETER  ( M_DEG =     3 )
      PARAMETER  ( M_ERM =  8192 )
      PARAMETER  (    MP = 32768 )
      PARAMETER  (  MSES =  8192 )
      PARAMETER  ( MHEAD =   512 )
      INTEGER*4  ICMP, NC04, NUSN, NAPR
      LOGICAL*4  FL_UZT, FL_DUAL
      CHARACTER  CH_FLAG(MP)*1, DATE_BEG*21, DATE_END*21
      REAL*8     JD_C04(MP), JD_USN(MP), JD_APR(MP), &
     &           XP_C04(MP), YP_C04(MP), U1_C04(MP), PP_C04(MP), EP_C04(MP), &
     &           XE_C04(MP), YE_C04(MP), UE_C04(MP), PE_C04(MP), EE_C04(MP), &
     &           XP_USN(MP), YP_USN(MP), U1_USN(MP), PP_USN(MP), EP_USN(MP), &
     &           XE_USN(MP), YE_USN(MP), UE_USN(MP), PE_USN(MP), EE_USN(MP), &
     &           XP_APR(MP), YP_APR(MP), U1_APR(MP), PP_APR(MP), EP_APR(MP), &
     &           XE_APR(MP), YE_APR(MP), UE_APR(MP), PE_APR(MP), EE_APR(MP)  
      INTEGER*4  MJD_BEG, MJD_END
      REAL*8     JD_LAST, TAI_BEG, TAI_END
      CHARACTER  FILE_C04*128, FILE_USN*128, FILE_APR*128, STR*32
      INTEGER*4  IDER, IUER
      INTEGER*4, EXTERNAL ::  ILEN, I_LEN
!      
      IF ( IARGC() < 5 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: dif_c04_usno <c04> <usno-file> <date_beg> <date_end> <cmp> [-dual]'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FILE_C04 )
           CALL GETARG ( 2, FILE_USN )
           CALL GETARG ( 3, DATE_BEG )
           CALL GETARG ( 4, DATE_END )
           CALL GETARG ( 6, STR   )
           CALL TRAN ( 11, STR, STR )
           IF ( STR == 'E1' ) THEN
                ICMP = 1
                IDER = 0
              ELSE IF ( STR == 'E2' ) THEN
                ICMP = 2
                IDER = 0
              ELSE IF ( STR == 'E3' ) THEN
                ICMP = 3
                IDER = 0
                FL_UZT = .FALSE.
              ELSE IF ( STR == 'E3Z' ) THEN
                ICMP = 3
                IDER = 0
                FL_UZT = .TRUE.
              ELSE IF ( STR == 'D1' ) THEN
                ICMP = 1
                IDER = 1
              ELSE IF ( STR == 'D2' ) THEN
                ICMP = 2
                IDER = 1
              ELSE IF ( STR == 'D3' ) THEN
                ICMP = 3
                IDER = 1
                FL_UZT = .FALSE.
              ELSE IF ( STR == 'D3Z' ) THEN
                ICMP = 3
                IDER = 1
                FL_UZT = .TRUE.
              ELSE IF ( STR == 'S1' ) THEN
                ICMP = 1
                IDER = 2
              ELSE IF ( STR == 'S2' ) THEN
                ICMP = 2
                IDER = 2
              ELSE IF ( STR == 'S3' ) THEN
                ICMP = 3
                IDER = 2
                FL_UZT = .FALSE.
              ELSE IF ( STR == 'S3Z' ) THEN
                ICMP = 3
                IDER = 2
                FL_UZT = .TRUE.
              ELSE
                IUER = -1
                CALL ERR_LOG ( 8001, IUER, 'DIF_C04_USNO', 'Unrecognized '// &
     &              'component: one of E1, E2, E3, E3Z, D1, D2, D3, D3Z, '// &
     &              'D1, D2, D3, D3Z  were expected' )
                CALL EXIT ( 1 )
           END IF
!
           IUER = -1
           CALL DATE_TO_TIME ( DATE_BEG, MJD_BEG, TAI_BEG, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
!
           IUER = -1
           CALL DATE_TO_TIME ( DATE_END, MJD_END, TAI_END, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
!
           IF ( IARGC() .GE. 7 ) THEN
                CALL GETARG ( 7, STR )
                IF ( STR == '-dual' ) THEN
                     FL_DUAL = .TRUE.
                END IF
           END IF
      END IF
!
      IUER = -1
      CALL RD_IERS_C04 ( FILE_C04, MP, NC04, JD_C04, XP_C04, XE_C04, &
     &                   YP_C04, YE_C04, U1_C04, UE_C04, PP_C04, &
     &                   PE_C04, EP_C04, EE_C04, CH_FLAG, IUER )
      IF ( IUER .NE. 0 ) STOP 'Error in reading IERS C04 file'
!
      IUER = -1
      CALL RD_FINALS ( FILE_USN, MP, NUSN, JD_USN, XP_USN, XE_USN, &
     &                 YP_USN, YE_USN, U1_USN, UE_USN, PP_USN, &
     &                 PE_USN, EP_USN, EE_USN, CH_FLAG, IUER )
      IF ( IUER .NE. 0 ) STOP 'Error in reading IERS USN file'
!
      IUER = -1
!
      IUER = -1
      CALL PLOT_DIF ( NC04, JD_C04, XP_C04, XE_C04, YP_C04, YE_C04, &
     &                      U1_C04, UE_C04, PP_C04, PE_C04, EP_C04, EE_C04, &
     &                NUSN, JD_USN, XP_USN, XE_USN, YP_USN, YE_USN, &
     &                      U1_USN, UE_USN, PP_USN, PE_USN, EP_USN, EE_USN, & 
     &                MJD_BEG, TAI_BEG, MJD_END, TAI_END, &
     &                ICMP, IDER, FL_UZT, FL_DUAL, IUER )
      IF ( IUER .NE. 0 ) STOP 'dif_c04_usno'
      END  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PLOT_DIF ( N_C04,  JD_C04, XP_C04, XE_C04, YP_C04, YE_C04, &
     &                      U1_C04, UE_C04, PP_C04, PE_C04, EP_C04, EE_C04, &
     &                      N_USN,  JD_USN, XP_USN, XE_USN, YP_USN, YE_USN, &
     &                      U1_USN, UE_USN, PP_USN, PE_USN, EP_USN, EE_USN, & 
     &                      MJD_BEG, TAI_BEG, MJD_END, TAI_END, &
     &                      ICMP, IDER, FL_UZT, FL_DUAL, IUER )
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INTEGER*4  N_C04, N_USN, MJD_BEG, MJD_END, ICMP, IDER, IUER
      REAL*8     JD_C04(N_C04), JD_USN(N_USN), &
     &           XP_C04(N_C04), YP_C04(N_C04), U1_C04(N_C04), PP_C04(N_C04), EP_C04(N_C04), &
     &           XE_C04(N_C04), YE_C04(N_C04), UE_C04(N_C04), PE_C04(N_C04), EE_C04(N_C04), &
     &           XP_USN(N_USN), YP_USN(N_USN), U1_USN(N_USN), PP_USN(N_USN), EP_USN(N_USN), &
     &           XE_USN(N_USN), YE_USN(N_USN), UE_USN(N_USN), PE_USN(N_USN), EE_USN(N_USN)
      REAL*8     TAI_BEG, TAI_END
      LOGICAL*4  FL_UZT, FL_DUAL
      INTEGER*4  MP
      PARAMETER  ( MP = 32*1024 )
      REAL*8     CF_C04(N_C04), CF_USN(N_USN), WORK(MP)
      REAL*8     T8(MP), X1(MP), X2(MP)
      REAL*8     TIM_BEG, TIM_END, STEP, JD_VAL, C04_VAL, USN_VAL
      INTEGER*4  J1, J2, J3, MJD_VAL, IER, IER1, IER2, IER3, &
     &           IXC_C04, IXC_USN
      REAL*8     E3, E3_DOT, E3_DT2, TAI_VAL
      REAL*8,    EXTERNAL :: FSPL8, DSPL8, D2SPL8
      INTEGER*4, EXTERNAL :: IXMN8, ILEN, I_LEN
!
      CALL ERR_PASS ( IUER, IER1 )
      CALL ERR_PASS ( IUER, IER2 )
      CALL ERR_PASS ( IUER, IER3 )
      IF ( ICMP == 1 ) THEN
           CALL MAKE_SPLINE ( 3, N_C04, JD_C04, XP_C04, 0.0D0, 0.D0, CF_C04, &
     &                        WORK, IER1 )
           CALL MAKE_SPLINE ( 3, N_USN, JD_USN, XP_USN, 0.0D0, 0.D0, CF_USN, &
     &                        WORK, IER2 )
         ELSE IF ( ICMP == 2 ) THEN
           CALL MAKE_SPLINE ( 3, N_C04, JD_C04, YP_C04, 0.0D0, 0.D0, CF_C04, &
     &                        WORK, IER1 )
           CALL MAKE_SPLINE ( 3, N_USN, JD_USN, YP_USN, 0.0D0, 0.D0, CF_USN, &
     &                        WORK, IER2 )
         ELSE IF ( ICMP == 3 ) THEN
           CALL MAKE_SPLINE ( 3, N_C04, JD_C04, U1_C04, 0.0D0, 0.D0, CF_C04, &
     &                        WORK, IER1 )
           CALL MAKE_SPLINE ( 3, N_USN, JD_USN, U1_USN, 0.0D0, 0.D0, CF_USN, &
     &                        WORK, IER2 )
      END IF
      IF ( IER1 .NE. 0  .OR.  IER2 .NE. 0  .OR.  IER3 .NE. 0 ) THEN
           CALL ERR_LOG ( 4811, IUER, 'PLOT_DIF', 'Error in an attempt '// &
     &         'to build coefficients of interpolating spline' )
           RETURN 
      END IF
!
      TIM_BEG = MAX ( ((MJD_BEG - J2000__MJD)*86400.0D0 - 43200.0D0 + TAI_BEG), &
     &                (JD_C04(1) - J2000__JD)*86400.0D0, &
     &                (JD_USN(1) - J2000__JD)*86400.0D0  )
!
      TIM_END = MIN ( ((MJD_END - J2000__MJD)*86400.0D0 - 43200.0D0 + TAI_END), &
     &                (JD_C04(N_C04) - J2000__JD)*86400.0D0, &
     &                (JD_USN(N_USN) - J2000__JD)*86400.0D0  )
      STEP = ( TIM_END - TIM_BEG )/(MP-1)
!
      DO 410 J1=1,MP
         JD_VAL = (TIM_BEG + STEP*(J1-1))/86400.0D0 + J2000__JD - 0.5D0
         T8(J1) = (JD_VAL - J2000__JD - 0.5D0)/365.25D0 + 2000.0D0
!
         IXC_C04 = IXMN8 ( N_C04, JD_C04, JD_VAL )
         IXC_USN = IXMN8 ( N_USN, JD_USN, JD_VAL )
         IF ( IDER == 0  .AND.  ICMP == 1 ) THEN
              C04_VAL = FSPL8  ( JD_VAL, N_C04, JD_C04, YP_C04, IXC_C04, CF_C04 )
              USN_VAL = FSPL8  ( JD_VAL, N_USN, JD_USN, YP_USN, IXC_USN, CF_USN )
              CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', &
     &                            'Differences IERS C04 versus USNO FINALS'// &
     &                            ' E1 (rad)' )
            ELSE IF ( IDER == 0  .AND.  ICMP == 2 ) THEN
              C04_VAL = FSPL8  ( JD_VAL, N_C04, JD_C04, XP_C04, IXC_C04, CF_C04 )
              USN_VAL = FSPL8  ( JD_VAL, N_USN, JD_USN, XP_USN, IXC_USN, CF_USN )
              CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', &
     &                            'Differences IERS C04 versus USNO FINALS'// &
     &                            ' E2 (rad)' )
            ELSE IF ( IDER == 0  .AND.  ICMP == 3 ) THEN
              C04_VAL = -FSPL8  ( JD_VAL, N_C04, JD_C04, U1_C04, IXC_C04, CF_C04 )*1.002737909D0
              USN_VAL = -FSPL8  ( JD_VAL, N_USN, JD_USN, U1_USN, IXC_USN, CF_USN )*1.002737909D0
              CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', &
     &                            'Differences IERS C04 versus USNO FINALS'// &
     &                            ' E3 (rad)' )
            ELSE IF ( IDER == 1  .AND.  ICMP == 1 ) THEN
              C04_VAL = DSPL8  ( JD_VAL, N_C04, JD_C04, YP_C04, IXC_C04, CF_C04 )/86400.0D0
              USN_VAL = DSPL8  ( JD_VAL, N_USN, JD_USN, YP_USN, IXC_USN, CF_USN )/86400.0D0
              CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', &
     &                            'Differences IERS C04 versus USNO FINALS'// &
     &                            ' E1_date (rad/s)' )
            ELSE IF ( IDER == 1  .AND.  ICMP == 2 ) THEN
              C04_VAL = DSPL8  ( JD_VAL, N_C04, JD_C04, XP_C04, IXC_C04, CF_C04 )/86400.0D0
              USN_VAL = DSPL8  ( JD_VAL, N_USN, JD_USN, XP_USN, IXC_USN, CF_USN )/86400.0D0
              CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', &
     &                            'Differences IERS C04 versus USNO FINALS'// &
     &                            ' E2_rate (rad/s)' )
            ELSE IF ( IDER == 1  .AND.  ICMP == 3 ) THEN
              C04_VAL = -DSPL8  ( JD_VAL, N_C04, JD_C04, U1_C04, IXC_C04, CF_C04 )*1.002737909D0/86400.0D0
              USN_VAL = -DSPL8  ( JD_VAL, N_USN, JD_USN, U1_USN, IXC_USN, CF_USN )*1.002737909D0/86400.0D0
              CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', &
     &                            'Differences IERS C04 versus USNO FINALS'// &
     &                            ' E3_rate (rad/s)' )
            ELSE IF ( IDER == 2  .AND.  ICMP == 1 ) THEN
              C04_VAL = D2SPL8 ( JD_VAL, N_C04, JD_C04, YP_C04, IXC_C04, CF_C04 )/86400.0D0**2
              USN_VAL = D2SPL8 ( JD_VAL, N_USN, JD_USN, YP_USN, IXC_USN, CF_USN )/86400.0D0**2
              CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', &
     &                            'Differences IERS C04 versus USNO FINALS'// &
     &                            ' E1_rate**2 (rad/s**2)' )
            ELSE IF ( IDER == 2  .AND.  ICMP == 2 ) THEN
              C04_VAL = D2SPL8 ( JD_VAL, N_C04, JD_C04, XP_C04, IXC_C04, CF_C04 )/86400.0D0**2
              USN_VAL = D2SPL8 ( JD_VAL, N_USN, JD_USN, XP_USN, IXC_USN, CF_USN )/86400.0D0**2
              CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', &
     &                            'Differences IERS C04 versus USNO FINALS'// &
     &                            ' E2_rate**2 (rad/s**2)' )
            ELSE IF ( IDER == 2  .AND.  ICMP == 3 ) THEN
              C04_VAL = -D2SPL8 ( JD_VAL, N_C04, JD_C04, U1_C04, IXC_C04, CF_C04 )*1.002737909D0/86400.0D0**2
              USN_VAL = -D2SPL8 ( JD_VAL, N_USN, JD_USN, U1_USN, IXC_USN, CF_USN )*1.002737909D0/86400.0D0**2
              CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', &
     &                            'Differences IERS C04 versus USNO FINALS'// &
     &                            ' E3_rate**2 (rad/s**2)' )
         END IF
!
         IF ( ICMP == 3  .AND.  FL_UZT  ) THEN
              CALL JD_TO_MJD_SEC ( JD_VAL, MJD_VAL, TAI_VAL )
              CALL E3ZT_DICKMAN1993  ( MJD_VAL, TAI_VAL, E3, E3_DOT, E3_DT2 )
         END IF
!
         IF ( FL_DUAL ) THEN
              X1(J1) = C04_VAL
              X2(J1) = USN_VAL
              IF ( ICMP == 3  .AND.  FL_UZT  .AND.  IDER == 0 ) THEN
                   X1(J1) = X1(J1) - E3
                   X2(J1) = X2(J1) - E3
                 ELSE IF ( ICMP == 3  .AND.  FL_UZT  .AND.  IDER == 1 ) THEN
                   X1(J1) = X1(J1) - E3_DOT
                   X2(J1) = X2(J1) - E3_DOT
                 ELSE IF ( ICMP == 3  .AND.  FL_UZT  .AND.  IDER == 2 ) THEN
                   X1(J1) = X1(J1) - E3_DT2
                   X2(J1) = X2(J1) - E3_DT2
              END IF
           ELSE IF ( .NOT. FL_DUAL ) THEN
              X1(J1) = C04_VAL - USN_VAL
         END IF
 410  CONTINUE 
!
      CALL DIAGI_SETDEF ( IER, 'DIAGI_ILST', 2 )
      CALL DIAGI_SETDEF ( IER, 'DIAGI_IPST', 1 )
!
      CALL ERR_PASS ( IUER, IER )
      IF ( FL_DUAL ) THEN
           CALL DIAGI_2 ( MP, T8, X1, MP, T8, X2, IER )
         ELSE 
           CALL DIAGI_1 ( MP, T8, X1, IER )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  
