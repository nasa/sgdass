      PROGRAM    DIF_EOP_EXT_MAIN
! ************************************************************************
! *                                                                      *
! *   Program DIF_EOP_EXT  computes the differnces between eop file from *
! *   Solve in EOB format and some external EOP time series.             *
! *                                                                      *
! * ### 06-JUN-2001    DIF_EOP_EXT   v4.1 (c) L. Petrov 19-MAR-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'getpar.i'
      INTEGER*4  MP, MSES, MHEAD
      PARAMETER  ( MP = 32*1024, MSES = 32*1024, MHEAD = 512 )
      CHARACTER  FILEXT*128, FILEOB*128, CH_FLAG(MP), BUF(MP)*256, SOLNAME*128
      CHARACTER  STR*128, FILNAM*128
      REAL*8     JD_EXT(MP), YR_EXT(MP), &
     &           XP_EXT(MP), YP_EXT(MP), U1_EXT(MP), DPSI_EXT(MP), DEPS_EXT(MP), &
     &           XP_ERR(MP), YP_ERR(MP), U1_ERR(MP), DPSI_ERR(MP), DEPS_ERR(MP)
      TYPE     ( EOP__STRU ) ::  EOP(MSES)
      REAL*8     REF_DATE, YR_BEG, YR_END
      PARAMETER  ( REF_DATE = 1997.000D0 )
      PARAMETER  ( YR_BEG   = 1976.000D0 )
      PARAMETER  ( YR_END   = 2070.000D0 )
      LOGICAL*4  FL_NOPLOT, FL_NOST, FL_1ST, FL_2ND, FL_BOTH
      INTEGER*4  NEXT, NSES, NHEAD, J1, IP, IL, IND_BEG, NP, NUMARG, IUER
      INTEGER*4, EXTERNAL :: LINDEX, ILEN, I_LEN
!
      CALL CLRCH ( FILEXT )
      CALL CLRCH ( FILEOB )
      NUMARG = IARGC ()
      FL_NOPLOT = .FALSE.
      FL_NOST   = .FALSE.
      FL_1ST    = .FALSE.
      FL_2ND    = .FALSE.
      FL_BOTH   = .FALSE.
      IF ( NUMARG .GE. 2 ) THEN
           CALL GETARG ( 1, FILEOB )
           CALL GETARG ( 2, FILEXT )
           IF ( NUMARG .GE. 3 ) THEN
                CALL GETARG ( 3, STR )
                IF ( STR(1:4) .EQ. '-nop' ) THEN
                     FL_NOPLOT = .TRUE.
                   ELSE IF ( STR(1:4) .EQ. '-nos' ) THEN
                     FL_NOST   = .TRUE.
                   ELSE IF ( STR(1:4) .EQ. '-1st' ) THEN
                     FL_1ST    = .TRUE.
                   ELSE IF ( STR(1:4) .EQ. '-2nd' ) THEN
                     FL_2ND    = .TRUE.
                   ELSE IF ( STR(1:5) .EQ. '-both' ) THEN
                     FL_BOTH    = .TRUE.
                   ELSE 
                     WRITE ( 6, * ) '3: Unsupported argument: '//STR(1:I_LEN(STR))
                     CALL EXIT ( 1 ) 
                END IF
           END IF
           IF ( NUMARG .GE. 4 ) THEN
                CALL GETARG ( 4, STR )
                IF ( STR(1:4) .EQ. '-nop' ) THEN
                     FL_NOPLOT = .TRUE.
                   ELSE IF ( STR(1:4) .EQ. '-nos' ) THEN
                     FL_NOST   = .TRUE.
                   ELSE IF ( STR(1:4) .EQ. '-1st' ) THEN
                     FL_1ST    = .TRUE.
                   ELSE IF ( STR(1:4) .EQ. '-2nd' ) THEN
                     FL_2ND    = .TRUE.
                   ELSE IF ( STR(1:5) .EQ. '-both' ) THEN
                     FL_BOTH    = .TRUE.
                   ELSE 
                     WRITE ( 6, * ) '4: Unsupported argument: '//STR(1:I_LEN(STR))
                     CALL EXIT ( 1 ) 
                END IF
           END IF
           IF ( NUMARG .GE. 5 ) THEN
                CALL GETARG ( 5, STR )
                IF ( STR(1:4) .EQ. '-nop' ) THEN
                     FL_NOPLOT = .TRUE.
                   ELSE IF ( STR(1:4) .EQ. '-nos' ) THEN
                     FL_NOST   = .TRUE.
                   ELSE IF ( STR(1:4) .EQ. '-1st' ) THEN
                     FL_1ST    = .TRUE.
                   ELSE IF ( STR(1:4) .EQ. '-2nd' ) THEN
                     FL_2ND    = .TRUE.
                   ELSE IF ( STR(1:5) .EQ. '-both' ) THEN
                     FL_2ND    = .TRUE.
                   ELSE 
                     WRITE ( 6, * ) '5: Unsupported argument: '//STR(1:I_LEN(STR))
                     CALL EXIT ( 1 ) 
                END IF
           END IF
        ELSE
           WRITE ( 6, '(A)' ) ' Usage: dif_eop_ext <eob-file> <EXT_EOP_file> '// &
     &                        '[-noplot] [-nostandard_units] [-1st] [-2nd] [-both]'
           CALL EXIT ( 1 )
      END IF
!
      CALL CLRCH ( SOLNAME )
      SOLNAME = FILEOB
      IL = LINDEX ( SOLNAME, '/' )
      IF ( IL .GT. 0 ) THEN
           CALL CLRCH ( SOLNAME(1:IL) )
           CALL CHASHL ( SOLNAME )
      END IF
      IP = INDEX ( SOLNAME, '.' )
      IF ( IP .GT. 0 ) CALL CLRCH  ( SOLNAME(IP:) )
!
!
      IF ( INDEX ( FILEXT, 'c04' ) > 0 ) THEN
           IUER = -1
           CALL RD_IERS_C04 ( FILEXT, MP, NEXT, JD_EXT, XP_EXT, XP_ERR, &
     &                        YP_EXT, YP_ERR, U1_EXT, U1_ERR, DPSI_EXT, &
     &                        DPSI_ERR, DEPS_EXT, DEPS_ERR, CH_FLAG, IUER )
           IF ( IUER .NE. 0 ) STOP 'Error in reading IERS EXT file'
!!!!!!!!!
!         ELSE IF ( INDEX ( FILEXT, 'igs' ) > 0 ) THEN
!           IUER = -1
!           CALL RD_IGS_EOP ( FILEXT, MP, NEXT, JD_EXT, XP_EXT, XP_ERR, &
!     &                       YP_EXT, YP_ERR, U1_EXT, U1_ERR, CH_FLAG, IUER )
!           IF ( IUER .NE. 0 ) STOP 'Error in reading IERS GPS file'
!!!!!!!!!
         ELSE IF ( INDEX ( FILEXT, 'gps' ) > 0 ) THEN
           IUER = -1
           CALL RD_GPS_EOP ( FILEXT, MP, NEXT, JD_EXT, XP_EXT, XP_ERR, &
     &                       YP_EXT, YP_ERR, U1_EXT, U1_ERR, CH_FLAG, IUER )
           IF ( IUER .NE. 0 ) STOP 'Error in reading IERS GPS file'
         ELSE  IF ( INDEX ( FILEXT, 'usno' ) > 0 ) THEN
           IUER = -1
           CALL RD_FINALS ( FILEXT, MP, NEXT, JD_EXT, XP_EXT, XP_ERR, &
     &                      YP_EXT, YP_ERR, U1_EXT, U1_ERR, DPSI_EXT, &
     &                      DPSI_ERR, DEPS_EXT, DEPS_ERR, CH_FLAG, IUER )
           IF ( IUER .NE. 0 ) STOP 'Error in reading IERS EXT file'
         ELSE  
           WRITE ( 6, * ) 'Unknown file type: '//FILEXT(1:I_LEN(FILEXT))
      END IF
!
      IND_BEG = 0
      NP = 0
      DO 410 J1=1,NEXT
         YR_EXT(J1) = 2000.0 + ( JD_EXT(J1) - 2451545.0D0)/365.25D0
         IF ( YR_EXT(J1) .GE. YR_BEG  .AND.  IND_BEG == 0 ) IND_BEG = J1
         IF ( YR_EXT(J1) .GE. YR_BEG  .AND.  YR_EXT(J1) .LE. YR_END ) THEN
              NP = NP + 1
         END IF
 410  CONTINUE
!
      IUER = -1
      CALL READ_EOB ( FILEOB, MHEAD, NHEAD, BUF, MSES, NSES, EOP, IUER )
      IF ( IUER .NE. 0 ) STOP 'Error in reading EOB file'
!
      IP = LINDEX ( FILEXT, '/' ) + 1
      FILNAM =  FILEXT(IP:)
!
      IUER = -1
      CALL DIF_EOP_EXT ( NP, YR_EXT(IND_BEG), XP_EXT(IND_BEG), XP_ERR(IND_BEG), &
     &                       YP_EXT(IND_BEG), YP_ERR(IND_BEG), U1_EXT(IND_BEG), &
     &                       U1_ERR(IND_BEG), DPSI_EXT(IND_BEG), &
     &                       DPSI_ERR(IND_BEG), DEPS_EXT(IND_BEG), &
     &                       DEPS_ERR(IND_BEG), &
     &                       NSES, EOP, SOLNAME, FILNAM, FL_NOPLOT, FL_NOST, &
     &                       FL_1ST, FL_2ND, FL_BOTH, REF_DATE, IUER )
      IF ( IUER .NE. 0 ) STOP 'Error in computing differences'
!
      END  !#!  DIF_EOP_EXT_MAIN  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIF_EOP_EXT ( NEXT, YR_EXT, XP_EXT, XP_ERR, YP_EXT, YP_ERR, &
     &                         U1_EXT, U1_ERR, DPSI_EXT, DPSI_ERR, DEPS_EXT, &
     &                         DEPS_ERR, NP, EOP, SOLNAME, FILNAME, &
     &                         FL_NOPLOT, FL_NOST, FL_1ST, FL_2ND, FL_BOTH, &
     &                         REF_DATE, IUER )
! ************************************************************************
! *                                                                      *
! *
! *                                                                      *
! *  ### 04-AUG-2000  DIF_EOP_EXT  v3.0 (c)  L. Petrov  04-JUN-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'getpar.i'
      INTEGER*4  NEXT, NP, IUER
      REAL*8     YR_EXT(NEXT), &
     &           XP_EXT(NEXT), YP_EXT(NEXT), U1_EXT(NEXT), DPSI_EXT(NEXT), &
     &           DEPS_EXT(NEXT), &
     &           XP_ERR(NEXT), YP_ERR(NEXT), U1_ERR(NEXT), DPSI_ERR(NEXT), &
     &           DEPS_ERR(NEXT)
      TYPE ( EOP__STRU ) ::  EOP(NP)
      REAL*8     REF_DATE
      CHARACTER  SESS_NAME(NP)*10, SOLNAME*128, FILNAME*128
      LOGICAL*4  FL_NOPLOT, FL_NOST, FL_1ST, FL_2ND, FL_BOTH
      INTEGER*4  MP
      PARAMETER  ( MP = M_SES )
      REAL*8     REA, K_FACTOR
      PARAMETER ( REA      = 6378136.3D0   )
      PARAMETER ( K_FACTOR = 1.002737909D0 )
      REAL*8     COEF(MP), WORK(MP), T8(MP), X8(MP), X1(MP), X2(MP), E1(MP), E2(MP), &
     &           E8(MP), W8(MP), &
     &           MEAN_T, DR_VAL, SH_VAL, DR_SIG, SH_SIG, AV, DW, CHI, CHI_NDG, &
     &           DATE_YEAR, PHI(3), OME(3)
      INTEGER*4  J1, J2, J3, NZ, IXC, KP, N$1, IER
      REAL*8     ERR_MAX_ST(8), ERR_MAX_NOST(8), ERR_VAL_MAX, ERR_RAT_MAX
!!      PARAMETER  ( ERR_VAL_MAX = 5.D-8  )
      PARAMETER  ( ERR_VAL_MAX = 2.D-9  )
      PARAMETER  ( ERR_RAT_MAX = 1.D-12 )
      CHARACTER  TITLE_ST(8)*26, TITLE_NOST(8)*26, TITLE(8)*26
      DATA       ( TITLE_ST(N$1), TITLE_NOST(N$1), ERR_MAX_ST(N$1), N$1=1,8 ) &
     &           /  &
     &           'X pole coord.      (rad)  ', 'X pole coord.    (mas)    ', ERR_VAL_MAX, &
     &           'Y pole coord.      (rad)  ', 'Y pole coord.    (mas)    ', ERR_VAL_MAX, &
     &           'UT1 angle          (rad)  ', 'UT1 angle        (musec)  ', ERR_VAL_MAX, &
     &           'X pole rate 1.D-14 (rad/s)', 'X pole rate      (mas/d)  ', ERR_RAT_MAX, &
     &           'Y pole rate 1.D-14 (rad/s)', 'Y pole rate      (mas/d)  ', ERR_RAT_MAX, &
     &           'UT1 rate    1.D-14 (rad/s)', 'UT1 rate         (musec/d)', ERR_RAT_MAX, &
     &           'Nutation DPSI      (rad)  ', 'Nutation DPSI    (mas)    ', ERR_VAL_MAX, &
     &           'Nutation DEPS      (rad)  ', 'Nutation DEPS    (mas)    ', ERR_VAL_MAX  &
     &           /
!
      REAL*8,    EXTERNAL :: FSPL8, DSPL8
      INTEGER*4, EXTERNAL :: IXMN8, I_LEN
!
      IF ( NEXT .GT. MP ) THEN
           CALL ERR_LOG ( 8281, IUER, 'DIF_EOP_EXT', 'Trap of internal '// &
     &         'control: NEXT > MP. Please update M_SES variable' )
           RETURN
      END IF
      ERR_MAX_NOST(1) = ERR_VAL_MAX/MAS__TO__RAD
      ERR_MAX_NOST(2) = ERR_VAL_MAX/MAS__TO__RAD   
      ERR_MAX_NOST(3) = ERR_VAL_MAX/(1.D-3*MSEC__TO__RAD)
      ERR_MAX_NOST(4) = ERR_RAT_MAX/MAS__TO__RAD*86400.0D0
      ERR_MAX_NOST(5) = ERR_RAT_MAX/MAS__TO__RAD*86400.0D0
      ERR_MAX_NOST(6) = ERR_RAT_MAX/(1.D-3*MSEC__TO__RAD)*86400.00
      ERR_MAX_NOST(7) = ERR_VAL_MAX/MAS__TO__RAD
      ERR_MAX_NOST(8) = ERR_VAL_MAX/MAS__TO__RAD
!
      WRITE ( 6, 110 ) '  Difference of our EOP series '// &
     &                 SOLNAME(1:I_LEN(SOLNAME))//' minus '// &
     &                 FILNAME(1:I_LEN(FILNAME))
 110  FORMAT ( A/ )
      DO 410 J1=1,8
         CALL ERR_PASS ( IUER, IER )
         IF ( J1 .EQ. 1 ) THEN
              CALL MAKE_SPLINE ( 1, NEXT, YR_EXT, XP_EXT, 0.0, 0.0, COEF, WORK, &
     &                           IER )
            ELSE IF ( J1 .EQ. 2 ) THEN
              CALL MAKE_SPLINE ( 1, NEXT, YR_EXT, YP_EXT, 0.0, 0.0, COEF, WORK, &
     &                           IER )
            ELSE IF ( J1 .EQ. 3 ) THEN
              CALL MAKE_SPLINE ( 1, NEXT, YR_EXT, U1_EXT, 0.0, 0.0, COEF, WORK, &
     &                           IER )
            ELSE IF ( J1 .EQ. 4 ) THEN
              CALL MAKE_SPLINE ( 1, NEXT, YR_EXT, XP_EXT, 0.0, 0.0, COEF, WORK, &
     &                           IER )
            ELSE IF ( J1 .EQ. 5 ) THEN
              CALL MAKE_SPLINE ( 1, NEXT, YR_EXT, YP_EXT, 0.0, 0.0, COEF, WORK, &
     &                           IER )
            ELSE IF ( J1 .EQ. 6 ) THEN
              CALL MAKE_SPLINE ( 1, NEXT, YR_EXT, U1_EXT, 0.0, 0.0, COEF, WORK, &
     &                           IER )
            ELSE IF ( J1 .EQ. 7 ) THEN
              CALL MAKE_SPLINE ( 1, NEXT, YR_EXT, DPSI_EXT, 0.0, 0.0, COEF, &
     &                           WORK, IER )
            ELSE IF ( J1 .EQ. 8 ) THEN
              CALL MAKE_SPLINE ( 1, NEXT, YR_EXT, DEPS_EXT, 0.0, 0.0, COEF, &
     &                           WORK, IER )
         END IF
         IF ( IER .NE. 0) THEN
              WRITE ( 6, * ) ' J1=',J1
              CALL ERR_LOG ( 8281, IUER, 'DIF_EOP_EXT', 'Error in '// &
     &            'computation spline for '//TITLE(J1) )
              RETURN
         END IF
         KP = 0
         DO 420 J2=1,NP
            IF ( J1 .EQ. 1 .OR. &
     &           J1 .EQ. 2 .OR. &
     &           J1 .EQ. 3 .OR. &
     &           J1 .EQ. 4 .OR. &
     &           J1 .EQ. 5 .OR. &
     &           J1 .EQ. 6      ) THEN
                 DATE_YEAR = 2000.0 + ( EOP(J2)%MJD_EOP - 51544.5D0 )/365.25D0
               ELSE IF ( J1 .EQ. 7  .OR.  J1 .EQ. 8 ) THEN
                 DATE_YEAR = 2000.0 + ( EOP(J2)%MJD_NUT - 51544.5D0 )/365.25D0
               ELSE
                 WRITE ( 6, * ) ' J1=',j1
                 STOP 'Bad j1'
            END IF
!
            IXC = IXMN8 ( NEXT, YR_EXT, DATE_YEAR )
            IF ( J1 .EQ. 1  .AND.  BTEST ( EOP(J2)%STATUS, XPL__GTP ) ) THEN
                 KP = KP + 1
                 IF ( FL_1ST ) THEN
                      X8(KP) = EOP(J2)%XPL_V
                    ELSE IF ( FL_2ND ) THEN
                      X8(KP) = FSPL8 ( DATE_YEAR, NEXT, YR_EXT, XP_EXT, IXC, COEF ) 
                    ELSE 
                      X8(KP) = EOP(J2)%XPL_V - FSPL8 ( DATE_YEAR, NEXT, &
     &                           YR_EXT, XP_EXT, IXC, COEF )
                 END IF
                 IF ( FL_BOTH ) THEN
                      X1(KP) = EOP(J2)%XPL_V
                      X2(KP) = FSPL8 ( DATE_YEAR, NEXT, YR_EXT, XP_EXT, IXC, COEF ) 
                      E2(KP) = 1.D-12
                 END IF 
                 E8(KP) = EOP(J2)%XPL_E
                 IF ( FL_NOST ) THEN
                      X8(KP) = X8(KP)/MAS__TO__RAD
                      X1(KP) = X1(KP)/MAS__TO__RAD
                      X2(KP) = X2(KP)/MAS__TO__RAD
                      E2(KP) = E2(KP)/MAS__TO__RAD
                      E8(KP) = E8(KP)/MAS__TO__RAD
                 END IF
               ELSE IF ( J1 .EQ. 2 .AND. BTEST( EOP(J2)%STATUS, YPL__GTP ) ) THEN
                 KP = KP + 1
                 IF ( FL_1ST ) THEN
                       X8(KP) = EOP(J2)%YPL_V
                    ELSE IF ( FL_2ND ) THEN
                       X8(KP) = FSPL8 ( DATE_YEAR, NEXT, YR_EXT, YP_EXT, IXC, COEF )
                    ELSE 
                       X8(KP) = EOP(J2)%YPL_V - FSPL8 ( DATE_YEAR, NEXT, &
     &                          YR_EXT, YP_EXT, IXC, COEF )
                 END IF
                 IF ( FL_BOTH ) THEN
                      X1(KP) = EOP(J2)%YPL_V
                      X2(KP) = FSPL8 ( DATE_YEAR, NEXT, YR_EXT, YP_EXT, IXC, COEF )
                      E2(KP) = 1.D-12
                 END IF
                 E8(KP) = EOP(J2)%YPL_E
                 IF ( FL_NOST ) THEN
                      X8(KP) = X8(KP)/MAS__TO__RAD
                      X1(KP) = X1(KP)/MAS__TO__RAD
                      X2(KP) = X2(KP)/MAS__TO__RAD
                      E2(KP) = E2(KP)/MAS__TO__RAD
                      E8(KP) = E8(KP)/MAS__TO__RAD
                 END IF
               ELSE IF ( J1 .EQ. 3 .AND. BTEST( EOP(J2)%STATUS, U1__GTP ) ) THEN
                 KP = KP + 1
                 IF ( FL_1ST ) THEN
                      X8(KP) = EOP(J2)%U1_V
                    ELSE IF ( FL_2ND ) THEN
                      X8(KP) = FSPL8 ( DATE_YEAR, NEXT, YR_EXT, U1_EXT, IXC, COEF )
                    ELSE 
                      X8(KP) = EOP(J2)%U1_V - FSPL8 ( DATE_YEAR, NEXT, &
     &                         YR_EXT, U1_EXT, IXC, COEF )
                 END IF
                 IF ( FL_BOTH ) THEN
                      X1(KP) = EOP(J2)%U1_V
                      X2(KP) = FSPL8 ( DATE_YEAR, NEXT, YR_EXT, U1_EXT, IXC, COEF )
                      E2(KP) = 1.D-12
                 END IF
                 E8(KP) = EOP(J2)%U1_E
                 IF ( FL_NOST ) THEN
                      X8(KP) = X8(KP)/(1.D-3*MSEC__TO__RAD)
                      X1(KP) = X1(KP)/MAS__TO__RAD
                      X2(KP) = X2(KP)/MAS__TO__RAD
                      E2(KP) = E2(KP)/MAS__TO__RAD
                      E8(KP) = E8(KP)/(1.D-3*MSEC__TO__RAD)
                 END IF
               ELSE IF ( J1 .EQ. 4  .AND.  BTEST ( EOP(J2)%STATUS, XPR__GTP ) ) THEN
                 KP = KP + 1
                 IF ( FL_1ST ) THEN
                      X8(KP) = EOP(J2)%XPR_V
                    ELSE IF ( FL_2ND ) THEN
                      X8(KP) = DSPL8 ( DATE_YEAR, NEXT, YR_EXT, XP_EXT, IXC, COEF )/ &
     &                                (86400.0D0*365.25D0)
                    ELSE
                      X8(KP) = EOP(J2)%XPR_V - DSPL8 ( DATE_YEAR, NEXT, &
     &                         YR_EXT, XP_EXT, IXC, COEF )/(86400.0D0*365.25D0)
                 END IF
                 IF ( FL_BOTH ) THEN
                      X1(KP) = EOP(J2)%XPR_V
                      X2(KP) = DSPL8 ( DATE_YEAR, NEXT, YR_EXT, XP_EXT, IXC, COEF )/ &
     &                                (86400.0D0*365.25D0)
                      E2(KP) = 1.D-16
                 END IF
                 E8(KP) = EOP(J2)%XPR_E
                 IF ( FL_NOST ) THEN
                      X8(KP) = X8(KP)/MAS__TO__RAD*86400.0D0
                      X1(KP) = X1(KP)/MAS__TO__RAD*86400.0D0
                      X2(KP) = X2(KP)/MAS__TO__RAD*86400.0D0
                      E2(KP) = E2(KP)/MAS__TO__RAD*86400.0D0
                      E8(KP) = E8(KP)/MAS__TO__RAD*86400.0D0
                 END IF
               ELSE IF ( J1 .EQ. 5  .AND.  BTEST ( EOP(J2)%STATUS, YPR__GTP ) ) THEN
                 KP = KP + 1
                 IF ( FL_1ST ) THEN
                      X8(KP) = EOP(J2)%YPR_V
                    ELSE IF ( FL_2ND ) THEN
                      X8(KP) = DSPL8 ( DATE_YEAR, NEXT, YR_EXT, YP_EXT, IXC, COEF )/ &
     &                               (86400.0D0*365.25D0)
                    ELSE
                      X8(KP) = EOP(J2)%YPR_V - DSPL8 ( DATE_YEAR, NEXT, &
     &                                 YR_EXT, YP_EXT, IXC, COEF )/(86400.0D0*365.25D0)
                 END IF
                 IF ( FL_BOTH ) THEN
                      X1(KP) = EOP(J2)%YPR_V
                      X2(KP) = DSPL8 ( DATE_YEAR, NEXT, YR_EXT, YP_EXT, IXC, COEF )/ &
     &                               (86400.0D0*365.25D0)
                      E2(KP) = 1.D-16
                 END IF
                 E8(KP) = EOP(J2)%YPR_E
                 IF ( FL_NOST ) THEN
                      X8(KP) = X8(KP)/MAS__TO__RAD*86400.0D0
                      X1(KP) = X1(KP)/MAS__TO__RAD*86400.0D0
                      X2(KP) = X2(KP)/MAS__TO__RAD*86400.0D0
                      E2(KP) = E2(KP)/MAS__TO__RAD*86400.0D0
                      E8(KP) = E8(KP)/MAS__TO__RAD*86400.0D0
                 END IF
               ELSE IF ( J1 .EQ. 6 .AND. BTEST( EOP(J2)%STATUS, UTR__GTP ) ) THEN
                 KP = KP + 1
                 IF ( FL_1ST ) THEN
                      X8(KP) = EOP(J2)%UTR_V
                    ELSE IF ( FL_2ND ) THEN
                      X8(KP) = DSPL8 ( DATE_YEAR, NEXT, YR_EXT, U1_EXT, IXC, COEF )/ &
     &                               (86400.0D0*365.25D0)
                    ELSE
                      X8(KP) = EOP(J2)%UTR_V - DSPL8 ( DATE_YEAR, NEXT, &
     &                         YR_EXT, U1_EXT, IXC, COEF )/(86400.0D0*365.25D0)
                 END IF
                 IF ( FL_BOTH ) THEN
                      X1(KP) = EOP(J2)%UTR_V
                      X2(KP) = DSPL8 ( DATE_YEAR, NEXT, YR_EXT, U1_EXT, IXC, COEF )/ &
     &                               (86400.0D0*365.25D0)
                      E2(KP) = 1.D-16
                 END IF
                 E8(KP) = EOP(J2)%UTR_E
                 IF ( FL_NOST ) THEN
                      X8(KP) = X8(KP)/(1.D-3*MSEC__TO__RAD)*86400.0D0
                      X1(KP) = X1(KP)/(1.D-3*MSEC__TO__RAD)*86400.0D0
                      X2(KP) = X2(KP)/(1.D-3*MSEC__TO__RAD)*86400.0D0
                      E2(KP) = E2(KP)/(1.D-3*MSEC__TO__RAD)*86400.0D0
                      E8(KP) = E8(KP)/(1.D-3*MSEC__TO__RAD)*86400.0D0
                 END IF
               ELSE IF ( J1 .EQ. 7 .AND. BTEST( EOP(J2)%STATUS, DPSI__GTP )) THEN
                 KP = KP + 1
                 IF ( FL_1ST ) THEN
                      X8(KP) = EOP(J2)%DPSI_V
                    ELSE IF ( FL_2ND ) THEN
                      X8(KP) = FSPL8 ( DATE_YEAR, NEXT, YR_EXT, DPSI_EXT, IXC, COEF )
                    ELSE
                      X8(KP) = EOP(J2)%DPSI_V - FSPL8 ( DATE_YEAR, NEXT, &
     &                           YR_EXT, DPSI_EXT, IXC, COEF )
                 END IF
                 IF ( FL_BOTH ) THEN
                      X1(KP) = EOP(J2)%DPSI_V
                      X2(KP) = FSPL8 ( DATE_YEAR, NEXT, YR_EXT, DPSI_EXT, IXC, COEF )
                      E2(KP) = 1.D-12
                 END IF
                 E8(KP) = EOP(J2)%DPSI_E
                 IF ( FL_NOST ) THEN
                      X8(KP) = X8(KP)/MAS__TO__RAD
                      X1(KP) = X1(KP)/MAS__TO__RAD
                      X2(KP) = X2(KP)/MAS__TO__RAD
                      E2(KP) = E2(KP)/MAS__TO__RAD
                      E8(KP) = E8(KP)/MAS__TO__RAD
                 END IF
!!   write ( 6, * ) 'DEE-453 j2= ', int2(j2), ' dpsi = ', EOP(J2)%DPSI_V ! %%%
               ELSE IF ( J1 .EQ. 8 .AND. BTEST( EOP(J2)%STATUS, DPSI__GTP )) THEN
                 KP = KP + 1
                 IF ( FL_1ST ) THEN
                      X8(KP) = EOP(J2)%DEPS_V
                    ELSE IF ( FL_2ND ) THEN
                      X8(KP) = FSPL8 ( DATE_YEAR, NEXT, YR_EXT, DEPS_EXT, IXC, COEF )
                    ELSE
                      X8(KP) = EOP(J2)%DEPS_V - FSPL8 ( DATE_YEAR, NEXT, &
     &                                 YR_EXT, DEPS_EXT, IXC, COEF )
                 END IF
                 IF ( FL_BOTH ) THEN
                      X1(KP) = EOP(J2)%DEPS_V
                      X2(KP) = FSPL8 ( DATE_YEAR, NEXT, YR_EXT, DEPS_EXT, IXC, COEF )
                      E2(KP) = 1.D-12
                 END IF
                 E8(KP) = EOP(J2)%DEPS_E
                 IF ( FL_NOST ) THEN
                      X8(KP) = X8(KP)/MAS__TO__RAD
                      E8(KP) = E8(KP)/MAS__TO__RAD
                      X1(KP) = X1(KP)/MAS__TO__RAD
                      X2(KP) = X2(KP)/MAS__TO__RAD
                      E2(KP) = E2(KP)/MAS__TO__RAD
                 END IF
            END IF
!
            IF ( KP .GT. 0 ) THEN
                 T8(KP) = DATE_YEAR
                 IF ( E8(KP) > 0.0D0 ) THEN
                      W8(KP) = 1.D0/E8(KP)
                    ELSE
                      W8(KP) = 1.0D0
                 END IF
                 IF ( FL_NOST ) THEN
                      IF ( E8(KP) .GT. ERR_MAX_NOST(J1) ) THEN
                           KP = KP - 1
                      END IF
                    ELSE 
                      IF ( E8(KP) .GT. ERR_MAX_ST(J1) ) THEN
                           KP = KP - 1
                      END IF
                 END IF
            END IF
 420     CONTINUE
!
         IF ( KP .GT. 2 ) THEN
              IER = -1
              CALL RGRW8  ( KP, T8, X8, W8, %VAL(0), MEAN_T, DR_VAL, SH_VAL, &
     &                      DR_SIG, SH_SIG, IER )
!!              IF ( IER .NE. 0 ) GOTO 410
              IER = -1
              CALL DISP_WTR8 ( KP, T8, X8, W8, DR_VAL, &
     &                         SH_VAL + (T8(1)-MEAN_T)*DR_VAL, %VAL(0), DW, NZ, IER )
              CHI = 0.0
              DO 430 J3=1,KP
                 CHI = CHI + &
     &                 (( X8(J3) - (SH_VAL + (T8(J3)-MEAN_T)*DR_VAL))*W8(J3))**2
 430          CONTINUE
              CHI_NDG = CHI/(KP-2)
              IF ( FL_NOST ) THEN
                   TITLE(J1) = TITLE_NOST(J1)
                 ELSE 
                   TITLE(J1) = TITLE_ST(J1)
                   IF ( J1 == 1 .OR. &
     &                  J1 == 2 .OR. & 
     &                  J1 == 3 .OR. & 
     &                  J1 == 7 .OR. & 
     &                  J1 == 8      ) THEN
!
                        SH_VAL = SH_VAL*1.D9
                        SH_SIG = SH_SIG*1.D9 
                        DR_VAL = DR_VAL*1.D9
                        DR_SIG = DR_SIG*1.D9
                        DW     = DW*1.D9
                      ELSE IF ( J1 == 4 .OR. &
     &                          J1 == 5 .OR. &
     &                          J1 == 6      ) THEN
                        SH_VAL = SH_VAL*1.D14
                        SH_SIG = SH_SIG*1.D14
                        DR_VAL = DR_VAL*1.D14
                        DR_SIG = DR_SIG*1.D14
                        DW     = DW*1.D14
                   END IF
              END IF
!
              WRITE ( 6, 120 ) 'Diff. '//TITLE(J1), SH_VAL, SH_SIG, &
     &                             DR_VAL, DR_SIG, TITLE(J1), DW, CHI_NDG
 120          FORMAT ( A,'  shift: ',F8.2,' -+ ',F5.3,'  rate: ', &
     &                 F8.3,' -+ ',F5.3/6X,A, '  w.r.m.s. = ', F9.3, &
     &                           '      Chi/ndg=',F6.2 )
!
              IF ( FL_NOPLOT ) THEN
                   IF ( J1 .EQ. 1 ) THEN
                       PHI(1) = - ( SH_VAL + DR_VAL*(REF_DATE - MEAN_T) )
                       OME(1) = - DR_VAL
                       IF ( FL_NOST ) THEN
                            PHI(1) = PHI(1)*MAS__TO__RAD
                            OME(1) = OME(1)*MAS__TO__RAD
                       END IF
                     ELSE IF ( J1 .EQ. 2 ) THEN
                       PHI(2) = ( SH_VAL + DR_VAL*(REF_DATE - MEAN_T) )* &
     &                            MAS__TO__RAD
                       OME(2) = - DR_VAL*MAS__TO__RAD
                       IF ( FL_NOST ) THEN
                            PHI(2) = PHI(2)*MAS__TO__RAD
                            OME(2) = OME(2)*MAS__TO__RAD
                       END IF
                     ELSE IF ( J1 .EQ. 3 ) THEN
                       PHI(3) = ( SH_VAL + DR_VAL*(REF_DATE - MEAN_T) )/K_FACTOR
                       OME(3) = - DR_VAL/K_FACTOR
                       IF ( FL_NOST ) THEN
                            PHI(3) = PHI(1)*(1.D3*MSEC__TO__RAD)
                            OME(3) = OME(1)*(1.D3*MSEC__TO__RAD)
                       END IF
                   END IF
                ELSE IF ( .NOT. FL_NOPLOT .AND. .NOT. FL_BOTH ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'Differences '// &
     &                  SOLNAME(1:I_LEN(SOLNAME))//' minus EXT in '//TITLE(J1) )
                   CALL DIAGI_SETDEF ( IER, 'DIAGI_IBST', 4         )
                   CALL DIAGI_SETDEF ( IER, 'DIAGI_ILST', 1         )
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL DIAGI_1E ( KP, T8, X8, E8, IER )
                ELSE IF ( .NOT. FL_NOPLOT .AND. FL_BOTH ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', SOLNAME(1:I_LEN(SOLNAME))// &
     &                 ' and EXT in '//TITLE(J1) )
                   CALL DIAGI_SETDEF ( IER, 'DIAGI_IBST', 4         )
                   CALL DIAGI_SETDEF ( IER, 'DIAGI_ILST', 1         )
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL DIAGI_2E ( KP, T8, X1, E8, KP, T8, X2, E2, IER )
              END IF
            ELSE
              WRITE ( 6, 130 ) TITLE(J1), KP
 130          FORMAT ( A,' -- to few points: ',I1 )
         END IF
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DIF_EOP_EXT  #!#
