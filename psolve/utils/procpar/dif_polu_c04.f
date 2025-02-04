      PROGRAM    DIF_POLU_C04
! ************************************************************************
! *                                                                      *
! *   Program for comparison ERP and C04.                                *
! *                                                                      *
! * ###  11-APR-2006  DIF_POLU_C04  v2.0 (c)  L. Petrov  12-OCT-2019 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'getpar.i'
      INCLUDE   'vtd.i'
      TYPE ( VTD__TYPE ) :: VTD
      INTEGER*4  MP, MB_LEAP, MHEAD
      PARAMETER  ( MP = 32768, MB_LEAP = 512, MHEAD = 256 )
      TYPE ( EOP__STRU ) ::  EOP(MP)
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
      CHARACTER  CH_FLAG(MP), BUF_LEAP(MB_LEAP)*256, HEAD_BUF(MHEAD)*80
      CHARACTER  FINAM_EOP*128, C04_FILE*128, C04_TYPE*4
      REAL*8     TIM_BEG, TIM_END
      INTEGER*4  N_ERP, N_C04, NUMARG, IP1, IP2, J1, J2, J3, J4, &
     &           NHEAD, MJD, IER, IUER
      REAL*8,    EXTERNAL :: FSPL8 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IXMN8 
!
      C04_TYPE = 'usno'
      C04_TYPE = 'iers'
      NUMARG = IARGC ()
      IF ( NUMARG .GE. 2 ) THEN
           CALL CLRCH  (    FINAM_EOP )
           CALL CLRCH  (    C04_FILE  )
           CALL GETARG ( 1, FINAM_EOP )
           CALL GETARG ( 2, C04_FILE  )
         ELSE
           WRITE ( 6, * ) ' Usage: dif_polu_c04.e  <eop_polu_file> <c04>'
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL VTD_INIT ( VTD, IUER )
      IF ( IUER .NE. 0 ) STOP 'Error in initializing VTD'
!
      VTD%CONF%EOP_TIME_SCALE = VTD__TAI 
      VTD%CONF%UZT_USE        = VTD__NONE
      IUER = -1
      CALL VTD_UEOP_INIT ( VTD, FINAM_EOP, IUER )
      IF ( IUER .NE. 0 ) STOP 'Error in reading input erp file'
!
      IUER = -1
      CALL RD_IERS_C04 ( C04_FILE, MP, N_C04, JD_C04, XP_C04, XP_ERR, YP_C04, &
     &                   YP_ERR, U1_C04, U1_ERR, DPSI_C04, DPSI_ERR, &
     &                   DEPS_C04, DEPS_ERR, CH_FLAG, IUER )
!
!      IUER = -1
!      CALL READ_EOB ( C04_FILE, MHEAD, NHEAD, HEAD_BUF, MP, N_C04, EOP, IUER )
!      IF ( IUER .NE. 0 ) STOP 'Error in reading IERS file'
!
      N_ERP = VTD%UEOP%NP
      DO 410 J1=1,N_ERP
         TIM1(J1) = VTD%UEOP%TIM(J1)
         EOP1_VAL(J1,1) = VTD%UEOP%VAL(J1,UEOP__XPL)
         EOP1_ERR(J1,1) = 1.D-10
         EOP1_VAL(J1,2) = VTD%UEOP%VAL(J1,UEOP__YPL)
         EOP1_ERR(J1,2) = 1.D-10
         EOP1_VAL(J1,3) = VTD%UEOP%VAL(J1,UEOP__UT1_M_TAI)*SEC__TO__RAD 
         EOP1_ERR(J1,3) = 1.D-10
 410  CONTINUE 
!
      DO 420 J2=1,N_C04
         IF ( C04_TYPE == 'iers' ) THEN
              TIM2(J2) = (JD_C04(J2) - J2000__JD)*86400.0D0 
              EOP2_VAL(J2,1) = XP_C04(J2)
              EOP2_ERR(J2,1) = XP_ERR(J2)
              EOP2_VAL(J2,2) = YP_C04(J2)
              EOP2_ERR(J2,2) = YP_ERR(J2)
              EOP2_VAL(J2,3) = U1_C04(J2)
              EOP2_ERR(J2,3) = U1_ERR(J2)
            ELSE IF ( C04_TYPE == 'usno' ) THEN
              TIM2(J2) = (EOP(J2)%MJD_EOP - J2000__MJD)*86400.0D0 
              EOP2_VAL(J2,1) = EOP(J2)%XPL_V
              EOP2_VAL(J2,2) = EOP(J2)%YPL_V
              EOP2_VAL(J2,3) = EOP(J2)%U1_V
              EOP2_ERR(J2,1) = EOP(J2)%XPL_E
              EOP2_ERR(J2,2) = EOP(J2)%YPL_E
              EOP2_ERR(J2,3) = EOP(J2)%U1_E
         END IF
 420  CONTINUE 
!
!   call diagi_2 ( N_ERP, TIM1, EOP1_VAL(1,1), N_C04, TIM2, EOP2_VAL(1,1), iuer ) ! %%%%%%
!   call diagi_2 ( N_ERP, TIM1, EOP1_VAL(1,2), N_C04, TIM2, EOP2_VAL(1,2), iuer ) ! %%%%%%
   call diagi_2 ( N_ERP, TIM1, EOP1_VAL(1,3), N_C04, TIM2, EOP2_VAL(1,3), iuer ) ! %%%%%%
      DO 430 J3=1,3
         IER = -1
         CALL MAKE_SPLINE ( 3, N_ERP, TIM1, EOP1_VAL(1,J3), 0.0D0, 0.D0, &
     &                      COEF1, WORK, IER )
         CALL MAKE_SPLINE ( 3, N_C04, TIM2, EOP2_VAL(1,J3), 0.0D0, 0.D0, &
     &                      COEF2, WORK, IER )
!
         TIM_BEG = MAX ( TIM1(9), TIM2(1) )
         TIM_END = MIN ( TIM1(N_ERP-8), TIM2(N_C04) )
         WW = 0.0D0
         WRMS = 0.0D0
         DO 440 J4=1,MP
            T8(J4) = TIM_BEG + (J4-1)*(TIM_END-TIM_BEG)/(MP-1)
            IP1 = IXMN8 ( N_ERP, TIM1, T8(J4) )
            IP2 = IXMN8 ( N_C04, TIM2, T8(J4) )
            X8(J4) = FSPL8 ( T8(J4), N_ERP, TIM1, EOP1_VAL(1,J3), IP1, COEF1 ) - &
     &               FSPL8 ( T8(J4), N_C04, TIM2, EOP2_VAL(1,J3), IP2, COEF2 )
            X1(J4) = FSPL8 ( T8(J4), N_ERP, TIM1, EOP1_VAL(1,J3), IP1, COEF1 )
            X2(J4) = FSPL8 ( T8(J4), N_C04, TIM2, EOP2_VAL(1,J3), IP2, COEF2 )
            T8(J4) = (T8(J4) - 43200.0D0)/(86400.0D0*365.25D0) + 2000.0D0
            IF ( T8(J4) > 1996.0  .AND.  T8(J4) < 2006.0 ) THEN
                 WW = WW + 1.0D0
                 WRMS = WRMS + X8(J4)**2
            END IF
 440     CONTINUE 
         CALL DIAGI_1 ( MP, T8, X8, -3 )
!@         CALL DIAGI_2 ( MP, T8, X1, MP, T8, X2, -3 )
         WRITE ( 6, * ) ' WRMS = ', DSQRT ( WRMS/WW )
 430  CONTINUE 
!
      END  PROGRAM  DIF_POLU_C04  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE RD_ERP ( FINAM, MP, NP, JD, XR_VAL, YR_VAL, UR_VAL, &
     &                    XR_ERR, YR_ERR, UR_ERR, JD_LAST, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  RD_ERP reads EOP file in erp format and extracts EOP      *
! *   series from there: X pole coordinates, Y ppole coordinates,        *
! *   UT1-TAI as well as their formal uncertainties.                     *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    FINAM ( CHARACTER ) -- EOP file name                              *
! *       MP ( INTEGER*4 ) -- Maximim number of points allowed to be in  *
! *                           the EOP file                               *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *       NP ( INTEGER*4 ) -- Actual number of points read from the EOP  *
! *                           file which are in the dates range.         *
! *       JD ( REAL*8    ) -- Array of Julian dates for EOP. Units: days,*
! *                           dimension: NP.                             *
! *   XP_VAL ( REAL*8    ) -- Array of X pole coordinates. Units: arcsec,*
! *                           dimension: NP.                             *
! *   XP_ERR ( REAL*8    ) -- Array of formal uncertainties of X pole    *
! *                           coordinates. Units: arcsec, dimension: NP. *
! *   YP_VAL ( REAL*8    ) -- Array of Y pole coordinates. Units: arcsec,*
! *                           dimension: NP.                             *
! *   YP_ERR ( REAL*8    ) -- Array of formal uncertainties of Y pole    *
! *                           coordinates. Units: arcsec, dimension: NP. *
! *   U1_VAL ( REAL*8    ) -- Array of UT1-TAI angles. Units: microsec   *
! *                           of time, dimension: NP.                    *
! *   U1_ERR ( REAL*8    ) -- Array of formal uncertainties of UT1-TAI   *
! *                           angles. Units: microsec of time,           *
! *                           dimension: NP.                             *
! *  JD_LAST ( REAL*8    ) -- The Julian last date for which the file    *
! *                           has been computed.                         *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 03-NOV-2000     RD_ERP   v1.2 (c)  L. Petrov  29-JUN-2004  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MP, NP, IUER
      REAL*8     JD(MP), XR_VAL(MP), XR_ERR(MP), YR_VAL(MP), YR_ERR(MP), &
     &           UR_VAL(MP), UR_ERR(MP), JD_LAST
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 16384 )
      CHARACTER  FINAM*(*), BUF(MBUF)*80, STR1*80, STR2*80
      LOGICAL*4  FL_LAST_SES
      INTEGER*4  NBUF, J1, IO, IUR, IER
      INTEGER*4  I_LEN
!
! --- Read the file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FINAM, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8221, IUER, 'RD_ERP', 'Error in reading '// &
     &         'file '//FINAM )
           RETURN
      END IF
!
      NP = 0
      FL_LAST_SES = .FALSE.
      DO 410 J1=2,NBUF
         IF ( BUF(J1)(1:15) .EQ. '# Last session:' ) THEN
              FL_LAST_SES = .TRUE.
              READ ( UNIT=BUF(J1)(36:46), FMT='(F11.5)' ) JD_LAST
         END IF
!
         IF ( BUF(J1)(1:1) .NE. '#' ) THEN
              NP = NP + 1
              READ ( UNIT=BUF(J1)(1:9), FMT='(F9.5)', IOSTAT=IO ) JD(NP)
              IF ( IO .NE. 0 ) GOTO 710
!
              READ ( UNIT=BUF(J1)(11:17), FMT='(F7.4)', IOSTAT=IO ) XR_VAL(NP)
              IF ( IO .NE. 0 ) GOTO 710
              XR_VAL(NP) = XR_VAL(NP)*0.1
!
              READ ( UNIT=BUF(J1)(19:25), FMT='(F7.4)', IOSTAT=IO ) YR_VAL(NP)
              IF ( IO .NE. 0 ) GOTO 710
              YR_VAL(NP) = YR_VAL(NP)*0.1
!
              READ ( UNIT=BUF(J1)(27:35), FMT='(I9)', IOSTAT=IO ) IUR
              UR_VAL(NP) = IUR*0.000001
!
              READ ( UNIT=BUF(J1)(38:42), FMT='(F5.3)', IOSTAT=IO ) XR_ERR(NP)
              XR_ERR(NP) = XR_ERR(NP)*0.1
              IF ( IO .NE. 0 ) GOTO 710
!
              READ ( UNIT=BUF(J1)(45:49), FMT='(F5.3)', IOSTAT=IO ) YR_ERR(NP)
              YR_ERR(NP) = YR_ERR(NP)*0.1
              IF ( IO .NE. 0 ) GOTO 710
!
              READ ( UNIT=BUF(J1)(51:57), FMT='(F7.5)', IOSTAT=IO ) UR_ERR(NP)
              UR_ERR(NP) = UR_ERR(NP)*1.D-6
              IF ( IO .NE. 0 ) GOTO 710
              UR_ERR(NP) = DABS ( UR_ERR(NP) )
         END IF
 410  CONTINUE
 710  CONTINUE
!
      IF ( .NOT. FL_LAST_SES  ) THEN
           JD_LAST = JD(NP)
      END IF
!
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR1 )
           CALL INCH  ( IO, STR1 )
           CALL CLRCH ( STR2 )
           CALL INCH  ( J1, STR2 )
           CALL ERR_LOG ( 8222, IUER, 'RD_ERP', 'Error '// &
     &          STR1(1:I_LEN(STR1))//' at the '//STR2(1:I_LEN(STR2))// &
     &          '-th line '//BUF(J1)(1:I_LEN(BUF(J1)))//' of the USNO '// &
     &          'finals eop file '//FINAM )
           RETURN
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  RD_ERP #!#
