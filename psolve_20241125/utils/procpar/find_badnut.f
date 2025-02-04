      PROGRAM    FIND_BADEOP
! ************************************************************************
! *                                                                      *
! *   Program FIND_BADEOP
! *                                                                      *
! *  ### 16-FEB-2004  FIND_BADEOP  v1.0 (c)  L. Petrov  16-FEB-2009 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'getpar.i'
      INCLUDE   'precm.i'
      INTEGER*4  MBUF, MHEAD, MG_BUF
      PARAMETER  ( MBUF   = 128*1024 )
      PARAMETER  ( MHEAD  =     2048 )
      PARAMETER  ( MG_BUF =     MBUF )
      CHARACTER  FIL_EOB*128, FIL_CAT*128, FIL_C04*128, FIL_OUT*128, &
     &           BUF(MBUF)*384, HEAD_BUF(MBUF)*384, G_BUF(MBUF)*128
      REAL*8     JDP(MBUF), XP_VAL(MBUF), YP_VAL(MBUF), UT1_VAL(MBUF), &
     &           DPSI_VAL(MBUF), DEPS_VAL(MBUF), XP_SIG(MBUF), YP_SIG(MBUF), &
     &           UT1_SIG(MBUF), DPSI_SIG(MBUF), DEPS_SIG(MBUF), &
     &           COR_XY(MBUF), COR_XU(MBUF), COR_YU(MBUF)
      TYPE ( EOP__STRU ) ::  EOP(MBUF)
!
      REAL*8     JDI(MBUF), XI_VAL(MBUF), XI_ERR(MBUF), YI_VAL(MBUF), &
     &           YI_ERR(MBUF), UTI_VAL(MBUF), UTI_ERR(MBUF), DPSII_VAL(MBUF), &
     &           DPSII_ERR(MBUF), DEPSI_VAL(MBUF), DEPSI_ERR(MBUF)
      REAL*8     COEF_EPL(MBUF), COEF_PPL(MBUF), WORK(MBUF)
      LOGICAL*4  FL_BYPASS_REP  
!
      CHARACTER  SESS_NAME(MBUF)*10, CH_FLAG(MBUF)*1, OUT(MBUF)*80, GVF_NAME*10
      REAL*8     EPS_SIG, EPS_COR, EPS_VAL
      PARAMETER  ( EPS_SIG = 7.D-9 )  ! 1.4 mas
      PARAMETER  ( EPS_COR = 0.96  )
      PARAMETER  ( EPS_VAL = 7.D-9 )  ! 1.4 mas
      REAL*8     T8(MBUF), X8(MBUF), E8(MBUF), X9(MBUF)
      INTEGER*4  NEOP, NC04, NCAT, NOUT, NHEAD, NUMARG, IV(MBUF), &
     &           NP, LG_BUF, J1, J2, J3, J4, IUER
      INTEGER*4  NSPL, IXC
      REAL*8,    EXTERNAL :: FSPL8
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IXMN8
!
      CALL CLRCH ( FIL_EOB )
      CALL CLRCH ( FIL_CAT )
      CALL CLRCH ( FIL_OUT )
      CALL CLRCH ( FIL_C04 )
      NUMARG = IARGC()
!
      FL_BYPASS_REP = .TRUE. ! bypass sessions already marked as bad
      FIL_C04 = '/vlbi/solve/save_files/eopc04.62-now'
!
      IF ( NUMARG .GE. 2 ) THEN
           CALL GETARG ( 1, FIL_EOB )
           CALL GETARG ( 2, FIL_CAT )
        ELSE
           WRITE ( 6, 110 )
 110       FORMAT ( 1X,'Usage: find_badnut <eob-file> <cat-file>' )
           CALL EXIT ( 1 )
      END IF
!
      CALL GETENVAR ( 'SAVE_DIR', PRE_SAV_DIR )
      PRE_SV_LEN = I_LEN(PRE_SAV_DIR)
!
      FIL_OUT = '/tmp/badnut.out'
!
      IUER = -1
      CALL READ_EOB ( FIL_EOB, MHEAD, NHEAD, HEAD_BUF, MBUF, NEOP, EOP, IUER )
!@      CALL READ_EOB ( FIL_EOB, MBUF, BUF, NEOP, JDP, XP_VAL, YP_VAL, UT1_VAL, &
!@     &                DPSI_VAL, DEPS_VAL, XP_SIG, YP_SIG, UT1_SIG, DPSI_SIG, &
!@     &                DEPS_SIG, COR_XY, COR_XU, COR_YU, SESS_NAME, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      IUER = -1
      CALL  RD_IERS_C04 ( FIL_C04, MBUF, NC04, JDI, XI_VAL, XI_ERR, YI_VAL, &
     &                    YI_ERR, UTI_VAL, UTI_ERR, DPSII_VAL, DPSII_ERR, &
     &                    DEPSI_VAL, DEPSI_ERR, CH_FLAG, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      IUER = -1
      CALL RD_TEXT ( FIL_CAT, MBUF, BUF, NCAT, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      NSPL = NC04
!
      IUER = -1
      CALL MAKE_SPLINE ( 1, NSPL, JDI, DPSI_VAL,  0.0, 0.0, COEF_PPL, WORK, IUER )
      IUER = -1
      CALL MAKE_SPLINE ( 1, NSPL, JDI, DEPSI_VAL, 0.0, 0.0, COEF_EPL, WORK, IUER )
!
      NP = 0
      DO 410 J1=1,NEOP
         IF ( FL_BYPASS_REP  .AND. &
     &        ( EOP(J1)%DPSI_E == 0.0D0  .AND. EOP(J1)%DEPS_E == 0.0D0 ) ) THEN
              GOTO 410
         END IF
         IV(J1) = 0
         JDP(J1) = EOP(J1)%MJD_EOP - J2000__MJD  - 0.5D0 + J2000__JD   
         IF ( IV(J1) .EQ. 0  .AND.  EOP(J1)%DPSI_E*0.4D0  .GT. EPS_SIG ) THEN
              IV(J1) = 1
         END IF
         IF ( IV(J1) .EQ. 0  .AND.  EOP(J1)%DEPS_E  .GT. EPS_SIG ) THEN
              IV(J1) = 2
         END IF
         IF ( IV(J1) .EQ. 0  .AND.  DABS(EOP(J1)%C_PE) .GT. EPS_COR ) THEN
              IV(J1) = 3
         END IF
!
         IXC = IXMN8 ( NSPL, JDI, JDP(J1) )
         DPSI_VAL(J1)  = EOP(J1)%DPSI_V - FSPL8 ( JDP(J1), NSPL, JDI, DPSII_VAL, IXC, &
     &                                            COEF_PPL )
         DEPS_VAL(J1)  = EOP(J1)%DEPS_V - FSPL8 ( JDP(J1), NSPL, JDI, DEPSI_VAL, IXC, &
     &                                            COEF_EPL )
         IF ( IV(J1) .EQ. 0  .AND.  DABS(DPSI_VAL(J1))*0.4 .GT. EPS_SIG ) THEN
              IV(J1) = 4
         END IF
         IF ( IV(J1) .EQ. 0  .AND.  DABS(DEPS_VAL(J1))     .GT. EPS_SIG ) THEN
              IV(J1) = 5
         END IF
         SESS_NAME(J1) = EOP(J1)%DBNAME
!
         IF ( IV(J1) .EQ. 0 ) THEN
              NP = NP + 1
              T8(NP) = (JDP(J1) - 2451545.0)/365.25 + 2000.0
!@              X8(NP) = XP_VAL(J1)
!@              E8(NP) = XP_SIG(J1)
!@              X8(NP) = UT1_VAL(J1)
!@              E8(NP) = UT1_SIG(J1)
         END IF
 410  CONTINUE
!@      IUER = -1
!@      CALL DIAGI_1E ( NP, T8, X8, E8, IUER )
!
      OPEN ( UNIT=16, FILE=FIL_OUT, STATUS='UNKNOWN' )
      NOUT = 0
      LG_BUF = 0
      DO 420 J2=1,NCAT
         IUER = -1
         CALL MARK3_TO_GVF_NAME ( MG_BUF, LG_BUF, G_BUF, BUF(J2)(1:10), &
     &                            GVF_NAME, IUER )
         IF ( ILEN(GVF_NAME) > 0 ) THEN
              BUF(J2)(1:10) = GVF_NAME
         END IF
 420  CONTINUE 
  write ( 6, * ) ' lg_buf = ', lg_buf, ' neop = ', neop, ' ncat = ', ncat! %%%%%%%
      NOUT = 0
      DO 430 J3=1,NEOP
         IF ( IV(J3) .NE. 0 ) THEN
              NOUT = NOUT + 1
              DO 440 J4=1,NCAT
                 IF ( SESS_NAME(J3) .EQ. BUF(J4)(1:10) ) THEN
                      WRITE ( 16, 120 ) NOUT, IV(J3), BUF(J4)(1:10)// &
     &                       BUF(J4)(45:I_LEN(BUF(J4)))
 120                  FORMAT ( I4, ' ', I2, ' ', A )
                      GOTO 840
                 END IF
 440          CONTINUE
              WRITE ( 16, 120 ) NOUT, IV(J3),  ' @@ '//SESS_NAME(J3)
 840          CONTINUE
         END IF
 430  CONTINUE
      CLOSE ( UNIT=16 )
!
      WRITE ( 6, * ) NOUT, ' bad eop sessions were put in file '// &
     &               FIL_OUT(1:I_LEN(FIL_OUT))
!
      END  !#!  FIND_BADEOP  #!#
