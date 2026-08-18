      SUBROUTINE A2JST_NOEOP ( SCSIG, EOPTRACE, KCONS, MAT, NPARM, &
     &                         EOP_INDICIES, LBUF_LEN, LBUF, IPTR, PAGEWID, &
     &                         N_APR, APR_VAL, APR_SIG, EST_VAL, IUER )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!     A2JST_NOEOP handles the parameters after the EOP parameters. It was
!     was hewn out of the pre-1997 A2JST.
!
! 1.2 REFERENCES:
!
! 2.  A2JST INTERFACE
!
! 2.1 Parameter File
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
!
! 2.2 INPUT Variables: None
!
      REAL*8     MAT(*)
      INTEGER*2  KBITN, KBITN4
      INTEGER*4  EOP_INDICIES(3,3), NUT_INDICIES(2), NPARM
      INTEGER*4  LBUF_LEN, IPTR, PAGEWID, N_APR
      CHARACTER  LBUF(LBUF_LEN)*120
!
! 2.3 OUTPUT Variables:
!
      INTEGER*4  IUER, IER
      REAL*8     SCSIG(*)
      REAL*8     EOPTRACE
      REAL*8     APR_VAL(N_APR), APR_SIG(N_APR), EST_VAL(N_APR)
      LOGICAL*2  KCONS
!
! EOPTRACE - sum of earth orientation constraint shares
! KCONS - True if constraints are applied
! SCSIG - Scaled sigmas
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'erm.i'
      INCLUDE 'vtd.i'
      INCLUDE 'socom.i'
      INCLUDE 'socom_plus.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'precm.i'
      INCLUDE 'glbcm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: adjst
!       CALLED SUBROUTINES: eopell, rotcor, namnut, eop_share, epoc,
!                           intrp_eomod, intrp_eovr,
!                           parms, index_parm, index_parm_trot,
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 ISTS,IWDS,nd1,nd2
      integer*2 istr_len/20/
      PARAMETER (ISTS = 1)
      PARAMETER (IWDS = 10)
      INTEGER*2 IPARMS(IWDS,M_GPA),IPARMN
      character*20 lparms(M_GPA)
      equivalence (lparms,iparms)
      CHARACTER*20 CPARM
      INTEGER*2 MAX_EOP_COR
      PARAMETER (MAX_EOP_COR=12*MAX_ROT)
      INTEGER*2 NAMNX(4,2)
!      INTEGER*2 NAMRT(4,3),NAMTD(4,3),ptype(MAX_ROT)
      INTEGER*2 NAMRT(4,3),NAMTD(4,3),ptype(14)
      INTEGER*2 NDIMT(2,2),MSC(3)
      INTEGER*2 NAMNT(4,2),INUTC,NNUTC, IOS
      INTEGER*4  IOS4
      character*80 cbuf
      character*(NAME_SIZE) fname
      CHARACTER*24 QNAMRT,QNAMTD
      CHARACTER*16 QNAMNT
      EQUIVALENCE (QNAMRT,NAMRT(1,1))
      EQUIVALENCE (QNAMNT,NAMNT(1,1))
      EQUIVALENCE (QNAMTD,NAMTD(1,1))
      CHARACTER    QMSC*6 , QNDIMT*8, UNITS*2
      EQUIVALENCE (QMSC,MSC(1))
      EQUIVALENCE (QNDIMT,NDIMT(1,1))
      INTEGER*2  I 
      INTEGER*4  JA, JB, JS, J1, J2, J3, J4, J5, MJD, NPARMR, NP
      REAL*8     CNVRT, PVAL, DIF_NUT, VAL, TAI, TDB, A_SIG, S_SIG
      REAL*8     DPSI_VAL, DEPS_VAL, DPSI_SIG, DEPS_SIG, DPSI_ERR, DEPS_ERR, &
     &           DX_VAL, DY_VAL, DX_SIG, DY_SIG, DX_ERR, DY_ERR
      REAL*8     NUTPS_VAL(2), NUTPS_SIG(2), NUTPS_ERR(2), &
     &           NUTXY_VAL(2), NUTXY_SIG(2), NUTXY_ERR(2)
!
      LOGICAL*2  KBIT, KBIT4, KPRINT, INTRP_RATES, FULL_ROT
      CHARACTER  QTYP(2)*3, QNUT(2)*15, XY_NUT(2)*40, XY_NUT_SEG(2)*4, &
     &           RNUT(2)*40, TYPEPR*6, BAS_DEL*1
      INTEGER*2 IP, J, KP, L, NROTC
      LOGICAL*2 EOP_ON
!
      LOGICAL*4 CHECK_STABIT
      REAL*8    EOPSTA(3), EOPRMS(3), EOPTRA(3)
      REAL*8       EPSILON_0 
      PARAMETER  ( EPSILON_0 = 0.4090928041D0 )  ! rad
      REAL*8    APR_PSI, APR_EPS, APR_NTX, APR_NTY, ANO_PSI, ANO_EPS, NUT_FCT(2)
!
      INTEGER*2 EOPCNT(3)
!
      CHARACTER LABSDI(2)*12, LABSC(2)*4, QMSC_MICRO(3)*9
      INTEGER*2 ISTRUC1(MAX_STRUC)
      REAL*8    JD_TDB_EPOCH
!
      DATA QTYP     / 'PSI', 'EPS' /
      DATA QNUT     / 'longitude (Psi)', 'obliquity (Eps)' /
      DATA XY_NUT   / &
     &                'Nutation offset around X-axis       (dX)', &
     &                'Nutation offset around Y-axis       (dY)'  &
     &              /
      DATA XY_NUT_SEG   / &
     &                 'Dx  ', &
     &                 'Dy  '  &
     &                  /
      DATA RNUT     / &
     &                'Nutation offset wrt IAU 1980 model (Psi)', &
     &                'Nutation offset wrt IAU 1980 model (Eps)'  &
     &              /
      CHARACTER  NUT_MODEL(2)*15
      DATA       NUT_MODEL / '  apriori model', 'Wahr 1980 model' /
      CHARACTER  EOP_TAG*23
      DATA QNAMRT   /'X WobbleY WobbleUT1-TAI '/
      DATA QNAMNT   /'Psi     Eps     '/
      DATA QNAMTD   /'LOVE # lLOVE # hLAG.ANG.'/
      DATA QMSC     /'mama m'/
      data qmsc_micro /'microasec','microasec','microsec'/
      DATA QNDIMT   /'    DEG '/
      DATA EOPRMS/3*0/, EOPSTA/3*0.0/, EOPTRA/3*0.0/, EOPCNT/3*0/
      DATA LABSDI/'Diurnal     ','Semi-diurnal'/
      DATA LABSC/'Sine','Cos '/
      DATA INTRP_RATES /.TRUE./
      CHARACTER, EXTERNAL :: JD_TO_DATE*23, MJDSEC_TO_DATE*30
!
! 4.  HISTORY
!   WHO   WHEN    WHAT
!   DG    910701  Modified for new E.O. mod file mapping
!   KB    910816  Printing rate totals for original style of earth orientation
!                 parameterization (offsets and rates at individual epochs)
!   :94.01.14:jwr:Micro units introduced for adjstments and sigmas for eop offsets and
!                 rates.
!   :94.01.27:jwr:Missing test on kspool when writing hpeop info inserted.
!   :94.01.28:jwr:For no eop mod file case, modified code to interpolate apriori's
!                 rather than the contents of rotap.
!   jmg   960610  Remove holleriths.
!   JWR   970305  Crafted from A2JST.
!   PET   970611  Forced HI_FREQ option to work in fast mode
!   jwr   971115  Added code to create the eop/nut correlation matrix.
!   pet   971202  Added logic for bypassing deselected station
!   kdb   980205  Very small fix.  Change incorrect error trapping variable
!                 from ierr to ios in a read of the USRGxx file.
!   pet   990122  Changed format from F10.2 to F12.2 in printing adjustments of
!                 baseline dependnet clocks in interactive mode.
!   pet   990522  Increased number of difites for baseline dependent clocks
!   pet   1999.05.28. Made LBUF_LEN, LBUF, IPTR, PAGEWID formal arguments,
!                     eliminated common block adj_buf
!   pet   2001.01.26  Removed "additional nutation terms" and tides,
!                     they are not supported any more.
!   pet   2001.04.25  Replaced initialization of NUT_INDICES assignment
!                     operator instead o data operator.
!   pet   2002.04.03  Changed format of user partial parameters in spool-file
!                     from G19.4 to D21.14
!                     Adjustments are in the field 50:70,
!                     a-sigmas    are in the field 72:92,
!                     m-sigmas    are in the  field 94:114,
!                     flag global (if present) in the field 116:121
!   pet   2002.04.08  Added argumens APR_VAL, APR_SIG, EST_VAL. They should
!                     transport some values needed to be put in listing in
!                     SINEX format
!   pet    2002.05.08 The field "parameter index" was changed from I4 to I5
!                     in order to support solutions with more than 9999
!                     parameters. As a rasult all adjustments and their formal
!                     uncertainties were moved one character to the right edge
!                     of listing;
!                     Moved once charater to the right adjustments of nutation
!                     angles wrt IAU1980;
!   jwr   2002.12.19  Use of KBIT in call to SBIT replaced with KBITN.
!   jwr   2002.12.19  TRUE__L2 and FALSE__L2 introduced for -i2 removal
!   pet   2005.08.18  Added support of internal variable SEG_LISTING_STYLE.
!                     SEG_LISTING_STYLE == SEG_PRE2005 is reserved for backward
!                     compatibiltiy. If SEG_LISTING_STYLE == SEG_POST2005, then
!                     for clock, atmospheric path delay in zenith direction
!                     and atmospheric gtradients the time tag epoch is printed
!                     with accuracy one millisecond in ISO-compatible format.
!   pet   2005.08.18  Forced to print TAI time in time tag for parameters if &
!                     SEG_LISTING_STYLE == SEG_POST2005
!   pet   2020.02.22  Implemented a rigourous transformation for Dpsi, Deps --> dX, dY
!   pet   2020.02.25  Fixed a bug with NPARM, NPARMR, EOP_INDICIES, NUT_INDICIES type: it should be I*4
!   pet   2002.05.08  Added error control
!   pet   2021.06.01  Added support of LISTING_OPTIONS SRC_POST2021_SPOOL__FMT
!                     That option changes format
!
! 5.  A2JST PROGRAM STRUCTURE
!
! STATEMENTS FUNCTIONS
!
      KPRINT(NP) = ( KBIT4(ISTRUC1,NP) .EQV. KGLOBALS) .OR. .NOT. KBATCH .OR. &
     &             ( KBATCH .AND. ISLTY2 .EQ. 'I' )
      NUT_INDICIES(1) = 0
      NUT_INDICIES(2) = 0
      NUT_FCT(1) = 1.0D0/DSIN(EPSILON_0)
      NUT_FCT(2) = 1.0D0
!
      JA = 3*M_GPA
      JB = 2*M_GPA
      JS =   M_GPA
      NROTC = 0
      EOP_ON = .FALSE.
      IF ( EOP_STYLE(1).EQ.0 .AND. EOP_STYLE(2).EQ.0 ) THEN
           FULL_ROT = .TRUE.
         ELSE
           FULL_ROT = .FALSE.
      END IF
!
      DO J1=1,M_GPA
         CALL SBIT4 ( ISTRUC1, J1, KBITN4(ISTRUC,J1) )
      ENDDO
!
      TYPEPR = ' '
      IF ( KGLOBALS ) THEN
           TYPEPR='global'
           DO J1=1,M_GPA
              CALL SBIT4 ( ISTRUC1, J1, INT2(1) )
           ENDDO
      END IF
!
! --- Test relativity flag
!
      IF ( LREL .NE. 0 ) THEN
           NPARM = NPARM + 1
           PVAL = VREL + MAT(JB+NPARM)
!
! -------- If in batch, printing globals, and global parm, then print it
! -------- else if not in batch, then print parm
!
           IF ( KPRINT(NPARM) ) THEN
                IF ( KSPOOL ) THEN
                     WRITE ( 23, 1700 ) NPARM, PVAL, MAT(JB+NPARM), &
     &                                  MAT(JS+NPARM), SCSIG(NPARM), TYPEPR
 1700                FORMAT ( I5, ". Gamma", 24X, 4(F17.11,"     "), A6 )
                END IF
!
                IF ( KSCREEN ) THEN
                     IPTR=IPTR+1
                     WRITE ( LBUF(IPTR), 3700 ) NPARM, PVAL, MAT(JB+NPARM), &
     &                                          SCSIG(NPARM)
                     CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                     CALL NL_MN()
 3700                FORMAT ( I5, ". Gamma", 16X, 3(5X,F11.5) )
                END IF
           END IF
      ENDIF
!
      IF ( LPREC .NE. 0 ) THEN
!
! -------- Precession rate parameter
!
           NPARM = NPARM +1
           PVAL = VPREC + MAT(JB+NPARM) * 3600.0D0 * 180.0D0  / PI__NUM
           MAT(JB+NPARM) = MAT(JB+NPARM) * 3600.0D0 * 180.0D0 / PI__NUM
           MAT(JS+NPARM) = MAT(JS+NPARM) * 3600.0D0 * 180.0D0 / PI__NUM
           SCSIG(NPARM) = SCSIG(NPARM) * 3600.0D0 * 180.0D0   / PI__NUM
!
! -------- if in batch, printing globals, and global parm, then print it
! -------- else if not in batch, then print parm
!
           IF ( KPRINT(NPARM) ) THEN
                IF ( KSPOOL .AND. KFULLOUT ) WRITE ( 23, "(1X)" )
                IF ( KSPOOL ) THEN
                     WRITE(23,2100) NPARM, PVAL, MAT(JB+NPARM), MAT(JS+NPARM), &
     &                              SCSIG(NPARM), TYPEPR
 2100                FORMAT ( I5, ". Precession Constant ", &
     &                             4(F12.6, ' asec/cntry '), A6 )
                END IF
                IF ( KSCREEN ) THEN
                     IPTR=IPTR+1
                     WRITE ( LBUF(IPTR), 4100 ) NPARM, PVAL, MAT(JB+NPARM), &
     &                                          SCSIG(NPARM)
                     CALL ADDSTR_F(LBUF(IPTR)(:PAGEWID) )
                     CALL NL_MN()
 4100                FORMAT ( I5, ". Precession rate",10X,3F15.6)
                ENDIF
           ENDIF
      END IF
!
! --- Daily nutation parameters
!
      CNVRT =(180.0D0/PI__NUM) * 3600.0D0 * 1000.D0
      INUTC=NPARM+1
      NNUTC=0
!
! --- Get apriori nutation angles
!
      CALL ERR_PASS ( IUER, IER )
      CALL APRIORI_NUT_APPLIED ( APR_PSI, APR_EPS, ANO_PSI, ANO_EPS, &
     &                           APR_NTX, APR_NTY, JD_TDB_EPOCH, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3761, IUER, 'A2JST_NOEOP', 'Error in computing '// &
     &         'apriori nutation angles' )
           RETURN
      END IF
!
! --- Transform nutation reference data to MJD, TAI
!
      MJD = (JD_TDB_EPOCH - J2000__JD - 0.5D0) + J2000__MJD
      TDB = (JD_TDB_EPOCH - 0.5D0 - INT(JD_TDB_EPOCH - 0.5D0))*86400.0D0
      CALL TDB_TO_TAI ( MJD, TDB, TAI )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!   write ( 6, * ) ' JD_TDB_EPOCH = ', JD_TDB_EPOCH             ! %%%%%%%%%%%%
!   write ( 6, * ) ' MJD, TDB, TAI = ', MJD, TDB, TAI           ! %%%%%%%%%%%%
!   write ( 6, * ) ' dat1= ', JD_TO_DATE ( JD_TDB_EPOCH, IER )  ! %%%%%%%%%%%%
!   write ( 6, * ) ' dat2= ', MJDSEC_TO_DATE ( MJD, TDB, IER )  ! %%%%%%%%%%%%
!! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      IF ( NUT_USE_CODE == NUT__PSE ) THEN
!
! -------- Extract nutation adustmetns its undertainties andsigmas
!
           DPSI_VAL = MAT(JB+NPARM+1)
           DEPS_VAL = MAT(JB+NPARM+2)
           DPSI_SIG = SCSIG(NPARM+1)
           DEPS_SIG = SCSIG(NPARM+2)
           DPSI_ERR = MAT(JS+NPARM+1)
           DEPS_ERR = MAT(JS+NPARM+2)
!
! -------- Cnvert nutation anles Dpsi, Deps
!
           CALL DPSI_DEPSILON_TO_XY ( PREC__CAPITAINE2003, MJD, TAI, DPSI_VAL, DEPS_VAL, &
     &                                DX_VAL, DY_VAL, IUER  )
           CALL DPSI_DEPSILON_TO_XY ( PREC__CAPITAINE2003, MJD, TAI, DPSI_SIG, DEPS_SIG, &
     &                                DX_SIG, DY_SIG, IUER  )
           CALL DPSI_DEPSILON_TO_XY ( PREC__CAPITAINE2003, MJD, TAI, DPSI_ERR, DEPS_ERR, &
     &                                DX_ERR, DY_ERR, IUER  )
           NUTPS_VAL(1) = DPSI_VAL
           NUTPS_VAL(2) = DEPS_VAL
           NUTPS_SIG(1) = DPSI_SIG
           NUTPS_SIG(2) = DEPS_SIG
           NUTPS_ERR(1) = DPSI_ERR
           NUTPS_ERR(2) = DEPS_ERR
!
           NUTXY_VAL(1) = DX_VAL
           NUTXY_VAL(2) = DY_VAL
           NUTXY_SIG(1) = DX_SIG
           NUTXY_SIG(2) = DY_SIG
           NUTXY_ERR(1) = DX_ERR
           NUTXY_ERR(2) = DY_ERR
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!   write ( 16, * ) ' nutpsi_dif, nuteps_dif = ', nutpsi_dif, nuteps_dif  ! %%%%
!   write ( 16, * ) ' nutps_val = ', nutps_val                   ! %%%%%%%%%%%%%
!   write ( 16, * ) ' nutps_sig = ', nutps_sig                   ! %%%%%%%%%%%%%
!   write ( 16, * ) ' nutps_err = ', nutps_err                   ! %%%%%%%%%%%%%
!
!   write ( 16, * ) ' nutxy_val = ', nutxy_val                   ! %%%%%%%%%%%%%
!   write ( 16, * ) ' nutxy_sig = ', nutxy_sig                   ! %%%%%%%%%%%%%
!   write ( 16, * ) ' nutxy_err = ', nutxy_err                   ! %%%%%%%%%%%%%
!   write ( 16, * ) ' ANO_PSI/ANO_EPS = ', ano_psi, ano_eps      ! %%%%%%%%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      END IF
      DO I = 1, 2
         IF ( I .EQ. 1 ) DIF_NUT = ANO_PSI
         IF ( I .EQ. 2 ) DIF_NUT = ANO_EPS
!
         IF ( KBIT(LNUT(1),I) ) THEN
              NPARM = NPARM + 1
              NNUTC=NNUTC+1
              PTYPE(NNUTC+NROTC) = I+6
              NUT_INDICIES(I) = NPARM
              DO KP=1,4
                 NAMNX(KP,NNUTC)=NAMNT(KP,I)
              ENDDO
!
! ----------- If in batch, printing globals, and global parm, then print it
! ----------- else if not in batch, then print parm
!
              IF ( KPRINT(NPARM) ) THEN
                   IF ( KSCREEN ) THEN
                       IPTR=IPTR+1
!
! -------------------- Convert sigma to microarcseconds on the fly.
!
                       IF ( NUT_USE_CODE == NUT__XY ) THEN
                            WRITE ( LBUF(IPTR), 7510 )  NPARM, QNUT(I), &
     &                              MAT(JB+NPARM)*CNVRT*NUT_FCT(I), &
     &                              SCSIG(NPARM)*CNVRT*1.D3*NUT_FCT(I)
                          ELSE 
!@                            WRITE ( LBUF(IPTR), 7510 )  NPARM, QNUT(I), &
!@     &                              MAT(JB+NPARM)*CNVRT, SCSIG(NPARM)*CNVRT*1.D3
                            WRITE ( LBUF(IPTR), 7510 )  NPARM, QNUT(I), &
     &                              NUTPS_VAL(I)*CNVRT, NUTPS_SIG(I)*CNVRT*1.D3
                       END IF
                       CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                       CALL NL_MN()
 7510                  FORMAT ( I5,'. Nutation offset in ', A15, 8X, F11.4, &
     &                             ' mas',F9.2,' microasec' )
                       IF ( CALCV .LE. 0.0D0 .OR. &
     &                      ( CALCV .GT. 9.99D0  .AND.  CALCV .LT. 99.99D0 ) ) THEN
                            IPTR=IPTR+1
                            IF ( NUT_USE_CODE == NUT__XY ) THEN
                                 WRITE ( LBUF(IPTR), 7520 ) XY_NUT(I), &
     &                                   MAT(JB+NPARM)*CNVRT, &
     &                                   MAT(JS+NPARM)*CNVRT*1.D3
                               ELSE 
!@                                 WRITE ( LBUF(IPTR), 7520 ) XY_NUT(I), &
!@     &                                   MAT(JB+NPARM)*CNVRT/NUT_FCT(I), &
!@     &                                   SCSIG(NPARM)*CNVRT*1.D3/NUT_FCT(I)
                                 WRITE ( LBUF(IPTR), 7520 ) XY_NUT(I), &
                                         NUTXY_VAL(I)*CNVRT, NUTXY_SIG(I)*CNVRT*1.D3
                            END IF
                            CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                            CALL NL_MN()
                       END IF
!
                       IF ( .NOT. ( NUTPSI_DIF .EQ. 0.0D0 .AND. NUTPSI_DIF .EQ. 0.0D0 ) ) THEN
                             IPTR = IPTR+1
!@                           WRITE ( LBUF(IPTR), 7520 )  RNUT(I), &
!@     &                        (MAT(JB+NPARM)+DIF_NUT)*CNVRT, &
!@     &                         SCSIG(NPARM)*CNVRT*1.D3
                             WRITE ( LBUF(IPTR), 7520 )  RNUT(I), &
     &                              (NUTPS_VAL(I) + DIF_NUT)*CNVRT, &
     &                               NUTPS_SIG(I)*CNVRT*1.D3
                             CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                             CALL NL_MN()
 7520                        FORMAT ( 7X, A, 2X, F11.4, ' mas',F9.2,' microasec' )
                       END IF
                   ENDIF
!
                   IF ( KSPOOL ) THEN
!
! --------------------- Save nutation angles
!
                        IF ( I .EQ. 1 ) THEN
                             IF ( NUT_USE_CODE == NUT__XY ) THEN
                                  APR_VAL(NTX__SNX) = APR_NTX
                                  EST_VAL(NTX__SNX) = MAT(JB+NPARM) + APR_NTX
                                ELSE 
                                  APR_VAL(PSI__SNX) = APR_PSI
                                  EST_VAL(PSI__SNX) = MAT(JB+NPARM) + APR_PSI
                             END IF
                           ELSE IF ( I .EQ. 2 ) THEN
                             IF ( NUT_USE_CODE == NUT__XY ) THEN
                                  APR_VAL(NTY__SNX) = APR_NTY
                                  EST_VAL(NTY__SNX) = MAT(JB+NPARM) + APR_NTY
                                ELSE 
                                  APR_VAL(EPS__SNX) = APR_EPS
                                  EST_VAL(EPS__SNX) = MAT(JB+NPARM) + APR_EPS
                             END IF
                        END IF
!
                        IF ( SEG_LISTING_STYLE == SEG_PRE2005_SPOOL__FMT ) THEN
                             IF ( NUT_USE_CODE == NUT__XY ) THEN
                                  WRITE ( 23, 7610 ) NPARM, QNUT(I), &
     &                                               MAT(JB+NPARM)*CNVRT*NUT_FCT(I), &
     &                                               MAT(JS+NPARM)*CNVRT*1.D3*NUT_FCT(I), &
     &                                               SCSIG(NPARM)*CNVRT*1.D3*NUT_FCT(I), &
     &                                               TYPEPR
                                ELSE 
!@                                  WRITE ( 23, 7610 ) NPARM, QNUT(I), &
!@     &                                               MAT(JB+NPARM)*CNVRT, &
!@     &                                               MAT(JS+NPARM)*CNVRT*1.D3, &
!@     &                                               SCSIG(NPARM)*CNVRT*1.D3, &
!@     &                                               TYPEPR
                                  WRITE ( 23, 7610 ) NPARM, QNUT(I), &
     &                                               NUTPS_VAL(I)*CNVRT, &
     &                                               NUTPS_ERR(I)*CNVRT*1.D3, &
     &                                               NUTPS_SIG(I)*CNVRT*1.D3, &
     &                                               TYPEPR
                             END IF
 7610                        FORMAT ( I5,". Nutation offset in ",A15,5X, &
     &                                 F16.3,' mas ',2(F10.1,' microasec '),A6)
!
                             IF ( CALCV .LE. 0.0D0 .OR. &
     &                            ( CALCV .GT. 9.99D0  .AND.  CALCV .LT. 99.99D0 ) ) THEN
                                  IF ( NUT_USE_CODE == NUT__XY ) THEN
                                       WRITE ( 23, 7620 ) XY_NUT(I), &
     &                                         MAT(JB+NPARM)*CNVRT, &
     &                                         MAT(JS+NPARM)*CNVRT*1.D3, &
     &                                         SCSIG(NPARM)*CNVRT*1.D3, &
     &                                         TYPEPR
                                     ELSE
!@                                       WRITE ( 23, 7620 ) XY_NUT(I), &
!@     &                                         MAT(JB+NPARM)*CNVRT*NUT_FCT(I), &
!@     &                                         MAT(JS+NPARM)*CNVRT*1.D3*NUT_FCT(I), &
!@     &                                         SCSIG(NPARM)*CNVRT*1.D3*NUT_FCT(I), &
!@     &                                          TYPEPR
                                       WRITE ( 23, 7620 ) XY_NUT(I), &
     &                                         NUTXY_VAL(I)*CNVRT, &
     &                                         NUTXY_ERR(I)*CNVRT*1.D3, &
     &                                         NUTXY_SIG(I)*CNVRT*1.D3, &
     &                                         TYPEPR
                                 END IF
                             END IF
!
                             IF ( NUT_USE_CODE == NUT__XY ) THEN
                                  WRITE ( 23, 7620 ) RNUT(I), &
     &                                   (MAT(JB+NPARM)*NUT_FCT(I)+DIF_NUT)*CNVRT, &
     &                                    MAT(JS+NPARM)*CNVRT*1.D3, &
     &                                   SCSIG(NPARM)*CNVRT*1.D3, TYPEPR
                                ELSE IF ( .NOT. ( NUTPSI_DIF .EQ. 0.0D0 .AND. NUTPSI_DIF .EQ. 0.0D0 ) ) THEN
!@                                  WRITE ( 23, 7620 ) RNUT(I), &
!@     &                                   (MAT(JB+NPARM)+DIF_NUT)*CNVRT, &
!@     &                                    MAT(JS+NPARM)*CNVRT*NUT_FCT(I)*1.D3, &
!@     &                                   SCSIG(NPARM)*CNVRT*NUT_FCT(I)*1.D3, TYPEPR
                                  WRITE ( 23, 7620 ) RNUT(I), &
     &                                   (NUTPS_VAL(I) + DIF_NUT)*CNVRT, &
     &                                    NUTPS_ERR(I)*CNVRT*1.D3, &
     &                                    NUTPS_SIG(I)*CNVRT*1.D3, TYPEPR
                                ELSE
                                  CONTINUE 
                             END IF
 7620                        FORMAT ( 7X, A, F15.3, ' mas ', &
     &                                2(F10.1,' microasec ' ), A6 )
                           ELSE IF ( SEG_LISTING_STYLE == SEG_POST2005_SPOOL__FMT ) THEN
!
! -------------------------- Modern way
!
                             EOP_TAG = JD_TO_DATE ( JD_TDB_EPOCH - &
     &                                              32.184D0/86400.0D0, IER )
                             IF ( NUT_USE_CODE == NUT__XY ) THEN
                                  WRITE ( 23, 7630 ) NPARM, QTYP(I), &
     &                                               NUT_MODEL(1), EOP_TAG, &
     &                                               MAT(JB+NPARM)*NUT_FCT(I)*CNVRT, &
     &                                               MAT(JS+NPARM)*NUT_FCT(I)*CNVRT*1.D3, &
     &                                               SCSIG(NPARM)*NUT_FCT(I)*CNVRT*1.D3, &
     &                                               TYPEPR
                                ELSE
!@                                  WRITE ( 23, 7630 ) NPARM, QTYP(I), &
!@     &                                               NUT_MODEL(1), EOP_TAG, &
!@     &                                               MAT(JB+NPARM)*CNVRT, &
!@     &                                               MAT(JS+NPARM)*CNVRT*1.D3, &
!@     &                                               SCSIG(NPARM)*CNVRT*1.D3, &
!@     &                                               TYPEPR
                                  WRITE ( 23, 7630 ) NPARM, QTYP(I), &
     &                                               NUT_MODEL(1), EOP_TAG, &
     &                                               NUTPS_VAL(I)*CNVRT, &
     &                                               NUTPS_ERR(I)*CNVRT*1.D3, &
     &                                               NUTPS_SIG(I)*CNVRT*1.D3, &
     &                                               TYPEPR
                             END IF
 7630                        FORMAT ( I5,'. Nutation D',A,' wrt ',A,1X, A, 1X, &
     &                                 F9.3,' mas ',2(F10.1,' microasec '),A6)
!
                             IF ( CALCV .LE. 0.0D0 .OR. &
     &                            ( CALCV .GT. 9.99D0  .AND.  CALCV .LT. 99.99D0 ) ) THEN
                                  IF ( NUT_USE_CODE == NUT__XY ) THEN
                                       WRITE ( 23, 7640 ) XY_NUT_SEG(I), &
     &                                         NUT_MODEL(1), EOP_TAG, &
     &                                         MAT(JB+NPARM)*CNVRT, &
     &                                         MAT(JS+NPARM)*CNVRT*1.D3, &
     &                                         SCSIG(NPARM)*CNVRT*1.D3, &
     &                                         TYPEPR
                                     ELSE
!@                                       WRITE ( 23, 7640 ) XY_NUT_SEG(I), &
!@     &                                         NUT_MODEL(1), EOP_TAG, &
!@     &                                         MAT(JB+NPARM)*CNVRT/NUT_FCT(I), &
!@     &                                         MAT(JS+NPARM)*CNVRT*1.D3/NUT_FCT(I), &
!@     &                                         SCSIG(NPARM)*CNVRT*1.D3/NUT_FCT(I), &
!@     &                                         TYPEPR
                                       WRITE ( 23, 7640 ) XY_NUT_SEG(I), &
     &                                         NUT_MODEL(1), EOP_TAG, &
     &                                         NUTXY_VAL(I)*CNVRT, &
     &                                         NUTXY_ERR(I)*CNVRT*1.D3, &
     &                                         NUTXY_SIG(I)*CNVRT*1.D3, &
     &                                         TYPEPR
                                 END IF
                             END IF
 7640                        FORMAT ( 7X, 'Nutation ',A,' wrt ',A,1X, A, 1X, &
     &                                 F9.3,' mas ',2(F10.1,' microasec '),A6)
!
                             IF ( NUT_USE_CODE == NUT__XY ) THEN
                                  WRITE ( 23, 7650 ) QTYP(I), &
     &                                         NUT_MODEL(2), EOP_TAG, &
     &                                         (MAT(JB+NPARM)*NUT_FCT(I)+DIF_NUT)*CNVRT, &
     &                                          MAT(JS+NPARM)*NUT_FCT(I)*CNVRT*1.D3, &
     &                                          SCSIG(NPARM)*NUT_FCT(I)*CNVRT*1.D3, &
     &                                          TYPEPR
                                ELSE 
!@                                  WRITE ( 23, 7650 ) QTYP(I), &
!@     &                                         NUT_MODEL(2), EOP_TAG, &
!@     &                                         (MAT(JB+NPARM)+DIF_NUT)*CNVRT, &
!@     &                                          MAT(JS+NPARM)*CNVRT*1.D3, &
!@     &                                          SCSIG(NPARM)*CNVRT*1.D3, &
!@     &                                          TYPEPR
                                  WRITE ( 23, 7650 ) QTYP(I), &
     &                                         NUT_MODEL(2), EOP_TAG, &
     &                                        (NUTPS_VAL(I) + DIF_NUT)*CNVRT, &
     &                                         NUTPS_ERR(I)*CNVRT*1.D3, &
     &                                         NUTPS_SIG(I)*CNVRT*1.D3, &
     &                                         TYPEPR
                             END IF
 7650                        FORMAT ( 7X, 'Nutation D',A,' wrt ',A,1X, A, 1X, &
     &                                 F9.3,' mas ',2(F10.1,' microasec '),A6)
                        END IF
                   ENDIF
             END IF
         END IF
      END DO ! Psi, Eps
!
! --- Write the eop/nutation correlation matrix when the eop is turned on.
!
      IF ( EOP_INDICIES(1,1) .NE. 0 .AND. &
     &     EOP_INDICIES(1,2) .NE. 0 .AND. &
     &     EOP_INDICIES(1,3) .NE. 0       ) THEN
!
           CALL IND_ROTCOR ( EOP_INDICIES, NUT_INDICIES, MAT )
      END IF
!
!
! --- Dirty trick: I claim that of socom_plus is defined. It is not true,
! --- but GET_NAMES doesn't need information from socom_plus
!
      SOCOM_PLUS_FIRST = SPL__DONE
!
! -------- Forming the list of parameters
!
      CALL GET_NAMES ( LPARMS, ISTR_LEN, M_GPA, IPARMN, TRUE__L2, &
     &                 FALSE__L2 )
!
! --- Returning status of socom_plus: undefined
!
      SOCOM_PLUS_FIRST = SPL__UNDF
      IF ( KGLOBALS .OR. SOLTYP_CH == 'I' ) THEN
           IF ( SOU_ADM_FLAG == SOUADM__GLB_ALL      .OR. &
     &          SOU_ADM_FLAG == SOUADM__GLB_LIST_NO  .OR. &
     &          SOU_ADM_FLAG == SOUADM__GLB_LIST_YES      ) THEN
!
! ------------- Admitance of source structure to delay estimated as 
! ------------- global parameter(s)
!
                DO 410 J1=1,IPARMN
                   IF ( LPARMS(J1)(1:11) == 'GLB_SOU_ADM' ) THEN
                        VAL   = MAT(JB+J1)
                        A_SIG = MAT(JS+J1)
                        S_SIG = SCSIG(J1)
                        WRITE ( 23, 110 ) J1, LPARMS(J1)(1:20), VAL, VAL, &
     &                                    A_SIG, S_SIG, 'globl'
 110                    FORMAT ( I5,'.', 2X, A, 9X, F12.5, 4X, F12.5, &
     &                                   '     d/l    ', &
     &                                   2(F11.5, '     d/l   '), &
     &                                   '      d/l    ', A )
                   END IF
 410            CONTINUE 
           END IF
         ELSE 
           IF ( SOU_ADM_FLAG == SOUADM__LCL_ALL      .OR. &
     &          SOU_ADM_FLAG == SOUADM__LCL_LIST_NO  .OR. &
     &          SOU_ADM_FLAG == SOUADM__LCL_LIST_YES      ) THEN
!
! ------------- Admitance of source structure to delay estimated as 
! ------------- local parameter(s)
!
                DO 420 J2=1,IPARMN
                   IF ( LPARMS(J2)(1:11) == 'LCL_SOU_ADM' ) THEN
                        VAL   = MAT(JB+J2)
                        A_SIG = MAT(JS+J2)
                        S_SIG = SCSIG(J2)
                        WRITE ( 23, 110 ) J2, LPARMS(J2)(1:20), VAL, VAL, &
     &                                    A_SIG, S_SIG, 'local'
                   END IF
 420            CONTINUE 
           END IF
      END IF
!
! --- Baseline-dependent clocks
!
      IF ( LOGBCL ) THEN
           IF ( KSCREEN ) THEN
                IPTR=IPTR+1
                WRITE ( LBUF(IPTR), "(' ')" )
                CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                CALL NL_MN()
           ENDIF
           IF ( KSPOOL ) WRITE ( 23, "(' ')" )
!
           DO I = 1,MIN(MAX_ARC_STA,NUMSTA)-1
!
! ----------- Check STABIT_P or STABIT_G bit fields to bypass deselcted station
!
              IF ( .NOT. CHECK_STABIT ( I ) ) GOTO 820
!
              IP = I + 1
              DO J = IP,MIN(MAX_ARC_STA,NUMSTA)
!
! -------------- Check STABIT_P or STABIT_G bit fields to bypass
! -------------- deselcted station
!
                 IF ( .NOT. CHECK_STABIT ( J ) ) GOTO 830
!
                 IF ( KBIT(ICLOCK(1,I),J) .OR. KBIT(ICLOCK(1,J),I) ) THEN
                      NPARM = NPARM + 1
                      PVAL  = MAT(JB+NPARM) * 1.D9
                      MAT(JS+NPARM) = MAT(JS+NPARM) * 1.D9
                      SCSIG(NPARM) = SCSIG(NPARM) * 1.D9
                      IF ( SRC_LISTING_STYLE == SRC_POST2021_SPOOL__FMT ) THEN
                           BAS_DEL = " "
                         ELSE 
                           BAS_DEL = "-"
                      END IF
                      IF ( KSCREEN ) THEN
                           IPTR=IPTR+1
                           WRITE ( LBUF(IPTR), 5009 ) NPARM, &
     &                                               (ISITN(L,I),L=1,4), BAS_DEL, (ISITN(L,J),L=1,4), &
     &                                                PVAL*1.D3, SCSIG(NPARM)*1.D3
                           CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                           CALL NL_MN()
 5009                      FORMAT ( I5, ".", 1X, 4A2, A1, 4A2, &
     &                              1X,'Clock offset',6X,2(   F15.2,' ps'))
                      ENDIF
                      IF ( KSPOOL ) THEN
                           WRITE ( 23, 5019 ) NPARM, &
     &                                        (ISITN(L,I),L=1,4), BAS_DEL, (ISITN(L,J),L=1,4), &
     &                                        PVAL*1.D3, MAT(JS+NPARM)*1.D3, &
     &                                        SCSIG(NPARM)*1.D3
 5019                      FORMAT ( I5,".",1X,4A2,A1,4A2,1X,"Clock offset", &
     &                             15X, 3(F19.3,' ps') )
                      END IF
                 END IF
 830             CONTINUE
              END DO
 820          CONTINUE
           END DO
      END IF
!
      IF ( IOS_EST == IOS__SES ) THEN
           NPARM = NPARM + 1
           PVAL  = MAT(JB+NPARM)
           CBUF = 'IOS_SES '//DBNAME_CH//'  '
           IF ( KSCREEN ) THEN
                IPTR=IPTR+1
                WRITE ( LBUF(IPTR), 5021 ) NPARM, CBUF(1:20), &
     &                                     PVAL, SCSIG(NPARM)
                CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                CALL NL_MN()
           END IF
!
           IF ( KSPOOL ) THEN
                WRITE ( 23, 5022 ) NPARM, CBUF(1:20), &
     &                             PVAL, MAT(JS+NPARM), SCSIG(NPARM)
           END IF
 5021      FORMAT ( I5, ".", 1X, A20, 15X, 2(F16.5,'   ') )
 5022      FORMAT ( I5, ".", 1X, A20, 28X, 3(F16.5,'      ') )
        ELSE IF ( IOS_EST == IOS__STA ) THEN
           DO 430 J3=1,NUMSTA
              IF ( CHECK_STABIT ( INT2(J3) ) ) THEN
                   NPARM = NPARM + 1
                   PVAL  = MAT(JB+NPARM)
                   CALL CLRCH ( CBUF )
                   CBUF = 'IOS_STA '//ISITN_CHR(J3)//'    '
!
                   IF ( KSCREEN ) THEN
                        IPTR=IPTR+1
                        WRITE ( LBUF(IPTR), 5021 ) NPARM, CBUF(1:20), &
     &                                             PVAL, SCSIG(NPARM)
                        CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                        CALL NL_MN()
                   END IF
!
                   IF ( KSPOOL ) THEN
                        WRITE ( 23, 5022 ) NPARM, CBUF(1:20), &
     &                                     PVAL, MAT(JS+NPARM), SCSIG(NPARM)
                   END IF
              END IF
 430       CONTINUE 
        ELSE IF ( IOS_EST == IOS__BAS ) THEN
           DO 440 J4=1,NUMSTA
              IF ( CHECK_STABIT ( INT2(J4) ) ) THEN
                   IP = J4 + 1
                   DO 450 J5=IP,NUMSTA
                      IF ( CHECK_STABIT ( INT2(J5) ) ) THEN
                           IF ( KBIT(ICLOCK(1,J4),J5) .OR. KBIT(ICLOCK(1,J5),J4) ) THEN
                                NPARM = NPARM + 1
                                PVAL  = MAT(JB+NPARM)
                                CALL CLRCH ( CBUF )
                                CBUF = 'IOB_'//ISITN_CHR(J4)//' / '//ISITN_CHR(J5)
!
                                IF ( KSCREEN ) THEN
                                     IPTR=IPTR+1
                                     WRITE ( LBUF(IPTR), 5023 ) NPARM, CBUF(1:23), &
     &                                                          PVAL, SCSIG(NPARM)
                                     CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                                     CALL NL_MN()
                                END IF
!
                                IF ( KSPOOL ) THEN
                                     WRITE ( 23, 5024 ) NPARM, CBUF(1:23), &
     &                                                  PVAL, MAT(JS+NPARM), SCSIG(NPARM)
                                END IF
 5023                           FORMAT ( I5, ".", 1X, A23, 12X, 2(F16.5,'   ') )
 5024                           FORMAT ( I5, ".", 1X, A23, 25X, 3(F16.5,'      ') )
                           END IF
                      END IF
 450               CONTINUE 
              END IF
 440       CONTINUE 
      END IF
!
! --- High-frequency EOP (tidal components)
!
      IF ( KHFEOP.EQ.2 .OR. KHFEOP.EQ.3 ) THEN
!
! -------- Dirty trick: I claim that of socom_plus is defined. It is not true,
! -------- but GET_NAMES doesn't need information from socom_plus
!
           SOCOM_PLUS_FIRST = SPL__DONE
!
! -------- Forming the list of parameters
!
           CALL GET_NAMES ( LPARMS, ISTR_LEN, M_GPA, IPARMN, TRUE__L2, &
     &                      FALSE__L2 )
!
! -------- Returning status of socom_plus: undefined
!
           SOCOM_PLUS_FIRST = SPL__UNDF
!
! -------- Cycle on high frequency EOP
!
           DO I=1,(NUM_SDE_UT1+NUM_SDE_XY)*2
              NPARM = NPARM +1
              CPARM=LPARMS(NPARM)
              IF ( CPARM(1:1).EQ.'P' ) THEN
                   UNITS = 'ua'
                 ELSE
                   UNITS = ' u'
              ENDIF
!
! ----------- If in batch, printing globals, and global parm, then print it
! ----------- else if not in batch, then print parm
!
              IF ( KPRINT(NPARM) ) THEN
                 IF ( CPARM(2:2) .EQ. 'C' ) ND1=1
                 IF ( CPARM(2:2) .EQ. 'S' ) ND1=2
                 ND2=(I+1)/2
                 IF ( CPARM(1:1) .EQ. 'P' ) THEN
                      MAT(JB+NPARM)=MAT(JB+NPARM)*1.D3
                        MAT(JS+NPARM) = MAT(JS+NPARM)*1.D3
                      SCSIG(NPARM) = SCSIG(NPARM)*1.D3
                        PVAL = MAT(JB+NPARM) + SDC_VAL(ND1,ND2)*1.D3
                     ELSE
                        PVAL = MAT(JB+NPARM) + SDC_VAL(ND1,ND2)*1.D3/15.D0
                    ENDIF
                    IF ( KSPOOL ) THEN
                         WRITE(23,2101) NPARM, CPARM, PVAL, UNITS, &
     &                                  MAT(JB+NPARM), UNITS, MAT(JS+NPARM), &
     &                                  UNITS, SCSIG(NPARM), UNITS, TYPEPR
 2101                    FORMAT ( I5,". ",a20,6x,4(5X,F12.6,1X,A2, 'sec '),A6)
                    END IF
                    IF ( KSCREEN ) THEN
                         IPTR=IPTR+1
                         WRITE ( LBUF(IPTR), 4101) NPARM, CPARM, PVAL, &
     &                           MAT(JB+NPARM), SCSIG(NPARM)
                         CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                         CALL NL_MN()
 4101                    FORMAT ( I5, ". ", A20, 10X, 3F15.6 )
                    ENDIF
              ENDIF
           ENDDO
      END IF
!
! --- User-defined parameters
!
      IF ( KUSER_PART ) THEN
           IF ( KSCREEN ) THEN
                IPTR=IPTR+1
                WRITE ( LBUF(IPTR), "(' ')" )
                CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                CALL NL_MN()
           ENDIF
!
           IF ( KSPOOL ) WRITE ( 23, "(' ')" )
           IF ( KGLOBALS ) THEN
                FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'USRG'//PRE_LETRS
             ELSE
                FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'USRP'//PRE_LETRS
           ENDIF
           OPEN ( 66, FILE=FNAME, IOSTAT=IOS4 )
           IOS = IOS4
           CALL FERR ( IOS, "a2jst: Opening "//FNAME, INT2(0), INT2(0) )
           READ ( 66, *, IOSTAT=IOS4 ) NUM_USER_PART
           IF ( NUM_USER_PART .LT. 0 ) IOS4 = 1
           IOS = IOS4
           CALL FERR ( IOS, "a2jst: Reading "//FNAME, INT2(0), INT2(0) )
!
! -------- We recycle NPARMR, which is used above.
! -------- Here NPARMR is used to number the parameter
! --------       NPARM is used to point where to get the values.
!
           NPARMR=NPARM
           DO I = 1,NUM_USER_PART
              NPARM = NPARM + 1
              READ ( 66, '(A)', IOSTAT=IOS4 ) CBUF
              IOS = IOS4
              CALL FERR ( IOS, "a2jst: Reading "//fname, INT2(0), INT2(0) )
              IF ( CBUF(22:22) .EQ. "G" .AND. .NOT. KGLOBALS ) GOTO 5120
              NPARMR=NPARMR+1
              PVAL = MAT(JB+NPARM)
              IF ( KSCREEN ) THEN
                   IPTR=IPTR+1
                   WRITE ( LBUF(IPTR), 5109 ) NPARMR, CBUF(1:20), PVAL, &
     &                                        SCSIG(NPARM)
                   CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                   CALL NL_MN()
 5109              FORMAT ( I5, ".", 1X, A20, 20X, 2(   G10.3,'   ') )
              ENDIF
              IF ( KSPOOL ) THEN
                   WRITE ( 23, 5119 ) NPARMR, CBUF(1:20), PVAL, MAT(JS+NPARM), &
     &                                SCSIG(NPARM), TYPEPR
5119               FORMAT ( I5, ".", 1X, A20, 23X, 3(1PD21.14,' '), A6 )
              ENDIF
5120          CONTINUE
            END DO
!
            CLOSE ( UNIT=66 )
      END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!@        write ( 6, * ) '2 apriori_nut: apr_psi=',apr_psi, &
!@     &                 ' est_tot_psi=',est_val(psi__snx), &
!@     &                 ' est_tot_ntx=',est_val(ntx__snx)  ! %%%
!@        write ( 6, * ) '2 apriori_nut: apr_eps=',apr_eps, &
!@     &                 ' est_tot=',est_val(eps__snx), &   ! %%%%
!@     &                 ' est_tot_nty=',est_val(nty__snx)  ! %%%
!@        call pause ( 'a2jst_noeop' )      ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  A2JST_NOEOP  #!#
