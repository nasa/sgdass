      SUBROUTINE GETPAR ( C_BAS, LRA_VAL, LDL_VAL, IEXP_TRP, CSTA_TRP, &
     &                    MJD_TRP, ZEN_TRP, ADJ_TRP, ERR_TRP, IUER )
! ************************************************************************
! *                                                                      *
! *   Program  GETPAR  parses a spool file of the listing of Solve       *
! *   solution, find some fields and writes down the following files:    *
! *                                                                      *
! *   1) .sta -file with global positions of the stations. The list is   *
! *      sorted in alphabetic order. Stations before and after episodic  *
! *      motions are treated as different stations. Correlations between *
! *      station positions and velocities are also written.              *
! *   2) .vel -file with global velocities of the stations. The list is  *
! *      sorted in alphabetic order. Stations before and after episodic  *
! *      motions are treated as the same stations.                       *
! *   3) .sou -file with global positions of the sources. The list is    *
! *      sorted in increasing right ascensions. Correlations between     *
! *      right ascension and declination are also written.               *
! *   4) .eop -file with of EOP series : Xpole, Ypole, Ut1, Ut1_rate,    *
! *           Ut1_acceration.                                            *
! *   5) .nut -file with daily series of nutation angles.                *
! *   6) .crl -file with series of covariances matrices between EOP of   *
! *      the same session.                                               *
! *   7) .lso -file with series of positions of sources estimated as     *
! *      local parameters.                                               *
! *   8) .lst -file with series of positions of stations estimated as    *
! *      local parameters.                                               *
! *   9) .bas -file with series of baseline length.                      *
! *  10) .eob -file with EOP series in IERS EOP-B format.                *
! *  11) .trp -file with adjustments of troposphere path delay.          *
! *  12) .trs -file with statistics of troposphere path delay.           *
! *  13) .erm -file with adjustments of the B-spline Earth rotation      *
! *      model.                                                          *
! *  14) .heo -file with adjustments of the harmonic Earth rotation      *
! *      model.                                                          *
! *  15) .npv -file with adjustments and covariance matrixes for         *
! *                 non-linear variations in site positions.             *
! *  16) .bsp -file with adjustments to coeffientes of site position     *
! *                 variations expansion over the B-spline basis.        *
! *  17) .hps -file with adjustments to harmonic coeffientes of site     *
! *                 position variations. (expansion over Fourer basis).  *
! *  18) .apr -file with description of the apriori model used in the    *
! *                 solution.                                            *
! *  19) .rms -file with weighted root mean squares of postfit residuals *
! *                 for each session sorted in decreasing rms.           *
! *  20) .cns -file with description of constraints imposed on global    *
! *                 parametrs.                                           *
! *                                                                      *
! *  Usage: getpar <spool_file> <prefix> [apr_nut]                       *
! *      where spool_file is the name of Solve spool file;               *
! *            prefix is the main portion of the ouput filenames         *
! *            including path. The actual names of the output files are  *
! *            results of concatenation of prefix with extension.        *
! *      if the third argument is APR_NUT, then nutation angles with     *
! *  respect to apriori angles are extracted. If teh third argument is   *
! *  missed, then the angles wrt IAU1980 are extracted.                  *
! *                                                                      *
! *  Parameters and some huge arrays are passed as arguments to GETPAR   *
! *  in order to avoid problems with allocating huge arrays in stack.    *
! *                                                                      *
! *  ###  15-JUN-1999   GETPAR   v 17.0 (c) L. Petrov  09-JUL-2024  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'param.i'
      INCLUDE   'getpar.i'
      INCLUDE   'erm.i'
      INCLUDE   'vtd.i'
      INTEGER*4  IUER
      CHARACTER  FILSPL*128, FILSTA*128, FILVEL*128, FILSOU*128, FILEOP*128, &
     &           FILNUT*128, FILCRL*128, FILLSO*128, FILLST*128, FILBAS*128, &
     &           FILRMS*128, FILEOB*128, FILTRP*128, FILTRS*128, FILERM*128, &
     &           FILHEO*128, FILNPV*128, FILBSP*128, FILHPS*128, FILAPR*128, &
     &           FILCNS*128, FILPRP*128, STR*256, PREF*128, SDAT*19
      INTEGER*4  M_ERM, M_HEO, M_NPV, M_APR, M_TRS, M_CNS
      PARAMETER  ( M_ERM = 512 + (ERM__MSPL**2 + 1)*ERM__MKNOT*3 )
      PARAMETER  ( M_HEO = 8192     )
      PARAMETER  ( M_NPV = 128*1024 )
      PARAMETER  ( M_APR = 256      )
      PARAMETER  ( M_TRS = MAX_STA*M_SES )
      PARAMETER  ( M_CNS = 32*1024 )
      CHARACTER  C_SOU(M_SOU)*8, J_SOU(M_SOU)*10,         &
     &           C_PRP(M_SOU)*8, J_PRP(M_SOU)*10,         &
     &           RA_VAL(M_SOU)*17,  RA_ERR(M_SOU)*10, &
     &           DL_VAL(M_SOU)*17,  DL_ERR(M_SOU)*10, &
     &           RAP_VAL(M_SOU)*10, RAP_ERR(M_SOU)*10, &
     &           DLP_VAL(M_SOU)*10, DLP_ERR(M_SOU)*10, &
     &           LSO_NAME(M_LSO)*8, LJO_NAME(M_LSO)*10, &
     &                 LRA_VAL(M_LSO)*17, LRA_ERR(M_LSO)*10, &
     &                 LDL_VAL(M_LSO)*17, LDL_ERR(M_LSO)*10, &
     &                 LCR_VAL(M_LSO)*7,                     &
     &                 USO_VAL(M_LSO)*5,  TSO_VAL(M_LSO)*5,  &
     &                 USC_LSO(M_LSO)*3,  TSC_LSO(M_LSO)*3,  &
     &                 USC_SOU(M_LSO)*6,  TSC_SOU(M_LSO)*6,  &
     &           LST_NAME(M_LST)*8, &
     &           CL_VAL(M_COMP,M_LST)*14, CL_ERR(M_COMP,M_LST)*10, &
     &           C_COO(M_STA)*15,         CSTA_SRT(M_STA)*20,      &
     &           C_CRL(15,M_STA)*5,       S_CRL(M_SOU)*6, P_CRL(M_SOU)*69, &
     &           C_VAL(M_COMP,M_STA)*14,  C_ERR(M_COMP,M_STA)*10,  &
     &           C_VEL(M_STA)*8,          CVEL_SRT(M_STA)*13,      &
     &           V_VAL(M_COMP,M_STA)*8,   V_ERR(M_COMP,M_STA)*8,   &
     &           C_BAS(M_BAS)*112,        START(M_SES)*14,         &
     &           C_NPV(M_NPV)*128,        C_APR(M_APR)*128, &
     &           C_CNS(M_CNS)*128, SOL_ID*64, SOL_DATE*19
      CHARACTER, ALLOCATABLE ::  C_TRS(:)*512
      CHARACTER  OBU_SOU(M_SOU)*7, OBT_SOU(M_SOU)*7, &
     &           SEU_SOU(M_SOU)*5, SET_SOU(M_SOU)*5, &
     &           DAF_SOU(M_SOU)*10, DAL_SOU(M_SOU)*10
      CHARACTER  OBU_STA(M_STA)*7, OBT_STA(M_STA)*7, &
     &           SEU_STA(M_STA)*5, SET_STA(M_STA)*5, &
     &           DAF_STA(M_STA)*10, DAL_STA(M_STA)*10
      CHARACTER  XEOP_VAL(M_SES)*11,  XEOP_ERR(M_SES)*10, &
     &           YEOP_VAL(M_SES)*11,  YEOP_ERR(M_SES)*10, &
     &           XREOP_VAL(M_SES)*11, XREOP_ERR(M_SES)*10, &
     &           YREOP_VAL(M_SES)*11, YREOP_ERR(M_SES)*10, &
     &           UEOP_VAL(M_SES)*11,  UEOP_ERR(M_SES)*10, &
     &           REOP_VAL(M_SES)*11,  REOP_ERR(M_SES)*10, &
     &           QEOP_VAL(M_SES)*11,  QEOP_ERR(M_SES)*10, &
     &           PEOP_VAL(M_SES)*11,  PEOP_ERR(M_SES)*10, &
     &           EEOP_VAL(M_SES)*11,  EEOP_ERR(M_SES)*10, &
     &           CEOP(28,M_SES)*6,    RMS_STR(M_SES)*64, &
     &           C_ERM(M_ERM)*160,    C_HEO(M_HEO)*128, &
     &           CN_BAS(M_SES)*(6*MA_BAS), C_NET(M_SES)*(2*MA_STA), &
     &           SOU_MID_EPOCH(M_SOU)*8
      CHARACTER  DBNAME(M_SES)*16, EXPNAME(M_SES)*8, USED(M_SES)*6, &
     &           DURA(M_SES)*10, TAG(M_SES)*14, EPOCH(M_SES)*10, &
     &           RMS_GLO_STR*64, CSTA_TRP(M_TRP)*8, DATE_CHR*23, MASTER_DIR*128
      INTEGER*4  LSO_SESIND(M_LSO), LST_SESIND(M_LST), IEXP_TRP(M_TRP), &
     &           L_ERM, L_HEO, L_NPV, N_BAS(M_SES), &
     &           KR_BAS(1,1), KU_BAS(1,1), MJD_STA_REF, MJD_SOU_REF, &
     &           NSCA_USED(M_SOU), NSCA_TOT(M_SOU)
      REAL*8     ZEN_TRP(M_TRP), ADJ_TRP(M_TRP), ERR_TRP(M_TRP), &
     &           MJD_EOP(M_SES), MJD_NUT(M_SES), MJD_TRP(M_TRP)
      CHARACTER  COMP*(M_COMP)
!
      DATA       COMP / 'XYZUEN' /  ! component names
      LOGICAL*4  LEX, FL_SESCODE, FL_EOP_ADJ_ONLY
      INTEGER*4  K1, K2, K3, K4, K5, K6, K7, K8, K9, K11, K12, K13, K14, &
     &           K15, K16, K17, K18, K19, K1A, IP, NR, N1, N_SES, N_LSO, &
     &           N_LST, IYEAR, IND, N_LIN, NUT_USAGE, MJD_VAL, &
     &           L_TRS, L_CNS, IER
      INTEGER*4  L_SOU, L_PRP, IND_SOU(M_SOU), IND_PRP(M_SOU)
      INTEGER*4  NUMARG, L_COO, L_VEL, L_BAS, L_TRP, L_APR
      REAL*8     SEC_VAL, RMS_VAL(M_SES), RMS_IND(M_SES), TAI_STA_REF, TAI_SOU_REF
      CHARACTER, EXTERNAL :: JD_TO_DATE*30, GET_VERSION*54
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
#ifdef INTEL
      INTEGER*4, EXTERNAL :: IARGC
#endif
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      INCLUDE 'getpar_version.i' ! Set revision date of the current version
!
      CALL CLRCH  ( FILSPL )
      CALL CLRCH  ( PREF   )
      CALL GETENVAR ( 'GETPAR_EOP_ADJ_ONLY', STR )
      IF ( STR(1:1) == 'Y' .OR. STR(1:1) == 'y' ) THEN
           FL_EOP_ADJ_ONLY = .TRUE.
         ELSE
           FL_EOP_ADJ_ONLY = .FALSE.
      END IF
!
      ALLOCATE( C_TRS(M_TRS), STAT=IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH8 ( INT8(M_TRS)*INT8(LEN(C_TRS(1))), STR )
           CALL ERR_LOG ( 1700, IUER, 'GETPAR', 'Error in allocating '// &
     &          TRIM(STR)//' bytes of dynamic memory for array C_TRS' )
           CALL EXIT ( 1 )
      END IF
!
! --- Get parameters
!
      NUMARG = IARGC ()
      CALL SET_SIGNAL_CTRLC ( 2 )
      NUT_USAGE = 2
      IF ( NUMARG .GE. 2 ) THEN
           CALL GETARG ( 1, FILSPL )
           CALL GETARG ( 2, PREF   )
           IF ( NUMARG .GE. 3 ) THEN
                CALL GETARG ( 3, STR )
                CALL TRAN ( 11, STR, STR )
                IF ( STR(1:7) .EQ. 'APR_NUT' ) THEN
                     NUT_USAGE = 1
                   ELSE
                     WRITE( 6, * ) 'Wrong third argument: '//STR(1:I_LEN(STR))
                     CALL EXIT ( 1 )
                END IF
           END IF
         ELSE
           CALL CLRCH ( STR )
           STR = GET_VERSION ( )
           WRITE ( 6, * ) STR(1:I_LEN(STR))//'  Usage:  getpar '// &
     &                    '<spool_file> <prefix> [apr_nut]'
           CALL EXIT ( 127 )
      END IF
!
! --- Set names of output files
!
      FILSTA  = PREF(1:I_LEN(PREF))//'.sta'
      FILVEL  = PREF(1:I_LEN(PREF))//'.vel'
      FILSOU  = PREF(1:I_LEN(PREF))//'.sou'
      FILEOP  = PREF(1:I_LEN(PREF))//'.eop'
      FILNUT  = PREF(1:I_LEN(PREF))//'.nut'
      FILCRL  = PREF(1:I_LEN(PREF))//'.crl'
      FILLSO  = PREF(1:I_LEN(PREF))//'.lso'
      FILLST  = PREF(1:I_LEN(PREF))//'.lst'
      FILBAS  = PREF(1:I_LEN(PREF))//'.bas'
      FILEOB  = PREF(1:I_LEN(PREF))//'.eob'
      FILTRP  = PREF(1:I_LEN(PREF))//'.trp'
      FILTRS  = PREF(1:I_LEN(PREF))//'.trs'
      FILERM  = PREF(1:I_LEN(PREF))//'.erm'
      FILHEO  = PREF(1:I_LEN(PREF))//'.heo'
      FILNPV  = PREF(1:I_LEN(PREF))//'.npv'
      FILBSP  = PREF(1:I_LEN(PREF))//'.bsp'
      FILHPS  = PREF(1:I_LEN(PREF))//'.hps'
      FILAPR  = PREF(1:I_LEN(PREF))//'.apr'
      FILRMS  = PREF(1:I_LEN(PREF))//'.rms'
      FILCNS  = PREF(1:I_LEN(PREF))//'.cns'
      FILPRP  = PREF(1:I_LEN(PREF))//'.prp'
!
! --- Does spool file exist?
!
      INQUIRE ( FILE=FILSPL, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 1701, IUER, 'GETPAR', 'Input spool file '// &
     &                    FILSPL(1:I_LEN(FILSPL))//' was not found' )
           RETURN
      END IF
!
! --- Parse spool file
!
      CALL ERR_PASS ( IUER, IER )
      CALL GETPAR_PARSE ( FILSPL, &
     &           M_SES, M_SOU, M_STA, M_COMP, M_LSO, M_LST, M_BAS, M_TRP, M_TRS, &
     &           M_ERM, M_APR, MA_STA, MA_BAS, M_CNS, NUT_USAGE, C_SOU, C_PRP, &
     &           J_SOU, J_PRP, IND_SOU, IND_PRP, RA_VAL, RA_ERR, DL_VAL, DL_ERR, &
     &           RAP_VAL, RAP_ERR, DLP_VAL, DLP_ERR, &
     &           OBU_SOU, OBT_SOU, SEU_SOU, SET_SOU, DAF_SOU, DAL_SOU, &
     &           LSO_NAME, LJO_NAME, LRA_VAL, LRA_ERR, LDL_VAL, LDL_ERR, LCR_VAL, &
     &           USO_VAL, TSO_VAL, &
     &           USC_LSO, TSC_LSO, &
     &           USC_SOU, TSC_SOU, &
     &           LST_NAME, L_ERM, &
     &           CL_VAL, CL_ERR, C_COO, CSTA_SRT, C_CRL, S_CRL, P_CRL, &
     &           C_VAL, C_ERR, C_VEL, CVEL_SRT, V_VAL, V_ERR, C_BAS, &
     &           C_ERM, OBU_STA, OBT_STA, SEU_STA, SET_STA, DAF_STA, DAL_STA, &
     &           XEOP_VAL, XEOP_ERR, XREOP_VAL, XREOP_ERR, &
     &           YEOP_VAL, YEOP_ERR, YREOP_VAL, YREOP_ERR, &
     &           UEOP_VAL, UEOP_ERR, &
     &           REOP_VAL, REOP_ERR, &
     &           QEOP_VAL, QEOP_ERR, &
     &           PEOP_VAL, PEOP_ERR, &
     &           EEOP_VAL, EEOP_ERR, &
     &           CEOP, &
     &           M_HEO, L_HEO, C_HEO, &
     &           M_NPV, L_NPV, C_NPV, &
     &           RMS_STR, RMS_VAL, RMS_IND, RMS_GLO_STR, &
     &           DBNAME, EXPNAME, USED, START, DURA, TAG, EPOCH, MJD_EOP, MJD_NUT, &
     &           LSO_SESIND, LST_SESIND, &
     &           N_LIN, N_SES, N_LSO, N_LST, L_SOU, L_PRP, L_COO, L_VEL, L_BAS, &
     &           L_TRP, IEXP_TRP, CSTA_TRP, MJD_TRP, ZEN_TRP, &
     &           ADJ_TRP, ERR_TRP, L_TRS, C_TRS, L_APR, C_APR, SOL_ID, SOL_DATE, &
     &           N_BAS, CN_BAS, KR_BAS, KU_BAS, C_NET, L_CNS, C_CNS, &
     &           MJD_STA_REF, TAI_STA_REF, MJD_SOU_REF, TAI_SOU_REF, &
     &           SOU_MID_EPOCH, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1702, IUER, 'GETPAR', 'Error in parsing '// &
     &         'spool file '//FILSPL )
           RETURN
      END IF
!
!      WRITE ( 6, * ) ' L_COO = ', L_COO, ' L_VEL = ' ,L_VEL, ' L_SOU = ' ,L_SOU, ' L_PRP = ', L_PRP
!      WRITE ( 6, * ) ' N_SES = ', N_SES, ' N_LSO = ', N_LSO, ' N_LST = ', N_LST, ' L_BAS = ', L_BAS
!
      STR = GET_VERSION ( )
      WRITE ( 6, * )  TRIM(STR)
      WRITE ( 6, * ) 'Spool file '//FILSPL(1:I_LEN(FILSPL))//' has ', N_LIN, ' lines'
!
! --- Writing down station positions
!
      OPEN ( UNIT=11, FILE=FILSTA, STATUS='UNKNOWN' )
      WRITE ( 11, FMT='(A)' ) SIG_STA
      WRITE ( 11, FMT='(A)' ) '# Solution listing: '//TRIM(FILSPL)
      WRITE ( 11, FMT='(A)' ) '# Solution ID:      '//TRIM(SOL_ID)
      IF ( MJD_STA_REF > 0 ) THEN
           IER = -2
           STR = MJDSEC_TO_DATE ( MJD_STA_REF, TAI_STA_REF, IER )
           WRITE ( 11, FMT='(A)' ) '# Position reference date: '//STR(1:19)
         ELSE
           WRITE ( 11, FMT='(A)' ) '# Position reference date: unknown'
      END IF
      WRITE ( 11, FMT='(A)' ) '# '
!
! --- Sort file of station names
!
      CALL SORT_CH ( L_COO, CSTA_SRT )
      DO 510 K1=1,L_COO
         CALL CHIN ( CSTA_SRT(K1)(16:20), IP )
         WRITE ( 11, 110 ) 'STA_GCX', C_COO(IP), &
     &                      COMP(1:1), C_VAL(1,IP), C_ERR(1,IP), &
     &                      COMP(2:2), C_VAL(2,IP), C_ERR(2,IP), &
     &                      COMP(3:3), C_VAL(3,IP), C_ERR(3,IP), &
     &                      OBU_STA(IP), OBT_STA(IP), &
     &                      SEU_STA(IP), SET_STA(IP), &
     &                      DAF_STA(IP), DAL_STA(IP)
 110     FORMAT ( A, ':  ',A,'  ',A,':  ',A,' -+ ',A, &
     &                       '  ',A,':  ',A,' -+ ',A, &
     &                       '  ',A,':  ',A,' -+ ',A, &
     &                       ' Obs_used: ',A, ' Obs_tot: ',A, &
     &                       ' Ses_used: ',A, ' Ses_tot: ',A, &
     &                       ' Date_beg: ',A, ' Date_end: ', A )
!
         WRITE ( 11, 120 ) 'STA_GCU', C_COO(IP), &
     &                      COMP(4:4), C_VAL(4,IP), C_ERR(4,IP), &
     &                      COMP(5:5), C_VAL(5,IP), C_ERR(5,IP), &
     &                      COMP(6:6), C_VAL(6,IP), C_ERR(6,IP)
 120     FORMAT ( A, ':  ',A,'  ',A,':  ',A,' -+ ',A, &
     &                       '  ',A,':  ',A,' -+ ',A, &
     &                       '  ',A,':  ',A,' -+ ',A )
         DO 511 K1A=1,15
            CALL TRAN ( 13, C_CRL(K1A,IP), C_CRL(K1A,IP) )
 511     CONTINUE
         WRITE  ( 11, 125 ) C_COO(IP), (C_CRL(N1,IP), N1=1,15)
 125     FORMAT ( 'STA_CRL:  ',A,'  C:',15('  ',A) )
 510  CONTINUE
      CLOSE ( UNIT=11 )
!
! --- Writing down station velocities
!
      OPEN ( UNIT=22, FILE=FILVEL, STATUS='UNKNOWN' )
      WRITE ( 22, FMT='(A)' ) SIG_VEL
      WRITE ( 22, FMT='(A)' ) '# Solution listing: '//FILSPL(1:I_LEN(FILSPL))
      WRITE ( 22, FMT='(A)' ) '# Solution ID:      '//TRIM(SOL_ID)
      WRITE ( 22, FMT='(A)' ) '# '
!
! --- Sort file of station velocities
!
      CALL SORT_CH ( L_VEL, CVEL_SRT )
      DO 520 K2=1,L_VEL
         CALL CHIN ( CVEL_SRT(K2)(9:13), IP )
         WRITE ( 22, 120 ) 'STA_GVX', C_VEL(IP), &
     &                      COMP(1:1), V_VAL(1,IP), V_ERR(1,IP), &
     &                      COMP(2:2), V_VAL(2,IP), V_ERR(2,IP), &
     &                      COMP(3:3), V_VAL(3,IP), V_ERR(3,IP)
!
         WRITE ( 22, 120 ) 'STA_GVU', C_VEL(IP), &
     &                      COMP(4:4), V_VAL(4,IP), V_ERR(4,IP), &
     &                      COMP(5:5), V_VAL(5,IP), V_ERR(5,IP), &
     &                      COMP(6:6), V_VAL(6,IP), V_ERR(6,IP)
 520  CONTINUE
      CLOSE ( UNIT=22 )
!
! --- Writing down source positions
!
      OPEN ( UNIT=33, FILE=FILSOU, STATUS='UNKNOWN' )
      WRITE ( 33, FMT='(A)' ) SIG_SOU
      WRITE ( 33, FMT='(A)' ) '# Solution listing: '//FILSPL(1:I_LEN(FILSPL))
      WRITE ( 33, FMT='(A)' ) '# Solution ID:      '//SOL_ID(1:I_LEN(SOL_ID))
      IF ( L_SOU > 0 ) THEN
           IF ( MJD_SOU_REF > 0 ) THEN
                IER = -2
                STR = MJDSEC_TO_DATE ( MJD_SOU_REF, TAI_SOU_REF, IER )
                WRITE ( 33, FMT='(A)' ) '# Position reference date: '//STR(1:19)
             ELSE
                WRITE ( 33, FMT='(A)' ) '# Position reference date: unknown'
           END IF
        ELSE
           WRITE ( 33, FMT='(A)' ) '# Position reference date: unknown'
      END IF
      WRITE ( 33, FMT='(A)' ) '# '
!
      DO 530 K3=1,L_SOU
         IP = IND_SOU(K3)
         WRITE (  33, 130 ) C_SOU(IP), J_SOU(IP), RA_VAL(IP), RA_ERR(IP), &
     &                      DL_VAL(IP), DL_ERR(IP), S_CRL(IP), &
     &                      OBU_SOU(IP), OBT_SOU(IP), &
     &                      USC_SOU(IP), TSC_SOU(IP), &
     &                      SEU_SOU(IP), SET_SOU(IP), &
     &                      DAF_SOU(IP), DAL_SOU(IP), SOU_MID_EPOCH(IP)
 130     FORMAT ( 'SOU_GCO:  ', A, 2X, A, &
     &            '  R:  ',A,' -+ ',A,'  D:  ',A,' -+ ',A, '  C:  ',A, &
     &            ' Obs_used: ',  A, ' Obs_tot: ',A, &
     &            ' Scan_used: ', A, ' Scan_tot: ',A, &
     &            ' Ses_used: ',  A, ' Ses_tot: ',A, &
     &            ' Date_beg: ',  A, ' Date_end: ', A, &
     &            ' Mid_epoch: ', A )
 530  CONTINUE
      CLOSE ( UNIT=33 )
!
! --- Writing down XPOLE, YPOLE, UT1, UT1-rate, UT1-acceleration
!
      OPEN ( UNIT=44, FILE=FILEOP, STATUS='UNKNOWN' )
      WRITE ( 44, FMT='(A)' ) SIG_EOP
      WRITE ( 44, FMT='(A)' ) '# '//FILSPL(1:I_LEN(FILSPL))
      IF ( FL_EOP_ADJ_ONLY ) THEN
           WRITE ( 44, FMT='(A)' ) '# ONLY ADJUMSNETS and units are micro-arsec/microsec'
      END IF
!
      DO 540 K4=1,N_SES
         IF ( ILEN(TAG(K4)) .EQ. 0 ) GOTO 540
         IF ( TAG(K4)(1:1) == '#' ) THEN
              MJD_VAL = MJD_EOP(K4)
              SEC_VAL = (MJD_EOP(K4) - MJD_VAL)*86400.0D0
              SDAT = MJDSEC_TO_DATE ( MJD_VAL, SEC_VAL, -2 )
            ELSE
              SDAT = '19'//TAG(K4)(1:2)//'.'//TAG(K4)(4:5)//'.'//TAG(K4)(7:8)//'-'// &
     &                TAG(K4)(10:14)
              CALL CHIN ( TAG(K4)(1:2), IYEAR )
              IF ( IYEAR .LT. 70 ) SDAT(1:2) = '20'
              IF ( SDAT .EQ. '2000.00.00-00000' ) SDAT = '$$$$$$$$$$$$$$$$'
         END IF
         WRITE  ( 44, 140 ) DBNAME(K4), SDAT(1:16), USED(K4), &
     &                      XEOP_VAL(K4),  XEOP_ERR(K4), &
     &                      YEOP_VAL(K4),  YEOP_ERR(K4), &
     &                      UEOP_VAL(K4),  UEOP_ERR(K4), &
     &                     XREOP_VAL(K4), XREOP_ERR(K4), &
     &                     YREOP_VAL(K4), YREOP_ERR(K4), &
     &                      REOP_VAL(K4),  REOP_ERR(K4), &
     &                      QEOP_VAL(K4),  QEOP_ERR(K4)
 140     FORMAT ( 'EOP_LOC:  ',A,'  TAG: ',A,'  USED:  ',A, &
     &                     '  X: ',A,' -+ ',A,'  Y: ',A,' -+ ',A, &
     &                     '  U: ',A,' -+ ',A,' XR: ',A,' -+ ',A, &
     &                     ' YR: ',A,' -+ ',A,' UR: ',A,' -+ ',A, &
     &                     ' UQ: ',A,' -+ ',A )
 540  CONTINUE
      CLOSE ( UNIT=44 )

!
! --- Writing down nutation
!
      OPEN ( UNIT=55, FILE=FILNUT, STATUS='UNKNOWN' )
      WRITE ( 55, FMT='(A)' ) SIG_NUT
      WRITE ( 55, FMT='(A)' ) '# '//FILSPL(1:I_LEN(FILSPL))
      IF ( NUT_USAGE .EQ. 1 )  THEN
           WRITE ( 55, FMT='(A)' ) '# Nutation angles are wrt apriori expansion'
         ELSE IF ( NUT_USAGE .EQ. 2 ) THEN
           WRITE ( 55, FMT='(A)' ) '# Nutation angles are wrt Wahr1980 expansion'
      END IF
!
      DO 550 K5=1,N_SES
         IF ( ILEN(PEOP_VAL(K5)) .EQ. 0  .OR.  ILEN(EEOP_ERR(K5)) .EQ. 0 ) &
     &   GOTO 550
!
         WRITE  ( 55, 150 ) DBNAME(K5), EPOCH(K5), USED(K5), &
     &                      PEOP_VAL(K5), PEOP_ERR(K5), &
     &                      EEOP_VAL(K5), EEOP_ERR(K5)
 150     FORMAT ( 'NUT_LOC:  ',A,'  EPOCH:  ',A,' USED: ',A, &
     &            ' P: ',A,' -+ ',A,'  E: ',A,' -+ ',A )
 550  CONTINUE
      CLOSE ( UNIT=55 )
!
! --- Writing down EOP correaltions
!
      OPEN ( UNIT=66, FILE=FILCRL, STATUS='UNKNOWN' )
      WRITE ( 66, FMT='(A)' ) SIG_CRL
      WRITE ( 66, FMT='(A)' ) '# '//FILSPL(1:I_LEN(FILSPL))
!
      DO 560 K6=1,N_SES
         WRITE  ( 66, 160 ) DBNAME(K6), ( CEOP(N1,K6), N1=1,28 )
 160     FORMAT ( 'CRL_LOC:  ',A,' ',28(' ',A) )
 560  CONTINUE
      CLOSE ( UNIT=66 )
!
! --- Writing down local source positions
!
      OPEN ( UNIT=66, FILE=FILLSO, STATUS='UNKNOWN' )
      WRITE ( 66, FMT='(A)' ) SIG_LSO
      WRITE ( 66, FMT='(A)' ) '# '//FILSPL(1:I_LEN(FILSPL))
!
      DO 570 K7=1,N_LSO
         IP = LSO_SESIND(K7)
!
         WRITE  ( 66, 170 ) LSO_NAME(K7), LJO_NAME(K7), DBNAME(IP), EPOCH(IP), &
     &                      LRA_VAL(K7),  LRA_ERR(K7), &
     &                      LDL_VAL(K7),  LDL_ERR(K7), LCR_VAL(K7), &
     &                      USO_VAL(K7),  TSO_VAL(K7), &
     &                      USC_LSO(K7),  TSC_LSO(K7)
 170     FORMAT ( 'SOU_LCO:  ', A, 2X, A, '  ', A, '  EPOCH:  ',A, &
     &            '  R:  ',A,' -+ ',A,'  D:  ' ,A, ' -+ ',A,' Corr: ',A, &
     &            ' Obs: ',A,' / ',A, ' Sca: ', A, ' / ', A )
 570  CONTINUE
      CLOSE ( UNIT=66 )
!
! --- Writing down local station positions
!
      OPEN ( UNIT=66, FILE=FILLST, STATUS='UNKNOWN' )
      WRITE ( 66, FMT='(A)' ) SIG_LST
      WRITE ( 66, FMT='(A)' ) '# '//FILSPL(1:I_LEN(FILSPL))
!
      DO 580 K8=1,N_LST
         IP = LST_SESIND(K8)
         WRITE  ( 66, 180 ) 'STA_LCX', LST_NAME(K8), &
     &                      DBNAME(IP), EPOCH(IP), &
     &                      COMP(1:1), CL_VAL(1,K8), CL_ERR(1,K8), &
     &                      COMP(2:2), CL_VAL(2,K8), CL_ERR(2,K8), &
     &                      COMP(3:3), CL_VAL(3,K8), CL_ERR(3,K8)
         WRITE  ( 66, 180 ) 'STA_LCU', LST_NAME(K8), &
     &                      DBNAME(IP), EPOCH(IP), &
     &                      COMP(4:4), CL_VAL(4,K8), CL_ERR(4,K8), &
     &                      COMP(5:5), CL_VAL(5,K8), CL_ERR(5,K8), &
     &                      COMP(6:6), CL_VAL(6,K8), CL_ERR(6,K8)
 180     FORMAT ( A,':  ',A,'  ',A,'  EPOCH:  ',A, &
     &                      '  ',A,':  ',A,' -+ ',A, &
     &                      '  ',A,':  ',A,' -+ ',A, &
     &                      '  ',A,':  ',A,' -+ ',A )
 580  CONTINUE
      CLOSE ( UNIT=66 )
!
! --- Writing down local baseline vector coordinates
!
      OPEN ( UNIT=66, FILE=FILBAS, STATUS='UNKNOWN' )
      WRITE ( 66, FMT='(A)' ) SIG_BAS
      WRITE ( 66, FMT='(A)' ) '# '//FILBAS(1:I_LEN(FILBAS))
!
      DO 590 K9=1,L_BAS
         WRITE  ( 66, 190 ) 'BAS_LCL', C_BAS(K9)
 190     FORMAT ( A,':  ',A )
 590  CONTINUE
      CLOSE ( UNIT=66 )
!
! --- Get directory for master files
!
      CALL GETENVAR ( 'MASTER_DIR', MASTER_DIR )
      IF ( ILEN(MASTER_DIR) .LE. 0 ) THEN
           MASTER_DIR = MASTER_DIR_DEF
           IF ( MASTER_DIR(I_LEN(MASTER_DIR):I_LEN(MASTER_DIR)) .NE. '/' ) THEN
                MASTER_DIR = MASTER_DIR(1:I_LEN(MASTER_DIR))//'/'
           END IF
      END IF
!
! --- Writing EOP in EOB-format
!
      CALL ERR_PASS ( IUER, IER )
      CALL GETPAR_TO_EOB ( FILEOB, FILSPL, N_SES, DBNAME, EXPNAME, &
     &           USED, DURA, TAG, XEOP_VAL,  XEOP_ERR, YEOP_VAL, &
     &           YEOP_ERR, XREOP_VAL, XREOP_ERR, YREOP_VAL, YREOP_ERR, &
     &           UEOP_VAL,  UEOP_ERR, REOP_VAL,  REOP_ERR, &
     &           QEOP_VAL,  QEOP_ERR, PEOP_VAL,  PEOP_ERR, &
     &           EEOP_VAL,  EEOP_ERR, CEOP,      RMS_STR, MJD_EOP, MJD_NUT, &
     &           NUT_USAGE, C_NET, SOL_ID, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1703, IUER, 'GETPAR', 'Error in writing EOB-file '// &
     &                    FILEOB )
           RETURN
      END IF
!
! --- Writing w.r.m.s file
!
      OPEN ( UNIT=66, FILE=FILRMS, STATUS='UNKNOWN' )
      IF ( N_SES .GE. 1 ) THEN
!
! -------- Write down format label
!
           IF ( RMS_STR(1)(28:28) .EQ. '.' ) THEN
                WRITE ( 66, FMT='(A)' ) SIG_RMS2
              ELSE
                WRITE ( 66, FMT='(A)' ) SIG_RMS1
           END IF
         ELSE
           WRITE ( 66, FMT='(A)' ) SIG_RMS2
      END IF
!
      IF ( ILEN(RMS_GLO_STR) .GT. 0 ) THEN
           WRITE  ( 66, 1100 ) 'RMS_DEL', RMS_GLO_STR, ' '
           CALL REPEAT ( '~', 72, STR )
           STR(1:1) = '*'
           WRITE  ( 66, '(A)' ) STR(1:71)
      END IF
 1100 FORMAT ( A,':  ',A, 2X, A )
      DO 5110 K11=1,N_SES
         IND = NINT ( RMS_IND(K11) )
         WRITE  ( 66, 1100 ) 'RMS_DEL', RMS_STR(IND)(1:35), EXPNAME(IND)
 5110 CONTINUE
      CLOSE ( UNIT=66 )
!
! --- Writing down troposphere path delay parameters
!
      OPEN ( UNIT=66, FILE=FILTRP, STATUS='UNKNOWN' )
      WRITE ( 66, FMT='(A)' ) SIG_TRP
      WRITE ( 66, FMT='(A)' ) '# '//FILSPL(1:I_LEN(FILSPL))
      DO 5120 K12=1,L_TRP
         IER = 0
         DATE_CHR = JD_TO_DATE ( 2400000.5D0 + MJD_TRP(K12) + 1.D-9, IER )
         WRITE ( 66, 1120 ) 'TRP_SEG: ', DBNAME(IEXP_TRP(K12)), DATE_CHR(1:19), &
     &                       CSTA_TRP(K12), ZEN_TRP(K12), ADJ_TRP(K12), &
     &                       ZEN_TRP(K12)+ADJ_TRP(K12), ERR_TRP(K12)
 1120    FORMAT ( A, 1X, A, 1X, 'Tag', 1X, A, 1X, 'Sta', 1X, A8, 1X, &
     &            'Apr', 1X, F14.3, 1X, &
     &            'Adj', 1X, F14.3, 1X, &
     &            'Tot', 1X, F14.3, 1X, &
     &            'Err', 1X, F14.3 )
 5120 CONTINUE
      CLOSE ( UNIT=66 )
!
      IF ( L_ERM > 0 ) THEN
           OPEN ( UNIT=66, FILE=FILERM, STATUS='UNKNOWN' )
           WRITE ( 66, FMT='(A)' ) TRIM(C_ERM(1))
           WRITE ( 66, FMT='(A)' ) '# '
           WRITE ( 66, FMT='(A)' ) '# Generated by pSolve and extracted by getpar'
           WRITE ( 66, FMT='(A)' ) '# '
           WRITE ( 66, FMT='(A)' ) '#  NB: Apr provides value of the apriori EOP'
           WRITE ( 66, FMT='(A)' ) '#      Adj provides the coefficient of the B-spline'
           WRITE ( 66, FMT='(A)' ) '# '
           WRITE ( 66, FMT='(A)' ) 'ERM SOL  Solution: '//TRIM(SOL_ID)
!
           DO 5130 K13=2,L_ERM
              WRITE ( 66, '(A)' ) TRIM(C_ERM(K13))
 5130      CONTINUE
           WRITE ( 66, FMT='(A)' ) '# '
           WRITE ( 66, FMT='(A)' ) TRIM(C_ERM(1))
           CLOSE ( UNIT=66 )
      END IF
!
      OPEN ( UNIT=66, FILE=FILHEO, STATUS='UNKNOWN' )
      IF ( L_HEO == 0 ) THEN
           WRITE ( 66, FMT='(A)' ) 'HEO  Format version of 2004.03.12 '
           WRITE ( 66, FMT='(A)' ) '# '
           WRITE ( 66, FMT='(A)' ) '# No estimates are available'
           WRITE ( 66, FMT='(A)' ) '# '
           WRITE ( 66, FMT='(A)' ) '# N  VLBI solution '//TRIM(SOL_ID)
           WRITE ( 66, FMT='(A)' ) '# E  2000.01.01-00:00:00'
           WRITE ( 66, FMT='(A)' ) '# '
           WRITE ( 66, FMT='(A)' ) 'HEO  Format version of 2004.03.12 '
         ELSE
           DO 5140 K14=1,L_HEO
              WRITE ( 66, '(A)' ) C_HEO(K14)(1:I_LEN(C_HEO(K14)))
 5140      CONTINUE
      END IF
      CLOSE ( UNIT=66 )
!
      CALL ERR_PASS ( IUER, IER )
      CALL NPV_TO_BSP ( L_NPV, C_NPV, SOL_ID, SOL_DATE, FILBSP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL ERR_LOG ( 1704, IER, 'GETPAR', 'Error in an attempt to '// &
     &         'generate output file coefficients of B-spline '// &
     &         'coefficients for modeling site positions with B-spline' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL NPV_TO_HPS ( L_NPV, C_NPV, SOL_ID, SOL_DATE, FILHPS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1705, IUER, 'GETPAR', 'Error in an attempt to '// &
     &         'generate the output file woith estimates of coefficients '// &
     &         'of harmonic site positions variations' )
           RETURN
      END IF
!
      OPEN ( UNIT=66, FILE=FILNPV, STATUS='UNKNOWN' )
      WRITE ( 66, FMT='(A)' ) 'NPV  Format version of 2007.10.31 '
      WRITE ( 66, FMT='(A)' ) '# '
      WRITE ( 66, FMT='(A)' ) '# Empirical model of non-linear site position variations'
      WRITE ( 66, FMT='(A)' ) '# in the form of an expansions over the Fourier and the B-spline '
      WRITE ( 66, FMT='(A)' ) '# basis from the VLBI solution '
      WRITE ( 66, FMT='(A)' ) '# Spool file: '//FILSPL(1:I_LEN(FILSPL))
      WRITE ( 66, FMT='(A)' ) '# '
      WRITE ( 66, FMT='(A)' ) 'SOL_ID:   '//SOL_ID(1:I_LEN(SOL_ID))
      WRITE ( 66, FMT='(A)' ) 'SOL_DATE: '//SOL_DATE(1:I_LEN(SOL_DATE))
      WRITE ( 66, FMT='(A)' ) '# '
      IF ( L_NPV == 0 ) THEN
           WRITE ( 66, FMT='(A)' ) '# No estimates are available'
           WRITE ( 66, FMT='(A)' ) '# '
         ELSE
           DO 5150 K15=1,L_NPV
              WRITE ( 66, '(A)' ) C_NPV(K15)(1:I_LEN(C_NPV(K15)))
 5150      CONTINUE
      END IF
!
      WRITE ( 66, FMT='(A)' ) '# '
      WRITE ( 66, FMT='(A)' ) 'NPV  Format version of 2007.10.31 '
      CLOSE ( UNIT=66 )
!
      OPEN ( UNIT=66, FILE=FILAPR, STATUS='UNKNOWN' )
      WRITE ( 66, FMT='(A)' ) 'SOLVE-APR  Format version of 2006.11.14  '
      WRITE ( 66, FMT='(A)' ) '# '
      WRITE ( 66, FMT='(A)' ) '# Apriori model used in Solve solution '
      WRITE ( 66, FMT='(A)' ) '# Spool file: '//FILSPL(1:I_LEN(FILSPL))
      WRITE ( 66, FMT='(A)' ) '# '
      IF ( L_APR > 0 ) THEN
           DO 5160 K16=1,L_APR
              WRITE ( 66, '(A)' ) C_APR(K16)(1:I_LEN(C_APR(K16)))
 5160      CONTINUE
      END IF
      WRITE ( 66, FMT='(A)' ) '# '
      WRITE ( 66, FMT='(A)' ) 'SOLVE-APR  Format version of 2006.11.14  '
      CLOSE ( UNIT=66 )
!
      OPEN ( UNIT=66, FILE=FILTRS, STATUS='UNKNOWN' )
      WRITE ( 66, FMT='(A)' ) 'SOLVE-TRS  Format version of 2021.09.03  '
      WRITE ( 66, FMT='(A)' ) '# '
      WRITE ( 66, FMT='(A)' ) '# Statistics of the atmosperic parameters'
      WRITE ( 66, FMT='(A)' ) '# Spool file: '//FILSPL(1:I_LEN(FILSPL))
      WRITE ( 66, FMT='(A)' ) '# '
      IF ( L_TRS > 0 ) THEN
           DO 5170 K17=1,L_TRS
              WRITE ( 66, '(A)' ) C_TRS(K17)(1:I_LEN(C_TRS(K17)))
 5170      CONTINUE
      END IF
      WRITE ( 66, FMT='(A)' ) '# '
      WRITE ( 66, FMT='(A)' ) 'SOLVE-TRS  Format version of 2021.09.03  '
      CLOSE ( UNIT=66 )
!
      OPEN ( UNIT=66, FILE=FILCNS, STATUS='UNKNOWN' )
      WRITE ( 66, FMT='(A)' ) '# SOLVE_CNS  Format version of 2023.02.27'
      WRITE ( 66, FMT='(A)' ) '# '
      WRITE ( 66, FMT='(A)' ) '# Information about global constraints'
      WRITE ( 66, FMT='(A)' ) '# Spool file: '//FILSPL(1:I_LEN(FILSPL))
      IF ( L_CNS > 0 ) THEN
           DO 5180 K18=1,L_CNS
              WRITE ( 66, '(A)' ) C_CNS(K18)(1:I_LEN(C_CNS(K18)))
 5180      CONTINUE
      END IF
      WRITE ( 66, FMT='(A)' ) '# '
      WRITE ( 66, FMT='(A)' ) '# SOLVE_CNS  Format version of 2023.02.27'
      CLOSE ( UNIT=66 )
!
! --- Writing down source positions
!
      OPEN ( UNIT=33, FILE=FILPRP, STATUS='UNKNOWN' )
      WRITE ( 33, FMT='(A)' ) SIG_PRP
      WRITE ( 33, FMT='(A)' ) '# Solution listing: '//FILSPL(1:I_LEN(FILSPL))
      WRITE ( 33, FMT='(A)' ) '# Solution ID:      '//SOL_ID(1:I_LEN(SOL_ID))
      WRITE ( 33, FMT='(A)' ) '# '
!
      DO 5190 K19=1,L_PRP
         IP = IND_PRP(K19)
         WRITE (  33, 1190 ) C_PRP(IP), J_PRP(IP), RAP_VAL(IP), RAP_ERR(IP), &
     &                      DLP_VAL(IP), DLP_ERR(IP), P_CRL(IP)
 1190    FORMAT ( 'SOU_PRP:  ', A, 2X, A, &
     &            '  R:  ',A,' -+ ',A,'  D:  ',A,' -+ ',A, &
     &            '  C:  ',A )
 5190 CONTINUE
      CLOSE ( UNIT=33 )
!
      WRITE ( 6, * ) 'Station positions        are written in '//FILSTA(1:I_LEN(FILSTA))
      WRITE ( 6, * ) 'Station velocities       are written in '//FILVEL(1:I_LEN(FILVEL))
      WRITE ( 6, * ) 'Source  positions        are written in '//FILSOU(1:I_LEN(FILSOU))
      WRITE ( 6, * ) 'EOP: Xp, Yp, Ut, Ru      are written in '//FILEOP(1:I_LEN(FILEOP))
      WRITE ( 6, * ) 'Nutation angles          are written in '//FILNUT(1:I_LEN(FILNUT))
      WRITE ( 6, * ) 'EOP correlations         are written in '//FILCRL(1:I_LEN(FILCRL))
      WRITE ( 6, * ) 'Local source positions   are written in '//FILLSO(1:I_LEN(FILLSO))
      WRITE ( 6, * ) 'Local stations positions are written in '//FILLST(1:I_LEN(FILLST))
      WRITE ( 6, * ) 'Local baseline vectors   are written in '//FILBAS(1:I_LEN(FILBAS))
      WRITE ( 6, * ) 'EOP in B-format          are written in '//FILEOB(1:I_LEN(FILEOB))
      WRITE ( 6, * ) 'Troposphere parameters   are written in '//FILTRP(1:I_LEN(FILTRP))
      WRITE ( 6, * ) 'Troposphere statustcs    are written in '//FILTRS(1:I_LEN(FILTRS))
      WRITE ( 6, * ) 'ERM         parameters   are written in '//FILERM(1:I_LEN(FILERM))
      WRITE ( 6, * ) 'HEO         parameters   are written in '//FILHEO(1:I_LEN(FILHEO))
      WRITE ( 6, * ) 'NPV         parameters   are written in '//FILNPV(1:I_LEN(FILNPV))
      WRITE ( 6, * ) 'BSP         parameters   are written in '//FILBSP(1:I_LEN(FILBSP))
      WRITE ( 6, * ) 'HPS         parameters   are written in '//FILHPS(1:I_LEN(FILHPS))
      WRITE ( 6, * ) 'Apriori model description is written in '//FILAPR(1:I_LEN(FILAPR))
      WRITE ( 6, * ) 'w.r.m.s.                 are written in '//FILRMS(1:I_LEN(FILRMS))
      WRITE ( 6, * ) 'Global constraint info    is wrttten in '//FILCNS(1:I_LEN(FILCNS))
      WRITE ( 6, * ) 'Source proper motions    are written in '//FILPRP(1:I_LEN(FILPRP))
!
      CALL EXIT ( 0 )
      END  SUBROUTINE  GETPAR  !#!#
