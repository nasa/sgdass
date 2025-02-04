      PROGRAM    EOP_ALIGNMENT
! ************************************************************************
! *                                                                      *
! *   Program  EOP_ALIGNMENT is for computing right hand side of         *
! *   NO_NET_rotation/translation constraint equations of global VLBI    *
! *   solutions in order to eliminate relative shift and drift of polar  *
! *   motion and UT1 time series with respect to an external EOP series  *
! *   in either USNO Finals or IERS C04 format.                          *
! *                                                                      *
! *   Algorithm:                                                         *
! *                                                                      *
! *   1) Compute the difference between EOP from the Solve solution      *
! *      with zero right part and the external EOP series. Compute       *
! *      weighted secular drift and the shift with respect to the        *
! *      external EOP file at the reference epoch for which station      *
! *      positions in this solution were obtained.                       *
! *                                                                      *
! *   2) Compute the right hand side of constraint equations.            *
! *                                                                      *
! *      Let us formulate the condition the following way:               *
! *      the new set of positions for L stations Xn is expressed via     *
! *      the set of old positions Xo as Xn = Xo + A*N + r, where         *
! *      A is the matrix of translation-rotation of dimension 3Lx6,      *
! *      N is the vector of translation-rotation, r is the vector        *
! *      of residuals of dimension 3L. In a case of L = 1, the equation  *
! *      does not have a unique solution, and if L>2, it can be          *
! *      satisfied in the mean square sense, i.e. sum r^2 reaches the    *
! *      minimum and A(T) * r = 0                                        *
! *                                                                      *
! *      Matrix A can be expressed via Cartesian components of station   *
! *      coordinates  a,b,c, etc as                                      *
! *                                                                      *
! *       1  0  0    0   a3 -a2                                          *
! *       0  1  0  -a3   0   a1                                          *
! *       0  0  1   a2  -a1   0                                          *
! *       1  0  0    0   b3 -b2                                          *
! *       0  1  0  -b3   0   b1                                          *
! *       0  0  1   b2  -b1   0                                          *
! *       1  0  0    0   c3 -c2                                          *
! *       0  1  0  -c3   0   c1                                          *
! *       0  0  1   c2  -c1   0                                          *
! *       ...                                                            *
! *                                                                      *
! *   Vector N of translation rotation consists of three                 *
! *   component of translation along axes 1,2,3 and small                *
! *   rotation along axes 1,2,3. NB: a finite rotation cannot be         *
! *   described as a vector product of a rotation vector and station     *
! *   positions. We require the rotation be small in a sense that        *
! *   square of the rotation vector components can be neglected.         *
! *                                                                      *
! *   Vector N is found by least squares as                              *
! *                                                                      *
! *      N = (A(T)*A)^{-1} * A(T) * dX  where dX = Xnew - Xold.          *
! *                                                                      *
! *   Multiplying both part by (A(T)*A), we get                          *
! *                                                                      *
! *      (A(T)*A) * N = A(T) * dX                                        *
! *                                                                      *
! *   We re-write the equation above for 6 components as                 *                                                                *
! *                                                                      *
! *      Sum dX1              = ((A(Tra)*A) * N )_1                      *
! *      Sum dX2              = ((A(Tra)*A) * N )_2                      *
! *      Sum dX3              = ((A(Tra)*A) * N )_3                      *
! *      Sum -X3*dX2 + X2*DX3 = ((A(Tra)*A) * N )_4                      *
! *      Sum  X3*dX1 - X1*DX3 = ((A(Tra)*A) * N )_5                      *
! *      Sum -X2*dX1 + X1*DX2 = ((A(Tra)*A) * N )_6                      *
! *                                                                      *
! *   We can notice that first three equations are net translations      *
! *   along axes 1, 2, and 3 and three last equations are net rotations  *
! *   along axes 1, 2, and 3.                                            *
! *                                                                      *
! *   Program eop_alignment finds the mean shift of the EOP from the     *
! *   VLBI trial solution with zero right-band side of the               *
! *   net-translation and net-rotation with respect to the reference     *
! *   EOP time series. Then if uses the following vector N to set the    *
! *   right-hand of net translation and net rotation                     *
! *                                                                      *
! *        0.0                                                           *
! *        0.0                                                           *
! *        0.0                                                           *
! *       -Y_shift                                                       *
! *       -X_shift                                                       *
! *       -UT1_shift                                                     *
! *                                                                      *
! *   in such a way the EOP series of the new solution with these right  *
! *   hand side would    produce EOP with zero shift with respect to     *
! *   the reference solution and the new site positron catalog would     *
! *   have zero translation for the subset of L stations with respect    *
! *   to the old catalog when a translation-rotation transformation of   *
! *   the new catalog with respect to to the old catalogue is sought.    *
! *                                                                      *
! *   NB: translation and rotation taken alone is not orthogonal.        *
! *   That means that requiring a net translation to be zero is not      *
! *   equivalent to requiring Xo as Xn = Xo + A*N + r is not             *
! *   equivalent to requring Xn = Xo + T*N + r and Xn = Xo + R*N + r     *
! *   where T and R are 3x3 matrix of translation and rotation.          *
! *   Translation-rotation should be considered together.                *
! *                                                                      *
! *   Analogously, we compute right hand-side of constraint equation for *
! *   net-translation/net-rotation for velocities. NB: in summing the    *
! *   stations with episodic motion are counted only once.               *
! *                                                                      *
! *   Usage: eop_alignment <sol-file> <nn-cons-list> <filsta_ref>        *
! *                        <filvel_ref>  <eop_fmt>  <ext_EOP_file>       *
! *                        [date_eop_ref] [date_beg] [date_end]          *
! *                        [max_sig_mas] [nsig] [ivrb]                   *
! *                                                                      *
! *   <sol-file> -- generic name of the output files obtained from       *
! *                 parsing spool file with using program getpar.        *
! *   <nn-cons-list> -- List of NNT-POS constraints used in solution.    *
! *                     This list can be found in Spool file just before *
! *                     the section of global parameters. Cut this list, *
! *                     put it into the file and feed if eop_alignment.  *
! *   <eop_fmt>      -- format of the external file or computation mode: *
! *                     INIT         or                                  *
! *                     NULL         or                                  *
! *                     IERS_C04     or                                  *
! *                     USNO_FINALS.                                     *
! *                     In INIT or NULL modes, the EOP file is in ERM    *
! *                     format.                                          *
! *   <ext_EOP_file> -- external EOP file in either USNO Finals or       *
! *                     IERS C04 as it was on 2002.05.20                 *
! *   [date_ref]     -- Reference data of the target global solution.    *
! *   [date_beg]     -- start date of the range of the external EOP      *
! *                     series which is considered for computation of    *
! *                     the shift and drift of the differences in the    *
! *                     EOP series. If omitted, then the first date in   *
! *                     the external file is used.                       *
! *   [date_end]     -- end date of the range of the external EOP series *
! *                     which is considered for computation of the shift *
! *                     and drift of the differences in the EOP          *
! *                     series. If omitted, then the last date in the    *
! *                     external file is used.                           *
! *   [max_sig_mas]  -- discard EOP with reported uncertainty above the  *
! *                     given thresold expressed in mas.If omitted,      *
! *                     no filter of observations with high reported     *
! *                     uncertainties is peformed.                       *
! *   [nsig]         -- to run an iterative filter that discard EOP that *
! *                     deviate by more than N formala uncertainies      *
! *                     from the linear trend. The operation is          *
! *                     performed iteratively: the EOP with the largest  *
! *                     normalized uncertainty is found, if its          *
! *                     normalized incertainty is > N, it is discarded   *
! *                     and the procedure is repeated till no            *
! *                     observations with normalized residual > N        *
! *                     remains.                                         *
! *   [ivrb]         -- verbosity level.                                 *
! *                     0 -- silent mode;                                *
! *                     1 -- normal verbotity level;                     *
! *                    >1 -- debugging mode.                             *
! *                                                                      *
! *   eop_alignment generates the excerpt of Solve control file for      *
! *   applying net-translation and net-rotation constraints with the     *
! *   specified right-hand side.                                         *
! *                                                                      *
! *   Caveat: the present version assumes that all stations which        *
! *           participated in no-net-translation constraints for         *
! *           positions participated in no-net-translation for velocity  *
! *           and no-net-rotation for both position and velocity.        *
! *                                                                      *
! *   NB:     the right-hand side of net-translation and rotations       *
! *           depends on the apriori catalogue of station positions and  *
! *           velocities. If the apriori catalogue has been changed,     *
! *           the reference solution with zero right hand sides of net   *
! *           translation and rotation contraints has to be recomputed,  *
! *           and eop_alignment should be run again.                     *
! *                                                                      *
! *  ### 15-MAY-2002  EOP_ALIGNMENT v4.4 (c)  L. Petrov  28-JUN-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'vtd.i'
      INCLUDE   'erm.i'
      INCLUDE   'getpar.i'
      INTEGER*4  MP, M_PAR, M_ERM, M_HEAD, M_HEO, MIND
      PARAMETER  ( MP     = 32768 )
      PARAMETER  ( M_PAR  =  1024 )
      PARAMETER  ( M_ERM  = 32768 )
      PARAMETER  ( M_HEAD =   516 )
      PARAMETER  ( M_HEO  =  8192 )
      PARAMETER  ( MIND   =   128 )
      CHARACTER  FILEOP*128, FILNN*128, FILSOL*128, FILEOB*128, FILERM*128,  &
     &           FILHEO*128, FILAPR*128, FILSRC*128, FILSTA*128, FILVEL*128, &
     &           FILSRC_APR*128, FILSTA_APR*128, FILVEL_APR*128,             &
     &           FILSRC_REF*128, FILSTA_REF*128, FILVEL_REF*128,             &
     &           FILCNT*128, FILSES*128, &
     &           FIL_NN_CNS_OUT*128, FIL_ERM_CNS_OUT*128
      CHARACTER  CH_FLAG(MP), BUF(MP)*256, SOLNAME*128, CN_STA(M_STA)*8, &
     &           CC_STA(M_STA)*15, CC_APR_STA(M_STA)*15, CC_REF_STA(M_STA)*15, &
     &           CV_STA(M_STA)*15, CV_APR_STA(M_STA)*15, CV_REF_STA(M_STA)*15, &
     &           BUF_CNT(MP)*512, HEAD_BUF(M_HEAD)*128, &
     &           EOP_FMT*12, DATE_EOP_REF*14, DATE_BEG*36, DATE_END*14, STR*80, &
     &           INP_EOP_FMT*3, NAME_HEO(M_HEO)*64, BUF_APR(M_PAR)*256, &
     &           SOL_ID*64, OUT(MP)*256, BUF_SES(MP)*1024, BUF_TMP(MP)*1024
      CHARACTER  GET_VERSION*54, ARG_STR*256
      REAL*8     JD_EXT(MP), YR_EXT(MP), &
     &           XP_EXT(MP), YP_EXT(MP), U1_EXT(MP), DPSI_EXT(MP), DEPS_EXT(MP), &
     &           XP_ERR(MP), YP_ERR(MP), U1_ERR(MP), DPSI_ERR(MP), DEPS_ERR(MP), &
     &           TIM_STEP
      TYPE ( ERM__TYPE  ) :: ERM
      TYPE ( EOP__STRU  ) :: EOP(MP)
      TYPE ( HEO__STRUC ) :: HEO(M_HEO)
      REAL*8     RH_VEC(12), MAX_SIG, MAX_SIG_MAS, MAX_SIG_MAX, REF_DATE, &
     &           JD_REF_DATE, HEO_EPOCH_SEC
      REAL*8     REA, MAX_SIG_DEF, EPS_EOP
      PARAMETER  ( REA         = 6378136.3D0 )
      PARAMETER  ( MAX_SIG_DEF = 1.0         ) ! In radians :-)
      PARAMETER  ( EPS_EOP     = 90.0        ) ! Tolerance for the difference in reference epocs wrt ERM
      CHARACTER    APR__LABEL*39
      PARAMETER  ( APR__LABEL = 'SOLVE-APR  Format version of 2006.11.14' )
      LOGICAL*1  LEX, FL_ARCS
      INTEGER*4  NP, NHEAD, IP, IL, ID, NEXT, MJD, LN_STA, &
     &           LC_STA, LV_STA, LC_APR_STA, LV_APR_STA, LC_REF_STA, LV_REF_STA, &
     &           NA, J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, NO_HEADER, &
     &           IS, IVRB, UNIX_DATE, EOB__MIN_SIZE, N_ERM(3), L_HEO, N_CNT, &
     &           LIND, IND(2,MIND), UZT_MODEL, UZT_USE, IER, NO, LUN, NS, NSF, &
     &           MJD_REF, MJD_APR, MJD_EST, MJD_EOP_REF, NSES, NSA, IUER
      PARAMETER  ( EOB__MIN_SIZE = 1024 )
      INTEGER*8  SIZE_EOB_I8, SIZE_ERM_I8
      REAL*8     COO_STA(3,M_STA),     VEL_STA(3,M_STA), &
     &           COO_APR_STA(3,M_STA), VEL_APR_STA(3,M_STA), &
     &           COO_REF_STA(3,M_STA), VEL_REF_STA(3,M_STA), &
     &           PHI(3), OME(3), SEC, &
     &           JD_EOP_BEG, JD_EOP_END, NSIG, TAI_REF, TAI_APR, TAI_EST, TAI_EOP_REF
      REAL*8     MJD_SEC_TO_JD
#ifdef INTEL
      INTEGER*4, EXTERNAL ::  IARGC
#endif
      INTEGER*4, EXTERNAL ::  GET_UNIT, FILE_INFO, ILEN, I_LEN, LINDEX
      CHARACTER, EXTERNAL ::  MJDSEC_TO_DATE*30, GET_CDATE*19
!
      INCLUDE   'eop_alignment_version.i'
!
      CALL CLRCH ( FILEOP )
      CALL CLRCH ( FILSOL )
      CALL CLRCH ( FILNN  )
      MAX_SIG = MAX_SIG_DEF
!
      CALL CLRCH ( STR )
!
! --- Get arguments
!
      CALL CLRCH ( ARG_STR )
      IF ( IARGC () .GE. 6 ) THEN
!
! -------- Two forms of the input arguments are supported: 
! -------- when no solution is available ( EOP_FMT == NULL ) and
! -------- when a solution with EOP estimation is available
!
           CALL GETARG ( 1, FILSOL     )
           CALL GETARG ( 2, FILNN      )
           CALL GETARG ( 3, FILSTA_REF )
           CALL GETARG ( 4, FILVEL_REF )
           CALL GETARG ( 5, EOP_FMT )
           CALL TRAN   ( 11, EOP_FMT, EOP_FMT )
           IF ( .NOT. ( EOP_FMT == 'INIT'        .OR. &
     &                  EOP_FMT == 'NULL_ERM'    .OR. &
     &                  EOP_FMT == 'NULL_STA'    .OR. &
     &                  EOP_FMT == 'IERS_C04'    .OR. &
     &                  EOP_FMT == 'USNO_FINALS'      ) ) THEN
                 IUER = -1
                 CALL ERR_LOG ( 8201, IUER, 'EOP_ALIGNMENT', 'Unsupported '// &
     &               'EOP format. One of INIT, NULL_ERM, NULL_STA, IERS_C04, or '// &
     &               'USNO_FIMNALS were expected' )
                 CALL EXIT ( 1 )
           END IF
           CALL GETARG ( 6, FILEOP     )
           IF ( IARGC() .GE. 7 ) THEN
                CALL GETARG ( 7, DATE_EOP_REF   )
              ELSE
                CALL CLRCH ( DATE_EOP_REF )
           END IF
           IF ( IARGC() .GE. 8 ) THEN
                CALL GETARG ( 8, DATE_BEG )
              ELSE IF ( EOP_FMT(1:4) .NE. 'NULL' ) THEN
                DATE_BEG = '1950.01.01'
              ELSE
                CALL CLRCH ( DATE_BEG )
           END IF
           IF ( IARGC() .GE. 9 ) THEN
                CALL GETARG ( 9, DATE_END )
              ELSE IF ( EOP_FMT(1:4) .NE. 'NULL' ) THEN
                DATE_END = '2049.12.31'
              ELSE
                CALL CLRCH ( DATE_END )
           END IF
!
! -------- Collect the argument string
!
           ARG_STR = 'eop_alignment '// &
     &               TRIM(FILSOL)//' '// &
     &               TRIM(FILNN)//' '// &
     &               TRIM(FILSTA_REF)//' '// &
     &               TRIM(FILVEL_REF)//' '// &
     &               TRIM(EOP_FMT)//' '// &
     &               TRIM(FILEOP)//' '// &
     &               TRIM(DATE_EOP_REF)//' '// &
     &               TRIM(DATE_BEG)//' '// &
     &               TRIM(DATE_END)
           IF ( IARGC() .GE. 10 ) THEN
                CALL GETARG ( 10, STR )
                IF ( INDEX ( STR, '.' ) < 1 ) STR = TRIM(STR)//'.0'
                READ ( UNIT=STR, FMT='(F10.4)' ) MAX_SIG_MAS
                MAX_SIG = MAX_SIG_MAS*MAS__TO__RAD
                ARG_STR = TRIM(ARG_STR)//' '//TRIM(STR)
              ELSE
                IVRB = 0
                ARG_STR = TRIM(ARG_STR)//' 0'
           END IF
           IF ( IARGC() .GE. 11 ) THEN
                CALL GETARG ( 11, STR )
                IF ( INDEX ( STR, '.' ) < 1 ) STR = TRIM(STR)//'.0'
                READ ( UNIT=STR, FMT='(F10.4)' ) NSIG
                ARG_STR = TRIM(ARG_STR)//' '//TRIM(STR)
              ELSE
                NSIG = 0.0D0
                ARG_STR = TRIM(ARG_STR)//' 0'
           END IF
           IF ( IARGC() .GE. 12 ) THEN
                CALL GETARG ( 12, STR )
                CALL CHIN   ( STR, IVRB )
                ARG_STR = TRIM(ARG_STR)//' '//TRIM(STR)
              ELSE
                IVRB = 0
                ARG_STR = TRIM(ARG_STR)//' 0'
           END IF
           IF ( EOP_FMT == 'INIT' .OR. EOP_FMT(1:4) == 'NULL' ) THEN
                IF ( IARGC() .GE. 6 ) THEN
                     CALL GETARG ( 6, STR )
                     CALL CHIN   ( STR, IVRB )
                   ELSE 
                     IVRB = 0
                END IF
           END IF
        ELSE
           WRITE ( 6, '(A)' ) 'Usage: eop_alignment <generic_solution_name> '// &
     &                        '<nn-cons-list> <ref_cat_sta> <ref_cat_vel> '// &
     &                        '<eop_fmt|init|null> <external_EOP_file|verb> ref_date '// &
     &                        '[date_beg] [date_end] [max_sig_mas] [nsig ] [ivrb]'
           CALL EXIT ( 1 )
      END IF
!
! --- Extract solution name from the input arguments
!
      CALL CLRCH ( SOLNAME )
      SOLNAME = FILSOL
      IL = LINDEX ( SOLNAME, '/' )
      IF ( IL .GT. 0 ) THEN
           CALL CLRCH  ( SOLNAME(1:IL) )
           CALL CHASHL ( SOLNAME )
      END IF
      IP = INDEX ( SOLNAME, '.' )
      IF ( IP .GT. 0 ) CALL CLRCH  ( SOLNAME(IP:) )
!
      IF ( .NOT. ( EOP_FMT .EQ. 'INIT' .OR. EOP_FMT(1:4) .EQ. 'NULL' ) ) THEN
!
! -------- Parse dates of the reference date of the VLBI solution and 
! -------- start and stop dates of the range for the EOP series for computation
! -------- of the mean bias and the rate of the external EOP time series
!
           IUER = -1
           CALL DATE_TO_TIME ( DATE_EOP_REF, MJD_EOP_REF, TAI_EOP_REF, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 8202, -1, 'EOP_ALIGNMENT', 'Error in parsing '// &
     &              'the reference date' ) 
                CALL EXIT ( 1 )
           END IF
           JD_REF_DATE = MJD_SEC_TO_JD ( MJD_EOP_REF, TAI_EOP_REF )
           REF_DATE = 2000.0D0 + (JD_REF_DATE - J2000__JD - 0.5D0)/JYEAR__DAYS
!
           IUER = -1
           CALL DATE_TO_TIME ( DATE_BEG, MJD, SEC, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 8203, -1, 'EOP_ALIGNMENT', 'Error in parsing '// &
     &              'the EOP begin date' ) 
                CALL EXIT ( 1 )
           END IF
           JD_EOP_BEG = MJD_SEC_TO_JD ( MJD, SEC )
!
           IUER = -1
           CALL DATE_TO_TIME ( DATE_END, MJD, SEC, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 8204, -1, 'EOP_ALIGNMENT', 'Error in parsing '// &
     &              'the EOP stop date' ) 
                CALL EXIT ( 1 )
           END IF
           JD_EOP_END = MJD_SEC_TO_JD ( MJD, SEC )
      END IF
!
! --- Check: whether the input reference files exit
!
      INQUIRE ( FILE=FILSTA_REF, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 8205, -1, 'EOP_ALIGNMENT', 'File with '// &
     &         ' reference site positions '//TRIM(FILSTA_REF)// &
     &         ' was not found' )
           CALL EXIT ( 1 )
      END IF
      INQUIRE ( FILE=FILVEL_REF, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 8206, -1, 'EOP_ALIGNMENT', 'File with '// &
     &         ' reference site velocities '//TRIM(FILVEL_REF)// &
     &         ' was not found' )
           CALL EXIT ( 1 )
      END IF
!
! --- Build file names
!
      FILEOB = FILSOL(1:I_LEN(FILSOL))//'.eob'
      FILERM = FILSOL(1:I_LEN(FILSOL))//'.erm'
      FILHEO = FILSOL(1:I_LEN(FILSOL))//'.heo'
      FILAPR = FILSOL(1:I_LEN(FILSOL))//'.apr'
      FILSTA = FILSOL(1:I_LEN(FILSOL))//'.sta'
      FILVEL = FILSOL(1:I_LEN(FILSOL))//'.vel'
      FILCNT = FILSOL(1:I_LEN(FILSOL))//'.cnt'
      ID = LINDEX ( FILSOL, '/' ) + 1
      FIL_ERM_CNS_OUT = '/tmp/'//FILSOL(ID:I_LEN(FILSOL))//'_erm_cns.cnt'
      IF ( EOP_FMT == 'NULL_ERM' ) THEN
           FIL_NN_CNS_OUT  = '/tmp/'//FILSOL(ID:I_LEN(FILSOL))//'_zero_net_sta_cns.cnt'
         ELSE
           FIL_NN_CNS_OUT  = '/tmp/'//FILSOL(ID:I_LEN(FILSOL))//'_net_sta_cns.cnt'
      END IF
!
! --- Check: whether input files exit
!
      INQUIRE ( FILE=FILCNT, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 8207, -1, 'EOP_ALIGNMENT', 'Control file '// &
     &          FILCNT(1:I_LEN(FILCNT))//' was not found' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL RD_TEXT ( FILCNT, MP, BUF_CNT, N_CNT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 8208, -1, 'EOP_ALIGNMENT', 'Error in reading '// &
     &         'control file '//FILCNT )
           CALL EXIT ( 1 )
      END IF
!
      FL_ARCS = .FALSE.
      NSES    = 0
      DO 410 J1=1,N_CNT
         IF ( BUF_CNT(J1)(1:1) == '*' ) GOTO 410
         CALL EXWORD ( BUF_CNT(J1), MIND, LIND, IND, CHAR(32)//CHAR(9), IUER )
         IF ( EOP_FMT == 'INIT' .OR. EOP_FMT(1:4) == 'NULL' ) THEN
              IF ( BUF_CNT(J1)(IND(1,1):IND(2,1)) == 'ID' ) THEN
!
! ---------------- Get the solution ID
!
                  SOL_ID = BUF_CNT(J1)(IND(1,2):IND(2,2)) 
                ELSE IF ( BUF_CNT(J1)(IND(1,1):IND(2,1)) == 'VTD_CONF' ) THEN
!
! --------------- Get VTD control file. We put it to in FILAPR
!
                   FILAPR = BUF_CNT(J1)(IND(1,2):IND(2,2)) 
              END IF
         END IF
         IF ( BUF_CNT(J1)(1:5) == '$ARCS' ) THEN
              FL_ARCS = .TRUE.
         END IF
         IF ( FL_ARCS .AND. BUF_CNT(J1)(IND(1,1):IND(2,1)) == 'OBS' ) THEN
              NSES = NSES + 1
              BUF_SES(NSES) = BUF_CNT(J1)
         END IF
         IF ( FL_ARCS .AND. BUF_CNT(J1)(IND(1,1):IND(2,1)) == 'ARCFILE' ) THEN
              FILSES = BUF_CNT(J1)(IND(1,2):IND(2,2)) 
              INQUIRE ( FILE=FILSES, EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   CALL ERR_LOG ( 8207, -1, 'EOP_ALIGNMENT', 'Session file '// &
     &                  FILSES(1:I_LEN(FILSES))//' specfied as ARCFILE in '// &
     &                 'the control file '//FILCNT(1:I_LEN(FILCNT))// &
     &                 ' was not found' )
                  CALL EXIT ( 1 )
              END IF
!
              IUER = -1
              CALL RD_TEXT ( FILSES, MP, BUF_TMP, NSA, IUER )
              IF ( IUER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 8208, -1, 'EOP_ALIGNMENT', 'Error in reading '// &
     &                 'session file '//FILSES )
                   CALL EXIT ( 1 )
              END IF
!
              DO 420 J2=1,NSA
                 IF ( BUF_TMP(J2)(1:1) == '*' ) GOTO 420
                 IUER = -1
                 CALL EXWORD ( BUF_TMP(J2), MIND, LIND, IND, CHAR(32)//CHAR(9), IUER )
                 IF ( LIND < 2 ) GOTO 420
                 IF ( BUF_TMP(J2)(IND(1,1):IND(2,1)) == 'OBS' ) THEN
                      NSES = NSES + 1
                      BUF_SES(NSES) = BUF_TMP(J2)
                 END IF
 420          CONTINUE 
         END IF
 410  CONTINUE 
      IF ( NSES > 0 ) THEN
           CALL SORT_FAST_CH ( NSES, BUF_SES )
      END IF
      IF ( EOP_FMT == 'INIT' .OR. EOP_FMT(1:4) == 'NULL' ) THEN
           CALL CLRCH ( FILSTA )
           CALL CLRCH ( FILVEL )
        ELSE
!
! -------- The VLBI solutions was made and results are available. 
! -------- Check file names
!
           INQUIRE ( FILE=FILEOB, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                IUER = -1
                CALL ERR_LOG ( 8209, -1, 'EOP_ALIGNMENT', 'EOB file '// &
     &               FILEOB(1:I_LEN(FILEOB))//' was not found' )
                CALL EXIT ( 1 )
           END IF
!
           INQUIRE ( FILE=FILAPR, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                IUER = -1
                CALL ERR_LOG ( 8210, -1, 'EOP_ALIGNMENT', 'File with apriori model'// &
     &               FILAPR(1:I_LEN(FILAPR))//' was not found' )
                CALL EXIT ( 1 )
           END IF
           CALL CLRCH ( SOL_ID )
      END IF
!
! --- Check whether the constraint file exists
!
      INQUIRE ( FILE=FILNN, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           IUER = -1
           CALL ERR_LOG ( 8211, -1, 'EOP_ALIGNMENT', 'no-net-xxx file '// &
     &          FILNN(1:I_LEN(FILNN))//' was not found' )
           CALL EXIT ( 1 )
      END IF
!
! --- Determine whether the empirical Earth Rotation Model has been estimated.
! --- Check the size of these files. If the ERM file has size smaller than 
! --- the EOP file, that means ERM has not been estimated.
!
      IF ( EOP_FMT == 'INIT' .OR. EOP_FMT(1:4) == 'NULL' ) THEN
           INQUIRE ( FILE=FILERM, EXIST=LEX )
           IF ( LEX ) THEN
                INP_EOP_FMT = 'ERM'
              ELSE
                INP_EOP_FMT = EOP_FMT 
           END IF
         ELSE
           IS = FILE_INFO ( TRIM(FILEOB)//CHAR(0), UNIX_DATE, SIZE_EOB_I8 )
           IS = FILE_INFO ( TRIM(FILERM)//CHAR(0), UNIX_DATE, SIZE_ERM_I8 )
           IF ( SIZE_ERM_I8 > SIZE_EOB_I8 ) THEN
!
! ------------- ERM-file is bigger. ERM was estimiated
!
                INP_EOP_FMT = 'ERM'
              ELSE
!
! ------------- ERM-file is smaller. That means ERM was not estimiated. It contains
! ------------- the place holder
!
                INP_EOP_FMT = 'EOB'
           END IF
      END IF
!
      CALL TRAN ( 11, EOP_FMT, EOP_FMT )
      IF ( EOP_FMT(1:8) .EQ. 'IERS_C04' ) THEN
!
! -------- Read IERS C04 file
!
           IUER = -1
           CALL RD_IERS_C04 ( FILEOP, MP, NEXT, JD_EXT, XP_EXT, XP_ERR, YP_EXT, &
     &                        YP_ERR, U1_EXT, U1_ERR, DPSI_EXT, DPSI_ERR, &
     &                        DEPS_EXT, DEPS_ERR, CH_FLAG, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 8212, -1, 'EOP_ALIGNMENT', 'Error in attempt '// &
     &              'to read EOP files in IERS C04 format' )
                CALL EXIT ( 4 )
           END IF
         ELSE IF ( EOP_FMT(1:11) .EQ. 'USNO_FINALS' ) THEN
!
! -------- Parse finals.data file
!
           IUER  = -1
           CALL RD_FINALS ( FILEOP, MP, NEXT, JD_EXT, XP_EXT, XP_ERR, YP_EXT, &
     &                      YP_ERR, U1_EXT, U1_ERR, DPSI_EXT, DPSI_ERR, &
     &                      DEPS_EXT, DEPS_ERR, CH_FLAG, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 8213, -1, 'EOP_ALIGNMENT', 'Error in attempt '// &
     &              'to read EOP files in USNO finals format' )
                CALL EXIT ( 4 )
           END IF
         ELSE IF ( EOP_FMT == 'INIT' ) THEN
           CONTINUE 
         ELSE IF ( EOP_FMT == 'NULL_ERM' ) THEN
           CONTINUE 
         ELSE IF ( EOP_FMT == 'NULL_STA' ) THEN
           CONTINUE 
         ELSE
           IUER = -1
           CALL ERR_LOG ( 8214, -1, 'EOP_ALIGNMENT', 'Error in the 3-rd argument: '// &
     &          EOP_FMT//' -- one of IERS_C04, or USNO_FINALS, or NULL was expected' )
           CALL EXIT ( 5 )
      END IF
!
      DO 430 J3=1,NEXT
!
! ------ Transform arguments from Julian days to Julian years
!
         YR_EXT(J3) = 2000.0 + ( JD_EXT(J3) - 2451545.0D0 )/YEAR__TO__DAY
 430  CONTINUE
!
      IF ( INP_EOP_FMT == 'EOB' .AND. EOP_FMT(1:4) .NE. 'NULL' ) THEN
!
! -------- Read EOP series obtained in the Solve solution
!
           IUER = -1
           CALL READ_EOB ( FILEOB, M_HEAD, NHEAD, HEAD_BUF, MP, NP, EOP, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 8215, IUER, 'EOP_ALIGNMENT', 'Error in '// &
     &              'reading EOB file '//TRIM(FILEOB) )
                CALL EXIT ( 1 )
           END IF
           UZT_MODEL = UZT__NONE
           UZT_USE   = UZT__NONE
        ELSE IF ( INP_EOP_FMT == 'ERM' .AND. &
     &            .NOT. ( EOP_FMT(1:4) == 'NULL' .OR. EOP_FMT == 'INIT' ) ) THEN
!
! -------- Read EOP expansion into the B-spline basis
!
           IUER = -1
           CALL PARSE_ERM ( FILERM, ERM, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 8216, IUER, 'EOP_ALIGNMENT', 'Error in '// &
     &              'reading ERM file '//FILERM )
                CALL EXIT ( 1 )
           END IF
           UZT_MODEL = ERM%UZM 
           UZT_USE   = ERM%UZU 
!
           IF ( DABS( (ERM%MJD_REF_CNS - MJD_EOP_REF)*86400.0D0 + &
     &                (ERM%TAI_REF_CNS - TAI_EOP_REF)             ) > EPS_EOP ) THEN
                CALL CLRCH ( STR )
!
                IUER = 0
                STR = MJDSEC_TO_DATE ( ERM%MJD_REF_CNS, ERM%MJD_REF_CNS, IUER )
                IUER = -1
                CALL ERR_LOG ( 8217, IUER, 'EOP_ALIGNMENT', 'Discrepancy between '// &
     &              'the requested EOP reference time '//TRIM(DATE_EOP_REF)// &
     &              ' and the reference time for EOP shift constrain in ERM model '// &
     &              TRIM(STR)//' is different' )
                CALL EXIT ( 1 )
           END IF
!
! -------- Read high frequency Earth orientaion parameters
!
           INQUIRE ( FILE=FILHEO, EXIST=LEX )
           IF ( LEX ) THEN
                IUER = -1
                CALL READ_HEO ( FILHEO, M_HEO, L_HEO, HEO, NAME_HEO, &
     &                          HEO_EPOCH_SEC, IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 8218, IUER, 'EOP_ALIGNMENT', 'Error in '// &
     &                   'reading HEO file '//FILHEO )
                     CALL EXIT ( 1 )
                END IF
              ELSE
                L_HEO = 0
           END IF
!
           TIM_STEP = MIN ( ERM%TIM(2,1) - ERM%TIM(1,1), ERM%TIM(2,2) - ERM%TIM(1,2), &
     &                      ERM%TIM(2,3) - ERM%TIM(1,3) )
           IUER = -1
           CALL ERM_TO_EOP ( ERM, TIM_STEP, UZT_MODEL, UZT_USE, &
     &                       MP, NP, EOP, L_HEO, HEO, HEO_EPOCH_SEC, &
     &                       IVRB, IUER )
           IF ( IUER .NE. 0 ) STOP 'Error in transforming ERM to EOP'
           SOL_ID = ERM%SOL_ID
      END IF
!
      IF ( .NOT. ( EOP_FMT == 'INIT' .OR. EOP_FMT(1:4) == 'NULL' ) .AND. &
     &     ( INP_EOP_FMT == 'EOB' .OR. INP_EOP_FMT == 'ERM' )       ) THEN
!
! -------- Extract solution ID from the EOP time series
!
           DO 440 J4=1,NHEAD
              IF ( HEAD_BUF(J4)(1:17) == '# VLBI VTD/pSolve' ) THEN
                   SOL_ID = HEAD_BUF(J4)(28:)
              END IF
              IF ( HEAD_BUF(J4)(1:7) == 'ERM SOL' ) THEN
                   SOL_ID = HEAD_BUF(J4)(20:)
              END IF
              IF ( ILEN(SOL_ID) == 0 .AND. HEAD_BUF(J4)(1:13) == '# Spool file:' ) THEN
                   SOL_ID = HEAD_BUF(J4)(1:15)
                   IF ( ILEN(SOL_ID) > 5 ) THEN
                        CALL CLRCH ( SOL_ID(ILEN(SOL_ID)-3:ILEN(SOL_ID)-3) )
                   END IF
              END IF
 440       CONTINUE 
        ELSE
           SOL_ID = 'none'
           FILSOL = 'none'
           FILEOP = 'none'
      END IF
!
! --- Read the file with stations that participated in no-net constraints
!
      IUER = -1
      CALL READ_NN ( FILNN, MP, BUF, M_STA, LN_STA, CN_STA, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 8219, IUER, 'EOP_ALIGNMENT', 'Error in '// &
     &         'reading file with no-net constraint stations' )
           CALL EXIT ( 1 ) 
      END IF
      IF ( IVRB > 0 ) THEN
           IF ( ILEN(SOL_ID) == 0 ) CALL CLRCH ( SOL_ID )
           IF ( EOP_FMT == 'INIT' ) THEN
                WRITE ( 6, 110 ) TRIM(ARG_STR), TRIM(GET_VERSION()), &
     &                           TRIM(SOL_ID), 'not available', &
     &                           'not available', LN_STA
              ELSE IF ( EOP_FMT(1:4) == 'NULL' ) THEN
                WRITE ( 6, 110 ) TRIM(ARG_STR), TRIM(GET_VERSION()), &
     &                           TRIM(SOL_ID), 'not available', &
     &                           'not available', LN_STA
              ELSE
                WRITE ( 6, 110 ) TRIM(ARG_STR), TRIM(GET_VERSION()), &
     &                           TRIM(SOL_ID), TRIM(FILSOL), &
     &                           TRIM(FILEOP), LN_STA
           END IF
      END IF
 110  FORMAT ( '* '/ &
     &         '* ',                      A/ &
     &         '* ',                      A/ &
     &         '* VLBI solution ID:    ', A/ &
     &         '* Solution spool file: ', A/ & 
     &         '* EOP series:          ', A/ &
     &         '* Number of stations used in constraints: ', I3 )
      NO = 0
      NO = NO + 1 ; OUT(NO) = '*'
      NO = NO + 1 ; OUT(NO) = '* '//TRIM(ARG_STR)
      NO = NO + 1 ; OUT(NO) = '* '//TRIM(GET_VERSION())
      NO = NO + 1 ; OUT(NO) = '* VLBI solution ID:    '//TRIM(SOL_ID)
      IF ( EOP_FMT == 'INIT' ) THEN
           NO = NO + 1 ; OUT(NO) = '* No solution was made'
         ELSE IF ( EOP_FMT(1:4) == 'NULL' ) THEN
           NO = NO + 1 ; OUT(NO) = '* No solution was made'
         ELSE 
           NO = NO + 1 ; OUT(NO) = '* Solution spool file: '//TRIM(FILSOL)
           NO = NO + 1 ; OUT(NO) = '* EOP series:          '//TRIM(FILEOP)
      END IF
      NO = NO + 1 ; OUT(NO) = '* Constraint file was generated on '//GET_CDATE()
      WRITE ( UNIT=STR(1:3), FMT='(I3)' ) LN_STA
      NO = NO + 1 ; OUT(NO) = '* Number of stations used in constraints: '//TRIM(STR)
      NO = NO + 1 ; OUT(NO) = '*'
!
! --- Read the a priori or VTD control file
!
      IUER = -1
      CALL RD_TEXT ( FILAPR, MP, BUF_APR, NA, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 8220, IUER, 'EOP_ALIGNMENT', 'Error in '// &
     &         'reading file with apriori models '//FILAPR )
           CALL EXIT ( 1 ) 
      END IF
!
      IF ( BUF_APR(1)(1:LEN(APR__LABEL))     .NE. APR__LABEL     .AND. &
     &     BUF_APR(1)(1:LEN(VTD_CNF__LABEL)) .NE. VTD_CNF__LABEL       ) THEN
!
           IUER = -1
           CALL ERR_LOG ( 8221, IUER, 'EOP_ALIGNMENT', 'File with '// &
     &         'apriori models '//TRIM(FILAPR)//' has an unsupported '// &
     &         'format. The first line is '//TRIM(STR)//' while '// &
     &          APR__LABEL//' was expected' )
           CALL EXIT ( 1 ) 
      END IF
!
! --- Initialization
!
      FILSTA_APR = 'Undefined'
      FILVEL_APR = 'Undefined'
      FILSRC_APR = 'Undefined'
!
! --- Extract the a priori station position catalogue, station velocity catalogue,
! --- and source positino catalogue
!
      DO 450 J5=1,NA
         CALL EXWORD ( BUF_APR(J5), MIND, LIND, IND, &
     &                 CHAR(0)//CHAR(32)//CHAR(9), IUER )
         IF ( BUF_APR(J5)(IND(1,1):IND(2,1)) == 'VTD_FINAM_STACOO:'    .OR. &
     &        BUF_APR(J5)(IND(1,1):IND(2,1)) == 'STATION_COORDINATES:'      ) THEN
              FILSTA_APR = BUF_APR(J5)(IND(1,2):IND(2,2)) 
         END IF
         IF ( BUF_APR(J5)(IND(1,1):IND(2,1)) == 'VTD_FINAM_STAVEL:' .OR. &
     &        BUF_APR(J5)(IND(1,1):IND(2,1)) == 'STATION_VELOCITIES:'    ) THEN
              FILVEL_APR = BUF_APR(J5)(IND(1,2):IND(2,2)) 
         END IF
         IF ( BUF_APR(J5)(IND(1,1):IND(2,1)) == 'VTD_FINAM_SOUCOO(1):' .OR. &
     &        BUF_APR(J5)(IND(1,1):IND(2,1)) == 'SOURCE_COORDINATES:'       ) THEN
              FILSRC_APR = BUF_APR(J5)(IND(1,2):IND(2,2)) 
         END IF
 450  CONTINUE 
!
      IF ( FILSTA_APR == 'Undefined' ) THEN
           CALL ERR_LOG ( 8222, -1, 'EOP_ALIGNMENT', 'Cannot find apriori '// &
     &         'station position file in the apriori file '//FILAPR )
           CALL EXIT ( 1 )
      END IF
!
      IF ( FILVEL_APR == 'Undefined' ) THEN
           CALL ERR_LOG ( 8223, -1, 'EOP_ALIGNMENT', 'Cannot find apriori '// &
     &         'velocity file in the apriori file '//FILAPR )
           CALL EXIT ( 1 )
      END IF
      INQUIRE ( FILE=FILSTA_APR, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 8224, -1, 'EOP_ALIGNMENT', 'Apriori station position catalogue '// &
     &          FILSTA_APR(1:I_LEN(FILSTA_APR))//' was not found' )
           CALL EXIT ( 1 )
      END IF
!
      INQUIRE ( FILE=FILVEL_APR, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 8225, -1, 'EOP_ALIGNMENT', 'A priori velocity catalogue '// &
     &          FILVEL_APR(1:I_LEN(FILVEL_APR))//' was not found' )
           CALL EXIT ( 1 )
      END IF
!
      INQUIRE ( FILE=FILSRC_APR, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 8226, -1, 'EOP_ALIGNMENT', 'A priori source position catalogue '// &
     &          FILSRC_APR(1:I_LEN(FILSRC_APR))//' was not found' )
           CALL EXIT ( 1 )
      END IF
!
! --- Read the reference station position catalogue
!
      IUER = -1
      CALL READ_STA ( FILSTA_REF, MP, BUF, M_STA, LC_REF_STA, &
     &                CC_REF_STA, COO_REF_STA, MJD_REF, TAI_REF, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 8227, -1, 'EOP_ALIGNMENT', 'Error in reading '// &
     &         'reference station position file '//FILSTA )
           CALL EXIT ( 1 )
      END IF 
!
! --- Read the reference station velocity catalogue
!
      IUER = -1
      CALL READ_VEL ( FILVEL_REF, MP, BUF, M_STA, LV_REF_STA, CV_REF_STA, VEL_REF_STA, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 8228, -1, 'EOP_ALIGNMENT', 'Error in reading '// &
     &         'reference station velocity file '//FILVEL_REF )
           CALL EXIT ( 1 )
      END IF 
!
! --- Read the a priori station position catalogue
!
      IUER = -1
      CALL READ_STA ( FILSTA_APR, MP, BUF, M_STA, LC_APR_STA, &
     &                CC_APR_STA, COO_APR_STA, MJD_APR, TAI_APR, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 8229, -1, 'EOP_ALIGNMENT', 'Error in reading '// &
     &         'a priori station position file '//FILSTA )
           CALL EXIT ( 1 )
      END IF 
!
! --- Read the a priori station velocity catalogue
!
      IUER = -1
      CALL READ_VEL ( FILVEL_APR, MP, BUF, M_STA, LV_APR_STA, CV_APR_STA, VEL_APR_STA, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 8230, -1, 'EOP_ALIGNMENT', 'Error in reading '// &
     &         'apriori station velocity file '//FILVEL_APR )
           CALL EXIT ( 1 )
      END IF 
!
      IF ( .NOT. ( EOP_FMT .EQ. 'INIT' .OR. EOP_FMT(1:4) .EQ. 'NULL' ) ) THEN
!
! -------- Read aposteriori station coordinates
!
           IUER = -1
           CALL READ_STA ( FILSTA, MP, BUF, M_STA, LC_STA, CC_STA, COO_STA, &
     &                     MJD_EST, TAI_EST, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 8231, -1, 'EOP_ALIGNMENT', 'Error in reading '// &
     &              'a posteriory station position file '//FILSTA )
                CALL EXIT ( 1 )
           END IF 
!
! -------- Read aposteriori station velocities
!
           IUER = -1
           CALL READ_VEL ( FILVEL, MP, BUF, M_STA, LV_STA, CV_STA, VEL_STA, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 8232, -1, 'EOP_ALIGNMENT', 'Error in reading '// &
     &              'a posteriori station velocity file '//FILVEL )
                CALL EXIT ( 1 )
           END IF 
        ELSE 
!
! -------- The aposteriori positions are the same as apriori in the NULL solution mode
!
           LC_STA  = LC_APR_STA
           CC_STA  = CC_APR_STA
           COO_STA = COO_APR_STA
           MJD_EST = MJD_APR 
           TAI_EST = TAI_APR
!
           LV_STA  = LV_APR_STA
           CV_STA  = CV_APR_STA
           VEL_STA = VEL_APR_STA
      END IF
      IF ( IVRB > 0 ) THEN
           IF ( .NOT. ( EOP_FMT .EQ. 'INIT' .OR. EOP_FMT(1:4) .EQ. 'NULL' ) ) THEN
                WRITE ( 6, '(A)' ) '* FILSTA: '//TRIM(FILSTA)
                WRITE ( 6, '(A)' ) '* FILVEL: '//TRIM(FILVEL)
                WRITE ( 6, '(A)' ) '* FILAPR: '//TRIM(FILAPR)
           END IF
           IF ( .NOT. EOP_FMT .EQ. 'INIT' ) THEN
                WRITE ( 6, '(A)' ) '* FILSTA_APR: '//TRIM(FILSTA_APR)
                WRITE ( 6, '(A)' ) '* FILVEL_APR: '//TRIM(FILVEL_APR)
           END IF
      END IF
      IF ( .NOT. ( EOP_FMT .EQ. 'INIT' .OR. EOP_FMT(1:4) .EQ. 'NULL' ) ) THEN
!
! -------- Compute shift and drift of the EOP differences
!
           IUER = -1
           CALL DIF_EOP ( NEXT, YR_EXT, XP_EXT, XP_ERR, YP_EXT, YP_ERR, U1_EXT, &
     &                    U1_ERR, NP, EOP, NSES, BUF_SES, &
     &                    REF_DATE, JD_EOP_BEG, JD_EOP_END, PHI, OME, MAX_SIG, &
     &                    NSIG, SOL_ID, EOP_FMT, IVRB, MP, NO, OUT, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 8233, IUER, 'EOP_ALIGNMENT', 'Error in '// &
     &              'computing EOP differences' )
                CALL EXIT ( 5 )
           END IF 
         ELSE
           NO = 0
           PHI = 0.0D0
           OME = 0.0D0
      END IF
!
      IF ( EOP_FMT .EQ. 'INIT' .OR. EOP_FMT(1:4) .EQ. 'NULL' ) THEN
           NO = NO + 1; OUT(NO)(1:3) = '** '
      END IF
      NO = NO + 1; OUT(NO)(1:) = '* '
      IF ( EOP_FMT .NE. 'INIT' .AND. IVRB > 0 ) THEN
           WRITE  ( 6, '(A)' ) '* '
           WRITE  ( 6, 120 ) PHI(1)*REA, PHI(2)*REA, PHI(3)*REA
           WRITE  ( 6, 130 ) OME(1)*REA, OME(2)*REA, OME(3)*REA
           WRITE  ( 6, 140 ) PHI
           WRITE  ( 6, 150 ) OME/86400.0D0/365.25D0
           WRITE  ( 6, '(A)' ) '*'
      END IF
      IF ( EOP_FMT .EQ. 'INIT' .OR. EOP_FMT(1:4) .EQ. 'NULL' ) THEN
           NO = NO + 1 ; OUT(NO)(1:) = '* '//TRIM(ARG_STR)
           NO = NO + 1 ; OUT(NO)(1:) = '* '//TRIM(GET_VERSION())
           NO = NO + 1 ; OUT(NO)(1:) = '* '//TRIM(EOP_FMT)//' Constraint file '// &
     &                                 'was generated on '//GET_CDATE()
           NO = NO + 1; OUT(NO)(1:) = '* '
      END IF
      IF ( .NOT. EOP_FMT .EQ. 'INIT' ) THEN
           NO = NO + 1; WRITE  ( OUT(NO), 120 ) PHI(1)*REA, PHI(2)*REA, PHI(3)*REA
           NO = NO + 1; WRITE  ( OUT(NO), 130 ) OME(1)*REA, OME(2)*REA, OME(3)*REA
           NO = NO + 1; WRITE  ( OUT(NO), 140 ) PHI
           NO = NO + 1; WRITE  ( OUT(NO), 150 ) OME/86400.0D0/365.25D0
 120       FORMAT ( '*       PHI  ', 3(F14.10,1X),  ' * phi in meters' )
 130       FORMAT ( '*       OME  ', 3(F14.10,1X),  ' * ome in m/yr' )
 140       FORMAT ( '*       PHI  ', 3(1PD14.6,1X), ' * phi in rad' )
 150       FORMAT ( '*       OME  ', 3(1PD14.6,1X), ' * ome in rad/s' )
           NO = NO + 1; OUT(NO)(1:) = '* '
      END IF
      IF ( EOP_FMT .NE. 'INIT' ) THEN
!
! -------- Compute the right hand side of constraint equations ...
!
           IUER = -1
           CALL COMP_RH ( M_PAR, EOP_FMT, &
     &                    PHI, OME, LN_STA, CN_STA,              &
     &                    LC_APR_STA, CC_APR_STA, COO_APR_STA,   &
     &                    LC_REF_STA, CC_REF_STA, COO_REF_STA,   &
     &                    LC_STA,     CC_STA,     COO_STA,       &
     &                    LV_APR_STA, CV_APR_STA, VEL_APR_STA,   &
     &                    LV_REF_STA, CV_REF_STA, VEL_REF_STA,   &
     &                    LV_STA,     CV_STA,     VEL_STA,       &
     &                    MJD_APR,    MJD_REF,    MJD_EST,       &
     &                    TAI_APR,    TAI_REF,    TAI_EST,       &
     &                    RH_VEC, IVRB, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 8234, IUER, 'EOP_ALIGNMENT', 'Error in '// &
     &              'computing right hand sides' )
                CALL EXIT ( 5 )
           END IF 
         ELSE 
           RH_VEC = 0.0D0
      END IF
!
! --- And write them in the screen
!
      IF ( IVRB > 0 ) THEN
           WRITE  ( 6, 160 ) RH_VEC
 160       FORMAT ( '* '/'* Vector of right hand side: '/'* '/ &
     &         '*    RIGHT_PART  ',3(F12.8,1X),'  NO  EXCEPT   \ * nnt_pos'/ &
     &         '*    RIGHT_PART  ',3(F12.8,1X),'  NO  EXCEPT   \ * nnr_pos'/ &
     &         '*    RIGHT_PART  ',3(F12.8,1X),'  NO  EXCEPT   \ * nnt_vel'/ &
     &         '*    RIGHT_PART  ',3(F12.8,1X),'  NO  EXCEPT   \ * nnr_vel'/ &
     &         '* ' )
      END IF
      NO_HEADER = NO
      NO = NO + 1 ; OUT(NO) = '  NO_NET_TRANSLATION_POSITION  GLOBAL  SIGMA   1.0D-4   ALL UNIFORM  \ * nnt_pos'
!
      NO = NO + 1
      WRITE ( UNIT=OUT(NO), FMT="('    RIGHT_PART  ',3(F12.8,1X),'   NO EXCEPT  \ * nnt_pos')" ) RH_VEC(1:3)
      NS = LN_STA/7 + 1
      IF ( NS*7 - LN_STA == 7 ) NS = NS - 1
      DO 460 J6=1,NS
         NO = NO + 1
         WRITE ( UNIT=OUT(NO), FMT=170 ) CN_STA((J6-1)*7+1:J6*7)
 170     FORMAT ( 6X, 7(A,1X),'\' )
 460  CONTINUE 
      OUT(NO)(70:70) = ' '
!
      NO = NO + 1 ; OUT(NO) = '  NO_NET_TRANSLATION_VELOCITY GLOBAL   SIGMA   1.0D-4   ALL UNIFORM  \ * nnt_vel'
      NO = NO + 1
      WRITE ( UNIT=OUT(NO), FMT="('    RIGHT_PART  ',3(F12.8,1X),' NO   EXCEPT  \ * nnt_vel')" ) RH_VEC(7:9)
      DO 470 J7=1,NS
         NO = NO + 1
         WRITE ( UNIT=OUT(NO), FMT=170 ) CN_STA((J7-1)*7+1:J7*7)
 470  CONTINUE 
      OUT(NO)(70:70) = ' '
!
      IF ( INP_EOP_FMT == 'ERM' .OR. EOP_FMT == 'INIT_ERM' .OR. EOP_FMT == 'NULL_ERM' ) THEN
           NO = NO + 1 ; OUT(NO) = '  NO_NET_ROTATION_POSITION    NO' 
         ELSE
           NO = NO + 1 ; OUT(NO) = '  NO_NET_ROTATION_POSITION    GLOBAL   SIGMA   1.0D-4       UNIFORM  \ * nnr_pos'
           NO = NO + 1
           WRITE ( UNIT=OUT(NO), FMT="('    RIGHT_PART  ',3(F12.8,1X),' NO   EXCEPT  \ * nnr_pos')" ) RH_VEC(4:6)
           NS = LN_STA/7 + 1
           IF ( NS*7 - LN_STA == 7 ) NS = NS - 1
           DO 480 J8=1,NS
              NO = NO + 1
              WRITE ( UNIT=OUT(NO), FMT=170 ) CN_STA((J8-1)*7+1:J8*7)
 480       CONTINUE 
           OUT(NO)(70:70) = ' '
      END IF
!
      IF ( INP_EOP_FMT == 'ERM' .OR. EOP_FMT == 'INIT_ERM' .OR. EOP_FMT == 'NULL_ERM' ) THEN
           NO = NO + 1 ; OUT(NO) = '  NO_NET_ROTATION_VELOCITY    NO'
        ELSE
           NO = NO + 1 ; OUT(NO) = '  NO_NET_ROTATION_VELOCITY    GLOBAL   SIGMA   1.0D-4       UNIFORM  \ * nnr_vel'
           NO = NO + 1
           WRITE ( UNIT=OUT(NO), FMT="('    RIGHT_PART  ',3(F12.8,1X),' NO   EXCEPT  \ * nnr_vel')" ) RH_VEC(10:12)
           DO 490 J9=1,NS
              NO = NO + 1
              WRITE ( UNIT=OUT(NO), FMT=170 ) CN_STA((J9-1)*7+1:J9*7)
 490       CONTINUE 
           OUT(NO)(70:70) = ' '
      END IF
!
      IF ( EOP_FMT .EQ. 'INIT' .OR. EOP_FMT(1:4) .EQ. 'NULL' ) THEN
           NO = NO + 1; OUT(NO)(1:2) = '* '
           NO = NO + 1; OUT(NO)(1:2) = '**'
      END IF
!
      IUER = -1
      CALL WR_TEXT ( NO, OUT, FIL_NN_CNS_OUT, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      IF ( IVRB > 0 ) THEN
           WRITE ( 6, '(A)' ) '* Created NN  right-hand side constraint file '// &
     &                        TRIM(FIL_NN_CNS_OUT)
      END IF
!
      IF ( ( INP_EOP_FMT == 'ERM' .OR. INP_EOP_FMT == 'NUL' ) .AND. &
     &     EOP_FMT .NE. 'NULL_STA' ) THEN
!
           LUN = GET_UNIT()
           OPEN ( UNIT=LUN, FILE=FIL_ERM_CNS_OUT, STATUS="UNKNOWN" )
           IF ( IVRB > 0 ) THEN
                WRITE  ( 6,   180 ) PHI, OME/YEAR__TO__SEC
           END IF
           DO 4100 J10=1,NO_HEADER
              WRITE  ( LUN, '(A)' ) TRIM(OUT(J10))
 4100      CONTINUE 
 180       FORMAT ( '*              CNS_MEAN_RTP   E1       ', 1PD13.6, ' \'/ &
     &              '*              CNS_MEAN_RTP   E2       ', 1PD13.6, ' \'/ &
     &              '*              CNS_MEAN_RTP   E3       ', 1PD13.6, ' \'/ &
     &              '*              CNS_RATE_RTP   E1       ', 1PD13.6, ' \'/ &
     &              '*              CNS_RATE_RTP   E2       ', 1PD13.6, ' \'/ &
     &              '*              CNS_RATE_RTP   E3       ', 1PD13.6, ' '/  &
     &              '* ' )
!
           WRITE  ( LUN, '(A)' ) '* Constraints imposed on the empircal Earth rotation model'
           WRITE  ( LUN, '(A)' ) '* and its time derivative'
           WRITE  ( LUN, '(A)' ) '*'
!
           WRITE  ( LUN, 190 ) PHI, OME/YEAR__TO__SEC
 190       FORMAT ( '               CNS_MEAN_RTP   E1       ', 1PD13.6, ' \'/ &
     &              '               CNS_MEAN_RTP   E2       ', 1PD13.6, ' \'/ &
     &              '               CNS_MEAN_RTP   E3       ', 1PD13.6, ' \'/ &
     &              '               CNS_RATE_RTP   E1       ', 1PD13.6, ' \'/ &
     &              '               CNS_RATE_RTP   E2       ', 1PD13.6, ' \'/ &
     &              '               CNS_RATE_RTP   E3       ', 1PD13.6, ' '/  &
     &              '* ' )
           CLOSE ( UNIT=LUN )
           WRITE ( 6, '(A)' ) '* Created ERM right-hand side constraint file '// &
     &                         TRIM(FIL_ERM_CNS_OUT)
      END IF
!
      END  !#!  EOP_ALIGNMENT  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE READ_NN ( FILNN, MP, BUF, M_STA, LN_STA, CN_STA, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine READ_NN reads the station list which participate in     *
! *   net-tranlation or net-rotation constraints.                        *
! *                                                                      *
! *  ### 15-MAY-2002    READ_NN    v1.0 (c)  L. Petrov  20-MAY-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MP, M_STA, LN_STA, IUER
      CHARACTER  FILNN*(*), BUF(MP)*(*), CN_STA(M_STA)*(*)
      INTEGER*4  NBUF, IB, IE, J1, J2, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Read the file into the buffer
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILNN, MP, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8235, IUER, 'READ_NN', 'Error  in reading NN-file '// &
     &          FILNN(1:I_LEN(FILNN)) )
           RETURN
      END IF
!
      LN_STA = 0
      DO 410 J1=1,NBUF
!
! ------ Bypasss the lines which cannot be the line with station names
!
         IF ( ILEN(BUF(J1)) .LE. 0 ) GOTO 410
         CALL CHASHL ( BUF(J1) )
         IF ( BUF(J1)(1:1) .EQ. '#' ) GOTO 410
         IF ( BUF(J1)(1:1) .EQ. '*' ) GOTO 410
         IF ( BUF(J1)(1:1) .EQ. '$' ) GOTO 410
         IF ( INDEX ( BUF(J1), 'NNT_' ) .GT. 0 ) GOTO 410
         IF ( INDEX ( BUF(J1), 'NNR_' ) .GT. 0 ) GOTO 410
         IF ( INDEX ( BUF(J1), ':' )    .GT. 0 ) GOTO 410
!
         IB = 1
         DO 420 J2=1,8
            IE = IB+7
            IF ( ILEN(BUF(J1)(IB:IE)) .GT. 0 ) THEN
                 LN_STA = LN_STA + 1
                 IF ( LN_STA .GT. M_STA ) THEN
                      CALL ERR_LOG ( 8236, IUER, 'READ_NN', 'The number of '// &
     &                    'station in NN-file '//FILNN(1:I_LEN(FILNN))// &
     &                    ' exceeded thje limit M_STA' )
                      RETURN
                 END IF
!
                 CN_STA(LN_STA) = BUF(J1)(IB:IE)
            END IF
            IB = IE + 2
 420     CONTINUE
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  READ_NN  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIF_EOP ( NEXT, YR_EXT, XP_EXT, XP_ERR, YP_EXT, YP_ERR, &
     &                     U1_EXT, U1_ERR, &
     &                     NP, EOP, NSES, BUF_SES, REF_DATE, &
     &                     JD_EOP_BEG, JD_EOP_END, &
     &                     PHI, OME, MAX_SIG, NSIG, SOL_ID, EOP_FMT, IVRB, &
     &                     MO, NO, OUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine DIF_EOP computes secular drift and shift at the         *
! *   reference epoch of the EOP series under consideratio with respect  *
! *   to the external EOP series.                                        *
! *                                                                      *
! *  ### 20-MAY-2002     DIF_EOP   v1.5 (c)  L. Petrov  17-OCT-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  NEXT, NP, NSES, IVRB, MO, NO, IUER
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'vtd.i'
      INCLUDE   'getpar.i'
      REAL*8     YR_EXT(NEXT), XP_EXT(NEXT), YP_EXT(NEXT), U1_EXT(NEXT), &
     &           XP_ERR(NEXT), YP_ERR(NEXT), U1_ERR(NEXT), PHI(3), OME(3)
      REAL*8     JD_EOP_BEG, JD_EOP_END, MAX_SIG, NSIG
      CHARACTER  SOL_ID*(*), EOP_FMT*(*), OUT(MO)*(*), BUF_SES(NSES)*(*)
      TYPE ( EOP__STRU ) ::   EOP(NP)
      REAL*8     REF_DATE
      CHARACTER  SESS_NAME(NP)*10
      INTEGER*4  MP, MIND
      PARAMETER  ( MP   = 32768 )
      PARAMETER  ( MIND =   128 )
      CHARACTER  DB_ECS(MP)*10
      REAL*8      PI, ARCSEC_TO_RAD, MAS_TO_RAD, MUAS_TO_RAD, &
     &            SEC_TO_RAD, MUSEC_TO_RAD, REA, K_FACTOR, HW_GAUSS
      PARAMETER ( PI = PI__NUM ) ! PI number
      PARAMETER ( ARCSEC_TO_RAD = PI/(180.D0*3600.D0)        )
      PARAMETER ( MAS_TO_RAD    = PI/(180.D0*3600.D0*1000.0) )
      PARAMETER ( MUAS_TO_RAD   = PI/(180.D0*3600.D0*1.D6)   )
      PARAMETER ( SEC_TO_RAD    = PI2/86400.D0               )
      PARAMETER ( MUSEC_TO_RAD  = PI2/(86400.D0*1.D6)        )
      PARAMETER ( REA           = 6378136.3D0                )
      PARAMETER ( K_FACTOR      = 1.002737909D0              )
      PARAMETER ( HW_GAUSS      = -60.0D0/365.25D0           )
      REAL*8     COEF(MP), WORK(MP), T8(MP), X8(MP), E8(MP), W8(MP), &
     &           MEAN_T, DR_VAL, SH_VAL, DR_SIG, SH_SIG, DW, CHI, CHI_NDG, &
     &           DATE_YEAR, JD_EOP, JD_MIN, JD_MAX, X9(MP), E9(MP)
      REAL*8     D8(MP), A8(MP)
      REAL*8     MJD_R8, TAI, E3, E3_DOT, E3_DT2 
      REAL*8     X1(88888), X2(88888)
      LOGICAL*1  FL_EOP_CNS
      INTEGER*4  J1, J2, J3, J4, J5, NEC, NZ, IXC, KP, MJD, IV(MP), &
     &           LIND, IND(2,MIND), IER
      REAL*8     FSPL8
      CHARACTER  STR*64, TITLE(3)*20
      DATA          TITLE &
     &           / &
     &             'X pole coord. (mas) ', &
     &             'Y pole coord. (mas) ', &
     &             'UT1 angle   (musec) ' &
     &           /
      CHARACTER  JD_TO_DATE*23
      INTEGER*4, EXTERNAL :: IXMN8, ILEN, I_LEN, LTM_DIF
!
      NEC = 0
      DO 410 J1=1,NSES
         CALL EXWORD ( BUF_SES(J1), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IUER )
         IF ( INDEX ( BUF_SES(J1), 'EOP_CONSTRAINT' ) > 0 ) THEN
              NEC = NEC + 1
              DB_ECS(NEC) = BUF_SES(J1)(IND(1,2):IND(2,2))
         END IF
 410  CONTINUE 
!
      DO 420 J2=1,3
         IV = 1
!
! ------ Compute parameters of interpolating cubic spline
!
         CALL ERR_PASS ( IUER, IER )
         IF ( J2 .EQ. 1 ) THEN
              CALL MAKE_SPLINE ( 1, NEXT, YR_EXT, XP_EXT, 0.0, 0.0, COEF, WORK, &
     &                           IER )
            ELSE IF ( J2 .EQ. 2 ) THEN
              CALL MAKE_SPLINE ( 1, NEXT, YR_EXT, YP_EXT, 0.0, 0.0, COEF, WORK, &
     &                           IER )
            ELSE IF ( J2 .EQ. 3 ) THEN
              CALL MAKE_SPLINE ( 1, NEXT, YR_EXT, U1_EXT, 0.0, 0.0, COEF, WORK, &
     &                           IER )
         END IF
         IF ( IER .NE. 0 ) THEN
              WRITE ( 6, * ) ' J2= ', J2, ' IUER= ', IUER, ' IER= ', IER
              CALL ERR_LOG ( 8281, IUER, 'DIF_EOP', 'Error in computation of '// &
     &                      'spline' )
              RETURN
         END IF
!
         KP = 0
         JD_MIN =  1.D9
         JD_MAX = -1.D9
!
         DO 430 J3=1,NP
!
! --------- Transform arguments from Julian days to Julian years
!
            DATE_YEAR = 2000.0 + ( EOP(J3)%MJD_EOP - J2000__MJD - 0.5D0 )/YEAR__TO__DAY
!
! --------- Check validity of the dates for comparison
!
            JD_EOP = EOP(J3)%MJD_EOP + 2400000.5D0
            IF ( JD_EOP .LT. JD_EOP_BEG  .OR. JD_EOP .GT. JD_EOP_END ) GOTO 430
            IF ( ILEN(EOP(J3)%DBNAME) > 0 .AND. NEC > 0 ) THEN
                 IF ( LTM_DIF ( 0, NEC, DB_ECS, EOP(J3)%DBNAME ) > 0 ) THEN
                      GOTO 430
                 END IF
            END IF
            IF ( JD_EOP .LT. JD_MIN ) JD_MIN = JD_EOP
            IF ( JD_EOP .GT. JD_MAX ) JD_MAX = JD_EOP
            STR = JD_TO_DATE ( JD_EOP, IER )
!
! --------- Interpolate EXT to the epoch of the tested EOP series
!
            IXC = IXMN8 ( NEXT, YR_EXT, DATE_YEAR )
            IF ( J2 .EQ. 1  .AND.  BTEST ( EOP(J3)%STATUS, XPL__GTP ) ) THEN
                 IF ( EOP(J3)%XPL_E .LE. MAX_SIG ) THEN
                      KP = KP + 1
                      X8(KP) = ( EOP(J3)%XPL_V - FSPL8 ( DATE_YEAR, NEXT, &
     &                           YR_EXT, XP_EXT, IXC, COEF ) )/MAS_TO_RAD
                      E8(KP) = EOP(J3)%XPL_E/MAS_TO_RAD
                 END IF
               ELSE IF ( J2 .EQ. 2 .AND. BTEST( EOP(J3)%STATUS, YPL__GTP ) ) THEN
                 IF ( EOP(J3)%YPL_E .LE. MAX_SIG ) THEN
                      KP = KP + 1
                      X8(KP) = ( EOP(J3)%YPL_V - FSPL8 ( DATE_YEAR, NEXT, &
     &                           YR_EXT, YP_EXT, IXC, COEF ) )/MAS_TO_RAD
                      E8(KP) = EOP(J3)%YPL_E/MAS_TO_RAD
                 END IF
               ELSE IF ( J2 .EQ. 3 .AND. BTEST( EOP(J3)%STATUS, U1__GTP ) ) THEN
                 IF ( EOP(J3)%U1_E .LE. MAX_SIG ) THEN
                      KP = KP + 1
                      X8(KP) = ( EOP(J3)%U1_V - FSPL8 ( DATE_YEAR, NEXT, &
     &                           YR_EXT, U1_EXT, IXC, COEF ) )/MUSEC_TO_RAD
                      E8(KP) = EOP(J3)%U1_E/MUSEC_TO_RAD
                 END IF
            END IF
            IF ( KP .GT. 0 ) THEN
                 T8(KP) = DATE_YEAR
                 W8(KP) = 1.D0/MAX(E8(KP),1.D-20)
                 IF ( E8(KP) .GT. 1.D4 ) THEN
                      KP = KP - 1
                 END IF
            END IF
 430     CONTINUE
!
         IF ( KP .GT. 2 ) THEN
!
! ----------- Compute weighted regression
!
              IF ( NSIG > 0.0D0 ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL DESIG8 ( KP, T8, X8, IV, NZ, NSIG, IER )
              END IF
!
              IF ( IVRB .GE. 2 ) THEN
                   CALL GAUSS_FILTER ( KP, HW_GAUSS, T8, X8, IV, E8, X9, E9 )
                   E9 = 0.01D0*E9
                   IF ( J2 == 1 ) THEN
                        CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'Xpole differences VLBI solution '// &
     &                                 TRIM(SOL_ID)//' - '//TRIM(EOP_FMT)//' (mas)' )
                     ELSE IF ( J2 == 2 ) THEN
                       CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'Ypole differences VLBI solution '// &
     &                                     TRIM(SOL_ID)//' - '//TRIM(EOP_FMT)//' (mas)' )
                     ELSE IF ( J2 == 3 ) THEN
                       CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'UT1 differences VLBI solution '// &
     &                                     TRIM(SOL_ID)//' - '//TRIM(EOP_FMT)//' (musec)' )
                   END IF
                   CALL DIAGI_SETDEF ( IER, 'DIAGI_UNIT', 'Time in years' )
                   CALL DIAGI_SETDEF ( IER, 'DIAGI_IBST',  2 )
                   CALL DIAGI_SETDEF ( IER, 'DIAGI_ICL2', 10 )
                   CALL DIAGI_2E ( KP, T8, X8, E8, KP, T8, X9, E9, IER )
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL RGRW8  ( KP, T8, X8, W8, IV, MEAN_T, DR_VAL, SH_VAL, &
     &                      DR_SIG, SH_SIG, IER )
              IF ( J2 .EQ. 1 ) THEN
                   STR(1:19)  = JD_TO_DATE ( JD_MIN, IER )
                   STR(31:49) = JD_TO_DATE ( JD_MAX, IER )
                   IF ( IVRB > 0 ) THEN
                        WRITE ( 6, '(A)' ) '* Date_start: '//STR(1:10)//'       '// &
     &                                     'Date_end:   '//STR(31:40)
                   END IF
                   NO = NO + 1
                   WRITE ( OUT(NO), '(A)' ) '* Date_start: '//STR(1:10)//'       '// &
     &                                      'Date_end:   '//STR(31:40)
!
                   STR(1:19)  = JD_TO_DATE ( J2000__JD + &
     &                         (REF_DATE-J2000__YR)*JYEAR__DAYS - 0.5D0, IER )
                   STR(31:49) = JD_TO_DATE ( J2000__JD + &
     &                         (MEAN_T-J2000__YR)*JYEAR__DAYS - 0.5D0, IER )
                   IF ( IVRB > 0 ) THEN
                        WRITE ( 6, '(A)' ) '* Ref_date:   '//STR(1:10)//'       '// &
     &                                     'Mean_date:  '//STR(31:40)
                   END IF
                   NO = NO + 1
                   WRITE ( OUT(NO), '(A)' ) '* Ref_date:   '//STR(1:10)//'       '// &
     &                                      'Mean_date:  '//STR(31:40)
              END IF
!
! ----------- Compute some other statistics
!
              IER = -2
              CALL DISP_WTR8 ( KP, T8, X8, W8, DR_VAL, &
     &                         SH_VAL + (T8(1)-MEAN_T)*DR_VAL, IV, &
     &                         DW, NZ, IER )
              CHI = 0.0D0
              DO 450 J5=1,KP
                 IF ( IV(J5) == 1 ) THEN
                      CHI = CHI + &
     &                      (( X8(J5) - (SH_VAL + (T8(J5)-MEAN_T)*DR_VAL))*W8(J5))**2
                 END IF
 450          CONTINUE
              CHI_NDG = CHI/(NZ-2)
              IF ( IVRB > 0 ) THEN
                   WRITE ( 6, 120 ) TITLE(J2), SH_VAL, SH_SIG, DR_VAL, DR_SIG
 120               FORMAT ( '* Diff. ', A,' shift:     ', F9.3, &
     &                      ' -+ ', F7.4, '  rate: ', F8.4, ' -+ ', F7.4 )
                   WRITE ( 6, 130 ) TITLE(J2), DW, CHI_NDG, MEAN_T
 130               FORMAT ( '*       ',A, ' w.r.m.s. = ', F9.3, '     Chi/ndg=',F7.2, ' Mean_epoch= ', F8.3 )
              END IF
!
              NO = NO + 1
              WRITE ( OUT(NO), 120 ) TITLE(J2), SH_VAL, SH_SIG, DR_VAL, DR_SIG
              NO = NO + 1
              WRITE ( OUT(NO), 130 ) TITLE(J2), DW, CHI_NDG, MEAN_T
!
! ----------- Compute shift and drift of EOP series (in rad and rad/yr)
!
              IF ( J2 .EQ. 1 ) THEN
                   PHI(2) = -( SH_VAL + DR_VAL*(REF_DATE - MEAN_T) )*MAS_TO_RAD
                   OME(2) = -DR_VAL*MAS_TO_RAD
                 ELSE IF ( J2 .EQ. 2 ) THEN
                   PHI(1) = -( SH_VAL + DR_VAL*(REF_DATE - MEAN_T) )*MAS_TO_RAD
                   OME(1) = -DR_VAL*MAS_TO_RAD
                 ELSE IF ( J2 .EQ. 3 ) THEN
                   PHI(3) = ( SH_VAL + DR_VAL*(REF_DATE - MEAN_T) )* MUSEC_TO_RAD/K_FACTOR
                   OME(3) = DR_VAL*MUSEC_TO_RAD/K_FACTOR
              END IF
            ELSE
              WRITE ( 6, 140 ) TITLE(J2), KP
 140          FORMAT ( A,' -- to few points: ',I5 )
              CALL ERR_LOG ( 8282, IUER, 'DIF_EOP', 'to few points' )
              RETURN
         END IF
 420  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DIF_EOP  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GAUSS_FILTER ( M, HW, TIM, ARR1, IV, SIG1, ARR2, SIG2 )
! ************************************************************************
! *                                                                      *
! *   ROutine  GAUSS_FILTER
! *                                                                      *
! *  ### 15-DEC-2022 GAUSS_FILTER  v1.0 (c)  L. Petrov  15-DEC-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  M
      REAL*8     HW, TIM(M), ARR1(M), SIG1(M), ARR2(M), SIG2(M)
      REAL*8     WIN, WIN_SUM, EXP_VAR, SIG_ACC, EXP_ACC
      INTEGER*4  IV(M), J1, J2
!
      DO 410 J1=1,M
         WIN_SUM  = 0.0D0
         ARR2(J1) = 0.0D0
         SIG_ACC  = 0.0D0
         EXP_ACC  = 0.0D0
         DO 420 J2=1,M
            IF ( IV(J2) > 0 ) THEN
                 EXP_VAR = -( (TIM(J2) - TIM(J1))/HW )**2
                 IF ( EXP_VAR .LT. -40.0D0 ) EXP_VAR = -40.0D0
                 WIN_SUM = WIN_SUM + DEXP(EXP_VAR)/SIG1(J1)
                 ARR2(J1) = ARR2(J1) + ARR1(J2)*DEXP(EXP_VAR)/SIG1(J1)
                 SIG_ACC = SIG_ACC + ( DEXP(EXP_VAR)*SIG1(J1) )**2
                 EXP_ACC = EXP_ACC +   DEXP(EXP_VAR)
            END IF
 420     CONTINUE 
         IF ( WIN_SUM > 1.D-20 ) THEN
              ARR2(J1) = ARR2(J1)/WIN_SUM
              SIG2(J1) = DSQRT ( SIG_ACC/EXP_ACC )
         END IF
 410  CONTINUE 
!      
      RETURN
      END  SUBROUTINE  GAUSS_FILTER 
!
! ------------------------------------------------------------------------
!
      SUBROUTINE COMP_RH ( M_PAR, EOP_FMT, &
     &                     PHI, OME, LN_STA, CN_STA,            &
     &                     LC_APR_STA,  CC_APR_STA,  COO_APR_STA, &
     &                     LC_REF_STA,  CC_REF_STA,  COO_REF_STA, & 
     &                     LC_EST_STA,  CC_EST_STA,  COO_EST_STA, &
!
     &                     LV_APR_STA,  CV_APR_STA,  VEL_APR_STA, &
     &                     LV_REF_STA,  CV_REF_STA,  VEL_REF_STA, &
     &                     LV_EST_STA,  CV_EST_STA,  VEL_EST_STA, &
!
     &                     MJD_APR_STA, MJD_REF_STA, MJD_EST_STA, &
     &                     TAI_APR_STA, TAI_REF_STA, TAI_EST_STA, &
!
     &                     RH_VEC, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  COMP_RH computes the vector of right hand side of      *
! *   no-net-transtaion/no-net-rotation constraints. The right-hand      *
! *   side of thes constraint equations requires:                        *
! *   1) EOP offset and rate of change coincide with external vectors    *
! *      of PHI and OME with the opposite sign.                          *
! *   2) Net translation with resepect to the the subset of reference    *
! *      stations be zero.                                               *
! *                                                                      *
! *  ### 20-MAY-2002    COMP_RH   v4.0 (c)  L. Petrov  10-MAR-2023 ###   *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INTEGER*4  M_PAR, LN_STA, LC_EST_STA, LV_EST_STA, LC_APR_STA, LV_APR_STA, &
     &           LC_REF_STA, LV_REF_STA, MJD_APR_STA, MJD_REF_STA, MJD_EST_STA, &
     &           IVRB, IUER
      CHARACTER  EOP_FMT*(*)
      CHARACTER  CN_STA(LN_STA)*(*), &
     &           CC_EST_STA(LC_EST_STA)*(*), &
     &           CV_EST_STA(LV_EST_STA)*(*), &
     &           CC_APR_STA(LC_APR_STA)*(*), CV_APR_STA(LV_APR_STA)*(*), &
     &           CC_REF_STA(LC_REF_STA)*(*), CV_REF_STA(LV_REF_STA)*(*)
      REAL*8     TAI_APR_STA, TAI_REF_STA, TAI_EST_STA
      REAL*8     PHI(3), OME(3), RH_VEC(12), &
     &           COO_EST_STA(3,LC_EST_STA), COO_APR_STA(3,LC_APR_STA), COO_REF_STA(3,LC_REF_STA), &
     &           VEL_EST_STA(3,LV_EST_STA), VEL_APR_STA(3,LV_APR_STA), VEL_REF_STA(3,LV_REF_STA)
      REAL*8       REA
      PARAMETER  ( REA = 6378136.3D0 )
      REAL*8     NOR_MAT(6,6), INV_MAT(6,6), TRA_RH_VEC(6)
      REAL*8     TRA_MAT(6,3), TRA_VEC(6), TRA_VEC_ACC(6), VEC3(3), VEC6(6), &
     &           SIT_RA_DIF(3), SIT_OA_DIF(3), VEL_RA_DIF(3), VEL_OA_DIF(3), &
     &           AVR_SIT_RA_DIF(3), AVR_SIT_OA_DIF(3), AVR_SIT_OA_ROT(3), &
     &           AVR_VEL_RA_DIF(3), AVR_VEL_OA_DIF(3), AVR_VEL_RA_ROT(3), AVR_VEL_OA_ROT(3)
      REAL*8     VECA(3), TVEC(6), NOR_TRA(6,6), AT_VEC(6), AT_VEC_ACC(6)
      REAL*8     TRA_ROT_VEC(3), TRA_ROT_VEC_ACC(3), TIM_DIF_APR, TIM_DIF_REF
      INTEGER*4  IC, IA, IN, IR, IV, IP, L_PAR, INV, IAV, IRV, N_SIT, N_VEL, &
     &           J1, J2, J3, J4, IER
      INTEGER*4, EXTERNAL :: LTM_DIF
      CHARACTER, EXTERNAL :: GET_CDATE*19
!
! --- Initialization
!
      TRA_VEC_ACC = 0.0D0
      AVR_SIT_RA_DIF = 0.0D0
      AVR_SIT_OA_DIF = 0.0D0
      AVR_SIT_OA_ROT = 0.0D0
      AT_VEC_ACC     = 0.0D0
      N_SIT = 0
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) '*'
      END IF
      NOR_MAT = 0.0D0
      TRA_ROT_VEC_ACC = 0.0D0
      DO 410 J1=1,LC_EST_STA
!
! ------ Check whether the J1-th station participated in the NN-constraints
! ------ on station positions
!
         IN = LTM_DIF ( 0, LN_STA, CN_STA, CC_EST_STA(J1)(1:8) )
         IF ( IN .LE. 0 ) GOTO 410 ! not? bypass it.
!
         IA = LTM_DIF ( 0, LC_APR_STA, CC_APR_STA, CC_EST_STA(J1)(1:8) )
         IF ( IA == 0 ) THEN
              CALL ERR_LOG ( 3271, IUER, 'COMP_RH', 'Cannot find station '// &
     &             CC_EST_STA(J1)(1:8)//' in the site position apriori catalogue' )
              RETURN 
         END IF 
!
         IR = LTM_DIF ( 0, LC_REF_STA, CC_REF_STA, CC_EST_STA(J1)(1:8) )
         IF ( IR == 0 ) THEN
              CALL ERR_LOG ( 3272, IUER, 'COMP_RH', 'Cannot find station '// &
     &             CC_EST_STA(J1)(1:8)//' in the site position reference catalogue' )
              RETURN 
         END IF 
!
         INV = LTM_DIF ( 0, LV_EST_STA, CV_EST_STA, CC_EST_STA(J1)(1:8) )
         IF ( INV == 0 ) THEN
              CALL ERR_LOG ( 3273, IUER, 'COMP_RH', 'Cannot find station '// &
     &             CC_EST_STA(J1)(1:8)//' in the site velocity estimate catalogue' )
              RETURN 
         END IF 
!
         IAV = LTM_DIF ( 0, LV_APR_STA, CV_APR_STA, CC_EST_STA(J1)(1:8) )
         IF ( IAV == 0 ) THEN
              CALL ERR_LOG ( 3274, IUER, 'COMP_RH', 'Cannot find station '// &
     &             CC_EST_STA(J1)(1:8)//' in the site velocity apriori catalogue' )
              RETURN 
         END IF 
!
         IRV = LTM_DIF ( 0, LV_REF_STA, CV_REF_STA, CC_EST_STA(J1)(1:8) )
         IF ( IRV == 0 ) THEN
              CALL ERR_LOG ( 3275, IUER, 'COMP_RH', 'Cannot find station '// &
     &             CC_EST_STA(J1)(1:8)//' in the site velocity reference catalogue' )
              RETURN 
         END IF 
!
         TIM_DIF_APR = ((MJD_EST_STA - MJD_APR_STA)*86400.D0 + (TAI_EST_STA -TAI_APR_STA))/YEAR__TO__SEC
         TIM_DIF_REF = ((MJD_EST_STA - MJD_REF_STA)*86400.D0 + (TAI_EST_STA -TAI_REF_STA))/YEAR__TO__SEC
         COO_APR_STA(1:3,IA) = COO_APR_STA(1:3,IA) + VEL_APR_STA(1:3,IAV)*TIM_DIF_APR
         COO_REF_STA(1:3,IR) = COO_REF_STA(1:3,IR) + VEL_REF_STA(1:3,IRV)*TIM_DIF_APR
!
         SIT_RA_DIF(1:3)     = COO_REF_STA(1:3,IR) - COO_APR_STA(1:3,IA) 
         SIT_OA_DIF(1:3)     = COO_EST_STA(1:3,J1) - COO_APR_STA(1:3,IA) 
         AVR_SIT_OA_DIF(1:3) = AVR_SIT_OA_DIF(1:3) + SIT_OA_DIF(1:3)
         AVR_SIT_RA_DIF(1:3) = AVR_SIT_RA_DIF(1:3) + SIT_RA_DIF(1:3)
         AVR_SIT_OA_ROT(1)   = AVR_SIT_OA_ROT(1) &
     &                           - COO_EST_STA(3,J1)/REA*SIT_OA_DIF(2) + COO_EST_STA(2,J1)/REA*SIT_OA_DIF(3)
         AVR_SIT_OA_ROT(2) = AVR_SIT_OA_ROT(2) &
     &                           + COO_EST_STA(3,J1)/REA*SIT_OA_DIF(1) - COO_EST_STA(1,J1)/REA*SIT_OA_DIF(3)
         AVR_SIT_OA_ROT(3) = AVR_SIT_OA_ROT(3) &
     &                           - COO_EST_STA(2,J1)/REA*SIT_OA_DIF(1) + COO_EST_STA(1,J1)/REA*SIT_OA_DIF(2)
         N_SIT = N_SIT + 1
         IF ( IVRB .GE. 2 ) THEN
              WRITE ( 6, 210 ) CC_EST_STA(J1), COO_EST_STA(1:3,J1)     - COO_APR_STA(1:3,IA), &
     &                                     COO_REF_STA(1:3,IR) - COO_APR_STA(1:3,IA) 
 210          FORMAT ( '*       SIT: ',A, ' OA_dif: ', 3(F8.4,1X), ' RA_dif: ', 3(F8.4,1X) )
         END IF
!
! ------ Compute a translation that is due to rotation of the station coordinate
! ------ vector by PHI
!
         CALL VM83 ( PHI, COO_EST_STA(1,J1), TRA_ROT_VEC )
!
! ------ Accummulate this translation
!
         TRA_ROT_VEC_ACC = TRA_ROT_VEC_ACC + TRA_ROT_VEC
!
         TRA_MAT = 0.0
!
! ------ Compute elements of transformation matrix for this station
!
         TRA_MAT(1,1) =  1.0D0
         TRA_MAT(5,1) = -COO_EST_STA(3,J1)/REA
         TRA_MAT(6,1) =  COO_EST_STA(2,J1)/REA
!
         TRA_MAT(2,2) =  1.0D0
         TRA_MAT(4,2) =  COO_EST_STA(3,J1)/REA
         TRA_MAT(6,2) = -COO_EST_STA(1,J1)/REA
!
         TRA_MAT(3,3) =  1.0D0
         TRA_MAT(4,3) = -COO_EST_STA(2,J1)/REA
         TRA_MAT(5,3) =  COO_EST_STA(1,J1)/REA
!
! ------ And update the normal matrix
!
         CALL DIAD_CVT ( 1.0D0, 6, TRA_MAT(1,1), 6, TRA_MAT(1,1), NOR_MAT )
         CALL DIAD_CVT ( 1.0D0, 6, TRA_MAT(1,2), 6, TRA_MAT(1,2), NOR_MAT )
         CALL DIAD_CVT ( 1.0D0, 6, TRA_MAT(1,3), 6, TRA_MAT(1,3), NOR_MAT )
!
         CALL MUL_MV_IV_V ( 6, 3, TRA_MAT, 3, SIT_RA_DIF, 6, AT_VEC, IER )
         AT_VEC_ACC = AT_VEC_ACC + AT_VEC
 410  CONTINUE
      IF ( N_SIT < 2 ) THEN
           CALL ERR_LOG ( 3276, IUER, 'COMP_RH', 'Less then two reference '// &
     &         'stations for positon constraints have been found' )
           RETURN 
      END IF
!
! --- Compute the right hand side of the transformation vector.
! --- For the translation part, it is sum if the mean difference reference 
! --- minus apriori and the mean contribution due to rotation with the opposite sign
! --- For the rotational part, it is the rotation due to EOP with the opposite sign
!
      IF ( EOP_FMT(1:4) == 'NULL' ) THEN
           RH_VEC(1:6) = AT_VEC_ACC(1:6)
         ELSE IF ( EOP_FMT == 'OLD' ) THEN
           TRA_VEC(1:3) = (AVR_SIT_RA_DIF(1:3) - TRA_ROT_VEC_ACC(1:3))/N_SIT
           TRA_VEC(4:6) = -PHI(1:3)*REA
           CALL MUL_MV_IV_V ( 6, 6, NOR_MAT, 6, TRA_VEC, 6, RH_VEC(1), IER )
         ELSE ! INIT or ERM
           TRA_VEC(1:3) = -TRA_ROT_VEC_ACC(1:3)/N_SIT
           TRA_VEC(4:6) = -PHI(1:3)*REA
           CALL MUL_MV_IV_V ( 6, 6, NOR_MAT, 6, TRA_VEC, 6, RH_VEC(1), IER )
           RH_VEC(1:6) = RH_VEC(1:6) + AT_VEC_ACC(1:6)
      END IF
!
! --- Initialzation
!
      AVR_VEL_RA_DIF = 0.0D0
      AVR_VEL_OA_DIF = 0.0D0
      AVR_VEL_RA_ROT = 0.0D0
      AVR_VEL_OA_ROT = 0.0D0
      AT_VEC_ACC     = 0.0D0
      TRA_VEC_ACC    = 0.0D0
      N_VEL   = 0
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) '*'
      END IF
      NOR_MAT = 0.0D0
      TRA_ROT_VEC_ACC = 0.0D0
!
! --- Now cycle over the stations which velocities were determined
!
      DO 430 J3=1,LV_EST_STA
!
! ------ Check whether the J3-th station participated in the NN-constraints
! ------ on station velocities
!
         IN = LTM_DIF ( 0, LN_STA, CN_STA, CV_EST_STA(J3)(1:8) )
         IF ( IN .LE. 0 ) GOTO 430
!
         IC = LTM_DIF ( 0, LC_EST_STA, CC_EST_STA, CV_EST_STA(J3)(1:8) )
         IF ( IC .LE. 0 ) THEN
              CALL ERR_LOG ( 3277, IUER, 'COMP_RH', 'Trap of internal '// &
     &            'control' )
              RETURN
         END IF
!
         IA = LTM_DIF ( 0, LV_APR_STA, CV_APR_STA, CV_EST_STA(J3)(1:8) )
         IF ( IA .LE. 0 ) THEN
              CALL ERR_LOG ( 3278, IUER, 'COMP_RH', 'Cannot find station '// &
     &             CV_EST_STA(J3)(1:8)//' in the site velocity apriori catalogue' )
              RETURN 
         END IF
!
         IR = LTM_DIF ( 0, LV_REF_STA, CV_REF_STA, CV_EST_STA(J3)(1:8) )
         IF ( IR .LE. 0 ) THEN
              CALL ERR_LOG ( 3279, IUER, 'COMP_RH', 'Cannot find station '// &
     &             CV_EST_STA(J3)(1:8)//' in the site velocity reference catalogue' )
              RETURN 
         END IF
         VEL_RA_DIF(1:3)     = VEL_REF_STA(1:3,IR) - VEL_APR_STA(1:3,IA) 
         VEL_OA_DIF(1:3)     = VEL_EST_STA(1:3,J3) - VEL_APR_STA(1:3,IA) 
!
! ------ Update accummulators
! ------ APR_VEL_RA_DIF -- average reference minus apriori
! ------ APR_VEL_OA_DIF -- average sum of  estimated minus apriori
! ------ APR_VEL_OA_ROT -- averave rotatin of the estimated minus apriori
!
         AVR_VEL_OA_DIF(1:3) = AVR_VEL_OA_DIF(1:3) + VEL_OA_DIF(1:3)
         AVR_VEL_RA_DIF(1:3) = AVR_VEL_RA_DIF(1:3) + VEL_RA_DIF(1:3)
         AVR_VEL_OA_ROT(1) = AVR_VEL_OA_ROT(1) &
     &                         - COO_EST_STA(3,IC)/REA*VEL_OA_DIF(2) + COO_EST_STA(2,IC)/REA*VEL_OA_DIF(3)
         AVR_VEL_OA_ROT(2) = AVR_VEL_OA_ROT(2) &
     &                         + COO_EST_STA(3,IC)/REA*VEL_OA_DIF(1) - COO_EST_STA(1,IC)/REA*VEL_OA_DIF(3)
         AVR_VEL_OA_ROT(3) = AVR_VEL_OA_ROT(3) &
     &                         - COO_EST_STA(2,IC)/REA*VEL_OA_DIF(1) + COO_EST_STA(1,IC)/REA*VEL_OA_DIF(2)
         N_VEL = N_VEL + 1
         IF ( IVRB .GE. 2 ) THEN
              WRITE ( 6, 220 ) CV_EST_STA(J3), VEL_EST_STA(1:3,J3) - VEL_APR_STA(1:3,IA), &
     &                                         VEL_APR_STA(1:3,IA) - VEL_REF_STA(1:3,IR)
 220          FORMAT ( '*       VEL: ',A, ' OA_dif: ', 3(F8.4,1X), ' AR_dif: ', 3(F8.4,1X) )
         END IF
!
! ------ Compute a translation that is due to rotation of the station coordinate
! ------ vector by PHI
!
         CALL VM83 ( OME, COO_EST_STA(1,IC), TRA_ROT_VEC )
!
! ------ Accummulate this translation
!
         TRA_ROT_VEC_ACC = TRA_ROT_VEC_ACC + TRA_ROT_VEC
!
         TRA_MAT = 0.0
!
! ------ Compute elements of the transformation matrix for this station
!
         TRA_MAT(1,1) =  1.0D0
         TRA_MAT(5,1) = -COO_EST_STA(3,IC)/REA
         TRA_MAT(6,1) =  COO_EST_STA(2,IC)/REA
!
         TRA_MAT(2,2) =  1.0D0
         TRA_MAT(4,2) =  COO_EST_STA(3,IC)/REA
         TRA_MAT(6,2) = -COO_EST_STA(1,IC)/REA
!
         TRA_MAT(3,3) =  1.0D0
         TRA_MAT(4,3) = -COO_EST_STA(2,IC)/REA
         TRA_MAT(5,3) =  COO_EST_STA(1,IC)/REA
!
! ------ And update the normal matrix
!
         CALL DIAD_CVT ( 1.0D0, 6, TRA_MAT(1,1), 6, TRA_MAT(1,1), NOR_MAT )
         CALL DIAD_CVT ( 1.0D0, 6, TRA_MAT(1,2), 6, TRA_MAT(1,2), NOR_MAT )
         CALL DIAD_CVT ( 1.0D0, 6, TRA_MAT(1,3), 6, TRA_MAT(1,3), NOR_MAT )
!
         CALL MUL_MV_IV_V ( 6, 3, TRA_MAT, 3, VEL_RA_DIF, 6, AT_VEC, IER )
         AT_VEC_ACC = AT_VEC_ACC + AT_VEC
 430  CONTINUE
      IF ( N_VEL < 2 ) THEN
           CALL ERR_LOG ( 3280, IUER, 'COMP_RH', 'Less then two reference '// &
     &         'stations for velocity constraints have been found' )
           RETURN 
      END IF
!
! --- Compute the right hand side of the transformation vector.
! --- For the translation part, it is the sum of the mean difference 
! --- "reference minus apriori" and the mean contribution due to 
! --- a rotation with the opposite sign.
! --- For the rotational part, it is the rotation due to EOP with the opposite sign
!
      IF ( EOP_FMT(1:4) == 'NULL' ) THEN
           RH_VEC(7:12)  = AT_VEC_ACC(1:6)
        ELSE IF ( EOP_FMT == 'OLD' ) THEN
           TRA_VEC(1:3) = (AVR_VEL_RA_DIF(1:3) - TRA_ROT_VEC_ACC(1:3))/N_VEL
           TRA_VEC(4:6) = -OME(1:3)*REA
           CALL MUL_MV_IV_V ( 6, 6, NOR_MAT, 6, TRA_VEC, 6, RH_VEC(7), IER )
        ELSE  ! INIT or ERM
!
! -------- Generate transtion/rotation vector for rotation
!
           TRA_VEC(1:3) = -TRA_ROT_VEC_ACC(1:3)/N_VEL
           TRA_VEC(4:6) = -OME(1:3)*REA
           CALL MUL_MV_IV_V ( 6, 6, NOR_MAT, 6, TRA_VEC, 6, RH_VEC(7), IER )
           RH_VEC(7:12) = RH_VEC(7:12) + AT_VEC_ACC(1:6)
      END IF
!
      IF ( IVRB > 0 ) THEN
           WRITE ( 6, 110 ) N_SIT, 'OA', AVR_SIT_OA_DIF
           WRITE ( 6, 110 ) N_SIT, 'AR', AVR_SIT_RA_DIF
           WRITE ( 6, 120 ) N_SIT, 'OA', AVR_SIT_OA_ROT
           WRITE ( 6, '(A)' ) '*'
           WRITE ( 6, 130 ) N_VEL, 'OA', AVR_VEL_OA_DIF
           WRITE ( 6, 130 ) N_VEL, 'AR', AVR_VEL_RA_DIF
           WRITE ( 6, 140 ) N_VEL, 'OA', AVR_VEL_OA_ROT
 110       FORMAT ( '*       N_SIT= ', I4, ' AVR_SIT_',A2,'_DIF = ', 3(F10.5, 1X), ' m'     )
 120       FORMAT ( '*       N_SIT= ', I4, ' AVR_SIT_',A2,'_ROT = ', 3(F10.5, 1X), ' m'     )
 130       FORMAT ( '*       N_VEL= ', I4, ' AVR_VEL_',A2,'_DIF = ', 3(F10.5, 1X), ' mm/yr' )
 140       FORMAT ( '*       N_VEL= ', I4, ' AVR_VEL_',A2,'_ROT = ', 3(F10.5, 1X), ' mm/yr' )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  COMP_RH  !#!#
