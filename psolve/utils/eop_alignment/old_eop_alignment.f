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
! *      Shift and drift of EOP series with respect to a reference is    *
! *      equivalent to a net-translation/rotation of station positions   *
! *      and velocity field. It can be written as:                       *
! *                                                                      *
! *      sum_i M_i * T = r_p                                             *
! *      sum_i M_i * D = r_v                                             *
! *                                                                      *
! *      where M_i -- is a matrix of dimension 3*6 for the ith station.  *
! *                                                                      *
! *       1  0  0    0   r3 -r2                                          *
! *       0  1  0  -r3   0   r1                                          *
! *       0  0  1   r2  -r1   0                                          *
! *                                                                      *
! *    ... station 2, station 3 ... station N_vel                        *
! *                                                                      *
! *     T -- 6-dimensional vector of transformation: translation and     *
! *          rotation;                                                   *
! *     D -- 6-dimensional vector of station velocities due to           *
! *          a transformation.                                           *
! *                                                                      *
! *     Summing is done over all stations. If the station had            *
! *   an episodic motion, then it is counted twice for site position     *
! *   constraints ant once for site velocity constraints.                *
! *                                                                      *
! *     In order to compute corrections to the right hand-side vectors   *
! *   R of the old solution which produced EOP series with shifts and    *
! *   drifts with respect to the external EOP series we compute the      *
! *   6-dimensional vectors R_p adn R_v:                                 *
! *                                                                      *
! *     Rp = Sum_i ( M_i * (Phi x R_i) )                                 *
! *     Rv = Sum_i ( M_i * (Ome x R_i) )                                 *
! *                                                                      *
! *                                                                      *
! *            /  Y_shift   \                / Y_rate   \                *
! *     Phi = |   X_shift    |        Ome = |  X_rate    |               *
! *            \  UT1_shift /                \ UT1_rate /                *
! *                                                                      *
! *     R_i -- vector of station coordinates.                            *
! *                                                                      *
! *     Summing is done over all station (if a station had an episodic   *
! *     motion, than its position is counted twice for R_p).             *
! *                                                                      *
! *   These vectors Rp and Rv contain coefficients for the right         *
! *   hand-side of the constraint equations which, being added to the    *
! *   right hand side r_p, r_v used in the old solution, causes the EOP  *
! *   series from the global solution to have zero shift and drift with  *
! *   respect to the external EOP series:                                *
! *                                                                      *
! *                                                                      *
! *   ( Rp_1, Rp_2, Rp_3 ) -- right hand-side for the net-translation    *
! *                           constraint for station positions.          *
! *   ( Rp_4, Rp_5, Rp_6 ) -- right hand-side for the net-rotation       *
! *                           constraint for station positions.          *
! *                                                                      *
! *   ( Rv_1, Rv_2, Rv_3 ) -- right hand-side for the net-translation    *
! *                           constraint for station velocities.         *
! *   ( Rv_4, Rv_5, Rv_6 ) -- right hand-side for the net-rotation       *
! *                           constraint for station velocities.         *
! *                                                                      *
! *   Analogously we compute right hand-side of constraint equation for  *
! *   net-translation/net-rotation for velocities. But in summing the    *
! *   stations with episodic motion are counted only once.               *
! *                                                                      *
! *   Usage: eop_alignment <sol-file> <nn-cons-list> <eop_fmt>           *
! *                        <ext_EOP_file> [date_beg] [date_end]          *
! *                                                                      *
! *   <sol-file> -- generic name of the output files obtained from       *
! *                 parsing spool file with using program getpar.        *
! *   <nn-cons-list> -- List of NNT-POS constraints used in solution.    *
! *                     This list can be found in Spool file just before *
! *                     the section of global parameters. Cut this list, *
! *                     put it into the file and feed if eop_alignment.  *
! *   <eop_fmt>      -- format of the external file: IERS_C04 or         *
! *                     USNO_FINALS.                                     *
! *   <ext_EOP_file> -- external EOP file in either USNO Finals or       *
! *                     IERS C04 as it was on 2002.05.20                 *
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
! *                                                                      *
! *     eop_alignment write right hand side of constraint equations in   *
! *   the screen. Just copy these lines and insert them to your batch    *
! *   control file. If your previous solution had zero net-rotation,     *
! *   net-translation constraints, the new solution will have zero       *
! *   shift and drift with respect to the external EOP file. Station     *
! *   position and velocity will be reciprocally changed with respect    *
! *   to the previous solution, of course.                               *
! *                                                                      *
! *   NB: if your solution used non-zero right hand sides for constraint *
! *       equations, you should add the output eop_alignment to the old  *
! *       right-hand sides in order to reach desirable effect.           *
! *                                                                      *
! *   Caveat: the present version assumes that all stations which        *
! *           participated in no-net-translation constraints for         *
! *           positions participated in no-net-translation for velocity  *
! *           and no-net-rotation for both position and velocity.        *
! *                                                                      *
! *  ### 15-MAY-2002  EOP_ALIGNMENT v2.1 (c)  L. Petrov  2015.11.07  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'getpar.i'
      INTEGER*4  MP, M_PAR, M_HEAD
      PARAMETER  ( MP     = 16384 )
      PARAMETER  ( M_PAR  =  1024 )
      PARAMETER  ( M_HEAD =   516 )
      CHARACTER  FILEOP*128, FILNN*128, FILSOL*128, FILEOB*128, FILSTA*128, &
     &           FILVEL*128
      CHARACTER  CH_FLAG(MP), BUF(MP)*256, SOLNAME*128, CN_STA(M_STA)*8, &
     &           CC_STA(M_STA)*15, CV_STA(M_STA)*15, HEAD_BUF(M_HEAD)*128, &
     &           EOP_FMT*12, DATE_BEG*36, DATE_END*14, STR*80, SOL_ID*32
      CHARACTER  GET_VERSION*54
      REAL*8     JD_EXT(MP), YR_EXT(MP), &
     &           XP_EXT(MP), YP_EXT(MP), U1_EXT(MP), DPSI_EXT(MP), DEPS_EXT(MP), &
     &           XP_ERR(MP), YP_ERR(MP), U1_ERR(MP), DPSI_ERR(MP), DEPS_ERR(MP)
      TYPE ( EOP__STRU ) ::  EOP(MP)
      REAL*8     RH_VEC(12), MAX_SIG, MAX_SIG_MAS, MAX_SIG_MAX
      REAL*8     REF_DATE, REA, MAX_SIG_DEF
      PARAMETER  ( REF_DATE    = 2000.000    )
      PARAMETER  ( REA         = 6378136.3D0 )
      PARAMETER  ( MAX_SIG_DEF = 1.0         ) ! In radians :-)
      LOGICAL*4  LEX
      INTEGER*4  NP, NHEAD, IP, IL, NEXT, LN_STA, LC_STA, LV_STA, MJD, &
     &           J1, J2, IER, IVRB, IUER
      REAL*8     COO_STA(3,M_STA), VEL_STA(3,M_STA), PHI(3), OME(3), SEC, &
     &           JD_EOP_BEG, JD_EOP_END
      REAL*8     MJD_SEC_TO_JD
#ifdef INTEL
      INTEGER*4, EXTERNAL ::  IARGC
#endif
      INTEGER*4, EXTERNAL ::  ILEN, I_LEN, LINDEX
!
      INCLUDE   'eop_alignment_version.i'
!
      CALL CLRCH ( FILEOP )
      CALL CLRCH ( FILSOL )
      CALL CLRCH ( FILNN  )
      MAX_SIG = MAX_SIG_DEF
!
      CALL CLRCH ( STR )
      STR = GET_VERSION ()
      WRITE ( 6, '(A)' ) '* '//STR(1:I_LEN(STR))
!
! --- Get arguments
!
      IF ( IARGC () .GE. 4 ) THEN
           CALL GETARG ( 1, FILSOL  )
           CALL GETARG ( 2, FILNN   )
           CALL GETARG ( 3, EOP_FMT )
           CALL GETARG ( 4, FILEOP  )
           IF ( IARGC() .GE. 5 ) THEN
                CALL GETARG ( 5, DATE_BEG )
              ELSE
                DATE_BEG = '1950.01.01'
           END IF
           IF ( IARGC() .GE. 6 ) THEN
                CALL GETARG ( 6, DATE_END )
              ELSE
                DATE_END = '2049.12.31'
           END IF
           IF ( IARGC() .GE. 7 ) THEN
                CALL GETARG ( 7, STR )
                IF ( INDEX ( STR, '.' ) < 1 ) STR = TRIM(STR)//'.0'
                READ ( UNIT=STR, FMT='(F10.4)' ) MAX_SIG_MAS
                MAX_SIG = MAX_SIG_MAS*MAS__TO__RAD
              ELSE
                IVRB = 0
           END IF
           IF ( IARGC() .GE. 8 ) THEN
                CALL GETARG ( 8, STR )
                CALL CHIN   ( STR, IVRB )
              ELSE
                IVRB = 0
           END IF
        ELSE
           WRITE ( 6, '(A)' ) 'Usage: eop_alignment <generic_solution_name> '// &
     &                        '<nn-cons-list> <eop_fmt> <external_EOP_file> '// &
     &                        '[date_beg] [date_end] [max_sig_mas] [ivrb]'
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
      IUER = -1
      CALL DATE_TO_TIME ( DATE_BEG, MJD, SEC, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      JD_EOP_BEG = MJD_SEC_TO_JD ( MJD, SEC )
!
      IUER = -1
      CALL DATE_TO_TIME ( DATE_END, MJD, SEC, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      JD_EOP_END = MJD_SEC_TO_JD ( MJD, SEC )
!
! --- Build file names
!
      FILEOB = FILSOL(1:I_LEN(FILSOL))//'.eob'
      FILSTA = FILSOL(1:I_LEN(FILSOL))//'.sta'
      FILVEL = FILSOL(1:I_LEN(FILSOL))//'.vel'
!
! --- Check: whether input files exit
!
      INQUIRE ( FILE=FILEOB, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 8201, -1, 'EOP_ALIGNMENT', 'EOB file '// &
     &          FILEOB(1:I_LEN(FILEOB))//' was not found' )
           CALL EXIT ( 1 )
      END IF
!
      INQUIRE ( FILE=FILSTA, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 8202, -1, 'EOP_ALIGNMENT', 'STA file '// &
     &          FILSTA(1:I_LEN(FILSTA))//' was not found' )
           CALL EXIT ( 1 )
      END IF
!
      INQUIRE ( FILE=FILVEL, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 8203, -1, 'EOP_ALIGNMENT', 'VEL file '// &
     &          FILVEL(1:I_LEN(FILVEL))//' was not found' )
           CALL EXIT ( 1 )
      END IF
!
      INQUIRE ( FILE=FILNN, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 8204, -1, 'EOP_ALIGNMENT', 'no-net-xxx file '// &
     &          FILNN(1:I_LEN(FILNN))//' was not found' )
           CALL EXIT ( 1 )
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
                CALL ERR_LOG ( 8205, -1, 'EOP_ALIGNMENT', 'Error in attempt '// &
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
                CALL ERR_LOG ( 8206, -1, 'EOP_ALIGNMENT', 'Error in attempt '// &
     &              'to read EOP files in USNO finals format' )
                CALL EXIT ( 4 )
           END IF
         ELSE
           CALL ERR_LOG ( 8207, -1, 'EOP_ALIGNMENT', 'Erong 3-rd argument: '// &
     &          EOP_FMT//' -- one of IERS_C04 or USNO_FINALS was expected' )
           CALL EXIT ( 5 )
      END IF
!
      DO 410 J1=1,NEXT
!
! ------ Transform arguments from Julian days to Julian years
!
         YR_EXT(J1) = 2000.0 + ( JD_EXT(J1) - 2451545.0D0 )/YEAR__TO__DAY
!!         YR_EXT(J1) = 2000.0 + ( JD_EXT(J1) - 2451545.0D0 )/365.25D0
!@         IF ( EOP_FMT(1:11) .EQ. 'USNO_FINALS' ) THEN
!@!
!@! ----------- Transform to standard units (later RD_FINALS program should be
!@! ----------- updated, of course (pet 2003.05.23) )
!@!
!@              XP_EXT(J1) = XP_EXT(J1)*MAS__TO__RAD*1000.0D0
!@              XP_ERR(J1) = XP_ERR(J1)*MAS__TO__RAD*1000.0D0
!@              YP_EXT(J1) = YP_EXT(J1)*MAS__TO__RAD*1000.0D0
!@              YP_ERR(J1) = YP_ERR(J1)*MAS__TO__RAD*1000.0D0
!@              U1_EXT(J1) = U1_EXT(J1)*MSEC__TO__RAD*1000.0D0
!@              U1_ERR(J1) = U1_ERR(J1)*MSEC__TO__RAD*1000.0D0
!@         END IF
 410  CONTINUE
!
! --- Read EOP series in EOB obtained in the Solve solution
!
      IUER = -1
      CALL READ_EOB ( FILEOB, M_HEAD, NHEAD, HEAD_BUF, MP, NP, EOP, IUER )
      IF ( IUER .NE. 0 ) STOP 'Error in reading EOB file'
      CALL CLRCH ( SOL_ID )
      DO 420 J2=1,NHEAD
         IF ( HEAD_BUF(J2)(1:17) == '# VLBI VTD/pSolve' ) THEN
              SOL_ID = HEAD_BUF(J2)(28:)
         END IF
         IF ( ILEN(SOL_ID) == 0 .AND. HEAD_BUF(J2)(1:13) == '# Spool file:' ) THEN
              SOL_ID = HEAD_BUF(J2)(1:15)
              IF ( ILEN(SOL_ID) > 5 ) THEN
                   CALL CLRCH ( SOL_ID(ILEN(SOL_ID)-3:ILEN(SOL_ID)-3) )
              END IF
         END IF
 420  CONTINUE 
!
! --- Read the file with NNT-POS constraints
!
      IUER = -1
      CALL READ_NN ( FILNN, MP, BUF, M_STA, LN_STA, CN_STA, IUER )
      IF ( IUER .NE. 0 ) STOP 'Error in reading NN file'
      WRITE ( 6, 110 ) TRIM(SOL_ID), TRIM(FILSOL), TRIM(FILEOP), LN_STA
 110  FORMAT ( '* VLBI solution ID:    ', A/ &
     &         '* Solution spool file: ', A/ & 
     &         '* EOP series:          ', A/ &
     &         '* Number of stations used in constraints: ', I3 )
!
! --- Read station coordinates
!
      IUER = -1
      CALL READ_STA ( FILSTA, MP, BUF, M_STA, LC_STA, CC_STA, COO_STA, IUER )
      IF ( IUER .NE. 0 ) STOP 'Error in reading STA file'
!
! --- Read station velocities
!
      IUER = -1
      CALL READ_VEL ( FILVEL, MP, BUF, M_STA, LV_STA, CV_STA, VEL_STA, IUER )
      IF ( IUER .NE. 0 ) STOP 'Error in reading VEL file'
!
! --- Compute shift and drift of the EOP differences
!
      IUER = -1
      CALL DIF_EOP ( NEXT, YR_EXT, XP_EXT, XP_ERR, YP_EXT, YP_ERR, U1_EXT, &
     &               U1_ERR, NP, EOP, REF_DATE, JD_EOP_BEG, JD_EOP_END, &
     &               PHI, OME, MAX_SIG, SOL_ID, EOP_FMT, IVRB, IUER )
      IF ( IUER .NE. 0 ) STOP 'Error in computing EOP differences'
!
      WRITE  ( 6, '(A)' ) '* '
      WRITE  ( 6, 120 ) PHI(1)*REA, PHI(2)*REA, PHI(3)*REA
 120  FORMAT ( '*       PHI  ', 3(F14.10,1X), ' * phi in meters' )
      WRITE  ( 6, 130 ) OME(1)*REA, OME(2)*REA, OME(3)*REA
 130  FORMAT ( '*       OME  ', 3(F14.10,1X), ' * ome in m/yr' )
!
! --- Compute the right hand side of constraint equations ...
!
      IUER = -1
      CALL COMP_RH ( M_PAR, PHI, OME, LN_STA, CN_STA, LC_STA, &
     &               CC_STA, LV_STA, CV_STA, COO_STA, RH_VEC, IUER )
!
! --- And write them in the screen
!
      WRITE  ( 6, 140 ) RH_VEC
 140  FORMAT ( '* '/'* Vector of right hand side: '/'* '/ &
     &         '*   RIGHT_PART  ',3(F12.8,1X),' NO   EXCEPT   \ * nnt_pos'/ &
     &         '*   RIGHT_PART  ',3(F12.8,1X),' NO   EXCEPT   \ * nnr_pos'/ &
     &         '*   RIGHT_PART  ',3(F12.8,1X),' NO   EXCEPT   \ * nnt_vel'/ &
     &         '*   RIGHT_PART  ',3(F12.8,1X),' NO   EXCEPT   \ * nnr_vel' )
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
           CALL ERR_LOG ( 3211, IUER, 'READ_NN', 'Error  in reading NN-file '// &
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
                      CALL ERR_LOG ( 3212, IUER, 'READ_NN', 'The number of '// &
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
      SUBROUTINE READ_STA ( FILSTA, MP, BUF, M_STA, LC_STA, CC_STA, COO_STA, &
     &                      IUER )
! ************************************************************************
! *                                                                      *
! *   Soubroutine READ_STA reads the station coordinates from the file   *
! *   generated by getpar program. Units: meters.                        *
! *                                                                      *
! *  ### 20-MAY-2002    READ_STA   v1.0 (c)  L. Petrov  20-MAY-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MP, M_STA, LC_STA, IUER
      CHARACTER  FILSTA*(*), BUF(MP)*(*), CC_STA(M_STA)*(*)
      REAL*8     COO_STA(3,M_STA)
      INTEGER*4  NBUF, J1, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILSTA, MP, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3221, IUER, 'READ_STA', 'Error in reading '// &
     &         'STA-file '//FILSTA(1:I_LEN(FILSTA)) )
           RETURN
      END IF
!
      LC_STA = 0
      DO 410 J1=1,NBUF
         IF ( BUF(J1)(1:1) .EQ. 'W' ) GOTO 410
         IF ( ILEN(BUF(J1)) .LE.  0  ) GOTO 410
         IF ( BUF(J1)(28:28) .NE. 'X' ) GOTO 410
         LC_STA = LC_STA + 1
         CC_STA(LC_STA) = BUF(J1)(11:25)
         CALL UNDERSCORE_TO_BLANK ( CC_STA(LC_STA) )
         READ ( UNIT=BUF(J1)(31:45),  FMT='(F15.5)' ) COO_STA(1,LC_STA)
         READ ( UNIT=BUF(J1)(65:79),  FMT='(F15.5)' ) COO_STA(2,LC_STA)
         READ ( UNIT=BUF(J1)(99:113), FMT='(F15.5)' ) COO_STA(3,LC_STA)
         COO_STA(1,LC_STA) = COO_STA(1,LC_STA)*0.001D0
         COO_STA(2,LC_STA) = COO_STA(2,LC_STA)*0.001D0
         COO_STA(3,LC_STA) = COO_STA(3,LC_STA)*0.001D0
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  READ_STA  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE READ_VEL ( FILVEL, MP, BUF, M_STA, LV_STA, CV_STA, VEL_STA, &
     &                      IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine READ_VEL reads station velocities from the file         *
! *   generated by getpar program. Units: meter/year                     *
! *                                                                      *
! *  ### 20-MAY-2002    READ_VEL   v1.0 (c)  L. Petrov  20-MAY-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MP, M_STA, LV_STA, IUER
      CHARACTER  FILVEL*(*), BUF(MP)*(*), CV_STA(M_STA)*(*)
      REAL*8     VEL_STA(3,M_STA)
      INTEGER*4  NBUF, J1, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILVEL, MP, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3231, IUER, 'READ_VEL', 'Error in reading '// &
     &         'STA-file '//FILVEL(1:I_LEN(FILVEL)) )
           RETURN
      END IF
!
      LV_STA = 0
      DO 410 J1=1,NBUF
         IF ( BUF(J1)(1:1)  .EQ.  '#' ) GOTO 410
         IF ( ILEN(BUF(J1)) .LE.   0  ) GOTO 410
         IF ( BUF(J1)(21:21) .NE. 'X' ) GOTO 410
         LV_STA = LV_STA + 1
         CV_STA(LV_STA) = BUF(J1)(11:25)
         CALL UNDERSCORE_TO_BLANK ( CV_STA(LV_STA) )
         READ ( UNIT=BUF(J1)(24:32), FMT='(F9.4)' ) VEL_STA(1,LV_STA)
         READ ( UNIT=BUF(J1)(50:58), FMT='(F9.4)' ) VEL_STA(2,LV_STA)
         READ ( UNIT=BUF(J1)(76:84), FMT='(F9.4)' ) VEL_STA(3,LV_STA)
         VEL_STA(1,LV_STA) = VEL_STA(1,LV_STA)*0.001D0
         VEL_STA(2,LV_STA) = VEL_STA(2,LV_STA)*0.001D0
         VEL_STA(3,LV_STA) = VEL_STA(3,LV_STA)*0.001D0
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  READ_VEL  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIF_EOP ( NEXT, YR_EXT, XP_EXT, XP_ERR, YP_EXT, YP_ERR, &
     &                     U1_EXT, U1_ERR, &
     &                     NP, EOP, REF_DATE, JD_EOP_BEG, JD_EOP_END, &
     &                     PHI, OME, MAX_SIG, SOL_ID, EOP_FMT, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine DIF_EOP computes secular drift and shift at the         *
! *   reference epoch of the tested EOP series with respect to the       *
! *   external EOP series.                                               *
! *                                                                      *
! *  ### 20-MAY-2002     DIF_EOP   v1.2 (c)  L. Petrov  07-NOV-2015 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  NEXT, NP, IUER
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'getpar.i'
      REAL*8     YR_EXT(NEXT), XP_EXT(NEXT), YP_EXT(NEXT), U1_EXT(NEXT), &
     &           XP_ERR(NEXT), YP_ERR(NEXT), U1_ERR(NEXT), PHI(3), OME(3)
      REAL*8     JD_EOP_BEG, JD_EOP_END, MAX_SIG
      CHARACTER  SOL_ID*(*), EOP_FMT*(*)
      TYPE ( EOP__STRU ) ::   EOP(NP)
      REAL*8     REF_DATE
      CHARACTER  SESS_NAME(NP)*10
      INTEGER*4  MP
      PARAMETER  ( MP = 32768 )
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
      INTEGER*4  J1, J2, J3, NZ, IXC, IVRB, KP, IER
      REAL*8     FSPL8
      CHARACTER  STR*64, TITLE(3)*20
      DATA          TITLE &
     &           / &
     &             'X pole coord. (mas) ', &
     &             'Y pole coord. (mas) ', &
     &             'UT1 angle   (musec) ' &
     &           /
      CHARACTER  JD_TO_DATE*23
      INTEGER*4, EXTERNAL :: IXMN8
!
  write ( 6, * ) ' REF_DATE= ', REF_DATE ! %%%%%%%%%%%%%%%%
      DO 410 J1=1,3
!
! ------ Compute paramters of interpolating cubic spline
!
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
         END IF
         IF ( IER .NE. 0) THEN
              WRITE ( 6, * ) ' J1=',J1
              CALL ERR_LOG ( 8281, IUER, 'DIF_EOP', 'Error in computation of '// &
     &                      'spline' )
              RETURN
         END IF
!
         KP = 0
         JD_MIN =  1.D9
         JD_MAX = -1.D9
!
   write ( 6, * ) 'YEAR__TO__DAY = ', YEAR__TO__DAY 
         DO 420 J2=1,NP
!
! --------- Transform arguments from Julian dys to Julian years
!
!            DATE_YEAR = 2000.0   + ( EOP(J2)%MJD_EOP - 51544.5D0 )/365.25D0
!!            DATE_YEAR = 2000.0D0 + ( EOP(J2)%MJD_EOP - 51544.5D0 )/365.2D0
!!   write ( 6 ,* ) ' j2= ', j2, ' d1= ', 2000.0   + ( EOP(J2)%MJD_EOP - 51544.5D0 )/365.25D0, ' d2= ', 2000.0D0 + ( EOP(J2)%MJD_EOP - 51544.5D0 )/365.2 ! %%%%%%%%%%%%%%%%%%%
            DATE_YEAR = 2000.0 + ( EOP(J2)%MJD_EOP - 51544.5D0 )/YEAR__TO__DAY
!
! --------- Check validity of the dates for comparison
!
            JD_EOP = EOP(J2)%MJD_EOP + 2400000.5D0
            IF ( JD_EOP .LT. JD_EOP_BEG  .OR. JD_EOP .GT. JD_EOP_END ) GOTO 420
            IF ( JD_EOP .LT. JD_MIN ) JD_MIN = JD_EOP
            IF ( JD_EOP .GT. JD_MAX ) JD_MAX = JD_EOP
!
! --------- Interpolate EXT to the epoch of the tested EOP series
!
            IXC = IXMN8 ( NEXT, YR_EXT, DATE_YEAR )
            IF ( J1 .EQ. 1  .AND.  BTEST ( EOP(J2)%STATUS, XPL__GTP ) ) THEN
                 IF ( EOP(J2)%XPL_E .LE. MAX_SIG ) THEN
                      KP = KP + 1
                      X8(KP) = ( EOP(J2)%XPL_V - FSPL8 ( DATE_YEAR, NEXT, &
     &                           YR_EXT, XP_EXT, IXC, COEF ) )/MAS_TO_RAD
                      E8(KP) = EOP(J2)%XPL_E/MAS_TO_RAD
                 END IF
               ELSE IF ( J1 .EQ. 2 .AND. BTEST( EOP(J2)%STATUS, YPL__GTP ) ) THEN
                 IF ( EOP(J2)%YPL_E .LE. MAX_SIG ) THEN
                      KP = KP + 1
                      X8(KP) = ( EOP(J2)%YPL_V - FSPL8 ( DATE_YEAR, NEXT, &
     &                           YR_EXT, YP_EXT, IXC, COEF ) )/MAS_TO_RAD
                      E8(KP) = EOP(J2)%YPL_E/MAS_TO_RAD
                 END IF
               ELSE IF ( J1 .EQ. 3 .AND. BTEST( EOP(J2)%STATUS, U1__GTP ) ) THEN
                 IF ( EOP(J2)%U1_E .LE. MAX_SIG ) THEN
                      KP = KP + 1
                      X8(KP) = ( EOP(J2)%U1_V - FSPL8 ( DATE_YEAR, NEXT, &
     &                           YR_EXT, U1_EXT, IXC, COEF ) )/MUSEC_TO_RAD
                      E8(KP) = EOP(J2)%U1_E/MUSEC_TO_RAD
                 END IF
            END IF
            IF ( KP .GT. 0 ) THEN
                 T8(KP) = DATE_YEAR
                 W8(KP) = 1.D0/E8(KP)
                 IF ( E8(KP) .GT. 1.D4 ) THEN
                      KP = KP - 1
                 END IF
            END IF
 420     CONTINUE
         IF ( IVRB .GE. 1 ) THEN
              CALL GAUSS_FILTER ( KP, HW_GAUSS, T8, X8, E8, X9, E9 )
              E9 = 0.01D0*E9
              IF ( J1 == 1 ) THEN
                   CALL DIAGI_SETDEF ( IUER, 'DIAGI_CTIT', 'Xpole differences VLBI solution '// &
     &                                 TRIM(SOL_ID)//' - '//TRIM(EOP_FMT)//' (mas)' )
                 ELSE IF ( J1 == 2 ) THEN
                   CALL DIAGI_SETDEF ( IUER, 'DIAGI_CTIT', 'Ypole differences VLBI solution '// &
     &                                 TRIM(SOL_ID)//' - '//TRIM(EOP_FMT)//' (mas)' )
                 ELSE IF ( J1 == 3 ) THEN
                   CALL DIAGI_SETDEF ( IUER, 'DIAGI_CTIT', 'UT1 differences VLBI solution '// &
     &                                 TRIM(SOL_ID)//' - '//TRIM(EOP_FMT)//' (musec)' )
              END IF
              CALL DIAGI_SETDEF ( IUER, 'DIAGI_UNIT', 'Time in years' )
              CALL DIAGI_SETDEF ( IUER, 'DIAGI_IBST',  2 )
              CALL DIAGI_SETDEF ( IUER, 'DIAGI_ICL2', 10 )
              CALL DIAGI_2E ( KP, T8, X8, E8, KP, T8, X9, E9, IER )
         END IF
!
         IF ( KP .GT. 2 ) THEN
!
! ----------- Compute weighted regression
!
              CALL RGRW8  ( KP, T8, X8, W8, %VAL(0), MEAN_T, DR_VAL, SH_VAL, &
     &                      DR_SIG, SH_SIG, IER )
              IF ( J1 .EQ. 1 ) THEN
                   STR(1:19)  = JD_TO_DATE ( JD_MIN, IER )
                   STR(31:49) = JD_TO_DATE ( JD_MAX, IER )
                   WRITE ( 6, '(A)' ) '* Date_start: '//STR(1:10)//'       '// &
     &                                'Date_end:   '//STR(31:40)
!
                   STR(1:19)  = JD_TO_DATE ( J2000__JD + &
     &                         (REF_DATE-J2000__YR)*JYEAR__DAYS - 0.5D0, IER )
                   STR(31:49) = JD_TO_DATE ( J2000__JD + &
     &                         (MEAN_T-J2000__YR)*JYEAR__DAYS - 0.5D0, IER )
                   WRITE ( 6, '(A)' ) '* Ref_date:   '//STR(1:10)//'       '// &
     &                                'Mean_date:  '//STR(31:40)
              END IF
!
! ----------- Compute some other statistics
!
              IER = -2
              CALL DISP_WTR8 ( KP, T8, X8, W8, DR_VAL, &
     &                         SH_VAL + (T8(1)-MEAN_T)*DR_VAL, %VAL(0), &
     &                         DW, NZ, IER )
              CHI = 0.0D0
              DO 430 J3=1,KP
                 CHI = CHI + &
     &                 (( X8(J3) - (SH_VAL + (T8(J3)-MEAN_T)*DR_VAL))*W8(J3))**2
 430          CONTINUE
              CHI_NDG = CHI/(KP-2)
              WRITE ( 6, 120 ) '* Diff. '//TITLE(J1), SH_VAL, SH_SIG, &
     &                             DR_VAL, DR_SIG, TITLE(J1), DW, CHI_NDG
 120          FORMAT ( A,' shift:    ',F8.2,' -+ ',F6.4,'  rate: ', &
     &                 F7.3,' -+ ',F6.4/'*       ',A, ' w.r.m.s. = ', F8.3, &
     &                           '   Chi/ndg=',F6.2 )
!
! ----------- Compute shift and drift of EOP series (in rad and rad/yr)
!
              IF ( J1 .EQ. 1 ) THEN
                   PHI(2) = -( SH_VAL + DR_VAL*(REF_DATE - MEAN_T) )*MAS_TO_RAD
                   OME(2) = -DR_VAL*MAS_TO_RAD
                 ELSE IF ( J1 .EQ. 2 ) THEN
                   PHI(1) = -( SH_VAL + DR_VAL*(REF_DATE - MEAN_T) )*MAS_TO_RAD
                   OME(1) = -DR_VAL*MAS_TO_RAD
                 ELSE IF ( J1 .EQ. 3 ) THEN
                   PHI(3) = ( SH_VAL + DR_VAL*(REF_DATE - MEAN_T) )* &
     &                        MUSEC_TO_RAD/K_FACTOR
                   OME(3) = DR_VAL*MUSEC_TO_RAD/K_FACTOR
               END IF
            ELSE
              WRITE ( 6, 130 ) TITLE(J1), KP
 130          FORMAT ( A,' -- to few points: ',I5 )
              CALL ERR_LOG ( 8282, IUER, 'DIF_EOP', 'to few points' )
              RETURN
         END IF
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DIF_EOP  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE COMP_RH ( M_PAR, PHI, OME, LN_STA, CN_STA, LC_STA, &
     &                     CC_STA, LV_STA, CV_STA, COO_STA, RH_VEC, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  COMP_RH computes the vector of right hand side of      *
! *   no-net-transtaion/no-net-rotation constraints.                     *
! *                                                                      *
! *  ### 20-MAY-2002    COMP_RH   v1.0 (c)  L. Petrov  20-MAY-2002 ###   *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  M_PAR, LN_STA, LC_STA, LV_STA, IUER
      CHARACTER  CN_STA(LN_STA)*(*), CC_STA(LC_STA)*(*), CV_STA(LV_STA)*(*)
      REAL*8     PHI(3), OME(3), COO_STA(3,LC_STA), RH_VEC(12)
      REAL*8       REA
      PARAMETER  ( REA = 6378136.3D0 )
      REAL*8     RLEN_SQ, TRA_MAT(6,3), VEC3(3), VEC6(6)
      INTEGER*4  IC, IP, L_PAR, J1, J2, IER
      INTEGER*4, EXTERNAL :: LTM_DIF
!
! --- Initialization
!
      CALL NOUT_R8 ( 6*3, TRA_MAT )
      CALL NOUT_R8 ( 12,  RH_VEC )
!
      L_PAR = 0
      DO 410 J1=1,LC_STA
!
! ------ Check, whether the J1-th station participlated in NN-constraints
!
         IP = LTM_DIF ( 0, LN_STA, CN_STA, CC_STA(J1)(1:8) )
         IF ( IP .LE. 0 ) GOTO 410 ! not? bypass it.
         RLEN_SQ = COO_STA(1,J1)**2 + COO_STA(2,J1)**2 + COO_STA(3,J1)**2
!
! ------ Compute elements of transformation matrix for this station
!
         TRA_MAT(1,1) =  1.0D0
         TRA_MAT(5,1) =  COO_STA(3,J1)/RLEN_SQ*REA
         TRA_MAT(6,1) = -COO_STA(2,J1)/RLEN_SQ*REA
!
         TRA_MAT(2,2) =  1.0D0
         TRA_MAT(4,2) = -COO_STA(3,J1)/RLEN_SQ*REA
         TRA_MAT(6,2) =  COO_STA(1,J1)/RLEN_SQ*REA
!
         TRA_MAT(3,3) =  1.0D0
         TRA_MAT(4,3) =  COO_STA(2,J1)/RLEN_SQ*REA
         TRA_MAT(5,3) = -COO_STA(1,J1)/RLEN_SQ*REA
!
         CALL VM83 ( PHI, COO_STA(1,J1), VEC3 )
         IER = -1
         CALL MUL_MV_IV_V ( 6, 3, TRA_MAT, 3, VEC3, 6, VEC6, IER )
         CALL ADD_VV ( 6, RH_VEC(1), VEC6 )
 410  CONTINUE
!
! --- Now cycle over the stations which velocitis were determined
!
      DO 420 J2=1,LV_STA
         IP = LTM_DIF ( 0, LN_STA, CN_STA, CV_STA(J2)(1:8) )
         IF ( IP .LE. 0 ) GOTO 420
         IC = LTM_DIF ( 0, LC_STA, CC_STA, CV_STA(J2)(1:8) )
         IF ( IC .LE. 0 ) THEN
              CALL ERR_LOG ( 8566, IUER, 'COMP_RH', 'Trap of internal '// &
     &            'control' )
              RETURN
         END IF
!
         RLEN_SQ = COO_STA(1,IC)**2 + COO_STA(2,IC)**2 + COO_STA(3,IC)**2
!
         TRA_MAT(1,1) =  1.0D0
         TRA_MAT(5,1) =  COO_STA(3,IC)/RLEN_SQ*REA
         TRA_MAT(6,1) = -COO_STA(2,IC)/RLEN_SQ*REA
!
         TRA_MAT(2,2) =  1.0D0
         TRA_MAT(4,2) = -COO_STA(3,IC)/RLEN_SQ*REA
         TRA_MAT(6,2) =  COO_STA(1,IC)/RLEN_SQ*REA
!
         TRA_MAT(3,3) =  1.0D0
         TRA_MAT(4,3) =  COO_STA(2,IC)/RLEN_SQ*REA
         TRA_MAT(5,3) = -COO_STA(1,IC)/RLEN_SQ*REA
!
         CALL VM83 ( OME, COO_STA(1,IC), VEC3 )
         IER = -1
         CALL MUL_MV_IV_V ( 6, 3, TRA_MAT, 3, VEC3, 6, VEC6, IER )
         CALL ADD_VV ( 6, RH_VEC(7), VEC6 )
 420  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  COMP_RH  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GAUSS_FILTER ( M, HW, TIM, ARR1, SIG1, ARR2, SIG2 )
      IMPLICIT   NONE 
      INTEGER*4  M
      REAL*8     HW, TIM(M), ARR1(M), SIG1(M), ARR2(M), SIG2(M)
      REAL*8     WIN, WIN_SUM, EXP_VAR, SIG_ACC, EXP_ACC
      INTEGER*4  J1, J2
!
      DO 410 J1=1,M
         WIN_SUM  = 0.0D0
         ARR2(J1) = 0.0D0
         SIG_ACC  = 0.0D0
         EXP_ACC  = 0.0D0
         DO 420 J2=1,M
            EXP_VAR = -( (TIM(J2) - TIM(J1))/HW )**2
            IF ( EXP_VAR .LT. -40.0D0 ) EXP_VAR = -40.0D0
            WIN_SUM = WIN_SUM + DEXP(EXP_VAR)/SIG1(J1)
            ARR2(J1) = ARR2(J1) + ARR1(J2)*DEXP(EXP_VAR)/SIG1(J1)
            SIG_ACC = SIG_ACC + ( DEXP(EXP_VAR)*SIG1(J1) )**2
            EXP_ACC = EXP_ACC +   DEXP(EXP_VAR)
 420     CONTINUE 
         ARR2(J1) = ARR2(J1)/WIN_SUM
         SIG2(J1) = DSQRT ( SIG_ACC/EXP_ACC )
 410  CONTINUE 
!      
      RETURN
      END  SUBROUTINE  GAUSS_FILTER 
