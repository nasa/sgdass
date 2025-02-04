      SUBROUTINE BOXCAR ( MB, KP, TIM, VAL, ERR, KP1, TIM1, VAL1, ERR1 )
      IMPLICIT   NONE
      INTEGER*4  MB, KP, KP1
      REAL*8     TIM(KP), VAL(KP), ERR(KP), TIM1(KP), VAL1(KP), ERR1(KP)
      REAL*8     WW
      INTEGER*4  J1, J2
!
      DO 410 J1=1,KP-MB+1
         TIM1(J1) = 0.0D0
         VAL1(J1) = 0.0D0
         ERR1(J1) = 0.0D0
         WW = 0.0D0
         DO 420 J2=J1,J1+MB-1
            TIM1(J1) = TIM1(J1) + TIM(J2)
!          type *,' j2=',j2,' err=',err(j2) ! %%%
            VAL1(J1) = VAL1(J1) + VAL(J2)/ERR(J2)
            WW       = WW       + 1.D0/ERR(J2)
            ERR1(J1) = ERR1(J1) + ERR(J2)**2
 420     CONTINUE
         TIM1(J1) = TIM1(J1)/MB
         VAL1(J1) = VAL1(J1)/WW
         ERR1(J1) = DSQRT ( ERR1(J1) )/MB
 410  CONTINUE
      KP1 = KP - MB + 1
!
      RETURN
      END  !#!  BOXCAR  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE BOXCAR_FILTER ( NB, N, ARG, VAL1, VAL2 )
! ************************************************************************
! *                                                                      *
! *   Auxillry routine BOXCAR_FILTER
! *                                                                      *
! * ### 22-AUG-2002  BOXCAR_FILTER v1.0 (c)  L. Petrov  22-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  NB, N
      REAL*8     ARG(N), VAL1(N), VAL2(N)
      INTEGER*4  J1, J2, KP
!
      DO 410 J1=1,N
         KP = 0
         VAL2(J1) = 0.0D0
         DO 420 J2=J1-NB/2,J1+NB/2
            IF ( J2 .GE. 1   .AND.   J2 .LE. N ) THEN
                 KP = KP+1
                 VAL2(J1) = VAL2(J1) + VAL1(J2)
            END IF
 420    CONTINUE
        VAL2(J1) = VAL2(J1)/KP
 410  CONTINUE
      RETURN
      END  !#!  BOXCAR_FILTER  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE RD_ESM ( FILESM, M_STA, LE_STA, CE_STA, EPOCH_ESM, &
     &                    FL_NL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  RD_ESM
! *                                                                      *
! *  ### 28-JAN-2003     RD_ESM    v1.0 (c)  L. Petrov  28-JAN-2003 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  M_STA, LE_STA, IUER
      CHARACTER  FILESM*(*), CE_STA(M_STA)*(*)
      LOGICAL*4  FL_NL(M_STA)
      REAL*8     EPOCH_ESM(M_STA)
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 64 )
      CHARACTER  BUF(MBUF)*64, DATE_STR*19, YEAR_STR*2, MON_STR*2, DAY_STR*2, &
     &           STR*32
      INTEGER*4  J2000__MJD, IYEAR, MJD, NBUF, J1, IER
      PARAMETER  ( J2000__MJD  = 51544       ) ! 2000.01.01_00:00:00
      REAL*8     SEC
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILESM, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7101, IUER, 'RD_ESM', 'Error in reading '//FILESM )
           RETURN
      END IF
!
      LE_STA = 0
      DO 410 J1=1,NBUF
         IF ( BUF(J1)(1:1)  .EQ. '*' ) GOTO 410
         IF ( ILEN(BUF(J1)) .LE.  0  ) GOTO 410
!
         LE_STA = LE_STA + 1
         IF ( LE_STA .GT. M_STA ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( LE_STA, STR )
              CALL ERR_LOG ( 7102, IUER, 'RD_ESM', 'To many stations in the '// &
     &            'eccentricity file '//FILESM(1:I_LEN(FILESM))//' -- '// &
     &            'more than '//STR )
              RETURN
         END IF
         IF ( BUF(J1)(41:41) == '#' ) THEN
              FL_NL(LE_STA) = .TRUE.
            ELSE 
              FL_NL(LE_STA) = .FALSE.
         END IF
!
         CE_STA(LE_STA) = BUF(J1)(1:8)
         DATE_STR = BUF(J1)(10:)
         IF ( INDEX ( DATE_STR, '.' ) .LE. 0 ) THEN
              YEAR_STR = DATE_STR(1:2)
              MON_STR  = DATE_STR(3:4)
              DAY_STR  = DATE_STR(5:6)
              CALL CHIN ( YEAR_STR, IYEAR )
              IF ( IYEAR .LE. 70 ) THEN
                   DATE_STR = '20'//YEAR_STR//'.'//MON_STR//'.'//DAY_STR// &
     &                        '_00:00:00'
                 ELSE
                   DATE_STR = '19'//YEAR_STR//'.'//MON_STR//'.'//DAY_STR// &
     &                        '_00:00:00'
              END IF
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( DATE_STR, MJD, SEC, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7103, IUER, 'RD_ESM', 'Wrong date format '// &
     &                 'in line "'//BUF(J1)(1:I_LEN(BUF(J1)))// &
     &                 '" of the eposidic site motions epoch file '//FILESM )
                   RETURN
              END IF
              EPOCH_ESM(LE_STA) = (MJD - J2000__MJD)/365.2422D0 + 2000.0D0
         END IF
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  RD_ESM  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE REPEATABILITY ( IPAR, ST1, ST2, M_STA, M_EPC, LE_STA, CE_STA, &
     &                         EPOCH_ESM, NP, TIM, VAL, WEI, ERR, IV, L_EPC, &
     &                         KP, DR_VAL, DR_SIG, TIM_EPC, SH_VAL, SH_SIG, &
     &                         WRMS, CHI, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine REPEATABILITY  computes
! *                                                                      *
! * ### 28-JAN-2003  REPEATABILITY  v1.1 (c)  L. Petrov  03-OCT-2014 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IPAR, M_STA, M_EPC, LE_STA, L_EPC, NP, KP, IV(NP), IUER
      CHARACTER  ST1*8, ST2*8, CE_STA(M_STA)*8
      REAL*8     TIM(NP), VAL(NP), WEI(NP), ERR(NP), EPOCH_ESM(M_STA), &
     &           TIM_EPC(M_EPC)
      REAL*8     TIM_EPS
      PARAMETER  ( TIM_EPS = 1.D-3 ) 
      REAL*8     DR_VAL, DR_SIG, SH_VAL(M_EPC), SH_SIG(M_EPC)
      REAL*8     WW, WRMS, CHI, TIM_0, MEAN_T, EPS, TIM_FIRST, TIM_LAST, SHIFT
      PARAMETER  ( EPS = 0.005D0 )
      INTEGER*4  J1, J2, J3, J4, J5, J6, IER
      INTEGER*4  LTM_DIF
!
      IER = -1
      L_EPC = 0
      IF ( LTM_DIF ( 0, LE_STA, CE_STA, ST1 ) .LE. 0  .AND. &
     &     LTM_DIF ( 0, LE_STA, CE_STA, ST2 ) .LE. 0        ) THEN
!
           CALL ERR_PASS ( IUER, IER )
           CALL RGRW8    ( NP, TIM, VAL, WEI, IV, MEAN_T, DR_VAL, SH_VAL, &
     &                     DR_SIG, SH_SIG, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7111, IUER, 'REPEATABILITY', 'Error in '// &
     &              'computing the mean and trend' )
                RETURN
           END IF
         ELSE
           TIM_FIRST = -1.0
           DO 410 J1=1,NP
              IF ( IV(J1) .GT. 0 .AND. TIM_FIRST .LE. -1.0 ) TIM_FIRST = TIM(J1)
              IF ( IV(J1) .GT. 0  ) TIM_LAST = TIM(J1)
 410       CONTINUE
           DO 420 J2=1,LE_STA
              IF ( CE_STA(J2) .EQ. ST1                     .AND. &
     &             ( EPOCH_ESM(J2) - TIM_FIRST ) .GT. EPS  .AND. &
     &             ( TIM_LAST - EPOCH_ESM(J2)  ) .GT. EPS        ) THEN
                   IF ( L_EPC > 0 ) THEN
                        DO 430 J3=1,L_EPC
                           IF ( DABS(TIM_EPC(J3) - EPOCH_ESM(J2)) < TIM_EPS ) GOTO 420
 430                    CONTINUE 
                   END IF
                   L_EPC = L_EPC + 1
                   TIM_EPC(L_EPC) = EPOCH_ESM(J2)
              END IF
              IF ( CE_STA(J2) .EQ. ST2                     .AND. &
     &             ( EPOCH_ESM(J2) - TIM_FIRST ) .GT. EPS  .AND. &
     &             ( TIM_LAST - EPOCH_ESM(J2)  ) .GT. EPS        ) THEN
                   DO 440 J4=1,L_EPC
                      IF ( DABS(TIM_EPC(J4) - EPOCH_ESM(J2)) < TIM_EPS ) GOTO 420
 440               CONTINUE 
                   L_EPC = L_EPC + 1
                   TIM_EPC(L_EPC) = EPOCH_ESM(J2)
              END IF
 420       CONTINUE
!           
!             write ( 6, * ) ' tim_first=',tim_first,' tim_last=',tim_last ! %%%
!             write ( 6, * ) ' l_epc=',l_epc ! %%
!
           CALL ERR_PASS ( IUER, IER )
           CALL RGRW8_BR ( NP, TIM, VAL, WEI, IV, MEAN_T, L_EPC, TIM_EPC, &
     &                     DR_VAL, DR_SIG, SH_VAL, SH_SIG, IER )
           IF ( IER .NE. 0 ) THEN
                WRITE ( 6, * ) 'NP = ', NP
                WRITE ( 6, * ) 'L_EPC = ', L_EPC
                IF ( L_EPC > 0 ) WRITE ( 6, * ) 'TIM_EPC= ', SNGL(TIM_EPC(1:L_EPC)) 
                WRITE ( 6, * ) 'MEAN_T = ', MEAN_T
                WRITE ( 6, * ) 'TIM = ', SNGL(TIM(1:NP))
                WRITE ( 6, * ) 'VAL = ', SNGL(VAL(1:NP))
                WRITE ( 6, * ) 'WEI = ', SNGL(WEI(1:NP))
                CALL ERR_LOG ( 7112, IUER, 'REPEATABILITY', 'Error in '// &
     &              'computing the mean and trend at baseline '// &
     &               ST1//' / '//ST2 )
                CHI = 0.0
                RETURN
           END IF
      END IF
!
      KP = 0
      WW   = 0.0D0
      WRMS = 0.0D0
!
      TIM_0 = TIM(1)
!
      DO 450 J5=1,NP
         IF ( IV(J5) .EQ. 1 ) THEN
              SHIFT = SH_VAL(1)
              IF ( L_EPC .GT. 0 ) THEN
                   DO 460 J6=1,L_EPC
                      IF ( TIM(J5) .GT. TIM_EPC(J6) ) THEN
                           SHIFT = SHIFT + SH_VAL(J6+1)
                      END IF
 460               CONTINUE
              END IF
              WRMS = WRMS + ( ( VAL(J5) - &
     &                        ( SHIFT + DR_VAL*(TIM(J5) - MEAN_T) ) )* &
     &                          WEI(J5) )**2
              WW = WW + WEI(J5)**2
!
              KP = KP + 1
              TIM(KP) = TIM(J5)
              VAL(KP) = VAL(J5) - SH_VAL(1)
              ERR(KP) = ERR(J5)
!
              IF ( IPAR .GE. 1 ) THEN
                   VAL(KP) = VAL(KP) - DR_VAL*( TIM(J5) - MEAN_T )
              END IF
         END IF
 450  CONTINUE
      CHI  = WRMS/(NP - (L_EPC+1))
      WRMS = DSQRT ( WRMS/WW )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  REPEATABILITY  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE RGRW8_BR ( N, T, D, W, IV, MEAN_T, L_EPC, TIM_EPC, &
     &                      DR_VAL, DR_SIG, SH_VAL, SH_SIG, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine RGRW8 computes parameters of linear regression by using  *
! *   weighted LSQ for the function D(t) with weights W(t):              *
! *                                                                      *
! *     D(t) = SH_VAL + DR_VAL * ( t - mean_t ).                         *
! *                                                                      *
! *   Array IV consists of 0 and 1 and it marks the points which are     *
! *   used and which are not used. If IV(I)=1 then the I-th point        *
! *   participates in computation of regression, if IV(I)=0 then the     *
! *   I-th point is excluded from  computation.                          *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  N ( INTEGER*4      ) -- Number of elements of vectors D and T.      *
! *  T ( REAL*8         ) -- Array of arguments of the function.         *
! *                          Dimension: N.                               *
! *  D ( REAL*8         ) -- Array of values of the function.            *
! *                          Dimension: N.                               *
! *  W ( REAL*8,    OPT ) -- Array of weights. If the argument is        *
! *                          omitted then weights 1 are used for all     *
! *                          points.                                     *
! * IV ( INTEGER*4, OPT ) -- Participation vector. consists of 0 and 1.  *
! *                          If IV(I)=1 then the I-th point is taken     *
! *                          into account in computation of regression,  *
! *                          otherwise it is omitted.                    *
! * L_EPC ( INTEGER*4   ) -- The number of epochs with breaks.           *
! * TIM_EPC ( REAL*8    ) -- Array of time epochs of breaks. Dimension:  *
! *                          L_EPC.                                      *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * MEAN_T ( REAL*8     ) -- Mean value of the argument.                 *
! * DR_VAL ( REAL*8     ) -- Rate of change of regression.               *
! * DR_SIG ( REAL*8     ) -- Formal weighted uncertainty of the rate of  *
! *                          change. Multiplicative reweighting is       *
! *                          applied.                                    *
! * SH_VAL ( REAL*8     ) -- Array of regression epochs. Dimension:      *
! *                          L_EPC.                                      *
! * SH_SIG ( REAL*8     ) -- Array of formal weighted uncertainty of     *
! *                          the value of regressions. Multiplicative    *
! *                          reweighting is applied. Dimension: L_EPC.   *
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
! *  ###  28-JAN-2003   RGRW8_BR  v1.0 (c)  L. Petrov  28-JAN-2003  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT  NONE
        INTEGER*4 N, L_EPC, IUER
        REAL*8    T(N), D(N), W(N), MEAN_T, DR_VAL, DR_SIG, &
     &            TIM_EPC(L_EPC), SH_VAL(L_EPC+1), SH_SIG(L_EPC+1)
        INTEGER*4  IV(*)
        INTEGER*4  M_PAR, M_PR2
        PARAMETER  ( M_PAR = 64, M_PR2 = (M_PAR*(M_PAR+1))/2 )
        CHARACTER  STR*64
        REAL*8     A(M_PAR), SUSQ, NOR_VEC(M_PAR), NOR_MAT(M_PR2), EST(M_PAR), &
     &             MEAN_VAL, RC, SIG, WW, FREDEG
        INTEGER*4  J1, J2, J3, J4, J5, LL, NZ, IND(M_PAR), L_PAR, IER
!
        MEAN_T = 0.0D0
        MEAN_VAL = 0.0D0
        WW = 0.0D0
        NZ = 0
        DO 410 J1=1,N
           IF ( IV(J1) .GT. 0 ) THEN
                MEAN_T = MEAN_T + T(J1)*W(J1)
                WW = WW + W(J1)
                MEAN_VAL = MEAN_VAL + D(J1)*W(J1)
                NZ = NZ + 1
           END IF
 410    CONTINUE
!
        IF ( NZ .LE. 2 ) THEN
             CALL CLRCH ( STR )
             CALL INCH  ( NZ, STR )
             CALL ERR_LOG ( 741, IUER, 'RGRW8_BR', 'Too few used points: '// &
     &            STR )
             RETURN
        END IF
        MEAN_T   = MEAN_T/WW
        MEAN_VAL = MEAN_VAL/WW
!
! ----- Computation of coeficients of normal system
!
        L_PAR = L_EPC + 2
        IND(1) = 1
        IND(2) = 2
!
        CALL NOUT_R8 ( M_PAR, NOR_VEC )
        CALL NOUT_R8 ( M_PR2, NOR_MAT )
        SUSQ = 0.0D0
!
        DO 420 J2=1,N
           IF ( IV(J2) .GT. 0 ) THEN
                A(1) = T(J2) - MEAN_T
                A(2) = 1.0D0
                IF ( L_EPC .GT. 0 ) THEN
                     DO 430 J3=1,L_EPC
                        IND(J3+2) = J3+2
                        IF ( T(J2) .LT. TIM_EPC(J3) ) THEN
                             A(J3+2) = 0.0D0
                          ELSE
                             A(J3+2) = 1.0D0
                        END IF
  430                CONTINUE
                END IF
!
                CALL ADD_TRG ( (D(J2)-MEAN_VAL), 1.0D0/W(J2), L_PAR, IND, A, &
     &                         L_PAR, NOR_VEC, NOR_MAT )
                SUSQ = SUSQ + ( (D(J2)-MEAN_VAL)*W(J2) )**2
           END IF
  420   CONTINUE
!
! --- Invert normal matrix
!
!          write ( 6, * ) ' l_par=',l_par,' susq=',susq ! %%
!          write ( 6, * ) ' mean_val=',mean_val
!          write ( 6, * ) ' nor_mat =',nor_mat(1:10) ! %%
!          write ( 6, * ) ' tim_epc =',tim_epc(1:l_epc) ! %%
      CALL ERR_PASS ( IUER, IER )
      CALL INVS     ( L_PAR, NOR_MAT, RC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 742, IUER, 'RGRW8_BR', 'Error during inversion '// &
     &         'of the normal matrix' )
           RETURN
      END IF
!
! --- Find vector of the estimates of the parameters
!
      IER=-1
      CALL MUL_MV_SV_V ( L_PAR, NOR_MAT, L_PAR, NOR_VEC, L_PAR, EST, IER )
!
! --- Find the number of degrees of freedom
!
      FREDEG = NZ - L_PAR
      IF ( FREDEG .GT. 0.99 ) THEN
!
! -------- Extracting vector of estimates of uncertainties of adjustments
! -------- Transformation covariance matrix to correlation matrix
!
           CALL ERR_PASS ( IUER, IER )
           CALL LSQ_DISP ( L_PAR, NOR_MAT, NOR_VEC, SUSQ, EST, SIG, FREDEG, &
     &                     IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 743, IUER, 'RGRW8_BR', 'Error during '// &
     &              'calculation dispersion of the estimates' )
                RETURN
           END IF
         ELSE
!
! -------- Don't calculate mean error of unit weight for special cases
!
           DO 440 J4=1,N
              LL = (J4*(J4+1))/2
              NOR_VEC(J4) = DSQRT( NOR_MAT(LL) )
 440       CONTINUE
      END IF
!
      DR_VAL = EST(1)
      DR_SIG = NOR_VEC(1)
!
      DO 450 J5=0,L_EPC
         SH_VAL(J5+1) = EST(J5+2)
         SH_SIG(J5+1) = NOR_VEC(J5+2)
 450  CONTINUE
      SH_VAL(1) = SH_VAL(1) + MEAN_VAL
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  RGRW8_BR  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_QUAD_TREND ( M, TIM, VAL, SH, DR, SQ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_QUAD_TREND
! *                                                                      *
! *  ### 06-FEB-2001 GET_QUAD_TREND v1.0 (c) L. Petrov  06-FEB-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  M, IUER
      REAL*8     TIM(M), VAL(M), SH, DR, SQ
      REAL*8     COV(6), VEC(3), TAV, TMX, TLN, TRM_0, TRM_1, TRM_2, TARG, &
     &           RC, EST(3)
      INTEGER*4  J1, J2, IER, I, J, LOCC
      LOCC(I,J) = MIN(I,J) + (MAX(I,J)*(MAX(I,J)-1))/2
!
      TAV = 0.0
      TMX = 0.0
      DO 410 J1=1,M
         TAV = TAV + TIM(J1)
         IF ( TIM(J1) .GT. TMX ) TMX = TIM(J1)
 410  CONTINUE
      TAV = TAV/M
      TLN = TMX/2.0
!
      CALL NOUT_R8 ( 6, COV )
      CALL NOUT_R8 ( 3, VEC )
!
      DO 420 J2=1,M
         TRM_0 = 1.0
         TRM_1 = (TIM(J2)-TAV)/TLN
         TRM_2 = ( (TIM(J2)-TAV)/TLN )**2
!
         COV(LOCC(1,1)) = COV(LOCC(1,1)) + TRM_0
         COV(LOCC(1,2)) = COV(LOCC(1,2)) + TRM_0*TRM_1
         COV(LOCC(1,3)) = COV(LOCC(1,3)) + TRM_0*TRM_2
         COV(LOCC(2,2)) = COV(LOCC(2,2)) + TRM_1*TRM_1
         COV(LOCC(2,3)) = COV(LOCC(2,3)) + TRM_1*TRM_2
         COV(LOCC(3,3)) = COV(LOCC(3,3)) + TRM_2*TRM_2
!
         VEC(1) = VEC(1) + TRM_0*VAL(J2)
         VEC(2) = VEC(2) + TRM_1*VAL(J2)
         VEC(3) = VEC(3) + TRM_2*VAL(J2)
 420  CONTINUE
!
      CALL ERR_PASS ( IUER, IER )
      CALL INVS     ( 3, COV, RC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5677, IUER, 'GET_QUAD_TREND ', 'Failure to invert '// &
     &                   'the normal matrix' )
           RETURN
      END IF
!
! --- Find vector of the estimates of the parameters
!
      CALL ERR_PASS  ( IUER, IER )
      CALL MUL_MV_SV_V ( 3, COV, 3, VEC, 3, EST, IER )
!
      TARG = -TAV/TLN
      SH =   EST(1) + TARG*EST(2) + TARG**2*EST(3)
      DR = ( EST(2) + 2.0*TARG*EST(3) )/TLN
      SQ =   EST(3)/TLN**2
!
!!      type *,' tav=',tav,' tln=',tln  ! %%%%
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GET_QUAD_TREND  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ROOT_TREND ( M, TIM, VAL, A, B, IUER )
      IMPLICIT   NONE
      INTEGER*4  M, IUER
      REAL*8     TIM(M), VAL(M), COV(3), DSP(2), EST(2), RC, DA, DB, A, B, &
     &           TAV, TLN, TMX, TARG, A_FLOOR, A_MIN, A_MAX, A_STEP, A_BEST, &
     &           B_MIN, B_MAX, B_STEP, B_BEST, RES_SQ_MIN, RES_SQ
      PARAMETER  ( A_FLOOR = 0.01 )
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, MSTEP, IER
      PARAMETER  ( MSTEP = 1024 )
!
      TMX = 0.0
      DO 400 J1=1,M
         IF ( TIM(J1) .GT. TMX ) TMX = TIM(J1)
 400  CONTINUE
      TLN = TMX/2.0
!
      A = 2.0
      B = 6.0
!
      DO 420 J2=1,8
         CALL NOUT_R8 ( 3, COV )
         CALL NOUT_R8 ( 2, DSP )
         DO 430 J3=1,M
            TARG = TIM(J3)/TLN
            DA =         A / DSQRT ( A**2 + (B*TARG)**2 )
            DB = B*TARG**2 / DSQRT ( A**2 + (B*TARG)**2 )
!
            DSP(1) = DSP(1) + DA*( VAL(J3) - DSQRT ( A**2 + (B*TARG)**2 ) )
            DSP(2) = DSP(2) + DB*( VAL(J3) - DSQRT ( A**2 + (B*TARG)**2 ) )
            COV(1) = COV(1) + DA*DA
            COV(2) = COV(2) + DA*DB
            COV(3) = COV(3) + DB*DB
 430     CONTINUE
!
         IER = -1
         CALL INVS ( 2, COV, RC, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 5677, -1, 'BASREP', 'Failure to invert '// &
     &                   'the normal matrix' )
              RETURN
         END IF
!
! ------ Find vector of the estimates of the parameters
!
         IER = -1
         CALL MUL_MV_SV_V ( 2, COV, 2, DSP, 2, EST, IER )
!
         A = A + EST(1)
         B = B + EST(2)
!@  write ( 6, * ) ' a = ', a,' b = ', b ! %%%
         IF ( A .LT. A_FLOOR ) A = A_FLOOR
 420  CONTINUE
      B = B/TLN
!
      A_MIN = 0.0
      A_MAX = 10.0
      B_MIN = 0.0
      B_MAX = 20.0
!
      A_STEP = (A_MAX - A_MIN)/(MSTEP-1)
      B_STEP = (B_MAX - B_MIN)/(MSTEP-1)
!
      RES_SQ_MIN = 1.D20
      DO 440 J4=1,MSTEP
         A = A_MIN + (J4-1)*A_STEP
         DO 450 J5=1,MSTEP
            B = B_MIN + (J5-1)*B_STEP
            RES_SQ = 0.0D0
            DO 460 J6=1,M
               TARG = TIM(J6)/TLN
               RES_SQ = RES_SQ + ( VAL(J6) - DSQRT ( A**2 + (B*TARG)**2 ) )**2
 460        CONTINUE 
            IF ( RES_SQ < RES_SQ_MIN ) THEN
                 A_BEST = A
                 B_BEST = B
                 RES_SQ_MIN = RES_SQ 
            END IF
 450     CONTINUE 
 440  CONTINUE 
!@  write ( 6, * ) ' res_sq = ', res_sq, ' res_sq_min = ', res_sq_min ! %%%%%%
!@  write ( 6, * ) ' a_step = ', a_step, ' b_step = ', b_step ! %%
!@  write ( 6, * ) ' a_best = ', a_best, ' b_best = ', b_best ! %%
!@  call diagi_1 ( m, tim, val, -2 ) ! %%%%%%
      A = A_BEST
      B = B_BEST/TLN
      RETURN
      END  !#!  ROOT_TREND  #!#
