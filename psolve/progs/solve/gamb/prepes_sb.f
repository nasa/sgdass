        SUBROUTINE PREPES_SB ( LOBS, NB, ISTEP, SJ, CUTOFF, MINOBS, &
     &             IT, IV, T, OCT, OCF, TAV, TLN, USE, JMP, SH, DR, SQ, &
     &             NZ, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  PREPES_SB  makes pre-estimation of the quadratic model  *
! *   of clock using observations made at single baseline. Group delay   *
! *   ambiguities are being resolved during pre-estimation. The values   *
! *   of ambiguity jumps are being written into the special array JMP.   *
! *                                                                      *
! *     Observations are described by the following stochastic model:    *
! *                                                                      *
! *        X(I)=SH + DR*TT + SQ*TT**2 + XN + JN*SJ + AN                  *
! *                                                                      *
! *        where X(I) --  Value of O-C for group delay for the I-th      *
! *                       observation ( in sec ).                        *
! *              SH   --  Clock shift for basline clock.                 *
! *              DR   --  Clock drift for basline clock.                 *
! *              SQ   --  Frequency drift for baseline clock.            *
! *              TT   --  ( T(I) - TAV )/TLN  -- normalized time of the  *
! *                       observation, where                             *
! *                   TAV = ( T(LOBS) - T(1) ) /2 -- average moment of   *
! *                       observations at this baseline.                 *
! *                   TLN = ( T(LOBS) + T(1) ) /2 -- semilength of the   *
! *                       time span of observations at this baseline.    *
! *             T(I)  --  Time (at scale TAI) of the moments of I-th     *
! *                       observation elapsed from the 1-st observation  *
! *                       of the session (in sec).                       *
! *             XN    --  Noise. Stochastic quantity which is assumed to *
! *                       be less than specified constant CUTOFF.        *
! *                       CUTOFF.                                        *
! *             JN    --  stochastic quantity which may have only        *
! *                       integer values.                                *
! *             SJ    --  Group delay ambiguity constant -- reciprocal   *
! *                       to minimal spacing between frequency channels. *
! *                       (in sec).                                      *
! *             AN    --  Stochastic quantity occured in anomalous       *
! *                       observations. AN=0.D0 for normal observations  *
! *                       and it hase value greater in module than       *
! *                       CUTOFF for anomalous observations.             *
! *                                                                      *
! *     Recomendation: CUTOFF = 5.D-9 sec.                               *
! *                                                                      *
! *       Since CUTOFF << SJ, then parameters  SH, DR, SQ, JN can be     *
! *     determined. Routine PREPES_SB  determines SH, DR, SQ, array JMP  *
! *     ( JMP(I)=-JN(I)*SJ ), finds anomalous observations. The          *
! *     observation is considered as an anomalous one if its deviation   *
! *     from quadratic regression SH + DR*T(I) + SQ*T(I)**2 exceeds in   *
! *     module CUTOFF. USE(I) for detected anomalous observations is     *
! *     set up as .FALSE.                                                *
! *                                                                      *
! *     Description of the algoritm can be found in the text of the      *
! *     routine.                                                         *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *     LOBS ( INTEGER*4 ) -- Total number of observations in session.   *
! *       NB ( INTEGER*4 ) -- Code of analysed baseline.                 *
! *    ISTEP ( INTEGER*4 ) -- Step at whcih the process shoul stop.      *
! *                           6 -- the process should reach the end.     *
! *       SJ ( REAL*8    ) -- Group delay ambiguity constant (in sec).   *
! *   CUTOFF ( REAL*8    ) -- Cutoff limit for outliers detection (sec). *
! *   MINOBS ( INTEGER*4 ) -- Low limit of the humber of observations    *
! *                           at the baseline. If the number of remained *
! *                           observations appears less than this        *
! *                           baseline will marked as failed.            *
! *       IT ( INTEGER*4 ) -- Verbosity level. Less than 6 -- silent     *
! *                           mode. 6 -- debugging mode.                 *
! *       IV ( INTEGER*4 ) -- Vector containing codes of baselines for   *
! *                           each observations.                         *
! *        T ( REAL*8    ) -- Array which contains moment of time for    *
! *                           the observations, elapsed form the first   *
! *                           observation of the session.                *
! *      OCF ( REAL*8    ) -- Array which contain O-C for fringe rate.   *
! *      TAV ( REAL*8    ) -- TAV = ( T(LOBS) - T(1) ) /2 -- average     *
! *                           moment of observations at this baseline.   *
! *      TLN ( REAL*8    ) -- TLN = ( T(LOBS) + T(1) ) /2 -- semilength  *
! *                           of the time span of observations at this   *
! *                           baseline.                                  *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *      SH ( REAL*8     ) -- Clock shift at the moment TAV.             *
! *      DR ( REAL*8     ) -- Clock drift at the moment TAV.             *
! *      SQ ( REAL*8     ) -- Frequency drift at the moment TAV.         *
! *     TAV ( REAL*8     ) -- Average time moment of observations at the *
! *                           baseline.
! *     TLN ( REAL*8     ) -- Semilength of time span of observation at  *
! *                           the basline.                               *
! *      NZ ( REAL*8     ) -- Number remained used observations.         *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *      JMP ( REAL*8    ) -- Vector of corrections of group delay       *
! *                           observables for group delay ambiguties.    *
! *      OCT ( REAL*8    ) -- Array of O-C of group delays. It is being  *
! *                           corrected for group delay ambiguity after  *
! *                           processing.                                *
! *      USE ( LOGICAL*1 ) -- Participation vector. If USE(I) was .FALSE.*
! *                           before processing than I-th observation    *
! *                           was not in use. If I-th observation were   *
! *                           marked as an outlier during processing     *
! *                           then USE(I) will be .FALSE. All used       *
! *                           observations have values USE(I) = .TRUE.   *
! *   IUER ( INTEGER*4, OPT ) -- Universal error habdler.                *
! *                           Input: swicth IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successfull       *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *    Comment:                                                          *
! *             Values of the array IV should not exceed in module the   *
! *             values 1048575.                                          *
! *                                                                      *
! *  ###  11-AUG-94    PREPES_SB    v4.6 (c)  L. Petrov 31-JUL-2001 ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INTEGER*4  LOBS, NB, ISTEP, IV(LOBS), MINOBS, IT, NZ, IUER
        REAL*8     T(LOBS), OCT(LOBS), OCF(LOBS), JMP(LOBS), SJ, CUTOFF, &
     &             SH, DR, SQ, TAV, TLN
        LOGICAL*1  USE(LOBS)
!
        INTEGER*4  MAX_JMP, MAX_IT, MXF_TRY, M_SEG, MINOBS_1, IOUT_SHARE, &
     &             SHARE_FRE
        REAL*8     CUTOFF_SHARE, DEPART_SHARE, DEPART_TIM, CUTOFF_EXC, &
     &             MAX_FRE0, MAX_FRE, FRE_STEP, LEV_FRE, SHARE_NZ, SIG_FRE
        PARAMETER ( MAX_JMP=100      ) !  Max possible correction to group delay
!                                      !  ( in SJ*sec )
        PARAMETER ( MAX_IT=16        ) !  Maximal number of iterations
        PARAMETER ( M_SEG=64         ) !  Maximal number of segments
        PARAMETER ( CUTOFF_SHARE=0.5 ) !  Share of the interval of observations
!                                      !  for one segment ( in CUTOFF*sec )
        PARAMETER ( DEPART_SHARE=0.8 ) !  Share of the interval of observations
!                                      !  for departure from segment
!                                      !  ( in CUTOFF*sec )
        PARAMETER ( DEPART_TIM=10800.) !  min time interval for depart. criteria
        PARAMETER ( CUTOFF_EXC=2.0   ) !  Preliminary cutoff level for outliers
!                                      !  detection ( in CUTOFF*sec )
        PARAMETER ( MAX_FRE0= 1.D-10 ) !  Maximal acceptable o-c of FRE
        PARAMETER ( FRE_STEP= 2.0    ) !  Step of increasing MAX_FRE
        PARAMETER ( LEV_FRE = 5.D-12 ) !  Desirable level of o-c of FRE
        PARAMETER ( MINOBS_1= 4      ) !  Absolute min of number of observations
        PARAMETER ( IOUT_SHARE = 3   ) !  Parameter tuning of MAX_FRE
        PARAMETER ( MXF_TRY  = 16    ) !  The number of trying of tuning of
!                                      !  MAX_FRE
        PARAMETER ( SHARE_NZ = 0.3   ) !  Share of all observations which
!                                      !  maximal segment should contain to
!                                      !  reject fringe rate-based clock drift
!                                      !  value
        PARAMETER ( SIG_FRE   = 2.0  ) !  Criterion rejection for fringe rate
        PARAMETER ( SHARE_FRE = 0.5  ) !  Share of observations in fringe rate
!                                      !  when rejection iteration should be
!                                      !  stopped before reaching sigma criter.
!C
        CHARACTER  STR*10
        INTEGER*4  J1, J2, J3, J4, J6, J7, J8, J9, J10, J11, J12, &
     &             J13, J14, J15, J16, J17, J18, IP, KC, &
     &             L_SEG, ICN_SEG(M_SEG), MAX_SEG, IMX_SEG, IOUT, &
     &             J_LAST, J_LASTLAST, IP_MAX, NUM_IT, NZ_OLD, KC_OLD, &
     &             NZ_ALL
        REAL*8     AV_OCT, OC_LEV(M_SEG), OC, OC_CNT, OC_TST, TT, &
     &             ST, STT, STX, SX, DET, SH_FRE, DR_FRE, SH_OLD, DR_OLD, &
     &             TTL, OCL, JMP_OBS, OC_CNT_MAX, OC_TST_MAX, TT_DIF, &
     &             TT_LS(M_SEG), OC_LS(M_SEG)
        REAL*8     R2, RES2, R2_MAX, SIGMA, SIGMA_LAST, TEFF, SH0, DR0, SQ0
!
        LOGICAL*4  FL_DELETE
        INTEGER*4  IPUT_SEG, IDEL_SEG, IGET_SEG, IMARK_OBS, I, J
!
! ----- operator functions for manipulations with elements of baseline codes
! ----- and segment codes:
! -----     Segement code is written in 20-25 -th bits of baseline code value
! -----   during adding segment code.
! -----     20-25 -th bits are cleared during removing segment code from
! -----   baseline code.
! -----     Information is extracted from 20-25 -th bits of baseline code
! -----   during the extraction of segment code.
!
        IPUT_SEG(I,J) = MOD(I,1048576) + SIGN(1048576*J,I) ! add segment code
        IDEL_SEG(J)   = MOD(J,1048576)         !  remove segment code
        IGET_SEG(J)   = ABS(J/1048576)         !  extract segment code
        IMARK_OBS(I)  = IPUT_SEG(I,1)          !  mark the observation
!CCC
        MAX_FRE=MAX_FRE0
!
! ----- Step 1. Parameters of linear trend for O-C of delay rate
! ----- will be found. Observations with very large O-C for delay rate
! ----- will ignored at that stage. Then interative improvement of the trend
! ----- parameters will be done using SIG_FRE -sigma criterion. Iteration will
! ----- be continue even after the level SIG_FRE -sigma untill residuals will
! ----- reach the level LEV_FRE or the number of non-rejected firnge rate reach
! ----- low limit. Preliminary values of frequency shift and drift (DR0, SH0)
! ----- will be found. Attempts to restore the observations rejected at this
! ----- step will be made further.
!
        DO 410 J1=1,MXF_TRY
!
! -------- Making normal system for calculation parmeters of linear regression
! -------- of O-C of dealy rate
!
           NZ  = 0
           NZ_ALL = 0
           ST  = 0.0D0 ! Initialization of accumulators of time summs
           STT = 0.0D0 ! Initialization of acc. of summs of squares of time
           STX = 0.0D0 ! Initialization of acc. multiplications time and O-C
           SX  = 0.0D0 ! Initialization of accumulators of squares of O-C
           IOUT= 0
           DO 420 J2=1,LOBS
              IF ( USE(J2)  .AND.  IV(J2) .EQ. NB ) THEN
                   NZ_ALL = NZ_ALL + 1
                   IF ( DABS(OCF(J2)) .LT. MAX_FRE ) THEN
                        NZ  = NZ + 1
                        TT  = ( T(J2) - TAV )/TLN
                        ST  = ST  + TT
                        STT = STT + TT**2
                        STX = STX + OCF(J2)*TT
                        SX  = SX  + OCF(J2)
                     ELSE
                       IOUT=IOUT+1
                   END IF
               END IF
  420      CONTINUE
           IF ( NZ .GT. IOUT*IOUT_SHARE ) GOTO 810
!
! -------- If the number of outliers for O-C for FRE exceeded the share
! -------- IOUT_SHARE of the number of normal equations then increase
! -------- MAX_FRE by FRE_STEP times and do it oncfe more
!
           MAX_FRE=MAX_FRE*FRE_STEP
  410   CONTINUE
        J1=MXF_TRY
  810   CONTINUE
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!           write ( 6, * ) '1. nb=',nb,' nz=',nz,' iout=',iout  ! %%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! ----- Obtain regression parameters ( solving normal system )
!
        DET=NZ*STT-ST**2
        IF ( DABS(DET) .LT. 1.0D-30 ) THEN
             WRITE ( 6, * ) ' J1=',J1,' NZ=',NZ,' IOUT=',IOUT
             WRITE ( 6, * ) ' NZ=',NZ,' STT=',STT,' ST=',ST
             WRITE ( 6, * ) ' NB = ',NB
             CALL ERR_LOG ( 7561, IUER, 'PREPES_SB', 'Zero determinant '// &
     &           '(during initial calculations)' )
             RETURN
        END IF
!
        SH_FRE = (STT*SX - ST*STX)/DET
        DR_FRE = (NZ*STX - ST*SX )/DET
!
! ----- Iterative rejection of outliers of delay rate O-C using 3-sigma
! ----- criterion in oredr to improve parameters of linear trend of delay rate.
!
        IF ( NZ .LE. NZ_ALL*SHARE_FRE  .OR. &
     &       NZ .LE. MINOBS_1               ) THEN
!
! ---------- Unsufficient number of observations
!
             CALL ERR_PASS ( 7571, IUER )
             RETURN
        END IF
        SIGMA_LAST=0.D0
        DO 430 J3=1,NZ_ALL
           IF ( NZ .LE. NZ_ALL*SHARE_FRE ) GOTO 830  ! End of iterations:
!                                          ! too much rates alreaady rejected
           IF ( NZ .LE. MINOBS_1         ) GOTO 830  ! End of iterations:
!                                                    ! we reached low limit
!
! -------- Calculation sum of squares of residuals and
! -------- search of observation (with number IP_MAX) with the largest residual
! -------- of delay rate (in module)
!
           R2_MAX = 0.D0  ! storage for square of the residual
           IP_MAX = 0.D0
!
           R2=0.D0        ! storage for sum of squares of the residuals
           DO 440 J4=1,LOBS
              IF ( IV(J4) .EQ. NB ) THEN
                   IF ( USE(J4)  .AND.  DABS( OCF(J4) ) .LT. MAX_FRE ) THEN
                        TT=(T(J4) - TAV)/TLN
                        RES2 = ( OCF(J4) - ( SH_FRE + DR_FRE*TT ) )**2
                        R2 = R2 + RES2
                        IF ( RES2 .GT. R2_MAX ) THEN
                             R2_MAX =  RES2
                             IP_MAX = J4
                        END IF
                   END IF
              END IF
  440      CONTINUE
!
! -------- Calculation of average error of the unity of weight
!
           SIGMA  = DSQRT ( R2/(NZ-2) )
!
! -------- Test: is R_MAX below or higher 3-sigma criterion?
!
           IF ( ( SQRT(R2_MAX) .GT. SIG_FRE*SIGMA ) .OR. &  !  3-sigma criterion
     &          ( SIGMA .GT. LEV_FRE       ) &       !  r.m.s. greater than LEV_FRE
     &                                            )  THEN
!
! ------------- Update shift and drift parameters of frequency rate
!
                IV(IP_MAX)=IMARK_OBS( IV(IP_MAX) ) ! Mark this observation
                TT=( T(IP_MAX) - TAV )/TLN
!
! ------------- Correct normal matrix
!
                ST  = ST  - TT
                STT = STT - TT**2
                STX = STX - OCF(IP_MAX)*TT
                SX  = SX  - OCF(IP_MAX)
!
! ------------- Update estimates od frequency shift and drift
!
                NZ=NZ-1
                DET=NZ*STT-ST**2
                IF ( DABS(DET) .LT. 1.0D-30 ) THEN
                     CALL ERR_LOG ( 7562, IUER, 'PREPES_SB', 'Zero '// &
     &                   'determinant (during prelimimary iterative '// &
     &                   'imporvement)' )
                     RETURN
               END IF
!
               SH_FRE=(STT*SX - ST*STX)/DET
               DR_FRE=(NZ*STX - ST*SX )/DET
             ELSE
               GOTO 830 ! No one bad observation detected. The end of iterations
           END IF
           SIGMA_LAST = SIGMA
  430   CONTINUE
  830   CONTINUE
!
! ----- Remove "marks" from marked observations
!
        DO 460 J6=1,LOBS
           IV(J6)=IDEL_SEG( IV(J6) )
  460   CONTINUE
!
        DR0 = TLN*SH_FRE       !  Scaled clock *T    -term
        SQ0 = TLN/2.D0*DR_FRE  !  Scaled clock *T**2 -term
!
        IF ( ISTEP.LE.1 ) THEN
             TEFF = ( T(1) - TAV )/TLN
             SH =  TEFF*DR0 + TEFF**2*SQ0        !  Unscaled clock free   -term
             DR = (DR0 + 2.D0*TEFF*SQ0 )/TLN     !  Unscaled clock *t     -term
             SQ =  SQ0/(TLN**2)                  !  Unscaled clock *t**2  -term
             CALL ERR_LOG ( 0, IUER )
             RETURN
        END IF
!
        IF ( IT .GE. 6 ) THEN
             TEFF = ( T(1) - TAV )/TLN
             WRITE ( 6, * ) 'PREPES_SB: NB=',NB,' SIGMA_FRE = ',SIGMA,' ',NZ,'/',NZ_ALL
             WRITE ( 6, * ) ' 1.0 SH = ',TEFF*DR0 + TEFF**2*SQ0, &
     &                  ' DR = ',(DR0 + 2.D0*TEFF*SQ0 )/TLN, &
     &                  ' SQ = ',SQ0/(TLN**2)
        END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!         type *,' dr0= ',dr0,' sq0= ',sq0                 ! %%%
!         teff = ( t(1) - tav )/tln                        ! %%%
!         sh = teff*dr0 + teff**2*sq0                      ! %%%
!         dr = dr0/tln + 2.d0*teff*dr0                     ! %%%
!         sq = sq0/tln                                     ! %%%
!         type *,'2. sh = ',sh, ' dr = ',dr, ' sq = ',sq   ! %%%
!         type *,'2. nz=',nz                               ! %%%
!         type *,'2. sh_fre = ',sh_fre,' dr_fre = ',dr_fre ! %%%
!         type *,'2. sigma=',sigma,' sj=',sj               ! %%%
!         type *,'      '                                  ! %%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! ----- Step 2. Preliminary value of clock shift will be found.
! ----- A correction due to preliminary values of frequency shift and drift
! ----- will be substracted from O-C of group delay. Adjacent pairs of
! ----- observations with differencies between O-C greater than MAX_JMP*SJ limit
! ----- are not taken into account. They are called "wild outliers". Up to
! ----- MAX_IT-1 iterations: rejection observations deviated by more than
! ----- MAX_JMP*SJ, correction values AV_OCT due to rejection of observations.
! ----- Iterations will be terminated when after the next iteration no one
! ----- "wild outliers" will be found. If less than MINOBS observations remained
! ----- than an error is detected and the work will be stopped. Good
! ----- observations rejected at this step will be rehabilitated at the next
! ----- step in future.
!
        DO 470 J7=1,MAX_IT
           KC=0
           NZ=0
           J_LAST=0
           J_LASTLAST=0
           AV_OCT = 0.0D0
           DO 480 J8=1,LOBS
              IF ( USE(J8)  .AND.  IV(J8) .EQ. NB ) THEN
                 FL_DELETE = .FALSE.
                 TT=(T(J8) - TAV)/TLN
!
! -------------- Calculation corrected O-C for group delay
!
                 OC  = OCT(J8) - ( DR0*TT + SQ0*TT**2 )
                 IF ( J_LAST .NE. 0 ) THEN
                    TTL=(T(J_LAST) - TAV)/TLN
                    OCL = OCT(J_LAST) - ( DR0*TTL + SQ0*TTL**2 )
!
! ----------------- Test: does the difference of the current O-C and the
! ----------------- previous one exceed the wild outlier limit ?
!
                    IF ( DABS(OC-OCL) .GT. MAX_JMP*SJ ) THEN
!
! ---------------------- Ja! Let's play back: delete the previous observation
! ---------------------- from the accumulators for average.
!
                         NZ=NZ-1
                         KC=KC+1
                         AV_OCT=AV_OCT - OC
                         IV(J_LAST)=IMARK_OBS( IV(J_LAST) ) ! Mark previous
                         IV(J8)=IMARK_OBS( IV(J8) )    ! and current observation
                         J_LAST=J_LASTLAST
                         FL_DELETE = .TRUE.
                     END IF
                 END IF
!
                 IF ( .NOT. FL_DELETE ) THEN
                      AV_OCT=AV_OCT + OC
                      NZ=NZ+1  !  Increment the number of observations counter
                      J_LASTLAST=J_LAST
                      J_LAST=J8
                 END IF
              END IF
  480      CONTINUE
           IF ( KC.EQ.0 ) GOTO 870
  470   CONTINUE
  870   CONTINUE
!
! ----- Remove "marks" from observations
!
        DO 490 J9=1,LOBS
           IV(J9)=IDEL_SEG( IV(J9) )
  490   CONTINUE
        IF ( NZ .LT. MINOBS ) THEN
             CALL ERR_PASS ( 7571, IUER )
             RETURN
        END IF
!
! ----- The first determination of preliminary values of clock shift and drift
! ----- and frequency drift.
!
        SH0 = AV_OCT/NZ
!
        IF ( ISTEP .LE. 2 ) THEN
             TEFF = ( T(1) - TAV )/TLN
             SH =  SH0 + TEFF*DR0 + TEFF**2*SQ0  !  Unscaled clock free   -term
             DR = (DR0 + 2.D0*TEFF*SQ0 )/TLN     !  Unscaled clock *t     -term
             SQ =  SQ0/(TLN**2)                  !  Unscaled clock *t**2  -term
             CALL ERR_LOG ( 0, IUER )
             RETURN
        END IF
        IF ( IT .GE. 6 ) THEN
             TEFF = ( T(1) - TAV )/TLN
             WRITE ( 6, * ) ' 2.0 SH = ',SH0 + TEFF*DR0 + TEFF**2*SQ0, &
     &                  ' DR = ',(DR0 + 2.D0*TEFF*SQ0 )/TLN, &
     &                  ' SQ = ',SQ0/(TLN**2)
        END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!        type *,' av_oct = ',av_oct,' nz=',nz                   ! %%%%%%%%%%
!        type *,' 3.  sh0=',sh0,' dr0=',dr0,' sq0=',sq0         ! %%%%%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! ----- Step 3. Splitting observations onto segments. Observations which differ
! ----- no more than by CUTOFF*CUTOFF_SHARE will be put at the same segment.
! ----- Membership to the segment is written in free bits of IV vector (bits
! ----- 20-25 will contain the number of the segment). List ICN_SEG will contain
! ----- the number of points in each segment.
!
        L_SEG=0  !  Initial
        NZ=0     !  zeroing
        DO 4100 J10=1,LOBS
           IF ( USE(J10)  .AND.  IV(J10) .EQ. NB ) THEN
              TT = (T(J10) - TAV)/TLN
              OC = OCT(J10) - (SH0 + DR0*TT + SQ0*TT**2)  !  Calculation O-C,
!                !    corrected for clock offset, rate and frequency rate
              IF ( NZ.EQ.0 ) THEN
!
! --------------- This observation is the first observation at the baseline
!
                  L_SEG=1           !  Open the first segment
                  ICN_SEG(L_SEG)=1
                  IV(J10)=IPUT_SEG( IV(J10), L_SEG ) ! ... and put observation
                  OC_LEV(L_SEG)=OC                   !     there
                  TT_LS(L_SEG)=T(J10)
                  OC_LS(L_SEG)=OCT(J10)
                ELSE
!
! --------------- Scanning of all segments in pursue of the segment
! --------------- to which the observation would suit.
!
                  DO 4110 J11=1,L_SEG
                     TT_DIF = ( T(J10) - TT_LS(J11) )/TLN
!
! ------------------ We check:
! ------------------   1) Does this observation lay not too far from the
! ------------------      previous one for clock drift and frequency drift may
! ------------------      exceed considerable share of ambiguity spacing;
! ------------------   2) Does the difference the current O-C and the last O-C
! ------------------      of the segment exceed the limit;
! ------------------   3) Does difference O-C and O-C average for this segment
! ------------------      exceed limit.
! ------------------ If all three tests are OK -- we add this obervation to
! ------------------ this segment.
!
                     IF ( ( T(J10) - TT_LS(J11) ) .GT. DEPART_TIM ) THEN
                        IF ( &
     &                     DABS(DR0*TT_DIF + SQ0*TT_DIF**2) .GT. SJ*DEPART_SHARE &
     &                        .OR. &
     &                     DABS(OCT(J10)-OC_LS(J11)) .LT. SJ*DEPART_SHARE &
     &                     ) GOTO 4110
                     END IF
!
                     IF ( DABS(OC - OC_LEV(J11)) .LT. SJ*CUTOFF_SHARE ) THEN
!
! ----------------------- Oh yes! This observation suits to the current
! ----------------------- segment. Let's add it to the segment.
!
                          IV(J10)=IPUT_SEG( IV(J10), J11 )
                          OC_LEV(J11)=( OC_LEV(J11)*ICN_SEG(J11) + OC )/ &
     &                    ( ICN_SEG(J11)+1 )
                          ICN_SEG(J11)=ICN_SEG(J11)+1
                          TT_LS(J11)  =   T(J10)
                          OC_LS(J11)  = OCT(J10)
                          GOTO 8110
                     END IF
 4110             CONTINUE
!
! --------------- No. No segments to which an oibservation would suit has not
! --------------- been found. Let's create a new segment.
!
                  L_SEG = L_SEG + 1
                  IF ( L_SEG .GT. M_SEG ) THEN
                       CALL CLRCH (        STR )
                       CALL INCH  ( M_SEG, STR )
                       CALL ERR_LOG ( 7564, IUER, 'PREPES_SB', 'Internal '// &
     &                     'error: the limit of segments is exceeded: '// &
     &                     'M_SEG = '//STR )
                       RETURN
                  END IF
!
! --------------- Put an observation to the new segment.
!
                  ICN_SEG(L_SEG)=1
                  IV(J10)=IPUT_SEG( IV(J10), L_SEG )
                  OC_LEV(L_SEG)= OC
                  TT_LS(L_SEG) =   T(J10)
                  OC_LS(L_SEG) = OCT(J10)
 8110             CONTINUE
              END IF
              NZ=NZ+1
           END IF
 4100   CONTINUE
!
! ----- Find  IMX_SEG -- The number of a segment which contains the maximum
! ----- number of obsevations. Observations from this segment are the best
! ----- candidats for the first approximation.
!
        MAX_SEG=0
        IF ( IT .GE. 6 ) THEN
             WRITE ( 6, * ) 'PREPES_SB: L_SEG= ',L_SEG
        END IF
        DO 4120 J12=1,L_SEG
           IF ( ICN_SEG(J12) .GT. MAX_SEG ) THEN
                MAX_SEG = ICN_SEG(J12)
                IMX_SEG = J12
           END IF
           IF ( IT .GE. 6 ) THEN
                WRITE ( 6, * ) 'PREPES_SB: J12= ',J12,' ICN_SEG(J12)= ',ICN_SEG(J12)
           END IF
 4120   CONTINUE
        IF ( IT .GE. 6 ) THEN
             WRITE ( 6, * ) 'PREPES_SB: MAX_SEG = ',ICN_SEG(IMX_SEG), &
     &              ' NZ_ALL = ',NZ_ALL,' LIMIT = ', SHARE_NZ*NZ_ALL
        END IF
!
        IF ( ICN_SEG(IMX_SEG) .LT. 2 ) THEN
             CALL ERR_LOG ( 7565, IUER, 'PREPES_SB', 'There is no one '// &
     &           'segment which contains more than 1 observation' )
             RETURN
        END IF
        IF ( ISTEP.LE.3 ) THEN
             TEFF = ( T(1) - TAV )/TLN
             SH =  SH0 + TEFF*DR0 + TEFF**2*SQ0  !  Unscaled clock free   -term
             DR = (DR0 + 2.D0*TEFF*SQ0 )/TLN     !  Unscaled clock *t     -term
             SQ =  SQ0/(TLN**2)                  !  Unscaled clock *t**2  -term
             CALL ERR_LOG ( 0, IUER )
             RETURN
        END IF
        IF ( ICN_SEG(IMX_SEG) .GT. SHARE_NZ*NZ_ALL ) THEN
!
! ---------- Since maximal segment contains sufficient number of experiments
! ---------- we can reject fringe rate based value for clock drift and not to
! ---------- use it further
!
             DR0 = 0.D0
        END IF
!
! ----- Step 4. Calculation of normal equations for scaled clock shift and
! ----- drift (but without frequency drift) for the observations of
! ----- IMX_SEG-th segment. Then these observations are marked as not belonging
! ----- to any segment. Then parameterts of clock shift and drift are calculated
! ----- using these normal equations. Obtained clock shift and drift will be
! ----- used as the first approximation. They are calculated with respect to the
! ----- the preliminary values of clock parameters.
!
        NZ=0
        ST  = 0.0D0 ! Initialization of accumulators of time summs
        STT = 0.0D0 ! Initialization of accumulators of summs of squares of time
        STX = 0.0D0 ! Initialization of accumulators multiplications time by O-C
        SX  = 0.0D0 ! Initialization of accumulators of squares of O-C
        DO 4130 J13=1,LOBS
           IF ( IGET_SEG(IV(J13)) .EQ. IMX_SEG ) THEN
                TT=(T(J13) - TAV)/TLN
                NZ=NZ+1
                IV(J13)=IDEL_SEG( IV(J13) )
!
                ST =ST +TT
                STT=STT+TT**2
                STX=STX+( OCT(J13) - (SH0 + DR0*TT + SQ0*TT**2) )*TT
                SX =SX +  OCT(J13) - (SH0 + DR0*TT + SQ0*TT**2)
           END IF
 4130   CONTINUE
!
! ----- Calculation of determinant of the normal system
!
        DET=NZ*STT-ST**2
        IF ( DABS(DET) .LT. 1.0D-30 ) THEN
             CALL ERR_LOG ( 7566, IUER, 'PREPES_SB', 'Zero determinant '// &
     &           '( dirung initial calculations )' )
             RETURN
        END IF
!
! ----- Solving normal system and obtaining values of estimates for clock
! ----- shift and drift
!
        SH=(STT*SX - ST*STX)/DET
        DR=(NZ*STX - ST*SX )/DET
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!          type *,'4. sh0= ',sh0,' dr0= ',dr0,' sq0= ',sq0              ! %%%
!          type *,'4.  sh= ',sh, ' dr = ', dr,' nz=',nz                 ! %%%
!          sh =   (sh0+sh) + teff*(dr0+dr) + teff**2*(sq0+sq)           ! %%%
!          dr = ( (dr0+dr) + 2.d0*teff*(dr0+dr) )/tln                   ! %%%
!          sq =   (sq0+sq)/tln**2                                       ! %%%
!          type *,'4. FINE:  sh = ',sh, ' dr = ',dr, ' sq = ',sq        ! %%%
!                                                                       ! %%%
!          SH=(STT*SX - ST*STX)/DET                                     ! %%%
!          DR=(NZ*STX - ST*SX )/DET                                     ! %%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        IF ( ISTEP.LE.4 ) THEN
             TEFF = ( T(1) - TAV )/TLN
!
! ---------- SH -- unscaled clock free   -term
! ---------- DR -- unscaled clock *t     -term
! ---------- SQ -- unscaled clock *t**2  -term
!
             SH =   (SH0+SH) + TEFF*(DR0+DR) + TEFF**2*SQ0
             DR = ( (DR0+DR) + 2.D0*TEFF*SQ0 )/TLN
             SQ =   SQ0/(TLN**2)
!
             CALL ERR_LOG ( 0, IUER )
             RETURN
        END IF
        IF ( IT .GE. 6 ) THEN
             TEFF = ( T(1) - TAV )/TLN
             WRITE ( 6, * ) ' 3.0 SH = ',(SH0+SH) + TEFF*(DR0+DR) + TEFF**2*SQ0, &
     &                  ' DR = ',( (DR0+DR) + 2.D0*TEFF*SQ0 )/TLN, &
     &                  ' SQ = ',SQ0/(TLN**2)
        END IF
!
! ----- Step 5. Scanning of all other remained observations (in turn: first
! ----- from the left edge then form the right edge and so on steadily narrowing
! ----- the range). For each observation the following operations will be done:
! ----- 1) the value of group delay ambiguity will be found and O-C will  be
! ----- corrected for it; 2) the test will be done: does this observation
! ----- deviates form linear regression mor than by CUTOFF_EXC*CUTOFF ? If yes
! ----- then the observation will be (temporarily) rejected. If not then
! ----- 3) system of normal equations will be updated for entering this
! ----- observation. New updated values of estimates of clock shift and drift
! ----- will be obtained. Observations rejected at this step will be tried to
! ----- be restored at further stages.
!
        KC=0
        DO 4140 J14=1,LOBS
!
! -------- Find  IP -- the number of the observations under consideration. The
! -------- expression below generates such a sequence: (J14=1,IP=1),
! -------- (J14=2,IP=LOBS), (J14=3,IP=2), (J14=4,IP=LOBS-1), (J14=5,IP=3),
! -------- (J14=6,IP=LOBS-2) and so on. The sequence of points  IP  is steadily
! -------- narrowing span, converging to the middle point
!
           IP=MOD(J14,2)*(J14+1)/2 + &       ! J14 - odd, IP -- nearer to beginning
     &        MOD(J14+1,2)*(LOBS+1-J14/2) ! J14 - even, IP -- nearer to the end
!
           IF ( USE(IP)                     .AND. &
     &          IDEL_SEG( IV(IP) ) .EQ. NB  .AND. &
     &          IGET_SEG( IV(IP) ) .NE. 0          ) THEN
              TT=(T(IP) - TAV)/TLN
!
! ----------- Correct O-C for preliminary values of clock function
!
              OC_CNT = OCT(IP) - (     SH0  +     DR0*TT  + SQ0*TT**2 )
              OC_TST = OCT(IP) - ( (SH+SH0) + (DR+DR0)*TT + SQ0*TT**2 )
!
! ----------- Find group delay ambiguity for this observation and ...
!
              JMP_OBS = -SJ * NINT ( OC_TST/SJ )
!
! ----------- ... apply this correction to o-c of group delay
!
              JMP(IP) = JMP(IP) + JMP_OBS
              OCT(IP) = OCT(IP) + JMP_OBS
!
! ----------- ... apply this correction to tested and contr. o-c of group delay
!
              OC_TST = OC_TST + JMP_OBS
              OC_CNT = OC_CNT + JMP_OBS
!
! ----------- Test: should we reject this observation?
!
              IF ( DABS(OC_TST) .GT. CUTOFF*CUTOFF_EXC ) THEN
!
! ---------------- Alas, we should. It is so pity, but life is cruel sometimes..
!
                   USE(IP) = .FALSE.
                   KC=KC+1
                ELSE
!
! ---------------- ... No, no it is a good one!
!
! ---------------- New determination of the parameters of regression using
! ---------------- previous observations plus the current one.
!
                   NZ=NZ+1    !  Increment the number of used observations
!
! ---------------- Increment values in accumulator
!
                   ST  = ST  + TT
                   STT = STT + TT**2
                   STX = STX + OC_CNT*TT
                   SX  = SX  + OC_CNT
!
! ---------------- New calculation of normal matrix determinant
!
                   DET=NZ*STT-ST*ST
                   IF ( DABS(DET) .LT. 1.0D-30 ) THEN
                        CALL ERR_LOG ( 7567, IUER, 'PREPES_SB', 'Zero '// &
     &                      'determinant (during iterative improvement)' )
                        RETURN
                   END IF
!
! ---------------- New solving the system of nirmal equations.
!
                   SH=(SX*STT-ST*STX)/DET
                   DR=(NZ*STX-SX*ST)/DET
              END IF
           END IF
 4140   CONTINUE
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!       type *,'5. sh=',sh,' dr=',dr,' nz=',nz                       ! %%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! ----- Now we restore baseline vector
!
        DO 4150 J15=1,LOBS
           IV(J15)=IDEL_SEG( IV(J15) )
 4150   CONTINUE
!
        IF ( NZ.LT.MINOBS ) THEN
             CALL ERR_PASS ( 7572, IUER )
             RETURN
        END IF
        IF ( ISTEP.LE.5 ) THEN
             TEFF = ( T(1) - TAV )/TLN
!
! ---------- SH -- unscaled clock free   -term
! ---------- DR -- unscaled clock *t     -term
! ---------- SQ -- unscaled clock *t**2  -term
!
             SH =   (SH0+SH) + TEFF*(DR0+DR) + TEFF**2*SQ0
             DR = ( (DR0+DR) + 2.D0*TEFF*SQ0 )/TLN
             SQ =    SQ0/(TLN**2)
!
             CALL ERR_LOG ( 0, IUER )
             RETURN
        END IF
!
! ----- Step 6. Iterative rejection observations with deviations more than
! ----- CUTOFF. Values of the estimates of scales clock shift and drift will be
! ----- corrected after eigher rejection. Bad observaions irreversibly marked as
! ----- outliers at this step.
!
        KC=0
        NUM_IT = NZ - MINOBS + 1
        DO 4160 J16=1,NUM_IT
           OC_TST_MAX = 0.0D0
           OC_CNT_MAX = 0.0D0
           IP_MAX     = 0
!
! -------- Search of the observation with maximum residual from linear
! -------- regression.
!
           DO 4170 J17=1,LOBS
              IF ( USE(J17)  .AND.  IV(J17).EQ.NB ) THEN
                   TT =(T(J17) - TAV)/TLN
!
! ---------------- Calculation corrected O-C of group delay and searching the
! ---------------- maximum (in module) residule. We should differ TST- o-c --
! ---------------- "tested" o-c with applied refined clock function and CNT o-c
! ---------------- "contributed" o-c to coarse clock function with respect to
! ---------------- which we are adjusted refined clock function.
!
                   OC_TST = OCT(J17) - ( SH+SH0 + (DR+DR0)*TT + SQ0*TT**2 )
                   IF ( DABS(OC_TST) .GT. OC_TST_MAX ) THEN
                        OC_TST_MAX = DABS(OC_TST)
                        IP_MAX     = J17
                        OC_CNT_MAX = OCT(J17) - ( SH0 + DR0*TT + SQ0*TT**2 )
                   END IF
               END IF
 4170       CONTINUE
!
! --------- Test: Does the max residual exceed the limit?
!
! --------- To do this test we firstly exclude this observation from estimation,
! --------- make update of the solution and calculate residual for the updated
! --------- solutiuon. If observation remained good we restore solution
!
            NZ_OLD = NZ
            KC_OLD = KC
            SH_OLD = SH
            DR_OLD = DR
!
            TT = ( T(IP_MAX) - TAV)/TLN
            NZ = NZ-1
            KC = KC+1
!
! --------- Decrement accumulators
!
            ST  =  ST - TT
            STT = STT - TT**2
            STX = STX - OC_CNT_MAX*TT
            SX  =  SX - OC_CNT_MAX
!
! --------- New calculation of determinant of the normal system
!
            DET = NZ*STT-ST*ST
            IF ( DABS(DET) .LT. 1.0D-30 ) THEN
                 WRITE ( 6, * ) ' NZ=',NZ,' KC=',KC
                 CALL ERR_LOG ( 7568, IUER, 'PREPES_SB', 'Zero '// &
     &               'determinant (during iterative improvement)' )
                 RETURN
            END IF
!
! --------- Solving of the normal system
!
            SH = ( SX*STT - ST*STX )/DET
            DR = ( NZ*STX - SX*ST  )/DET
!
! --------- Calculation new residual
!
            OC_TST = OCT(IP_MAX) - ( (SH+SH0) + (DR+DR0)*TT + SQ0*TT**2 )
            IF ( DABS(OC_TST) .GT. CUTOFF ) THEN
!
! -------------- Alas, exceeded. We should claim that it is an outlier.
! -------------- Oh! We have to reject it. It so pity... but we must mark
! -------------- it as bad.
!
                 USE(IP_MAX) = .FALSE.  ! Farwell, IP_MAX -th observation...
              ELSE
!
! -------------- No! Observation appeared to be a good one.
!
                 NZ = NZ_OLD
                 KC = KC_OLD
                 SH = SH_OLD
                 DR = DR_OLD
!
! -------------- If so, let's stop this nasty business. All other observations
! -------------- are, of course, better than tested one. Let them live!
!
                 GOTO 8160
            END IF
 4160   CONTINUE
        IF ( NZ .LT. MINOBS ) THEN
!
! ---------- Too few observations remained after rejecting outliers. Something
! ---------- is going wrong.
!
             CALL ERR_PASS ( 7573, IUER )
             RETURN
        END IF
 8160   CONTINUE
!
! ----- Normal completion
!       ~~~~~~~~~~~~~~~~~
!
        TEFF = ( T(1) - TAV )/TLN
!
! ----- SH -- unscaled clock free   -term
! ----- DR -- unscaled clock *t     -term
! ----- SQ -- unscaled clock *t**2  -term
!
        SH =   (SH0+SH) + TEFF*(DR0+DR) + TEFF**2*SQ0
        DR = ( (DR0+DR) + 2.D0*TEFF*SQ0 )/TLN
        SQ =    SQ0/(TLN**2)
!
! ----- Now we resolve group delay ambiguity for not used observations
!
        DO 4180 J18=1,LOBS
           IF ( IV(J18) .EQ. NB   .AND.  .NOT. USE(J18)  ) THEN
!
! ------------- NB! Here we have unscaled coefficients of clock function
!
                TT = T(J18)
                OC = OCT(J18) - (SH + DR*TT + SQ*TT**2)  !  Calculation O-C,
!                    !    corrected for clock offset, rate and frequency rate
! ------------- Find group delay ambiguity for this observation and ...
!
                JMP_OBS = -SJ * NINT ( OC/SJ )
!
! ------------- ... apply this correction to o-c of group delay
!
                JMP(J18) = JMP(J18) + JMP_OBS
                OCT(J18) = OCT(J18) + JMP_OBS
           END IF
             if ( iv(j18) .eq. 1048785 ) then  ! %%%%%%%%%%%%%%%%%%%%%%%%%%
                  WRITE ( 6, * ) ' j18=',j18,' iv(j18) = ',iv(j18) ! %%%%%%%%%%%%%%
                  call pause ( 'prepes_sb' ) ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%
             end if ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 4180 CONTINUE
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!       type *,'6. sh=',sh,' dr=',dr,' sq=',sq,' nz=',nz             ! %%%%%
!       type *,' kc=',kc                                             ! %%%%%
!       type *,' '                                                   ! %%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  PREPES_SB  #!#
