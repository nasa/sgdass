      SUBROUTINE SOCOM_EXT()
! ************************************************************************
! *                                                                      *
! *   Routine  SOCOM_EXT  fills some fields of common area SOCOM_PLUS -- *
! *   extension of SOCOM. SOCOM_EXT may work or not work at all (imitate *
! *   the actual work ) in dependence on the value of the variable       *
! *   SOCOM_PLUS_FIRST (defined in socom_plus). If SOCOM_PLUS_FIRST .NE. *
! *   SPL__DONE then socom_ext makes actual work and sets the variable   *
! *   SOCOM_PLUS_FIRST = SPL__DONE. SOCOM_EXT will not do anything if it *
! *   is called the second time. If it is necessary to force SOCOM_EXT   *
! *   to work regardless the previous calls then the variable            *
! *   SOCOM_PLUS_FIRST should be set as SPL__UNDF.                       *
! *                                                                      *
! *     SOCOM_EXT will imitate work, but not to work always if mode      *
! *   of compatibility with SOLVE 9612 (SOLVE at 01-DEC-96) is set up    *
! *   regardless the value SOCOM_PLUS_FIRST.                             *
! *                                                                      *
! *     SOCOM_EXT recalculates epochs for clock, atmosphere and EOP and  *
! *   forms arrays of clock breaks JDATE_BRK, NUM_BRK. This information  *
! *   will be used when uniform segmentation will be in force. It also   *
! *   tests eligibility of implementation fast algorithms.               *
! *                                                                      *
! *     SOCOM_EXT sets values: UNF_CLO, UNF_CLA,  UNF_EOP, FAST_ELIG as  *
! *   .FALSE. if SOLVE_EMULATION mode is 9612. Segmented parameters will *
! *   be declared as non-uniform regardless their actual                 *
! *   parameterization. It is done to switch off fast-SOLVE              *
! *   parameterization scheme.                                           *
! *                                                                      *
! *   By effects:                                                        *
! *                                                                      *
! *     USE_COMMON, USE_PARFIL, USE_GLBFIL, USE_GLBFIL_4 will be closed. *
! *   The first file will be closed in any case, but the latter three -- *
! *   only in the case when actual interval fixing will take place.      *
! *                                                                      *
! *   Who When      Ver. What                                            *
! *                                                                      *
! *   pet 21-APR-97 1.0  The first version.                              *
! *                                                                      *
! *   pet 28-AUG-97 2.4  Bug corrected.                                  *
! *                                                                      *
! *   pet 03-OCT-97 3.0  SOLVE_COMPAT variable support added.            *
! *                                                                      *
! *   pet 24-OCT-97 3.1  Test of old_clocks variable added. If           *
! *                      old_clocks is .TRUE. then clock parameters are  *
! *                      marked as not-uniform.                          *
! *                                                                      *
! *   pet 16-JAN-98 3.2  Setting station deselection status in according *
! *                      with baseline selection added. Deselected       *
! *                      stations are bypassed during the analysis of    *
! *                      estimation configuration.                       *
! *                                                                      *
! *   pet 06-APR-98 3.3  Fixed a bug which prevented using socom_ext for *
! *                      socom area of global solution: variable L_STA   *
! *                      is used instead of global variable NUMSTA.      *
! *                      Additional condition of cgm_type is used.       *
! *                                                                      *
! *   pet 09-AUG-98 3.4  Changed the logic for setting the epochs of     *
! *                      segmented parameters. New logic check the       *
! *                      length of the interval between the last-but-one *
! *                      epochs and the last observation in the session. *
! *                      If this time is less than the specified share   *
! *                      of the shortest epoch interval, then the total  *
! *                      number of epochs decremented and small          *
! *                      overdraft behind the last epoch is allowed.     *
! *                      This scheme prohibits artificial segment with   *
! *                      small (or even zero) number of observations at  *
! *                      the end of session which may arise due to       *
! *                      rounding errors.                                *
! *                                                                      *
! *                      Eliminated "empirical correction of the last    *
! *                      observation of the session" to +1 minute.       *
! *                                                                      *
! *   pet 23-SEP-98 3.5  Corrected a coding error which prevented        *
! *                      normal work in the case when EOP are estmiated  *
! *                      as segmented parameters, but atmopshere not.    *
! *                                                                      *
! *   pet 08-MAY-99 3.6  Corrected a bug: variables CLO_INTERVAL,        *
! *                      ATM_INTERVAL, EOP_INTERVAL were not initialized *
! *                      what may lead to an error since they kept the   *
! *                      previous value.                                 *
! *                                                                      *
! *  pet 2001.07.16 3.7   Slightly changed the logic of roinding         *
! *                       interval length in order to prevent sortening  *
! *                       or widening time interval by 1 minute due to   *
! *                       rounding errors.                               *
! *                                                                      *
! *  pet 2002.03.19 3.8   Replaced variable SOLVE_COMPAT with            *
! *                       SOLVE_EMULATION                                *
! *                                                                      *
! *  jwr 2002.08.04       Comments starting with ! in column 1 are       *
! *                       replaced with comments with c in column 1      *
! *                                                                      *
! *  pet 2003.08.12 3.9   Added computing array BASCL_IND.               *
! *  pet 2003.08.22 3.91  Fixed bug in the 3.9 fix.                      *
! *  pet 2003.09.01 3.10  Fixed a bug in setting indexes for source      *
! *                       proper motion index array.                     *
! *  pet 2003.09.01 3.101 Fixed another bug in setting indexes for       *
! *                       source position and proper motion array:       *
! *                       SOCOM_EXT should not do it for CGM-type        *
! *                       socom blocks.                                  *
! *  pet 2004.11.26 3.11  Made the logic of computing the max degree of  *
! *                       the low degree clock polynomial more robust.   *
! *  pet 2005.08.23 3.12  Fixed a bug: the check for the number of       *
! *                       atmospheric zentih path delay parameters was   *
! *                       bypassed for the clock reference statsion.     *
! *  pet 2006.01.18 3.13  Added computation of IND_ERM_NOD -- index      *
! *                       of the pivotal element for ERM.                *
! *  pet 2021.12.04 3.14  Made checls for EOP B-spline more stringent.   *
! *                                                                      *
! *  ###  21-APR-1997  SOCOM_EXT  v3.14 (c)  L. Petrov  04-DEC-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'astro_constants.i'
      INCLUDE    'solve.i'
      INCLUDE    'erm.i'
      INCLUDE    'socom.i'
      INCLUDE    'socom_plus.i'
      INCLUDE    'glbcm.i'
      INCLUDE    'glbc4.i'
      INCLUDE    'fast.i'
      INTEGER*4  NS, NSEG_CLO, NSEG_ATM, NCOUNT_BSL, FM_POI, L_STA, &
     &           NCOUNT_POS, NCOUNT_PRP, IORD, IUER, &
     &           J0, J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12
      INTEGER*2  INT2_ARG, K2, OLD_USER_PART__SAVE
      CHARACTER  STR*20
      LOGICAL*4  CHECK_STABIT
      REAL*8     JFIRST_OBS, JLAST_OBS, JLAST_EPOCH, RL, &
     &           JDATE_CLO_LAST, JDATE_ATM_LAST, JDATE_EOP_LAST, &
     &           MIN_INTERVAL, MAX_INTERVAL
      REAL*8     EPS_SEC, SEG_OVD, MIN_OVD, MAX_OVD
      LOGICAL*4  DO_OVD
      PARAMETER  ( EPS_SEC = 0.1D0 ) ! max acceptable difference in epoch
      PARAMETER  ( SEG_OVD = 0.2D0 ) ! max acceptable overdraw in time epoch
!                                    ! for the last segment. By other words
!                                    ! the last segment will not have duration
!                                    ! less than <INTERVAL>*SEG_OVD.
      PARAMETER  ( MIN_OVD = 60.D0/86400.D0 ) ! min lehgth of the min interval
!                                    ! to allow to apply overdraw correction
      PARAMETER  ( MAX_OVD = 6.D0/24.D0     ) ! max lehgth of the min interval
!                                    ! (in days) to allow to apply overdraw
!                                    ! correction
      PARAMETER  ( DO_OVD = .TRUE. ) ! If yes then try to make overdraw
!                                    ! correction of the clock intervals
      INTEGER*4  INT4
      LOGICAL*2, EXTERNAL :: KBIT
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! --- Test: should we work or not?
!
      IF ( SOCOM_PLUS_FIRST .EQ. SPL__DONE ) RETURN
!
! --- Setting bits of station's  deselection status
!
      CALL SET_STABIT ( INT2(2) )
!
      UNF_CLO = .TRUE.
      UNF_ATM = .TRUE.
      UNF_EOP = .FALSE.
      FAST_ELIG = .FALSE.
!
      NUM_CLO = 0
      NUM_ATM = 0
      NUM_EOP = 0
!
      CALL USE_GLBFIL_4 ( 'ORC' )
      IF ( FAST_MODE .NE. F__NONE  .AND.  SOLVE_EMULATION .NE. 0 ) THEN
           FM_POI = 1
           DO 510 J1=1,FMI_VAR
              IF ( FM_VAL(J1) .EQ. FAST_MODE ) FM_POI = J1
 510       CONTINUE
!
           WRITE ( 6, * ) ' FAST_MODE = ',FM_STR(FM_POI)(1:FM_LEN(FM_POI))// &
     &            ' is not compatible with SOLVE_EMULATION = ', SOLVE_EMULATION
           WRITE ( 6, * ) ' Only FAST_MODE "NONE" is allowed in emulation '// &
     &                    'of SOLVE archaic parametrization style'
           STOP 'SOCOM_EXT'
      END IF
!
      IF ( SOLVE_EMULATION .NE. 0  .AND.  SOLVE_EMULATION .NE. 9612 ) THEN
           WRITE ( 6, * ) ' SOLVE_EMULATION has wrong value: ', SOLVE_EMULATION
           WRITE ( 6, * ) ' Acceptable values are only 0 or 9612 '
           STOP   'SOCOM_EXT'
      END IF
!
      IF ( SOLVE_EMULATION .EQ. 9612 ) THEN
!
! -------- Premature exit in the SOLVE 9612 compatibility mode.
!
           UNF_CLO   = .FALSE.
           UNF_ATM   = .FALSE.
           UNF_EOP   = .FALSE.
           FAST_ELIG = .FALSE.
!
           SOCOM_PLUS_FIRST = SPL__DONE
!
           RETURN
      END IF
!
! --- Test: 1) Eligibility of FAST MODE 2) Uniformity segements for clocks and
! --- atmosphere
!
      NSEG_CLO = -1
      NSEG_ATM = -1
      ICLMAX = 1
      IF ( .NOT. CGM_TYPE ) THEN
           DO 400 J0=1,NUMSTA
              IF ( NUMCLK(J0) .GT. 0 ) THEN
                   IF ( KBIT( ICLSTA(1,1+ICLSTR(J0)), INT2(J0) ) ) THEN
!
! ---------------------- Recalculate ICLMAX
!
                         DO IORD = 0,4 ! running over 0th to 4 order
!
! -------------------------- We test max degree for the first segment. We assume
! -------------------------- that max degree should be the same for all intervals
!
                             IF ( KBIT( LCLK(1+ICLSTR(J0)), INT2(IORD+1) ) ) THEN
                                  IF ( IORD+1 .GT. ICLMAX ) ICLMAX = IORD+1
                             ENDIF
                         ENDDO
                   END IF
              END IF
 400       CONTINUE
      END IF
      NPL_CLO  = ICLMAX - 1 ! max degree of global polinmial for clocks
!
! --- Initialize NUM_BRK
!
      CALL NOUT ( 4*MAX4_STA, NUM_BRK )
!
      IF ( .NOT. CGM_TYPE ) THEN
!
! ------ Single session mode
!
         L_STA = MIN ( INT4(NUMSTA), MAX4_STA )
         DO 410 J1=1,NUMSTA
            IF ( ICLSTR(J1) > MAX_CLK - 1 ) GOTO 410
            IF ( KBIT( ICLSTA(1,1+ICLSTR(J1)), INT2(J1)) ) THEN
!
! ------------ Bypass deselected station
!
               IF ( .NOT. CHECK_STABIT ( INT2(J1) ) ) GOTO 410
!
               IF ( NUMCLK(J1) .GT. 0 ) THEN
                    IF ( .NOT. CLK_BRK_STAT ) THEN
!
! ---------------------- If we don't have clock breaks we assume that the
! ---------------------- number of clock parameters for J1-th station
! ---------------------- is NUMCLK(J1)
!
                         IF ( NSEG_CLO .LT. 0 ) THEN
                              NSEG_CLO = NUMCLK(J1)
                            ELSE
                              IF ( NSEG_CLO .NE. NUMCLK(J1) ) UNF_CLO = .FALSE.
                         END IF
                       ELSE
!
! ---------------------- In the case of clock breaks we don't count the
! ---------------------- number of parameters, but we should gather
! ---------------------- informations about the number of breaks on the
! ---------------------- j1-th station and the epochs of the breaks.
!
                         NS = 1
                         DO 520 J2=2,NUMCLK(J1)
                            K2 = J2 + ICLSTR(J1)
!
! ------------------------- Test bit of participation of the j1-th station
! ------------------------- in estimation
!
                            IF ( KBIT(ICLSTA(1,K2), INT2(J1)) ) THEN
                                 IF ( KBIT( LCLK(K2), INT2(13)) ) THEN
!
! ----------------------------------- Attribute of continuous segment
!
                                      NS = NS+1
                                    ELSE
!
! ----------------------------------- Attribute of clock break segment
!
                                      NUM_BRK(J1) = NUM_BRK(J1) + 1
                                      JDATE_BRK(NUM_BRK(J1),J1) = FJDCL(J2)
                                 END IF
                            END IF
 520                     CONTINUE
                         IF ( NS .GT. 1 ) NSEG_CLO = NS
                    END IF ! clk_brk_stat
               END IF  ! NUMCLK
            END IF  ! iclsta
!
! --------- Test for atmosphere
!
            IF ( NUMATM(J1) .GT. 0 ) THEN
                 IF ( NSEG_ATM .LT. 0 ) THEN
                      NSEG_ATM = NUMATM(J1)
                    ELSE
                      IF ( NSEG_ATM .NE. NUMATM(J1) ) UNF_ATM = .FALSE.
                 END IF
            END IF
  410    CONTINUE
      END IF ! cgm_type
!
      IF (  OLD_CLOCKS     ) UNF_CLO = .FALSE.
      IF ( .NOT. BMODE_CL  ) UNF_CLO = .FALSE.
      IF ( NSEG_CLO .LE. 1 ) UNF_CLO = .FALSE.
      IF ( .NOT. BMODE_AT  ) UNF_ATM = .FALSE.
      IF ( NSEG_ATM .LE. 1 ) UNF_ATM = .FALSE.
!
! --- Test of uniformity segements for EOP
!
      IF ( EOP_STYLE(1).EQ. EOP__RATES_AND_SEGS .OR. &
     &     EOP_STYLE(1).EQ. EOP__SEGS_ONLY      .OR. &
     &     EOP_STYLE(2).EQ. EOP__RATES_AND_SEGS .OR. &
     &     EOP_STYLE(2).EQ. EOP__SEGS_ONLY           ) THEN ! segmented EOP
!
           IF ( NROT_A1(1) .GT. 1          ) UNF_EOP = .TRUE.
           IF ( NROT_A1(1) .NE. NROT_A1(2) ) UNF_EOP = .FALSE.
!
      END IF
!
! --- Reading environment variavle UNF_SEG
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'UNF_DISABLE', STR )
!
! --- In according with letters in UNF_ENV we disable some UNF- flags
!
      IF ( INDEX( STR,  'C') .GT. 0 ) UNF_CLO = .FALSE.
      IF ( INDEX( STR,  'A') .GT. 0 ) UNF_ATM = .FALSE.
      IF ( INDEX( STR,  'E') .GT. 0 ) UNF_EOP = .FALSE.
      IF ( INDEX( STR,  'B') .GT. 0 ) THEN
           IF ( CLK_BRK_STAT ) THEN
                UNF_CLO = .FALSE.
           END IF
      END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      if ( index( str,  'V') .gt. 0 ) then                            ! %%%%%%
         WRITE ( 6, * ) 'SOCOM_EXT: unf_clo = ',unf_clo,' unf_atm =',unf_atm, &  ! %%%%%%
     &          ' unf_eop =',unf_eop,' cgm_type = ',cgm_type          ! %%%%%%
      end if                                                          ! %%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!C
      IF ( UNF_CLO .OR. UNF_ATM .OR. UNF_EOP ) THEN
!
! -------- Get the time of the first and the last observation (Julian date)
!
           CALL OBSTM ( JFIRST_OBS, JLAST_OBS )
         ELSE
           JFIRST_OBS = 0.0D0
           JLAST_OBS  = 0.0D0
      END IF
!
! --- Now we adjust the last epoch of segmented parameters. We calculate
! --- preliminary value of last epoch for each parameters. And after that
! --- we find maximum.
!
      JDATE_CLO_LAST = JFIRST_OBS
      JDATE_ATM_LAST = JFIRST_OBS
      JDATE_EOP_LAST = JFIRST_OBS
      MIN_INTERVAL   = JLAST_OBS - JFIRST_OBS
      CLO_INTERVAL   = 0.0D0
      ATM_INTERVAL   = 0.0D0
      EOP_INTERVAL   = 0.0D0
!
      IF ( UNF_CLO ) THEN
         CLO_INTERVAL = DBLE ( NINT (CLOCK_INTERVAL*3600.0D0) )/86400.D0
         IF ( CLO_INTERVAL .LT. 1.D0/86400.D0 ) THEN
!
! ----------- Sometimes due to database inconsistency rare situatiuon occur:
! ----------- CLO_INTERVAL is zero. We should handle such a special case
!
              CLO_INTERVAL = DBLE( NINT (FJDCL(2) - FJDCL(1))*86400.0D0 )/ &
     &                       86400.D0
              IF ( CLO_INTERVAL .LT. 1.D0/86400.D0 ) THEN
!
! ---------------- If even such a trick doesn't help we set up it forcible to
! ---------------- 1 hour
!
                   CLO_INTERVAL = 1.D0/24.D0
              END IF
         END IF
         JDATE_CLO_LAST = ( INT((JLAST_OBS - JFIRST_OBS )/CLO_INTERVAL) + 1)* &
     &                    CLO_INTERVAL + JFIRST_OBS
         IF ( CLO_INTERVAL .LT. MIN_INTERVAL ) MIN_INTERVAL = CLO_INTERVAL
      END IF
!
      IF ( UNF_ATM ) THEN
         ATM_INTERVAL = DBLE ( NINT(ATMOS_INTERVAL*3600.0D0) )/86400.D0
         IF ( ATM_INTERVAL .LT. 1.D0/86400.D0 ) THEN
!
! ----------- The same about atmosphere
!
              ATM_INTERVAL = DBLE( NINT(TATM(2) - TATM(1))*86400.0D0 ) / &
     &                       86400.D0
              IF ( ATM_INTERVAL .LT. 1.D0/86400.D0 ) THEN
!
! ---------------- If even such a trick doesn't help we set up it forcible to
! ---------------- 1 hour
!
                   ATM_INTERVAL = 1.D0/24.D0
              END IF
         END IF
         JDATE_ATM_LAST = (INT(( JLAST_OBS - JFIRST_OBS )/ATM_INTERVAL)+ 1)* &
     &                    ATM_INTERVAL + JFIRST_OBS
         IF ( ATM_INTERVAL .LT. MIN_INTERVAL ) MIN_INTERVAL = ATM_INTERVAL
      END IF
!
      IF ( UNF_EOP ) THEN
         EOP_INTERVAL = DBLE ( NINT (ROT_INTERVAL(1)*3600.D0) )/3600.D0
         IF ( EOP_INTERVAL .LT. 1.D0/86400.D0 ) THEN
!
! ----------- The same about EOP
!
              EOP_INTERVAL = DBLE( NINT(TROT(2) - TROT(1))*86400.0D0 ) / &
     &                       86400.D0
              IF ( EOP_INTERVAL .LT. 1.D0/86400.D0 ) THEN
!
! ---------------- If even such a trick doesn't help we set up it forcible to
! ---------------- 1 hour
!
                   EOP_INTERVAL = 1.D0/24.D0
              END IF
         END IF
         JDATE_EOP_LAST = (INT( (JLAST_OBS - JFIRST_OBS)/EOP_INTERVAL)+ 1)* &
     &                    EOP_INTERVAL + JFIRST_OBS
         IF ( EOP_INTERVAL .LT. MIN_INTERVAL ) MIN_INTERVAL = EOP_INTERVAL
      END IF
!
      JLAST_EPOCH  = MAX ( JDATE_CLO_LAST, JDATE_ATM_LAST, JDATE_EOP_LAST )
      MAX_INTERVAL = MAX ( CLO_INTERVAL, ATM_INTERVAL, EOP_INTERVAL )
!
      IF ( DO_OVD                                         .AND. &
     &     MIN_INTERVAL .GT. MIN_OVD                      .AND. &
     &     MIN_INTERVAL .LT. MAX_OVD                      .AND. &
     &     JLAST_OBS - (JLAST_EPOCH - MAX_INTERVAL) .LT.        &
     &                  MIN_INTERVAL*SEG_OVD                    ) THEN
!
! -------- Check if the time interval between the last observation and
! -------- the last-but-one common epoch for all segmented parameters is less
! -------- than MIN_INTERVAL*SEG_OVD then we decrease the last epoch
! -------- by max_interval. Small overdraft after the last epoch will be allowed
!
           JLAST_EPOCH = JLAST_EPOCH - MAX_INTERVAL
      END IF
!
      IF ( UNF_CLO ) THEN
!
! -------- Adjusting clock intervals and clock epochs
!
           DO 420 J2=1,MAX4_CLO
              NUM_CLO = NUM_CLO + 1
              JDATE_CLO(J2) = JFIRST_OBS + (J2-1)*CLO_INTERVAL
              IF ((JDATE_CLO(J2) -JLAST_EPOCH)*86400.D0 .GT. -EPS_SEC) GOTO 820
 420       CONTINUE
!
           CALL CLRCH   ( STR )
           CALL INCH    ( MAX4_CLO, STR )
           WRITE ( 6, * ) 'SOCOM_EXT: Attempt to estimate more clock segments than '// &
     &            'it is allowed: MAX4_CLO = '//STR
           WRITE ( 6, * ) 'SOCOM_EXT: CLO_INTERVAL = ', &
     &     CLO_INTERVAL/(24.0*60.0),' min'
           WRITE ( 6, * ) 'SOCOM_EXT: Session lingered ', &
     &            (JLAST_EPOCH - JFIRST_OBS)*86400.D0, &
     &            ' secunds '
           STOP 'SOCOM_EXT'
!
 820       CONTINUE
           FJDCL(1) = JDATE_CLO(1)
      END IF
!
      IF ( UNF_ATM ) THEN
!
! -------- Adjusting atmosphere intervals and atmosphere epochs
!
           DO 430 J3=1,MAX4_ATM
              NUM_ATM = NUM_ATM + 1
              JDATE_ATM(J3) = JFIRST_OBS + (J3-1)*ATM_INTERVAL
              IF ((JDATE_ATM(J3) -JLAST_EPOCH)*86400.D0 .GT. -EPS_SEC) GOTO 830
 430       CONTINUE
!
           CALL CLRCH   ( STR )
           CALL INCH    ( MAX4_ATM, STR )
           WRITE ( 6, * ) 'Attempt to estimate more atmosphere segments than it is '// &
     &            'allowed: MAX4_ATM = '//STR
           WRITE ( 6, * ) 'SOCOM_EXT: ATM_INTERVAL = ', &
     &     ATM_INTERVAL/(24.0*60.0),' min'
           WRITE ( 6, * ) 'SOCOM_EXT: Session lingered ', &
     &            (JLAST_EPOCH - JFIRST_OBS)*86400.D0, &
     &            ' secunds '
           STOP 'SOCOM_EXT'
!
 830       CONTINUE
           TATM(1) = JDATE_ATM(1)
      END IF
!
      IF ( UNF_EOP ) THEN
!
! -------- Adjusting EOP intervals and EOP epochs
!
           TROT_A1 = JFIRST_OBS
!
! -------- Recalcuation the number of epoch and the dates of the epochs for
! -------- segmented EOP
!
           DO 440 J4=1,MAX4_EOP
!
! ----------- We run cycle until the epoch of the last reach the last
! ----------- observation
!
              NUM_EOP = J4
              JDATE_EOP(J4) = JFIRST_OBS + EOP_INTERVAL*(J4-1)
              IF ((JDATE_EOP(J4) -JLAST_EPOCH)*86400.D0 .GT. -EPS_SEC) GOTO 840
 440       CONTINUE
!
           CALL CLRCH   ( STR )
           CALL INCH    ( MAX4_EOP, STR )
           WRITE ( 6, * ) 'Attempt to estimate more EOP segments than it is '// &
     &            'allowed: MAX4_EOP = '//STR
           STOP 'SOCOM_EXT'
!
 840       CONTINUE
           NROT_A1(1) = NUM_EOP
           NROT_A1(2) = NUM_EOP
           TROT_A1    = JDATE_EOP(1)
      END IF
!
! --- Determination the condition of eligibility of fast mode. Parameters
! --- MIN_NSEG defined in ../include/socom_plus.i
!
      IF ( UNF_CLO .AND. NSEG_CLO .GT. MIN_NSEG ) THEN
           FAST_ELIG = .TRUE.
      END IF
      IF ( UNF_ATM .AND. NSEG_ATM .GT. MIN_NSEG ) THEN
           FAST_ELIG = .TRUE.
      END IF
!
      IF ( UNF_CLO .AND. UNF_ATM ) THEN
           RL = MAX(CLO_INTERVAL,ATM_INTERVAL) / &
     &          MIN(CLO_INTERVAL,ATM_INTERVAL)
           IF ( ABS( RL - INT(RL+0.49) )*86400.0*MAX( MAX4_CLO, MAX4_ATM ) .GT. &
     &          EPS_SEC )  FAST_ELIG = .FALSE.
      END IF
!
      IF ( UNF_CLO .AND. UNF_EOP ) THEN
           RL = MAX(CLO_INTERVAL,EOP_INTERVAL) / &
     &          MIN(CLO_INTERVAL,EOP_INTERVAL)
           IF ( ABS( RL - INT(RL+0.49) )*86400.0*MAX( MAX4_CLO, MAX4_EOP ) .GT. &
     &          EPS_SEC )  FAST_ELIG = .FALSE.
      END IF
!
      IF ( UNF_ATM .AND. UNF_EOP ) THEN
           RL = MAX(ATM_INTERVAL,EOP_INTERVAL) / &
     &          MIN(ATM_INTERVAL,EOP_INTERVAL)
           IF ( ABS( RL - INT(RL+0.49) )*86400.0*MAX( MAX4_ATM, MAX4_EOP ) .GT. &
     &          EPS_SEC )  FAST_ELIG = .FALSE.
      END IF
!
      SOCOM_PLUS_FIRST = SPL__DONE
!
      IF ( UNF_CLO .OR. UNF_ATM .OR. UNF_EOP ) THEN
!
! -------- Reading PARF-file. It is necessary to do since PARCN use it.
! -------- If not to do it PARCN will yield wrong results!
!
           CALL USE_PARFIL ( 'ORC' )
!
! -------- Reading GLBF-file. It is necessary to do since PARCN use it.
! -------- If not to do it PARCN will yield wrong results! Oh, my God...
! -------- People! Don't use common area, forget about such an operator
! -------- in FORTRAN!
!
! -------- Somehow variable old_user_part which habits in glbcm.i survives
! -------- savely from previus run. Oh! It is really "old". To prevent
! -------- failure of the solution we save current value of old_user_part
! -------- (at this point it is correct!)
!
           OLD_USER_PART__SAVE = OLD_USER_PART
           CALL USE_GLBFIL   ( 'ORC' )
           OLD_USER_PART = OLD_USER_PART__SAVE ! Resurrection
!
! -------- Recalculating the number of parameters
!
           CALL PARCN()
!
! -------- Write down the results of the work.
!
           CALL USE_COMMON ( 'OWC' )
      END IF
!
! --- Create the cross reference table for baseline-dependent clocks
! --- It is used later by partl for optimization
!
      NCOUNT_BSL = 0
      CALL NOUT_I2 ( INT4(MAX_ARC_STA)*INT4(MAX_ARC_STA), BASCL_IND )
      IF ( NUMSTA .LE. MAX_ARC_STA ) THEN
!
! -------- It has sence only for session-like CGM
!
           DO 450 J5=1,NUMSTA-1 ! Running over all stations but the last
              IF ( .NOT. CHECK_STABIT ( INT2(J5) ) ) GOTO 450
              DO 460 J6=J5+1,NUMSTA
                 IF ( .NOT. CHECK_STABIT ( INT2(J6) ) ) GOTO 460
                 IF ( KBIT(ICLOCK(1,J5), INT2(J6) ) ) THEN
                      NCOUNT_BSL = NCOUNT_BSL+1
                      BASCL_IND(J5,J6) = NCOUNT_BSL
                 END IF
 460          CONTINUE
 450       CONTINUE
      END IF
!
! --- Create the cross reference table for estimates of source position and
! --- proper motions. It is used later by partl for optimization
!
      NCOUNT_POS = 0
      NCOUNT_PRP = 0
      CALL NOUT_I2 ( INT4(MAX_ARC_SRC)*2, STAR_IND )
      CALL NOUT_I2 ( INT4(MAX_ARC_SRC)*2, PROP_IND )
      IF ( .NOT. CGM_TYPE ) THEN
!
! --------- It is done ony in non-cgm type socom blocks, since
! --------- STAR_IND, PROP_IND are sized for MAX_ARC_SRC, which is less than
! --------- MAX_ARC. Besides, it is used only by partl which uses non-cgm
! --------- types of socom blocks
!
            DO 470 J7=1,NUMSTR
               DO 480 J8=1,2
                  IF ( KBIT( LSTAR(1,J8), INT2(J7) ) ) THEN
                       NCOUNT_POS = NCOUNT_POS + 1
                       STAR_IND(J7,J8) = NCOUNT_POS  ! index after NSLAST
                  END IF
 480           CONTINUE
 470        CONTINUE
!
! --------- Now the turn of proper motion
!
            DO 490 J9=1,NUMSTR
               DO 4100 J10=1,2
                  IF ( KBIT(LPROP(1,J10), INT2(J9)) ) THEN
                       NCOUNT_PRP = NCOUNT_PRP + 1
                       PROP_IND(J9,J10) = NCOUNT_POS + NCOUNT_PRP ! index after NSLAST
                  END IF
 4100          CONTINUE
 490        CONTINUE
      END IF
!
      IF ( L_HPE > 0 .AND. ADR_HPE .NE. 0 ) THEN
           CALL HPESOL_CREATE ( %VAL(ADR_HPE) )
           CALL USE_GLBFIL_4 ( 'OWC' )
         ELSE
           FL_HPESOL = .FALSE.
      END IF
      IF ( L_SPE > 0 .AND. ADR_SPE .NE. 0 ) THEN
           CALL SPESOL_CREATE ( %VAL(ADR_SPE) )
           CALL USE_GLBFIL_4 ( 'OWC' )
         ELSE
           FL_SPESOL = .FALSE.
      END IF
!
      IND_EERM_NOD = -999
      EERM_OVR = 0
      IF ( L_EERM > 0 ) THEN
           CALL EERM_CREATE ( %VAL(ADR_EERM) )
           CALL USE_GLBFIL_4 ( 'OWC' )
         ELSE
           FL_EERM = .FALSE.
      END IF
!
      IF ( L_EHEO > 0 ) THEN
           CALL HEOSOL_CREATE ( L_EHEO, %VAL(ADR_EHEO) )
           FL_EHEO = .TRUE.
         ELSE
           FL_EHEO = .FALSE.
      END IF
!
      IF ( ( FL_HPESOL  .OR.  &
     &       FL_SPESOL  .OR.  &
     &       FL_EERM    .OR.  &
     &       FL_EHEO          ) .AND. &
     &     .NOT. CGM_TYPE             ) THEN
!
! -------- Recompute the number of parameters
!
           CALL PARCN()
!
! -------- Write down the results of the work.
!
           CALL USE_COMMON ( 'OWC' )
      END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      if ( index( str,  'V') .gt. 0 ) then                            ! %%%%%
         WRITE ( 6, * ) 'SOCOM_EXT: kuser_part =',kuser_part, &                  ! %%%%%
     &          ' num_user_part =',num_user_part, &                      ! %%%%%
     &          ' old_user_part =',old_user_part                      ! %%%%%
         WRITE ( 6, * ) 'SOCOM_EXT: atmos_interval=',atmos_interval, &           ! %%%%%
     &          ' clock_interval=',clock_interval                     ! %%%%%
         WRITE ( 6, * ) 'SOCOM_EXT: fjdcl(1)=',fjdcl(1),' fjdcl(2)=',fjdcl(2) ! %%%%%
         WRITE ( 6, * ) 'SOCOM_EXT: tatm(1) = ',tatm(1),' tatm(2) = ',tatm(2) ! %%%%%
         WRITE ( 6, * ) 'SOCOM_EXT: trot(1) = ',trot(1),' trot(2) = ',trot(2) ! %%%%%
         WRITE ( 6, * ) 'SOCOM_EXT: npl_clo = ',npl_clo,' nslast = ',nslast, &   ! %%%%%
     &          ' clo_interval = ',clo_interval*1440., &                 ! %%%%%
     &          ' atm_interval = ',atm_interval*1440., &                 ! %%%%%
     &          ' eop_interval = ',eop_interval*1440.,' min'          ! %%%%%
         WRITE ( 6, * ) 'SOCOM_EXT: nslast = ',nslast,' nparam = ',nparam, &     ! %%%%%
     &          ' num_clo/atm/eop =',num_clo,' ',num_atm,' ',num_eop, &  ! %%%%%
     &          ' fast_elig =',fast_elig                              ! %%%%%
         WRITE ( 6, 110 )  jfirst_obs, jlast_obs, (jlast_obs-jfirst_obs)      ! %%%%%
 110     format ( 1x,' first=',f15.6,' last=',f15.6,' duration=',f16.12 ) ! %
         WRITE ( 6, 120 )  jlast_epoch                                        ! %%%%%
 120     format ( 1x,' last_epoch = ',f15.6 )                         ! %%%%%
         write ( 6, * ) ' old_clocks=',old_clocks, &                     ! %%%%%
     &                    ' bmode_cl=',bmode_cl,' bmode_at=',bmode_at ! %%%%%
         write ( 6, * ) ' ncount_pos =',ncount_pos,' ncount_prp=',ncount_prp ! %%
!                                                                     ! %%%%%
      end if                                                          ! %%%%%
      if ( index( str,  'P') .gt. 0 ) then                            ! %%%%%
           call pause ( 'socom_ext' )                                 ! %%%%%
      end if                                                          ! %%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      RETURN
      END  !#!  SOCOM_EXT  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE HPESOL_CREATE ( HPE )
! ************************************************************************
! *                                                                      *
! *   Routine HPESOL_CREATE
! *                                                                      *
! *  ### 25-FEB-2005 HPESOL_CREATE  v1.0 (c) L. Petrov  25-FEB-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'erm.i'
      INCLUDE   'socom.i'
      INCLUDE   'socom_plus.i'
      INCLUDE   'prfil.i'
      INCLUDE   'glbc4.i'
      TYPE       ( HPE__TYPE ) HPE(L_HPE)
      INTEGER*4  J1, J2, J3
      CHARACTER  STA_NAM*8
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      LOGICAL*4, EXTERNAL :: CHECK_STABIT
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
      CALL NOUT ( INT4(MAX_STA)*L_HPE*SIZEOF(HPESOL(1,1)), HPESOL )
!
      DO 410 J1=1,NUMSTA
         IF ( .NOT. CGM_TYPE ) THEN
              IF ( .NOT. CHECK_STABIT ( INT2(J1) ) ) GOTO 410
         END IF
         DO 420 J2=1,L_HPE
            HPESOL(J1,J2)%FL_EST = .FALSE.
            DO 430 J3=1,HPE(J2)%L_STA
               STA_NAM = HPE(J2)%C_STA(J3)
               IF ( ISITN_CHR(J1) == STA_NAM ) THEN
                    HPESOL(J1,J2)%FL_EST  = .TRUE.
                    HPESOL(J1,J2)%IND_EQU = -1
                    HPESOL(J1,J2)%PHASE   = HPE(J2)%PHASE
                    HPESOL(J1,J2)%FREQ    = HPE(J2)%FREQ
                    HPESOL(J1,J2)%NAME    = HPE(J2)%NAME
                    FL_HPESOL = .TRUE.
               END IF
 430        CONTINUE
 420     CONTINUE
 410  CONTINUE
!
      RETURN
      END  SUBROUTINE  HPESOL_CREATE
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPESOL_CREATE ( SPE )
! ************************************************************************
! *                                                                      *
! *   Routine SPESOL_CREATE
! *                                                                      *
! *  ### 25-FEB-2005 SPESOL_CREATE  v1.2 (c) L. Petrov  08-MAR-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'erm.i'
      INCLUDE   'socom.i'
      INCLUDE   'socom_plus.i'
      INCLUDE   'prfil.i'
      INCLUDE   'glbc4.i'
      TYPE       ( SPE__TYPE ) SPE(L_SPE)
      CHARACTER  STA_NAM*8
      INTEGER*4  J1, J2, J3, J4, IND_NOD_REF(M__SPN), IND_NOD_END
      REAL*8     JFIRST_OBS, JLAST_OBS, SES_TIM_START, SES_TIM_END
      LOGICAL*4, EXTERNAL :: CHECK_STABIT
      INTEGER*4, EXTERNAL :: IXMN8
!
      IF ( L_SPE == 0 ) THEN
           RETURN
         ELSE IF ( L_SPE < 0 ) THEN
           WRITE ( 6, * ) ' L_SPE = ',L_SPE
           CALL ERR_LOG ( 101, -2, 'SPESOL_CREATE', 'Trap of internal '// &
     &         'control: wrong value of L_SPE' )
           CALL EXIT ( 101 )
         ELSE IF ( L_SPE > M__SPE ) THEN
           WRITE ( 6, * ) ' L_SPE = ',L_SPE, ' M__SPE=',M__SPE
           CALL ERR_LOG ( 102, -2, 'SPESOL_CREATE', 'Trap of internal '// &
     &         'control: wrong value of L_SPE' )
           CALL EXIT ( 101 )
      END IF
      CALL NOUT ( L_SPE*SIZEOF(SPESOL(1)), SPESOL )
!
      IF ( .NOT. CGM_TYPE ) THEN
           CALL OBSTM ( JFIRST_OBS, JLAST_OBS )
!
! -------- SES_TIM_START -- interval of TAI time from J2000.0 epoch
! --------                  to the nominal start of the session
!
           SES_TIM_START = (JFIRST_OBS - J2000__JD)*86400.0D0 - 32.184D0
           SES_TIM_END   = ( JLAST_OBS - J2000__JD)*86400.0D0 - 32.184D0
      END IF
!
      DO 410 J1=1,NUMSTA
         IF ( .NOT. CGM_TYPE ) THEN
              IF ( .NOT. CHECK_STABIT ( INT2(J1) ) ) GOTO 410
         END IF
         DO 420 J2=1,L_SPE
            STA_NAM = SPE(J2)%STATION
            IF ( ISITN_CHR(J1) == STA_NAM ) THEN
                 SPESOL(J2)%IND_STA = J1
                 SPESOL(J2)%L_NOD   = SPE(J2)%K_NOD
!
! -------------- Copy array of epoch from SPE to SPESOL.
! -------------- NB: SPE(J2)%TIM        starts from 1-SPE(J2)%DEGREE, while
! --------------     SPESOL(J2)%NOD_ARR starts from 1
!
                 CALL COPY_R8 ( SPE(J2)%K_NOD, SPE(J2)%TIM(1), &
     &                          SPESOL(J2)%NOD_ARR )
!
                 SPESOL(J2)%IND_EQU       = -1
                 SPESOL(J2)%DEGREE        = SPE(J2)%DEGREE
                 SPESOL(J2)%CORR_IND      = 0
                 SPESOL(J2)%FL_CHECK_OVER = .FALSE.
                 SPESOL(J2)%IND_NOD       = 0
!
                 IF ( CGM_TYPE ) THEN
                      CALL LIB$MOVC3 ( SPE(J2)%K_NOD+SPE(J2)%DEGREE, &
     &                                 SPE(J2)%USED(1-SPE(J2)%DEGREE), &
     &                                 SPESOL(J2)%USED(1-SPE(J2)%DEGREE) )
                    ELSE
                      DO 430 J3=1-SPE(J2)%DEGREE,SPE(J2)%K_NOD
                         SPESOL(J2)%USED = .FALSE.
 430                  CONTINUE
!
! ------------------- If we are in non-CGM mode, search for the pivotal node
!
                      SPESOL(J2)%IND_NOD = IXMN8 ( SPESOL(J2)%L_NOD, &
     &                                             SPESOL(J2)%NOD_ARR, &
     &                                             SES_TIM_START  )
                      IF ( SPESOL(J2)%IND_NOD .LT. 1 ) THEN
                           SPESOL(J2)%IND_STA = 0
                           GOTO 420
                      END IF
!
                      IF ( SPESOL(J2)%IND_NOD < SPESOL(J2)%L_NOD ) THEN
                           IND_NOD_END = IXMN8 ( SPESOL(J2)%L_NOD, &
     &                                           SPESOL(J2)%NOD_ARR, &
     &                                           SES_TIM_END  )
                           IF ( IND_NOD_END > SPESOL(J2)%IND_NOD  .AND. &
     &                          IND_NOD_END .LE. SPESOL(J2)%L_NOD       ) THEN
                                SPESOL(J2)%FL_CHECK_OVER = .TRUE.
                                SPESOL(J2)%CORR_IND = IND_NOD_END - &
     &                                                SPESOL(J2)%IND_NOD
                           END IF
                      END IF
                      DO 440 J4=-SPESOL(J2)%DEGREE,SPESOL(J2)%CORR_IND
                         SPESOL(J2)%USED(SPESOL(J2)%IND_NOD+J4) = .TRUE.
                         SPE   (J2)%USED(SPESOL(J2)%IND_NOD+J4) = .TRUE.
 440                  CONTINUE
                 END IF
                 FL_SPESOL = .TRUE.
            END IF
 420     CONTINUE
 410  CONTINUE
!
      RETURN
      END  SUBROUTINE  SPESOL_CREATE
!
! ------------------------------------------------------------------------
!
      SUBROUTINE EERM_CREATE ( ERM )
! ************************************************************************
! *                                                                      *
! *   Routine EERM_CREATE creates object ERM                             *
! *                                                                      *
! *  ### 24-JAN-2006  EERM_CREATE  v 1.7 (c) L. Petrov  04-DEC-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'erm.i'
      INCLUDE   'socom.i'
      INCLUDE   'socom_plus.i'
      INCLUDE   'glbc4.i'
      TYPE      ( ERM__TYPE ) ERM
      REAL*8    JFIRST_OBS, JLAST_OBS, SES_TIM_START , SES_TIM_STOP, &
     &          TIM_ERM_BEG, TIM_ERM_END
      INTEGER*4 J1, J2, J3, J4, IND_EERM_END(3), IER, IUER
      CHARACTER  MJDSEC_TO_DATE*30, JD_TO_DATE*23
      INTEGER*4, EXTERNAL :: IXMN8, IXMN8_S
!
      CALL OBSTM ( JFIRST_OBS, JLAST_OBS )
!
! --- If we are in non-CGM mode, search for the pivotal node
!
!      TIM_ERM_BEG = (ERM%MJD_BEG - J2000__MJD)*86400.0D0 + ERM%TAI_BEG
!      TIM_ERM_END = (ERM%MJD_END - J2000__MJD)*86400.0D0 + ERM%TAI_END
!
      tim_erm_beg = (erm%mjd_beg - j2000__mjd)*86400.0d0 - 43200.0d0 + erm%tai_beg
      tim_erm_end = (erm%mjd_end - j2000__mjd)*86400.0d0 - 43200.0d0 + erm%tai_end
      DO 410 J1=1,3
         DO 420 J2=1,ERM%NKNOTS(J1)
            ERM%TIM(J2,J1) = TIM_ERM_BEG + ERM%TIME_EST_SPAN(J1)*(J2-1)
 420     CONTINUE
         ERM%TIM(ERM%NKNOTS(J1),J1) = TIM_ERM_END
!
! ------ Add extra knots very close to the first knot
!
         DO 430 J3=1-ERM%DEGREE(J1),0
            ERM%TIM(J3,J1) = ERM%TIM(1,J1) - (J3-1)*ERM__TIM_EPS
 430     CONTINUE
!
! ------ Add extra knots very close to the last knot
!
         DO 440 J4=1,ERM%DEGREE(J1)
            ERM%TIM(ERM%NKNOTS(J1)+J4,J1) = ERM%TIM(ERM%NKNOTS(J3),J1) + ERM__TIM_EPS*J4
 440     CONTINUE
!
         IF ( .NOT. CGM_TYPE ) THEN
!
! ----------- SES_TIM_START -- interval of TAI time from J2000 epoch 
! ----------- to the nominal start of the session
!
!              SES_TIM_START = (JFIRST_OBS - J2000__JD - 0.5D0 )*86400.0D0 - 32.184D0
!              SES_TIM_STOP  = (JLAST_OBS  - J2000__JD - 0.5D0 )*86400.0D0 - 32.184D0
              ses_tim_start = (jfirst_obs - j2000__jd )*86400.0d0 - 32.184d0
              ses_tim_stop  = (jlast_obs  - j2000__jd )*86400.0d0 - 32.184d0
!
! ----------- Get IND_EERM_NOD(J1) -- spline knot index for the session start
!
              IND_EERM_NOD(J1) = IXMN8 ( ERM%NKNOTS(J1), ERM%TIM(1,J1), &
     &                                   SES_TIM_START )
              IF ( IND_EERM_NOD(J1) > 0 ) THEN
!
! ---------------- Check whether the OBS end epoch is within the range 
! ----------XW------ of the B-spline
!
                   IND_EERM_END(J1) = IXMN8_S ( IND_EERM_NOD(J1), ERM%NKNOTS(J1), &
     &                                          ERM%TIM(1,J1), SES_TIM_STOP )
                   IF ( IND_EERM_END(J1) == -1 ) THEN
                        IUER = -1
                        IER  = -1
                        CALL ERR_LOG ( 6664, IUER, 'SOCOM_EXT',  &
     &                      'Error in EERM_CREATE: the end time of '// &
     &                      'session '//DBNAME_CH//' -- '// &
     &                       JD_TO_DATE( JLAST_OBS, IER )//' is after '// &
     &                      'the last epoch of EERM: '// &
     &                       MJDSEC_TO_DATE( ERM%MJD_END, ERM%TAI_END, IER ) )
                        CALL EXIT ( 1 )
                   END IF
!
! ---------------- EERM_OVR(J1) -- knot index of the session end with respect to the 
! ----------------                 session start. If EERM_OVR(J1) is zero, then
! ----------------                 the session start and end are within the same
! ----------------                 spline interval
!
                   IF ( IND_EERM_END(J1) > 0 .AND. IND_EERM_NOD(J1) > 0 ) THEN
                        EERM_OVR(J1) = IND_EERM_END(J1) - IND_EERM_NOD(J1)
                   END IF
              END IF
         END IF
 410  CONTINUE
!
      EERM = ERM
      FL_EERM = .TRUE.
!
      RETURN
      END  SUBROUTINE  EERM_CREATE  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE HEOSOL_CREATE ( L_EHEO, EHEO )
! ************************************************************************
! *                                                                      *
! *   Routine HEOSOL_CREATE
! *                                                                      *
! *  ### 25-FEB-2005 HEOSOL_CREATE  v1.0 (c) L. Petrov  25-FEB-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'erm.i'
      INCLUDE   'socom.i'
      INCLUDE   'socom_plus.i'
      INCLUDE   'prfil.i'
      INTEGER*4  L_EHEO
      TYPE       ( EHEO__TYPE ) EHEO(L_EHEO)
      INTEGER*4  J1, J2, J3
      CHARACTER  STA_NAM*8
      LOGICAL*4, EXTERNAL :: CHECK_STABIT
!
      CALL NOUT ( L_EHEO*INT(SIZEOF(EHEO(1)),KIND=4), HEOSOL )
!
      DO 410 J1=1,L_EHEO
          HEOSOL(J1)%NAME          = EHEO(J1)%NAME
	  HEOSOL(J1)%FL_EST(1)     = EHEO(J1)%FL_EST(1)
	  HEOSOL(J1)%FL_EST(2)     = EHEO(J1)%FL_EST(2)
	  HEOSOL(J1)%FL_EST_VEL(1) = EHEO(J1)%FL_EST_VEL(1)
	  HEOSOL(J1)%FL_EST_VEL(2) = EHEO(J1)%FL_EST_VEL(2)
	  HEOSOL(J1)%FL_CNS(1)     = EHEO(J1)%FL_CNS(1)
	  HEOSOL(J1)%FL_CNS(2)     = EHEO(J1)%FL_CNS(2)
	  HEOSOL(J1)%FL_CNS_VEL(1) = EHEO(J1)%FL_CNS_VEL(1)
	  HEOSOL(J1)%FL_CNS_VEL(2) = EHEO(J1)%FL_CNS_VEL(2)
	  HEOSOL(J1)%PHAS          = EHEO(J1)%PHAS
	  HEOSOL(J1)%FREQ          = EHEO(J1)%FREQ
	  HEOSOL(J1)%ACCL          = EHEO(J1)%ACCL
	  HEOSOL(J1)%AMPL(1,1) = EHEO(J1)%AMPL(1,1)
	  HEOSOL(J1)%AMPL(2,1) = EHEO(J1)%AMPL(2,1)
	  HEOSOL(J1)%AMPL(1,2) = EHEO(J1)%AMPL(1,2)
	  HEOSOL(J1)%AMPL(2,2) = EHEO(J1)%AMPL(2,2)
	  HEOSOL(J1)%EST(1,1)  = EHEO(J1)%EST(1,1)
	  HEOSOL(J1)%EST(2,1)  = EHEO(J1)%EST(2,1)
	  HEOSOL(J1)%EST(1,2)  = EHEO(J1)%EST(1,2)
	  HEOSOL(J1)%EST(2,2)  = EHEO(J1)%EST(2,2)
	  HEOSOL(J1)%SIG(1,1)  = EHEO(J1)%SIG(1,1)
	  HEOSOL(J1)%SIG(2,1)  = EHEO(J1)%SIG(2,1)
	  HEOSOL(J1)%SIG(1,2)  = EHEO(J1)%SIG(1,2)
	  HEOSOL(J1)%SIG(2,2)  = EHEO(J1)%SIG(2,2)
 410  CONTINUE
!
      RETURN
      END  SUBROUTINE  HEOSOL_CREATE
