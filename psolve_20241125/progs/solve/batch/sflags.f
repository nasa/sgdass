      SUBROUTINE SFLAGS ( STAFLG, FIXSTA_CHR, STADIU, VELFLG, SRCFLG, &
     &           FIXSRC_CHR, PROFLG, NUTFLG, UT1FLG, PRCFLG, RELFLG, NRTARC, &
     &           ROTFLG, ISTAD, KFAIL, ORIENT, ATMFLG, INTRVL, &
     &           CLKPOL_FLG, CLKPOL_DEG, CLKFLG, CKNTRVL, FCNPR, AXSFLG, &
     &           OFFLG, RATFLG, ACCEOP_FLG, IEOP_FLG, REOP_FLG, &
     &           IEOPLL, FIXNSTA, BLCFLG, IOS_EST_BATCH, IOS_SIG_BATCH, &
     &           NEXCBL, IBLNM, BASDF, &
     &           EOPMID, IONFLG, RESFILE, KMIN_SIG, GRADFLG, GRINTRVL, &
     &           KOUTNRM, KZERONRM, KSTACONST, KVELCONST, DO_CLK_REF, &
     &           NUM_CLK_REF, LIST_CLK_REF, EOP_SUPR, BASDEP_CLO, FL_NOMAP, &
     &           DBNAME_MES, EOP_EPOCH_MJD, EOP_EPOCH_SEC, &
     &           EOP_BEFORE_SEC_TAI, EOP_AFTER_SEC_TAI   )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SFLAGS PROGRAM SPECIFICATION
!
! 1.1 Set flags for this arc.
!
! 1.2 REFERENCES:
!
! 2.  INTERFACE
      LOGICAL*2 L4TOL2
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) STAFLG, VELFLG, SRCFLG, PROFLG, NUTFLG(116), UT1FLG, &
     &              PRCFLG, RELFLG, FIXSTA_CHR, FIXSRC_CHR
      CHARACTER*(*) ROTFLG, ORIENT, STADIU, ATMFLG
      CHARACTER*(*) CLKPOL_FLG, CLKFLG, AXSFLG, BLCFLG, BASDF
      CHARACTER*(*) IONFLG, RESFILE, GRADFLG, EOP_SUPR, BASDEP_CLO
      INTEGER*2     NRTARC, ISTAD(*),INTRVL, GRINTRVL, IOS_EST_BATCH
      INTEGER*2 CLKPOL_DEG, CKNTRVL, OFFLG, RATFLG, ACCEOP_FLG, &
     &          IEOP_FLG(6), IEOPLL, EOPMID
      INTEGER*2 NEXCBL, IBLNM(8,50)
      REAL*8    REOP_FLG(4), IOS_SIG_BATCH
      LOGICAL*2 KFAIL, KMIN_SIG, KOUTNRM, KZERONRM, KSTACONST, KVELCONST
      REAL*8    FCNPR, EOP_EPOCH_SEC, EOP_BEFORE_SEC_TAI, EOP_AFTER_SEC_TAI  
      INTEGER*4 EOP_EPOCH_MJD
!
      LOGICAL*2 DO_CLK_REF   ! Do or don't specify ref clock in arc line.
      INTEGER*4 NUM_CLK_REF  ! Number of clock reference sites
      CHARACTER*8 LIST_CLK_REF(MAX_ARC_STA) ! List of 8-character names of
!                                           ! reference sites
      LOGICAL*4  FL_NOMAP
      CHARACTER  DBNAME_MES*(*)
      CHARACTER  ATMFLG_USE*1, CLKFLG_USE*1
      CHARACTER  BLCFLG_SES*1  ! local copy of BLCFLG
!
! AXSFLG - Axis ofset falg
! ATMFLG - Atmosphere flag
! CKNTRVL - Clock intervals in minutes
! CLKFLG - Clock flag
! FCNPR - Free core nutation period
! FIXSRC - Reference source name
! FIXSTA - Reference station name
! INTRVL - Atmosphere interval
! ISTAD  - Station data flag
! KFAIL - True if this arc fails
! NRTARC - Earth orientation epoch
! NUTFLG - Nutation flag
! ORIENT - Earth orientation flag (FIXED or FREE?)
! PRCFLG - Precession flag
! RELFLG - Relativity flag
! ROTFLG - Earth orientation components override
! SRCFLG - Source positions flag flag
! PROFLG - Source proper motioon fag
! STAFLG - Station position flag
! VELFLG - Station velocity flag
! STADIU - Station diurnal flag
! UT1FLG - Earth orientation flag
! OFFLG - Earth orientation offset flag
! RATFLG - Earth orientation rate flag
! ACCEOP_FLG - Earth orientation acceleration flag
! IOS_EST_BATCH - ionosphere path delay scale flag
! IEOP_FLG - Local earth orientation flags
! REOP_FLG - Earth orientation intervals and constraints
! IEOPLL -  Earth orientation plot flag
! EOP_SUPR -  Earth orientation suppression from estimation flag
! BASDEP_CLO - Baseline dependent clock estimation flag
! KSTACONST  - constraints on stations position flag
! KVELCONST  - constraints on stations velocities flag
! IOS_SIG_BATCH -- constraint on ionosphere path delay scale
!
! 2.3 OUTPUT Variables: None
!
      integer*2 fixnsta(4)
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'precm.i'
      INCLUDE 'exceptions.i'
      INCLUDE 'axscm.i'
      INCLUDE 'dmapp.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'batme.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: arcset
!       CALLED SUBROUTINES: rfsta,autat,autcl,rfsrc,ut1pm
!
! 3.  LOCAL VARIABLES
!
      CHARACTER   PHASE*1, COMP*1, TCLK*1, FIXNAM_CHR*8, STR*80
      INTEGER*2   FIXNAM(4), ISTA, IROTF, IBIT(3), BM_REF_CL_ORIG
      INTEGER*2   I, J, IDUM, IBIT1, IBIT2, IBIT3, IBIT4, IBIT5, KBITN
      INTEGER*2   FIXSTA(4), FIXSRC(4), JATM, CKN, ST1, ST2, BSET, INTR
      INTEGER*2   EST_STATUS, J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, &
     &            J11, J12, J13, J14, J15, J16, IL, TRIMLEN
      LOGICAL*2   KBIT, FAIL, EQUAL
      LOGICAL*2   FL_STA_USE(MAX_ARC_STA), FL_STA_EST(MAX_ARC_STA)
      INTEGER*4   IEXP, IUER
      REAL*8      MIN5, INTR8
      REAL*8      FJDOBS, LJDOBS
      CHARACTER   CLKPOL_FLG_USE*1
      INTEGER*2   CLKPOL_DEG_USE
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*4,  EXTERNAL :: DATYP_INQ, CHECK_STABIT
      INTEGER*4,  EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE   910419 Added proper motion stuff
!   AEE   910816 Set flyby inerpolation flag to indiacate LINear, CUBic, or
!                default (= not specified).
!   mwh  920519  Don't turn on velocity for continuous position sites
!   KDB  960416  Flag attempt to minimize sigmas without estimating velocities.
!   JWR  971010  Code to handle clock reference sites in arc line added.
!
!   PET  971210  Corrected bug with estimation baseline-dependent clocks
!                with multiple clock reference stations. Added capacity to
!                estimate source positions for sources specified "in superfile".
!                Added capacity to estimate baseline dependent clocks for
!                beslines specified "in superfile". Improved error messages
!
!   PET  971216  Corrected bug with setting clock reference station: it was
!                the error in logic.
!
!   PET  980731  Added a formal argument EOP_SUPR for suppression of EOP
!                component for arc. String EOP_SUPR has 6 characters: first for
!                X-pole, 2-nd for Y-pole, 3-rd for UT1, 4-th for LOD,
!                5-th for psi nutation offset, 6-th for eps nutation offset.
!                If the n-th element of the string EOP_SUPR is "Y" then that
!                component of EOP-vector will not be estimated in the solution
!                regardless the status flags set in $FLAG section.
!                For example, EOP_SUPR = 'YNNYNY' forbids to estimate
!                X-pole, UT1-rate, eps nutation. Estimation status of other
!                EOP-parameters (Y-pole, UT1, nutation psi) remains untouched.
!
!   PET  981104  Fixed a bug. If user specified a) CLOCK: DEFAULT, b)
!                BASELINE_CLOCK: YES and the session 1) didn't have specified
!                clock reference station; 2) was marked as "not batch mode"
!                clock the previous version stubbornly tried to check a clock
!                reference station in setting baseline dependent clocks
!
!   pet  990104  Added a formal argument BASDEP_CLO for specification of
!                estimation of baseline-dependent clocks for this arc only.
!                String BASDEP_CLO may have values: "   " -- use global flag of
!                estimation of baseline dependent clocks; "YES" -- estimate
!                all linearly independent baseline dependent clocks;
!                "NO" -- not to estimate; "IN" estimate baseline-dependent
!                clock in according with status recorded in the database.
!
!   pvt  990702  IBITS renamed to IBIT5 in order to avoid conflict with
!                intrinsic function
!
!   pet  990705  Removed unused variables. Changed logic: if the clock
!                reference station(s) was changed with respect to the value
!                saved in superfile then baseline depended clocks are not
!                estimated for any baselines with clock reference stations
!                to avoid singularity of normal matrix.
!
!   pet  2000.01.22  Extended using EOP_SUPR string: if EOP_SUPR(1:1) .EQ. 'Y'
!                    then not only X pole coordinate but X pole coordinate rate
!                    is suppressed. Analogously EOP_SUPR(2:2) .EQ. 'Y'
!                    suppresses Y pole rate.
!
!   pet  2000.03.17  Extended using EOP_SUPR string even more: if EOP_SUPR(1:1)
!                    .EQ. 'Y' then not only X pole coordinate but estimation of
!                    its 2nd order term is also suppressed; analgously
!                    EOP_SUPR(2:2) .EQ. 'Y' suppresses Y pole 2nd order term and
!                    EOP_SUPR(4:4) .EQ. 'Y' suppresses UT1 2nd order term
!
!   pet  2000.10.05  Corrected a bug: EOPMID should be of INTEGER*2 type!
!   pet  2000.11.24  Added support of KVELCONST flag. Removed suppot of tides
!                    flags
!   pet  2001.08.10  converted type of FIXSTA, FIXSRC ( INTEGER*2 ) to
!                    FIXSTA_CHR, FIXSRC_CHR ( CHARACTER ). Removed STACMP,
!                    SRCCMP, TIDFLG. Added VELFLG, PROFLG
!
!   pet  2001.08.14  Changed logic of assigning objects for estimation of
!                    site positions and velocities, source coordinates and
!                    proper motion
!
!   pet  2001.09.10  Changed logic of setting baseline-dependent clock flags:
!                    If BASELINE_CLOCKS YES, then bclock_max subroutine is
!                    called which makes maximal baseline-dependent clocks setup.
!                    If BASELINE_CLOCKS IN then the routine uses the setup
!                    saved in the database but deselects baseline dependent
!                    clocks at baselines with new clock reference site and
!                    at baselines with deselected stations.
!
!   pet  2001.09.17  Fixed the bug: the previous version did not check whether
!                    the station was deselected when it set up flags for
!                    axis offset estimation what caused crashes in some cases.
!
!   pet   2001.09.20 Made several changes in order to handle properly the
!                    case if the clock reference station appears to be
!                    deselected. Solve then will try to set it up.
!
!   pet   2001.12.13 Corrected the error madw in 2001.09.20 -- initialization
!                    was incorrect since it initialized amount of memory twice
!                    less than was needed. Added initilaization of antenna axis
!                    offsets status bits and baseline clocks when necessary.
!
!   pet   2002.07.29 Set BMODE_CL .TRUE. if CLOCKS AUTO was specified in
!                    $FLAGS section of the control file.
!                    Set BMODE_AT .TRUE. if ATMOSPHERES AUTO was specified in
!                    $FLAGS section of the control file.
!
!   jwr   2002.12.19 TRUE__L2 and FALSE__L2 introduced for -i2 removal
!
!   pet   2003.09.18 Added logic for setting GRAD_AUG2003_BUG variable
!
!   pet   2004.03.16 Updated logic for dealing with session-only options from
!                    the session-line which overrides global opotions specified
!                    in the control file
!
!   pet   2004.03.19 Corrected an error related with the case when no &
!                    gradients are set at all
!
!   pet   2004.07.01 Restored support of UEN station position parameterization
!
!   pet   2004.10.06 Fixed a bug. The previous version did not set correctly
!                    flags for atmospheric path delay when "ATMOSPEPHERS FORCE"
!                    keywords were in use
!
!   pet   2005.05.03 Fixed a bug. The previous version did not always set 
!                    correctly flags for atmospheric path delay when &
!                    "ATMOSPEPHERS FORCE" keywords were in use
!
!   pet   2006.12.08 Added support of EOP_EPOCH_SEC
!
!   pet   2008.04.09 Added support of XY_OFFSET flag
!
!   pet   2006.12.08 Added support of EOP_EPOCH_MJD
!
!   pet   2020.06.24 Added support of STA_ON, STA_OFF lists
!
!   pet   2021.03.10 Fixed a regression bug: $FLAGS STATIONS  exception list support was broken
!
!   pet   2021.03.10 Added support of IOS_EST_BATCH and IOS_SIG_BATCH flags
!
! 5.  SFLAGS PROGRAM STRUCTURE
!
!_____
!
      MIN5 = 5.0/(24.0*60.0) ! 5 min as fraction of day
!
!  Set flyby_interp flag ( LINear, CUBic, default = not specified):
!
      IF ( FLYBY_INTERPOLATION(1:1) .EQ. 'L') THEN ! Linear
             FLYBY_INTERP= 1
        ELSE IF (FLYBY_INTERPOLATION(1:1) .EQ. 'C') THEN ! Cubic
            FLYBY_INTERP= 3
        ELSE IF (FLYBY_INTERPOLATION(1:1) .EQ. 'S') THEN ! Cubic
            FLYBY_INTERP= 4
        ELSE
            FLYBY_INTERP= 0
      ENDIF
      UT1_RS_FLYBY = UT1RSFLYBY
!
! --- Get the nominal start and nominal end of the session
!
      CALL OBSTM ( FJDOBS, LJDOBS )
!
! --- No flags to set if we failed
!
      IF ( KFAIL ) RETURN
!
! === Stations
!     ~~~~~~~~
!
! --- Pick the reference station if specified
!
      CALL RFSTA ( FIXNAM_CHR, FIXSTA_CHR, ISTAD, ISITN_CHR, NUMSTA )
!
! --- Initialization
!
      CALL NOUT ( 2*INT4(STA_BIT_WORDS)*3, LSITEC )
      CALL SBIT ( IUEN, INT2(1), INT2(0) ) ! XYZ-parameterizatio by default
!
! --- Set flag FL_STA_USE whether to include a given station into analysis
!
      IF ( NUM_STAINC .GT. 0 ) THEN  ! Include in processing
           FL_STA_USE = .FALSE.
           DO 410 J1=1,NUM_STAINC
              DO 420 J2=1,NUMSTA
                 IF ( LIST_STAINC(J1) == ISITN_CHR(J2) ) THEN
                      FL_STA_USE(J2) = .TRUE.
                 END IF
 420          CONTINUE 
 410       CONTINUE 
         ELSE
!
! -------- If there no INCLUDE list, then use all the stations
!
           FL_STA_USE = .TRUE.
      END IF
!
      IF ( NUM_STAEXC .GT. 0 ) THEN
           DO 430 J3=1,NUM_STAEXC
              DO 440 J4=1,NUMSTA
                 IF ( LIST_STAEXC(J3) == ISITN_CHR(J4) ) THEN
                      FL_STA_USE(J4) = .FALSE.
                 END IF
 440          CONTINUE 
 430       CONTINUE 
      END IF
!
! --- Set flag FL_STA_EST whether to estimate positions and velocities of a given
! --- station
!
      IF ( NUM_STAON .GT. 0 ) THEN  ! Esimation positions
           FL_STA_EST = .FALSE.
           DO 450 J5=1,NUM_STAON
              DO 460 J6=1,NUMSTA
                 IF ( LIST_STAON(J5) == ISITN_CHR(J6) ) THEN
                      FL_STA_EST(J6) = .TRUE.
                 END IF
 460          CONTINUE 
 450       CONTINUE 
         ELSE
!
! -------- If there no ON list, then use all the stations
!
           FL_STA_EST = .TRUE.
      END IF
!
      IF ( NUM_STAOFF .GT. 0 ) THEN
           DO 470 J7=1,NUM_STAOFF
              DO 480 J8=1,NUMSTA
                 IF ( LIST_STAOFF(J7) == ISITN_CHR(J8) ) THEN
                      FL_STA_EST(J8) = .FALSE.
                 END IF
 480          CONTINUE 
 470       CONTINUE 
      END IF
!
      DO 490 J9=1,INT4(NUMSTA)
!
! ------ Check whether the station was excluded entirely from processing
!
         IF ( .NOT. KBIT ( ISTAD, J9 ) ) GOTO 490
!
! ------ Check whether estimation of station position of this station
! ------ was switched off for this session only
!
         IF ( .NOT. FL_STA_EST(J9) ) THEN
               CALL SBIT ( LSITEC(1,1), J9, INT2(0) )
               CALL SBIT ( LSITEC(1,2), J9, INT2(0) )
               CALL SBIT ( LSITEC(1,3), J9, INT2(0) )
               GOTO 490
         END IF
!
         IF ( STA_CMP_ALL(1:1) .EQ. 'U' .OR. &
     &        STA_CMP_ALL(2:2) .EQ. 'E' .OR. &
     &        STA_CMP_ALL(3:3) .EQ. 'N'      ) THEN
!
! ----------- Set bit of UEN parameterization
!
              CALL SBIT ( IUEN, INT2(1), INT2(1) )
         END IF
!
         IF ( NUMEXC_STA > 0 ) THEN
              IEXP = LTM_DIF ( 1, INT4(NUMEXC_STA), STA_NAM, ISITN_CHR(J9) )
            ELSE
              IEXP = 0
         END IF
         DO 4100 J10=1,3
!
! --------- Setting global flags
!
            IF ( STA_CMP_ALL(J10:J10) .NE. '-' ) THEN
                 IF ( STAFLG .EQ. 'Y' .AND. IEXP < 1 ) THEN
                      CALL SBIT ( LSITEC(1,J10), J9, INT2(1) )
                    ELSE IF ( STAFLG .EQ. 'N' .AND. IEXP > 0 ) THEN
                      CALL SBIT ( LSITEC(1,J10), J9, INT2(1) )
                 END IF
            END IF
            IF ( .NOT. FL_STA_EST(J9) ) THEN
!
! -------------- The J9-th station was in exception list
!
                 IF ( KBIT ( LSITEC(1,J10), J9 ) ) THEN
                      EST_STATUS = 0
                    ELSE
                      EST_STATUS = 1
                 END IF
                 CALL SBIT ( LSITEC(1,J10), J9, EST_STATUS )
            END IF
!
! --------- Setting for reference station if needed
!
            IF ( ISITN_CHR(J9) .EQ. FIXNAM_CHR ) THEN
                 CALL SBIT ( LSITEC(1,J10), J9, INT2(0) )
            END IF
 4100    CONTINUE
 490  CONTINUE
!
! === Velocities
!     ~~~~~~~~~~
!
! --- Initialization
!
      CALL NOUT ( 2*INT4(STA_BIT_WORDS)*3, LSITEV )
!
      DO 4110 J11=1,INT4(NUMSTA)
!
! ------ Check whether the station was excluded entirely from processing
!
         IF ( .NOT. KBIT ( ISTAD, J11 ) ) GOTO 4110
!
! ------ Check whether estimation of station position (and, therefore, 
! ------ velocity) of this station was switched off for this session only
!
         IF ( .NOT. FL_STA_EST(J11) ) THEN
              CALL SBIT ( LSITEV(1,1), J11, INT2(0) )
              CALL SBIT ( LSITEV(1,2), J11, INT2(0) )
              CALL SBIT ( LSITEV(1,3), J11, INT2(0) )
              GOTO 4110
         END IF
!
! ------ Check whether position of this station are estmated as spline of the
! ------ zero-th order
!
         IF ( PSITED(J11) .NE. 0 ) GOTO 4110
         IF ( NUMEXC_VEL > 0 ) THEN
              IEXP = LTM_DIF ( 1, INT4(NUMEXC_VEL), VEL_NAM, ISITN_CHR(J11) )
            ELSE
              IEXP = 0
         END IF
         DO 4120 J12=1,3
!
! --------- Setting global flags
!
            IF ( VELFLG .EQ. 'Y' ) THEN
                 CALL SBIT ( LSITEV(1,J12), J11, INT2(1) )
            END IF
!
            IF ( INT2(IEXP) .GT. 0 ) THEN
!
! -------------- The J11-th station was in exception list
!
                 IF ( VEL_CMP(IEXP)(J12:J12) .NE. '-' ) THEN
!
! ------------------- The J12-th station component was specified for the J11-th
! ------------------- station. Then we toggle the status of the estimation of
! ------------------- the J12-th component of the J11-th station
!
                      IF ( KBIT ( LSITEV(1,J12), J11 ) ) THEN
                           EST_STATUS = 0
                         ELSE
                           EST_STATUS = 1
                      END IF
                      CALL SBIT ( LSITEV(1,J12), J11, EST_STATUS )
                 END IF
            END IF
 4120    CONTINUE
 4110 CONTINUE
!
! === Diurnal station components
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      CALL KSBIT ( IUEN, INT2(2), 'D' .EQ. STADIU )
!
! === Sources
!     ~~~~~~~
!
      READ ( UNIT=FIXSRC_CHR, FMT='(4A2)' ) FIXSRC
      CALL RFSRC ( FIXNAM, FIXSRC, ISRSEL, ISTRN, NUMSTR )
      WRITE ( UNIT=FIXNAM_CHR, FMT='(4A2)' ) FIXNAM
!
      IF ( SRCFLG .NE. 'I' ) THEN
!
! -------- Initialization
!
           CALL NOUT ( 2*INT4(SRC_BIT_WORDS)*2, LSTAR )
      END IF
!
      DO 4130 J13=1,INT4(NUMSTR)
         IEXP = LTM_DIF ( 1, INT4(NUMEXC_SOU), SOU_NAM, ISTRN_CHR(J13) )
!
! ------ Check whether the source was excluded entirely from processing
!
         IF ( .NOT. KBIT ( ISRSEL, J13 ) ) THEN
              CALL SBIT ( LSTAR(1,1), J13, INT2(0) )
              CALL SBIT ( LSTAR(1,2), J13, INT2(0) )
              GOTO 4130
         END IF
!
! ------ Check whether estimation of source coordinates of this source
! ------ was switched off for this sessions only
!
         IF ( NUM_SOUOFF .GT. 0 ) THEN
              IEXP = LTM_DIF ( 1, NUM_SOUOFF, LIST_SOUOFF, ISTRN_CHR(J13) )
              IF ( IEXP .GT. 0 ) THEN
                   CALL SBIT ( LSTAR(1,1), J13, INT2(0) )
                   CALL SBIT ( LSTAR(1,2), J13, INT2(0) )
                   GOTO 4130
              END IF
         END IF
!
! ------ If SRCFLG was "IN" then nothing to do any more for this source: we keep
! ------ local settings read from the superfile
!
         IF ( SRCFLG .EQ. 'I' ) GOTO 4130
!
         DO 4140 J14=1,2
!
! --------- Setting global flags
!
            IF ( SOU_CMP_ALL(J14:J14) .NE. '-' ) THEN
                 IF ( SRCFLG .EQ. 'Y' ) THEN
                      CALL SBIT ( LSTAR(1,J14), J13, INT2(1) )
                 END IF
            END IF
!
            IF ( INT2(IEXP) .GT. 0 ) THEN
!
! -------------- The J13-th source was in exception list
!
                 IF ( SOU_CMP(IEXP)(J14:J14) .NE. '-' ) THEN
!
! ------------------- The J14-th source component was specified for the J13-th
! ------------------- source. Then we toggle the status of the estimation of
! ------------------- the J14-th component of the J13-th source
!
                      IF ( KBIT ( LSTAR(1,J14), J13 ) ) THEN
                           EST_STATUS = 0
                         ELSE
                           EST_STATUS = 1
                      END IF
                      CALL SBIT ( LSTAR(1,J14), J13, EST_STATUS )
                 END IF
            END IF
 4140    CONTINUE
 4130 CONTINUE
!
! === Proper_motions
!     ~~~~~~~~~~~~~~
!
!
! --- Initialization
!
      CALL NOUT ( 2*INT4(SRC_BIT_WORDS)*2, LPROP )
!
      DO 4150 J15=1,INT4(NUMSTR)
         IEXP = LTM_DIF ( 1, INT4(NUMEXC_PRO), PRO_NAM, ISTRN_CHR(J15) )
!
! ------ Check whether the source was excluded entirely from processing
!
         IF ( .NOT. KBIT ( ISRSEL, J15 ) ) GOTO 4150
!
! ------ Check whether estimation of source coordinates (and, therefore,
! ------ proper motion) of this source was switched off for this sessions only
!
         IF ( NUM_SOUOFF .GT. 0 ) THEN
              IEXP = LTM_DIF ( 1, NUM_SOUOFF, LIST_SOUOFF, ISTRN_CHR(J15) )
              IF ( IEXP .GT. 0 ) THEN
                   CALL SBIT ( LPROP(1,1), J15, INT2(0) )
                   CALL SBIT ( LPROP(1,2), J15, INT2(0) )
                   GOTO 4150
              END IF
         END IF
!
         DO 4160 J16=1,2
!
! --------- Setting global flags
!
            IF ( PRO_CMP_ALL(J16:J16) .NE. '-' ) THEN
                 IF ( PROFLG .EQ. 'Y' ) THEN
                      CALL SBIT ( LPROP(1,J16), J15, INT2(1) )
                 END IF
            END IF
!
            IF ( INT2(IEXP) .GT. 0 ) THEN
!
! -------------- The J15-th source was in exception list
!
                 IF ( PRO_CMP(IEXP)(J16:J16) .NE. '-' ) THEN
!
! ------------------- The J16-th source component was specified for the J15-th
! ------------------- source. Then we toggle the status of the estimation of
! ------------------- the J16-th component of the J15-th source
!
                      IF ( KBIT ( LPROP(1,J16), J15 ) ) THEN
                           EST_STATUS = 0
                         ELSE
                           EST_STATUS = 1
                      END IF
                      CALL SBIT ( LPROP(1,J16), J15, EST_STATUS )
                 END IF
            END IF
 4160    CONTINUE
 4150 CONTINUE
!
! === Axis offset ?
!     ~~~~~~~~~~~~~
!
      CALL NOUT ( 2*INT4(STA_BIT_WORDS), LAXOF )
!
      DO I=1,NUMSTA
!
! ------ Check whether the station was excluded entirely from processing
!
         IF ( KBIT ( ISTAD, I ) ) THEN
!
! ----------- First set the flag for this station
!
              CALL KSBIT ( LAXOF, I, L4TOL2 ( AXSFLG .EQ. 'Y') )
!
! ----------- ... and then look for exceptions. The exception toggles the flag
!
              DO J=1,NUM_AXIS
                 IF ( AXNM_CHR(J) .EQ. ISITN_CHR(I) ) THEN
                      CALL KSBIT ( LAXOF, I, L4TOL2 ( AXSFLG .NE. 'Y') )
                 ENDIF
              ENDDO
            ELSE
              CALL KSBIT ( LAXOF, I, FALSE__L2 ) ! The station was excluded
         END IF
!
      ENDDO
!
! --- Baselines clock selection ?
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      BSET=0
      IF ( BASDF .EQ. 'N' ) THEN
           BSET=1
           DO I = 1,NUMSTA
              DO J=1,NUMSTA
                 CALL SBIT ( IBLSEL_G(1,I), J, INT2(0) )
                 CALL SBIT ( IBLSEL_G(1,I), J, INT2(0) )
              ENDDO
           ENDDO
      ENDIF
      ST1 = 0
      ST2 = 0
      DO I=1,NEXCBL
         DO J=1,NUMSTA
            IF ( EQUAL( IBLNM(1,I), INT2(1), ISITN(1,J), INT2(1), INT2(8) )) &
     &           ST1 = J
            IF ( EQUAL( IBLNM(5,I), INT2(1), ISITN(1,J), INT2(1), INT2(8) )) &
     &           ST2 = J
         ENDDO
         IF ( ST1.NE.0 .AND. ST2.NE.0 ) THEN
              CALL SBIT ( IBLSEL_G(1,ST1), ST2, BSET )
              CALL SBIT ( IBLSEL_G(1,ST2), ST1, BSET )
         ENDIF
      ENDDO
!
!      IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! -------- In the case of phase solution type we make the following trick:
! -------- we copy baseline selection array from group delay solution type
! -------- array to phase solution baseline selection array. Group delay
! -------- solution baseline selection array was set before when we didn't
! -------- know solution type.
!
!           DO 4170 J17=1,ARC_STA_BIT_WORDS
!              DO 4180 J18=1,MAX_ARC_STA
!                 IBLSEL_P(J17,J18) = IBLSEL_G(J17,J18)
! 4180         CONTINUE
! 4170      CONTINUE
!      END IF
!
! --- Setting station deselection status bit field
!
      CALL SET_STABIT ( INT2(2) )
!
! === Atmospheres ?
!     ~~~~~~~~~~~~~
!
      ATMFLG_USE = ATMFLG
!
! --- Check: whether atmosphere interval type has been overriden for this 
! ---        session only
!
      IF ( ATM_INTV_TYP .NE. ' ' ) ATMFLG_USE = ATM_INTV_TYP 
      IF ( ATMFLG_USE .NE. 'D' ) THEN
           OLD_ATMS = .FALSE.
      ENDIF
      IF ( ATMFLG_USE .EQ. 'A' ) THEN
           BMODE_AT = .TRUE.
      ENDIF
      IF ( ATMFLG_USE .EQ. 'M'  .AND. &
     &     DBNAME_CH(1:1) .NE. '$'       ) THEN
           CALL ERR_LOG ( 4851, IUER, 'SFLAGS', 'Attempt to use '// &
     &         'ATMOSPHERE MOST in the $FLAGS section for the database '// &
     &          DBNAME_CH//' which has no atmosphere flag parameterization' )
           CALL EXIT ( 1 )
           RETURN 
      END IF
!
      IF ( ATMFLG_USE .EQ. 'F' ) THEN
!
! -------- This option sets estimation of only time independent offset
! -------- of zenith troposphere path delay
!
           CALL NOUT ( INT4(ATM_BIT_WORDS)*2*3, LATM )
           JATM = 0
           DO ISTA=1,NUMSTA
              IATSTR(ISTA) = JATM
              JATM = JATM + 1
!
              IF ( KBIT(ISTAD,ISTA) ) THEN
                   NUMATM(ISTA)  = 1
                   CALL SBIT ( LATM(1,1), JATM, INT2(1) )
                   TATM(JATM) = FJDOBS
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@              IF ( KBIT(ISTAD,ISTA) ) THEN
!@                   IF ( NUMATM(ISTA).LE.0 ) THEN
!@                        CALL FERR ( INT2(126), 'BATCH(sflags): Station '// &
!@     &                      'with no atmosphere epoch in session '// &
!@     &                       DBNAME_MES, INT2(0), INT2(0) )
!@                   END IF
!@                   DO JATM=IATSTR(ISTA)+1,IATSTR(ISTA)+NUMATM(ISTA)
!@                      IF ( ( .NOT. KBIT(LATM(1,1),JATM)) .AND. &
!@     &                     ( .NOT. KBIT(LATM(1,2),JATM))       ) THEN
!@                             CALL SBIT( LATM(1,1), JATM, INT2(1) )
!@                      ENDIF
!@                   ENDDO
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                ELSE
                   NUMATM(ISTA) = 0
              ENDIF
           ENDDO
         ELSE IF ( ATMFLG_USE .EQ. 'N' ) THEN
           DO ISTA=1,NUMSTA
              NUMATM(ISTA) = 0
              NUMATM(ISTA) = 0
              CALL SBIT ( LATM(1,1), INT2(1), INT2(0) )
           ENDDO
         ELSE IF ( ATMFLG_USE .EQ. 'A'  .OR.  ATMFLG_USE .EQ. 'M' ) THEN
           INTR = INTRVL
           IF ( ATM_INT_VAL .NE. 0 ) THEN
                INTR = ATM_INT_VAL 
           END IF
           IF ( BMODE_AT  .AND.  ATMFLG_USE .EQ. 'M' ) THEN
                INTR = MIN ( INT4(INTR), INT(ATMOS_INTERVAL*60.D0+0.5D0) )
           END IF
!
! -------- Run a procedure for automatic setting atmosphere epochs
!
           CALL AUTAT ( INTR, FAIL )
           IF ( FAIL ) THEN
                CALL FERR ( INT2(127), 'BATCH(sflags): Too many atmosphere '// &
     &              'epochs in session '//DBNAME_MES, INT2(0), INT2(0) )
           END IF
           IF ( ATMFLG_USE .EQ. 'A'  .OR. &
     &          ATMFLG_USE .EQ. 'M'       ) THEN
                CALL DO_ATMEXC ( ISTAD )
           END IF
      ENDIF  ! ATMFLG
!
! --- Turn off atmosphere parameters at stations that are deleted
!
      DO I=1,NUMSTA
         IF ( .NOT. KBIT(ISTAD,I) ) THEN
              NUMATM(I)=0
         ENDIF
      ENDDO
!
! === Atmopshere gradients
!     ~~~~~~~~~~~~~~~~~~~~
!
      INTR8 = GRINTRVL  ! From global control file
      IF ( GRAD_INT_VAL .GT. 1.D-15 ) THEN
!
! -------- Replace by the value from this session only
!
           INTR8 = GRAD_INT_VAL*60.0D0 
      END IF
!
      IF ( INTR8 .GT. 1.D-15 ) THEN
           CALL AUTGRAD ( INTR8, FAIL )
           IF ( FAIL ) THEN 
               CALL FERR ( INT2(137), 'BATCH(sflags): Too many gradient '// &
     &             'epochs in the session '//DBNAME_MES, INT2(0), INT2(0) )
           END IF
      END IF
      CALL DO_GRADEXC ( ISTAD )
!
! --- Special trick whcih has sense only for Solve testing 
! --- in August-September 2003
!
      CALL GETENV ( 'GRAD_AUG2003_BUG', STR )
      CALL TRAN ( 11, STR, STR ) 
      IF ( STR(1:3) .EQ. 'YES'  .OR.  STR(1:2) .EQ. 'ON' ) THEN
!
! -------- If a kludge variable GRAD_AUG2003_BUG we set a flag to use 
! -------- wrong logic. This flag is used later in partl
!
           GRAD_AUG2003_BUG = .TRUE.
        ELSE
           GRAD_AUG2003_BUG = .FALSE.
      END IF
!
! === Clocks
!     ~~~~~~
!
      BM_REF_CL_ORIG = BM_REF_CL
      CLOCK_REF_BITS(1) = 0
      CLOCK_REF_BITS(2) = 0
!
! --- Check whether we have to replace global clock flags with the clock
! --- flags defined for this sessions only
!
      CLKFLG_USE = CLKFLG
      IF ( CLOCK_INTV_TYP .NE. ' ' ) CLKFLG_USE = CLOCK_INTV_TYP 
      CLKPOL_FLG_USE = CLKPOL_FLG
      IF ( CLOCK_MD_TYP   .NE. ' ' ) CLKPOL_FLG_USE = CLOCK_MD_TYP 
      CLKPOL_DEG_USE = CLKPOL_DEG
      IF ( CLOCK_MD_VAL   .NE.  0  ) CLKPOL_DEG_USE = CLOCK_MD_VAL 
!
      IF ( CLKFLG_USE .EQ. 'A' ) THEN
           BMODE_CL   = .TRUE.
      END IF
      IF ( CLKFLG_USE .EQ. 'N' ) THEN
           BMODE_CL   = .FALSE.
      END IF
!
      IF ( CLKFLG_USE .EQ. 'M'  .AND. &
     &     DBNAME_CH(1:1) .NE. '$'       ) THEN
           CALL ERR_LOG ( 4852, IUER, 'SFLAGS', 'Attempt to use '// &
     &         'CLOCKS MOST in the $FLAGS section for the database '// &
     &          DBNAME_CH//' which has no clock flags parameterization' )
           CALL EXIT ( 1 )
           RETURN 
      END IF
!
      IF ( BM_REF_CL .GE. 1   .AND.   BM_REF_CL .LE. MAX_ARC_STA ) THEN
!
! -------- Setting CLOCK_REF_BITS in order to be compatible with
! -------- multireference feature of SOLVE
!
           IF ( CHECK_STABIT ( BM_REF_CL ) ) THEN ! if selected in solution
                CALL SBIT ( CLOCK_REF_BITS, BM_REF_CL, INT2(1) )
           END IF
      END IF
!
      IF ( ( CLKFLG_USE .NE. 'D'                 ) .OR. &
     &     .NOT. CHECK_STABIT ( BM_REF_CL_ORIG )        ) THEN
!
           TCLK = CLKFLG_USE(1:1)
           IF ( CLKFLG_USE .EQ. 'P'  .OR.  CLKFLG_USE .EQ. 'M' ) THEN
!
! ------------- Pick implies AUTO if clock breaks, FORCE otherwise
!
                IF ( CLK_BRK_STAT ) THEN
                     TCLK = 'A'
                  ELSE
                     TCLK = 'F'
                ENDIF
           ENDIF
!
! -------- Pick the clock interval, either requested or MOST
!
           CKN = CKNTRVL
           IF ( CLOCK_INT_VAL .GT. 1.D-15 ) CKN = CLOCK_INT_VAL 
           IF ( BMODE_CL .AND. CLKFLG_USE .EQ. 'M' ) THEN
                CKN = MIN ( INT4(CKN), INT(CLOCK_INTERVAL*60.0D0 + 0.49D0) )
           END IF
!
! -------- If reference clocks specified on the arc line, set the station
! -------- bit array based on the station names.
! -------- The station number of the (single) reference site is
! -------- historically saved in bm_ref_cl and it is stored that way in
! -------- databases and superfiles. BM_REF_CL supports only 1 clock ref
! -------- site, the new (NOV97) multi-ref-clock uses the 32 bits in clock
! -------- clock_ref_bits. With restored superfiles in the batch mode
! -------- that switch from BM_REF_CL to the bits is done here.
! -------- But it is important to remember that the guts of solve
! -------- estimation doesn't use BM_REF_CL or the bits, it uses the
! -------- various clock control arrays LCLK, ICLSTA and FJDCL.
! -------- CLOCK_REF_BITS is only used to set those arrays.
!
           IF ( DO_CLK_REF ) THEN ! Reference clock from arc lines
                CLOCK_REF_BITS(1) = 0
                CLOCK_REF_BITS(2) = 0
                BM_REF_CL = 0
!
                DO I = 1, NUM_CLK_REF
                   DO J = 1, NUMSTA
!
! ------------------- Comparing the master site list to arc line list
!
                      IF ( ISITN_CHR(J) .EQ. LIST_CLK_REF(I) ) THEN
                           CALL SBIT ( CLOCK_REF_BITS, J, INT2(1) )
                           BM_REF_CL = J
                      END IF
                   ENDDO
                ENDDO
           END IF ! do_clk_ref
!
           CALL AUTCL_MULT_REF ( CLKPOL_FLG_USE, CLKPOL_DEG_USE, TCLK, CKN, &
     &                           FAIL, ISTAD )
!
           OLD_CLOCKS = .FALSE.
           IF ( FAIL ) THEN
                CALL FERR ( INT2(128), 'BATCH(sflags): Too many clock '// &
     &             'intervals in the session '//DBNAME_MES, INT2(0), INT2(0) )
           END IF
!
! -------- Turn off clock parameters at stations that are deleted
!
           DO I=1,NUMSTA
              IF ( .NOT. CHECK_STABIT ( I ) ) THEN
                   DO J=1,NUMCLK(I)
                      CALL SBIT ( ICLSTA(1,J+ICLSTR(I)), I, INT2(0) )
                   ENDDO
              ENDIF
              IF ( KBIT ( CLOCK_REF_BITS, I ) )  NUMCLK(I) = 0
           ENDDO
      ENDIF
!
! === Baseline-dependent clocks
!     ~~~~~~~~~~~~~~~~~~~~~~~~~
!
      BLCFLG_SES = BLCFLG ! Copy global value of BLCFLG
!
! --- Check: was the session -specific baseline dependent clock flag set?
! --- If yes it overrides global flag
!
      IF ( BASDEP_CLO(1:3) .EQ. 'YES' ) THEN
           BLCFLG_SES = 'Y'
         ELSE IF ( BASDEP_CLO(1:2) .EQ. 'NO' ) THEN
           BLCFLG_SES = 'N'
         ELSE IF ( BASDEP_CLO(1:2) .EQ. 'IN' ) THEN
           BLCFLG_SES = 'I'
      END IF
!
      IF ( BLCFLG_SES .EQ. 'Y' ) THEN
!
! -------- Initialization
!
           CALL NOUT ( 2*INT4(ARC_STA_BIT_WORDS*MAX_ARC_STA), ICLOCK   )
           CALL NOUT ( 2*INT4(ARC_STA_BIT_WORDS*MAX_ARC_STA), ICLOCK_P )
!
! -------- Set baseline dependent clock
!
           IUER = -1
           CALL BCLOCK_MAX ( IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 4861, -3, 'SFLAGS', 'Error in '// &
     &              'an attempt to make a maximal baseline clocks setup' )
                STOP 'BATCH(sflags) Abnormal termination'
           END IF
           LOGBCL = .TRUE.
        ELSE IF ( BLCFLG_SES .EQ. 'N' ) THEN
!
! -------- Turn off for all baselines
!
           LOGBCL = .FALSE.
           DO I=1,NUMSTA
              DO J=1,NUMSTA
                 CALL SBIT ( ICLOCK(1,I), J, INT2(0) )
                 CALL SBIT ( ICLOCK(1,J), I, INT2(0) )
                 CALL SBIT ( ICLOCK_P(1,I), J, INT2(0) )
                 CALL SBIT ( ICLOCK_P(1,J), I, INT2(0) )
              ENDDO
           ENDDO
        ELSE IF ( BLCFLG_SES .EQ. 'I' ) THEN
!
! -------- Save as it was in superfile.
!
           CONTINUE
!
! -------- ... but disable for all baselines with clock reference station(s)
! -------- if the clock reference station was changed and also if the station
! -------- was deselected
!
           IF ( BM_REF_CL .NE. BM_REF_CL_ORIG ) THEN
                DO I=1,NUMSTA
                   IF ( BM_REF_CL .GE. 1  .AND.  BM_REF_CL .LE. NUMSTA ) THEN
                        CALL SBIT ( ICLOCK(1,I), BM_REF_CL, INT2(0) )
                        CALL SBIT ( ICLOCK(1,BM_REF_CL), I, INT2(0) )
                   END IF
!
                   DO J=1,NUMSTA
!
! ------------------- Disable baseline clocks for baselines with all clock
! ------------------- stations
!
                      IF ( KBIT ( CLOCK_REF_BITS, J ) ) THEN
                           CALL SBIT ( ICLOCK(1,I), J, INT2(0) )
                           CALL SBIT ( ICLOCK(1,J), I, INT2(0) )
                           CALL SBIT ( ICLOCK_P(1,I), J, INT2(0) )
                           CALL SBIT ( ICLOCK_P(1,J), I, INT2(0) )
                      END IF
                   END DO
               ENDDO
          ENDIF
!
! ------- Disable baselines clocks for baselines with deselected stations
!
          DO I=1,NUMSTA
             DO J=1,NUMSTA
                IF ( .NOT. CHECK_STABIT ( I ) ) THEN
                     CALL SBIT ( ICLOCK(1,I), J, INT2(0) )
                     CALL SBIT ( ICLOCK(1,J), I, INT2(0) )
                     CALL SBIT ( ICLOCK_P(1,I), J, INT2(0) )
                     CALL SBIT ( ICLOCK_P(1,J), I, INT2(0) )
                END IF
             END DO
          END DO
      ENDIF
!
! === UT1/PM ?
!     ~~~~~~~~
!
      CALL UT1PM ( ORIENT, UT1FLG, ROTFLG, NUMSTA, ISTAD, VSITEC, NRTARC, NROT, &
     &             TROT, ROTAP, IBIT, ISITN, CONSTRAINT_BITS, IDBEND(1), &
     &             EOPMID, EOP_EPOCH_MJD, EOP_EPOCH_SEC, EOP_BEFORE_SEC_TAI, &
     &             EOP_AFTER_SEC_TAI )
      IF ( IEOP_FLG(1).EQ.0  .AND. IEOP_FLG(2) .EQ.0 .OR. ORIENT.EQ.'N' ) THEN
!
! --------- Reset all the rotation flag bits back to off.
!
            DO I =1,ROT_BIT_WORDS
               DO J=1,3
                  LROT(I,J) = 0
               ENDDO
            ENDDO
!
! --------- Offset flags
!
            IF ( KBIT ( OFFLG, INT2(1) )  .AND.  EOP_SUPR(1:1) .NE. 'Y' ) THEN
                 IDUM = IROTF ( INT2(1), INT2(1), INT2(1), LROT)
            END IF
!
            IF ( KBIT ( OFFLG, INT2(2) )  .AND.  EOP_SUPR(2:2) .NE. 'Y' ) THEN
                 IDUM = IROTF ( INT2(1), INT2(2), INT2(1), LROT)
            END IF
!
            IF ( KBIT ( OFFLG, INT2(3) )  .AND.  EOP_SUPR(3:3) .NE. 'Y' ) THEN
                 IDUM = IROTF ( INT2(1), INT2(3), INT2(1), LROT)
            END IF
!
! --------- Rate flags
!
            IF ( KBIT ( RATFLG, INT2(1) ) .AND.  EOP_SUPR(1:1) .NE. 'Y' ) THEN
                 IDUM = IROTF ( INT2(1), INT2(1), INT2(2), LROT)
            END IF
            IF ( KBIT ( RATFLG, INT2(2) ) .AND.  EOP_SUPR(2:2) .NE. 'Y' ) THEN
                 IDUM = IROTF ( INT2(1), INT2(2), INT2(2), LROT)
            END IF
            IF ( KBIT ( RATFLG, INT2(3) ) .AND.  EOP_SUPR(4:4) .NE. 'Y' ) THEN
                 IDUM = IROTF ( INT2(1), INT2(3), INT2(2), LROT)
            END IF
!
! --------- 2nd order EOP flag
!
            IF ( KBIT ( ACCEOP_FLG, INT2(1) )  .AND.  EOP_SUPR(1:1) .NE. 'Y') &
     &           THEN
                 IDUM = IROTF ( INT2(1), INT2(1), INT2(3), LROT)
            END IF
            IF ( KBIT ( ACCEOP_FLG, INT2(2) )  .AND.  EOP_SUPR(2:2) .NE. 'Y') &
     &           THEN
                 IDUM = IROTF ( INT2(1), INT2(2), INT2(3), LROT)
            END IF
            IF ( KBIT ( ACCEOP_FLG, INT2(3) )  .AND.  EOP_SUPR(4:4) .NE. 'Y') &
     &           THEN
                 IDUM = IROTF ( INT2(1), INT2(3), INT2(3), LROT)
            END IF
          ELSE
            ROT_INTERVAL(1) =  REOP_FLG(1)
            ROT_INTERVAL(2) =  REOP_FLG(1)
            SEOCNST(1)      =  REOP_FLG(3)
            SEOCNST(2)      =  REOP_FLG(4)
!
            EOP_STYLE(1) = IEOP_FLG(1)
            EOP_STYLE(2) = IEOP_FLG(1)
            EOPA1_CHOICE(1) = 0
            EOPA1_CHOICE(2) = 0
            CALL SBIT ( CONSTRAINT_BITS, INT2(4), INT2(1) )
            CALL SBIT ( CONSTRAINT_BITS, INT2(5), INT2(1) )
            IF ( EOP_STYLE(1) .EQ. EOP__SINE ) THEN
!
! --------- Set the offset and rates from sine style
!
            NROT_A1(1) = 1
            NROT_A1(2) = 1
        ENDIF
      ENDIF
!
      IF ( EOP_STYLE(1) .EQ. EOP__SEGS_ONLY        .OR. &
     &     EOP_STYLE(1) .EQ. EOP__RATES_AND_SEGS          )  THEN
!
! -------- Determine number of eop epochs
!
           NROT_A1(1) = (LJDOBS-TROT_A1)/ROT_INTERVAL(1)
!
! -------- Make certain there is an epoch after the last obs/
!
           IF ( ROT_INTERVAL(1)*NROT_A1(1) + TROT_A1 .LT. LJDOBS ) THEN
                 NROT_A1(1) = NROT_A1(1)+1
           END IF
           NROT_A1(2) = NROT_A1(1)
      ENDIF
!
      IEOPL = IEOPLL ! flag for writing earth orientation adjs. to plot file
!
! === Do we solve for nutation -- what is the question!
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      DO J=1,6
         COMP =NUTFLG(J)(1:1)
         PHASE=NUTFLG(J)(2:2)
         IBIT5=0
         IF ( COMP .EQ. 'P'  .OR.  COMP(1:1) .EQ. 'B' ) IBIT5=1
         CALL SBIT ( LNUT(2), J, IBIT5 )
         IBIT5=0
         IF ( COMP .EQ. 'E'  .OR.  COMP(1:1) .EQ. 'B' ) IBIT5=1
         CALL SBIT ( LNUT(3), J, IBIT5 )
      ENDDO
!
! === Set bits of daily nutation estimation
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IBIT5 = 0
      IF ( ( NUTFLG(7)(1:1)  .EQ. 'P' .OR. &
     &       NUTFLG(7)(1:1)  .EQ. 'B' .OR. &
     &       NUTFLG(8)(1:1)  .EQ. 'P' .OR. &
     &       NUTFLG(8)(1:1)  .EQ. 'B'      ) .AND. &
     &       ORIENT .EQ. 'Y'                 .AND. &
     &       EOP_SUPR(5:5) .NE. 'Y'                ) THEN
           IBIT5=1
      END IF
      CALL SBIT ( LNUT(1), INT2(1), IBIT5 )
!
      IBIT5=0
      IF ( ( NUTFLG(7)(1:1)  .EQ. 'E' .OR. &
     &       NUTFLG(7)(1:1)  .EQ. 'B' .OR. &
     &       NUTFLG(8)(1:1)  .EQ. 'P' .OR. &
     &       NUTFLG(8)(1:1)  .EQ. 'B'      ) .AND. &
     &       ORIENT .EQ. 'Y'                 .AND. &
     &       EOP_SUPR(6:6) .NE. 'Y'                ) THEN
           IBIT5=1
      END IF
      IF ( NUTFLG(7)(1:1) .EQ. 'P' .OR. &
     &     NUTFLG(7)(1:1) .EQ. 'B'      ) THEN
           NUT_USE_CODE = NUT__PSE  
      END IF
      IF ( NUTFLG(8)(1:1) .EQ. 'P' .OR. &
     &     NUTFLG(8)(1:1) .EQ. 'B'      ) THEN
           NUT_USE_CODE = NUT__XY   
      END IF
      CALL SBIT ( LNUT(1), INT2(2), IBIT5 )
!
! === Other terms of nutation
!     ~~~~~~~~~~~~~~~~~~~~~~~
!
      IBIT5=0
      IF ( ( NUTFLG(9)(1:1).EQ.'P' .OR. NUTFLG(9)(1:1).EQ.'B' ) .AND. &
     &       ORIENT.EQ.'Y') IBIT5=1
      CALL SBIT ( FLPSI(1), INT2(1), IBIT5 )
      IBIT5=0
!
      IF ( ( NUTFLG(9)(1:1).EQ.'E' .OR. NUTFLG(9)(1:1).EQ.'B' ) .AND. &
     &       ORIENT.EQ.'Y') IBIT5=1
      CALL SBIT ( FLEPS(1), INT2(1), IBIT5 )
!
      IBIT5=0
      IF ( NUTFLG(10)(1:1).EQ.'P' .OR. NUTFLG(10)(1:1).EQ.'B' ) IBIT5=1
      CALL SBIT( FLPSI(1), INT2(2), IBIT5 )
      IBIT5=0
      IF ( NUTFLG(10)(1:1).EQ.'E' .OR. NUTFLG(10)(1:1).EQ.'B' ) IBIT5=1
      CALL SBIT ( FLEPS(1), INT2(2), IBIT5 )
!
      FCNPER=FCNPR
      IBIT5=0
      IF ( NUTFLG(11)(1:1).EQ.'P' .OR. NUTFLG(11)(1:1).EQ.'B' ) IBIT5=1
      CALL SBIT ( FLPSI(1), INT2(3), IBIT5 )
      CALL SBIT ( FLPSI(1), INT2(4), IBIT5 )
!
      IBIT5=0
      IF ( NUTFLG(11)(1:1).EQ.'E' .OR. NUTFLG(11)(1:1).EQ.'B' ) IBIT5=1
      CALL SBIT ( FLEPS(1), INT2(3), IBIT5 )
      CALL SBIT ( FLEPS(1), INT2(4), IBIT5 )
!
      NDPNUT=0
      DO I=13,116
         COMP =NUTFLG(I)(1:1)
         PHASE=NUTFLG(I)(2:2)
         IBIT1=0
         IF ( COMP.EQ.'P' .OR. COMP.EQ.'B' ) THEN
              IF ( PHASE.EQ.'I' .OR. PHASE.EQ.'B' ) IBIT1=1
         ENDIF
         CALL SBIT( FLPSI(1), INT2(4+(I-11)*2+2), IBIT1 )
         IBIT2=0
         IF ( COMP.EQ.'P' .OR. COMP.EQ.'B' ) THEN
              IF(PHASE.EQ.'O'.OR.PHASE.EQ.'B') IBIT2=1
         ENDIF
         CALL SBIT( FLPSI(1), INT2(4+(I-11)*2+1), IBIT2 )
         IBIT3=0
         IF ( COMP.EQ.'E' .OR. COMP.EQ.'B' ) THEN
              IF ( PHASE.EQ.'I' .OR. PHASE.EQ.'B' ) IBIT3=1
         ENDIF
         CALL SBIT( FLEPS(1), INT2(4+(I-11)*2+1), IBIT3 )
         IBIT4=0
         IF ( COMP.EQ.'E' .OR. COMP.EQ.'B' ) THEN
              IF ( PHASE.EQ.'O' .OR. PHASE.EQ.'B' ) IBIT4=1
         ENDIF
         CALL SBIT( FLEPS(1), INT2(4+(I-11)*2+2), IBIT4 )
         CALL SBIT( IDPNUT, INT2(I-10), INT2(0) )
         IF ( IBIT1+IBIT2+IBIT3+IBIT4 .GT. 0 ) THEN
              CALL SBIT( IDPNUT, INT2(I-10), INT2(1) )
              NDPNUT=NDPNUT+1
         ENDIF
      ENDDO
!
      NFLPSI=0
      NFLEPS=0
      DO I=1,216
         NFLPSI=NFLPSI+KBITN(FLPSI,I)
         NFLEPS=NFLEPS+KBITN(FLEPS,I)
      ENDDO
!
! --- Precession ?
!     ~~~~~~~~~~~~
!
      LPREC = 0
      IF ( PRCFLG .EQ. 'Y' ) LPREC=1
!
! --- Relativity ?
!     ~~~~~~~~~~~~
!
      LREL=0
      IF ( RELFLG .EQ. 'Y' )LREL=1
!
! --- Ionosphere correction?
!
      IF ( IONFLG .EQ. 'Y' ) THEN
           KIONO = .FALSE.
        ELSE
           KIONO = .TRUE.
      ENDIF
!
      IOS_EST = IOS_EST_BATCH
      IOS_SIG = IOS_SIG_BATCH
!
! --- Output residuals?
!
      IL = TRIMLEN(RESFILE)
      IF ( RESFILE == 'FULL' .OR. RESFILE == 'SPOOL' ) THEN
           RESOUTFILE = 'NONE'
           CALL SWBIT( IPRES, INT2(1) )
        ELSE IF ( RESFILE == 'NONE' .OR. RESFILE(1:1) == ' ' ) THEN
           RESOUTFILE = 'NONE'
        ELSE 
           RESOUTFILE = RESFILE(:IL)
      END IF
!
! --- Minimize position sigmas?
!
      IF ( KMIN_SIG ) THEN
           IF ( VEL_CMP_ALL.EQ. '---' ) CALL FERR ( INT2(6041), &
     &         'BATCH(sflags) SOLVE Cannot minimize sigmas without '// &
     &         'estimating ALL velocities', INT2(0), INT2(0) )
      ENDIF
      MINIMIZE_SIGS = KMIN_SIG
      FL_NOFLYBY = FL_NOMAP 
!
! --- Plate model scaling factor
!
      PLATE_FACT=PLATE_SCALE
!
! --- SITPL file
!
      SITPL_FIL = SITPLMAP
!
! --- Output normal matrix?
!
      IF ( KOUTNRM ) CALL SBIT ( IPRES, INT2(5), INT2(1) )
!
! --- Zero out normal matrix?
!
      IF ( KZERONRM ) CALL SBIT ( IPRES, INT2(6), INT2(1) )
!
! --- Weak station constraint?
!
      IF ( KSTACONST ) CALL SBIT ( IPRES, INT2(7), INT2(1) )
!
! --- Weak velocity constraint?
!
      IF ( KVELCONST ) CALL SBIT ( IPRES, INT2(8), INT2(1) )
!
      RETURN
      END  !#!  SFLAGS  #!#
