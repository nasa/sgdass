       SUBROUTINE GET_CALIB ( JCAPPL, JSITI, ITT, NOGOOD, ISITE, DT, RT, &
     &                  CALIBS, ICORR, GION, &
     &                  GIONSG, PHION, PHIONS, DERR, RERR, DPHER, ITTB, &
     &                  ET, SE, SS, CALIBB, CALIBM, OBCAPL, MCAPL, &
     &                  ISITN, ISTAR, VSTARC, AZ, ELEV, ATMPR, RELHU, TEMPC, &
     &                  DERR_RAW, RERR_RAW, DPHER_RAW, LATS, &
     &                  HEIGHTS, AX_OFFS, AX_TYPES, BARO_CALS, BARO_HEIGHTS, &
     &                  APX, JCAFFL, NFCAL, FCAL_NAMES, NAMSTA,IDB, &
     &                  EFFREQ, PHEFFREQ, REFFREQ, REFFREQ_XS, EFFREQ_XS, &
     &                  PHEFFREQ_XS, AXDIF, ISTRN_CHR, SOURCE_WEIGHT_FILE, &
     &                  SOURCE_WEIGHTS, AVG_ATM, KELDEP_NOISE, ATM_ZENDEL, &
     &                  RWT_EL_USE, RWT_SRC_USE, TROP_WZD, MAP_FUN, FJD, FRACTC, &
     &                  TRP, STS_TRP, TRP_USE, STS_VTD )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!
! 1.  GET_CALIB PROGRAM SPECIFICATION
!
! 1.1 Apply the weather, cable and ionosphere calibrations to the
!     theoretical delays and rates.
!
! 1.2 REFERENCES:
!
! 2.  GET_CALIB INTERFACE
!     Updated to specificaly type integers which
!-------------------------------------------------
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
      INCLUDE 'astro_constants.i'
!
! 2.2 INPUT Variables:
!
      REAL*8        EFFREQ, PHEFFREQ, REFFREQ, REFFREQ_XS, EFFREQ_XS, &
     &              PHEFFREQ_XS, AXDIF(*), FJD, FRACTC
      REAL*8        AVG_ATM(4,*)
      LOGICAL*2     KELDEP_NOISE
      CHARACTER     ISTRN_CHR*(*), SOURCE_WEIGHT_FILE*(*), SOURCE_WEIGHTS*(*)
      INTEGER*4     RWT_EL_USE, RWT_SRC_USE, STS_TRP, TRP_USE, STS_VTD
!
! 2.3 OUTPUT Variables:
!
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'glbc3.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'bindisp.i'
      INCLUDE 'flyby.i'
      INCLUDE 'trp.i'
!
      TYPE      ( TRP__TYPE ) :: TRP
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: calcalc
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2  ITTB(MAX_ARC_BSL), JCAPPL(MAX_ARC_STA), JSITI(MAX_ARC_STA)
      INTEGER*2  JCAFFL(7,MAX_ARC_STA), NFCAL,IDB
      CHARACTER  FCAL_NAMES(*)*8
      INTEGER*2  ITT(MAX_ARC_STA), ISITE(2)
      REAL*8     CALIBS(2,2,MAX_CAL), GION(2), GIONSG(2), PHIONS
      REAL*8     CALIBB(2,MAX_CONT),  CALIBM(6,M_CLM), ET(2,MAX_ARC_BSL), &
     &           SE(MAX_ARC_STA), SS(MAX_ARC_SRC), MAP_FUN(2), TROP_WZD(2)
      REAL*8     DT, RT, PHION, DERR, RERR, CONSTANTS(4)
      REAL*8     GROUP_F_RATIO_SQ, PHASE_F_RATIO_SQ, TROP_FRACT(4)
      INTEGER*2  OBCAPL, MCAPL
      LOGICAL*2  MTT_SEAS_DRY(2), MTT_SEAS_WET(2)
      LOGICAL*2  IFA_SEAS_DRY(2), IFA_SEAS_WET(2)
      INTEGER*2  ISTAR,AX_TYPES(MAX_ARC_STA)
      INTEGER*2  ID_CFAKBDRY, ID_CFAJJDRY, ID_CFAKBWET, ID_LANYI
      INTEGER*2  ID_MTTDRYSS, ID_MTTDYFLY, ID_IFADRYSS, ID_IFADYFLY
      INTEGER*2  ID_NMFDYFLY, ID_NMFWTFLY
      INTEGER*2  DO_LANWET, DO_PRCOR
      INTEGER*2  ISITN(4,MAX_STA)
      REAL*8     AZ(2), ELEV(2), ATMPR(2), RELHU(2), TEMPC(2), DERR_RAW, &
     &           RERR_RAW, ATM_ZENDEL(2)
      REAL*8     DPHER_RAW, DPHER, ELN(4,2)
      REAL*8     VSTARC(2,MAX_SRC), AX_OFFS(MAX_ARC_STA)
      REAL*8     CFAKBDRY(2,2), CFAKBWET(2,2)
      REAL*8     LATS(MAX_ARC_STA), HEIGHTS(MAX_ARC_STA)
      REAL*8     BARO_CALS(MAX_ARC_STA), BARO_HEIGHTS(MAX_ARC_STA)
      REAL*8     APP(2,2), APX(2,2)
      REAL*8     FREQ_GR_X, FREQ_GR_S, FREQ_PH_X, FREQ_PH_S, &
     &           FREQ_RATE_X, FREQ_RATE_S
      INTEGER*2  NAMSTA, IDB_SAVE, I, ICORR, ISTA1, ISTA2, ISTAT, J, JJ, &
     &           N, NOGOOD
      INTEGER*2  ID_JJ(2), ID_WET(3)
      LOGICAL*2  SET_THIS_ID, TWICE_IN_NAMF, IONO_APPLIED
      CHARACTER  ERRSTR*128, STR*128, STR1*8, STR2*8
      REAL*8     TEMP_GROUP, TEMP_PHASE, TDB_OBS, TAI_OBS, TIM_TLR, &
     &           EL_TLR, EL_MIN
      PARAMETER  ( TIM_TLR = 40.0D0 )
      PARAMETER  ( EL_TLR  =  1.0D0*DEG__TO__RAD )
      PARAMETER  ( EL_MIN  =  1.0D0*DEG__TO__RAD )
      INTEGER*4  MJD_TAI_OBS, IND_TRP(2), IND_SCA(2)
      LOGICAL*2  GET_SOURCE_WEIGHT
      INTEGER*2  ICTS, ICTT
      INTEGER*4  J1, J2, J3, IND_STA
      REAL*8     CAL_ADD, CAL_FRACT(2)
      REAL*8     CALIBB_DEL, CALIBB_RATE, ATM_ZENDEL_SAVE(2)
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      LOGICAL*2, EXTERNAL :: KBIT
      LOGICAL*4, EXTERNAL :: DATYP_INQ, IS_R8_NAN
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
! JCAPPL - Bit flag of station cable & atmosphere calibrations to be applied
! JCAFFL - Bit array of flyby calibrations to be applied
! OBCAPL - Bit flag of contributions to be applied
! MCAPL  - Bit flag of mode calibrations to be applied
! JSITI - Station ion calibration flags
! ITT - NAMFIL/PARFIL station correspondence table
! NOGOOD - Ionospheric correction. 1 means no good
! ISITE - Site number of the two stations in this observation
! DT - Delay theoretical
! RT - Rate Theoretical
! CALIBS -
! ICORR -
! GION -
! GIONSG -
! PHION -
! PHIONS -
! DERR -
! RERR -
! DPHER -
! ITTB - NAMFIL station/baseline correspondence table, packed
! ET - Baseline formal errors (from NAMFIL)
! CALIBB -
! ISITN - Array of site names
! ISTAR - Source number for this observation
! VSTARC - Array of source coordinates (RA and DEC, in radians)
! AZ - Azimuth
! ELEV - Elevation
! ATMPR - Atmospheric pressure, millibars
! RELHU - Relative humidity
! TEMPC - Temerature (Celsius)
! DERR_RAW -
! RERR_RAW -
! DPHER_RAW -
! LATS - Latitude of each station
! HEIGHTS - Heights of each station
! AX_OFFS - Antenna axis offset
! AX_TYPES - ANtenna axis type for each station
! BARO_CALS - Barometer calibration for each station
! BARO_HEIGHTS - Height of barometer for each station
! APX -
! ID_CFAKBDRY, ID_CFAKBWET, ID_CFAJJDRY
! namsta - # stations in namfil
!
      SAVE ID_CFAKBDRY, ID_CFAKBWET, ID_CFAJJDRY, ID_LANYI, &
     &     DO_LANWET,IDB_SAVE, ID_MTTDRYSS, ID_MTTDYFLY, &
     &     MTT_SEAS_DRY, MTT_SEAS_WET, &
     &     ID_IFADRYSS, ID_IFADYFLY, &
     &     IFA_SEAS_DRY, IFA_SEAS_WET, &
     &     ID_NMFDYFLY, ID_NMFWTFLY
!
      DATA IDB_SAVE /0/, TWICE_IN_NAMF /.FALSE./
      REAL*8     SOLVE_SE_QUAD 
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AM   8411??  Created
!   IS   860113  New calibrations/contributions handling scheme:
!                contributions are in the array CALIBS, and are applied
!                or not according to the bit flag in JCAPPL, where if
!                the Jth bit in JCAPPL(I) is set, the contribution stored
!                in CALIBS(*,*,J) is applied to the delay and rate value
!                for station I. The actual source (LCODE) of each element in
!                CALIBS is given in the CORFIL.
!   KDB  860303  The scheme listed in the 13 Jan
!                1986 modification applies to station dependent calibrations/
!                contributions.  Now GET_CALIB is being modified to handle
!                observation dependent contributions.  These contributions are
!                in array CALIBB, and are applied or not applied according to
!                bit flag OBCAPL, where if bit J in OBCAPL is set, the
!                contribution stored in CALIBB(*,J) is applied to the delay and
!                rate values for the data base.  The actual source (LCODE) of
!                each element in CALIBB is given in the CORFIL.
!   JRR 861216:  Fixed a mistake of my own making dealing with the
!                application of ionospheric corrections to phase data.
!
!   KDB 870817:  Code to handle special calibrations (CFAWET and CFADRY,
!                which can be applied, even if the data base does not
!                contain their lcodes and they're therefore technically
!                unavailable.
!   JWR 870606   Modified to return the raw (not reweighted) delay and rate
!                errors.
!   KDB 910715   Use the latest namfil/corfil scheme, where flyby and regular
!                calibrations separate in namfil, namfil carries calib/contrib
!                names and only SDBH accesses corfil.
!   AEE 911120   Added MTT and IFADIS dry and wet mapping functions.
!   AEE 920212   Added CHAO DRY stuff.
!   AEE 920627   Removed partial stuff and placed them in ATMPART.F
!
!   JMG 930428   Corrected for Bad ionosphere calculation. Previous version
!                did not take into correlation between iono sigma and gruop/
!                phase sigma.  This error was re-discovered in April, 93.
!                Approximate fix uses constant ratio between X and S band freq.
!   kdb 950831   Add atmospheric turbulence rate constant and mapping function
!                parameter error delay and rate constants.
!   kdb 950905   Add alternate atmospheric turbulence constants scheme:
!                a negative delay or rate constant indicates the application of
!                a specified fraction of the delay or rate part of the
!                flyby atmosphere calibration used.
!   kdb 970401   Reverse the sign of the EQE CONT contribution (correction to
!                the equation of the equinox to fix a CALC error.) (But only in
!                calc versions < 8.3, the version in which this will be fixed.)
!   kdb 970418   Add a small fudge factor to the reversal of the sign of the
!                EQE CONT contribution to account for the ratio of universal
!                to sidereal time.
!   pet 980203   Substituted hard-coded test of solution type by DATYP_INQ
!   pet 980212   Rewrote comments
!   pet 1999.11.17  Addded three new formal arguemnts: CALIBM, REFFREQ_XS, MCAPL
!                   Added support of mode calibrations.
!   pet 2000.03.27  Improved an error message about ITT error
!   pet 2001.04.30  Fixed the bug: the previous verion worked incorrectly if the
!                   the sky frequency was less than 5 MHz and the opposite
!                   band was not available
!   pet 2001.07.17  Fixed the bug: the previous verion inprocessing S-band data
!                   tried to get square root from a negative number.
!   pet 2002.03.05  Added saving of the atmosphere delay in zenith direction
!                   in variable ATM_ZENDEL
!   kdb 2003.10.21  Rearranged expression from B*-A to ( -B*A )
!   JWR 2004.05.20  Sleep bug in a series of if tests fixed.
!   pet 2008.02.21  Added support of external troposphere path delay
!   pet 2008.04.29  Fixed several bugs in longic for handling 
!                   the external troposphere path delay
!
! 5.  GET_CALIB PROGRAM STRUCTURE
!
!   INITIALIZE ION CORRECTION QUALITY FLAG: NOGOOD=0...USE THIS POINT
!                                           NOGOOD=1...DONT USE
      CALL GETENVAR ( 'SOLVE_SE_QUAD', STR )
      IF ( ILEN(STR) > 0 ) THEN
           READ ( UNIT=STR, FMT='(F20.10)' ) SOLVE_SE_QUAD 
         ELSE
           SOLVE_SE_QUAD = 0.D0
      END IF
!
      NOGOOD = 0
      ISTA1 = ITT(ISITE(1))
      ISTA2 = ITT(ISITE(2))
!
! --- Apply the observation dependent contributions where requested.
!
      DO J = 1, MAX_CONT
         IF ( KBIT (OBCAPL, J) ) THEN
              CALIBB_DEL  = CALIBB(1,J)
              CALIBB_RATE = CALIBB(2,J)
              IF ( J.EQ. IREVCONT ) THEN
!
! ---------------- Kluge: reverse the sign of up to one observation dependent
! ---------------- contribution selected by hard coding in ncort.
! ---------------- Plus use a small fudge factor, the ratio of universal to
! ---------------- sidereal time.)
!
                   CALIBB_DEL  = -1.0027379 * CALIBB_DEL
                   CALIBB_RATE = -1.0027379 * CALIBB_RATE
              ENDIF
!
              DT = DT + CALIBB_DEL * 1.0D6
              RT = RT + CALIBB_RATE
         END IF
      END DO
!
! --- Apply the selected non-flyby calibrations:
! --- Loop over stations and across the bits in JCAPPL, and apply the
! --- calibrations where requested.  Signs have been selected in SDBH
! --- such that all calibrations must be ADDED here.
!
! --- First loop over the stations (1 and 2)
!
      DO I = 1, 2
         ISTAT = ITT(ISITE(I))
         IF ( ISTAT .LE. 0  .OR. ISTAT .GT. 16384 ) THEN
              WRITE ( 6, * ) ' ISITE = ',ISITE(I),' ITT(ISITE) = ',ITT(ISITE(I))
              CALL LIB$MOVC3 ( 8, ISITN(1,ISITE(1)), STR1 )
              CALL LIB$MOVC3 ( 8, ISITN(1,ISITE(2)), STR2 )
              WRITE ( 6, * ) ' ISITE(1) = ',STR1
              WRITE ( 6, * ) ' ISITE(2) = ',STR2
              WRITE ( 6, * ) ' $$$  Station correspondence table ITT is spoiled '// &
     &              'or has not been loaded $$$'
              WRITE ( 6, * ) ' $$$  Such a case may occur when PARFIL has not '// &
     &               'been read  $$$'
              WRITE ( 6, * ) ' $$$  or it corresponds to CGM-type PARFIL  $$$'
              STOP '(GET_CALIB) Abnormal termination'
         END IF
         DO J = 1, MAX_CAL  !across the calibrations
            IF ( KBIT (JCAPPL(ISTAT), J) ) THEN  ! Apply this one
                 DT = DT + CALIBS (I, 1, J) * 1.0D6   ! Delays
                 RT = RT + CALIBS (I, 2, J)           ! Rates
            END IF
         END DO
      END DO
!
! --- Apply the selected flyby calibrations:
!
! --- First, if this is a new data base,
! --- locate specific calibrations within the namfil info
! --- This was once done in ncort, but will now be done here, so
! --- that as new flyby calibs are added, users will not have to
! --- keep modifying GET_CALIB's argument list and callers, to keep
! --- passing new id_ pointers.
!
      IF ( .NOT. FL_CAL_SAVE   .OR.  IDB .NE. IDB_SAVE ) THEN
           ID_CFAJJDRY = 0
           ID_CFAKBDRY = 0
           ID_CFAKBWET = 0
           ID_LANYI    = 0
           DO_LANWET   = 0
           ID_MTTDRYSS = 0
           ID_MTTDYFLY = 0
           ID_IFADRYSS = 0
           ID_IFADYFLY = 0
           ID_NMFDYFLY = 0
           ID_NMFWTFLY = 0
!
           DO J = 1, NFCAL
              I = 1
              SET_THIS_ID = .TRUE.
              DO WHILE ( I .LE. NAMSTA  .AND. SET_THIS_ID )
                 IF ( KBIT (JCAFFL(1,I),J)) THEN
                    SET_THIS_ID = .FALSE.
                    IF ( FCAL_NAMES(J) .EQ. 'CFAJJDRY' ) THEN
                         IF ( ID_CFAJJDRY.NE.0 ) TWICE_IN_NAMF = .TRUE.
                         ID_CFAJJDRY = J
                      ELSE IF ( FCAL_NAMES(J) .EQ. 'CFAKBDRY' ) THEN
                         IF ( ID_CFAKBDRY.NE.0 ) TWICE_IN_NAMF = .TRUE.
                         ID_CFAKBDRY = J
                      ELSE IF ( FCAL_NAMES(J) .EQ. 'CFAKBWET' ) THEN
                         IF ( ID_CFAKBWET.NE.0 ) TWICE_IN_NAMF = .TRUE.
                         ID_CFAKBWET = J
                      ELSE IF ( FCAL_NAMES(J) .EQ. 'MTTDRYSS' ) THEN
                         IF ( ID_MTTDRYSS.NE.0 ) TWICE_IN_NAMF = .TRUE.
                         ID_MTTDRYSS = J
                      ELSE IF ( FCAL_NAMES(J) .EQ. 'MTTDRFLY' ) THEN
                         IF ( ID_MTTDYFLY.NE.0 ) TWICE_IN_NAMF = .TRUE.
                         ID_MTTDYFLY = J
                      ELSE IF ( FCAL_NAMES(J) .EQ. 'IFADRYSS' ) THEN
                         IF ( ID_IFADRYSS.NE.0 ) TWICE_IN_NAMF = .TRUE.
                         ID_IFADRYSS = J
                      ELSE IF ( FCAL_NAMES(J) .EQ. 'IFADRFLY' ) THEN
                         IF ( ID_IFADYFLY.NE.0 ) TWICE_IN_NAMF = .TRUE.
                         ID_IFADYFLY = J
                      ELSE IF ( FCAL_NAMES(J) .EQ. 'NMFDRFLY' ) THEN
                         IF ( ID_NMFDYFLY.NE.0 ) TWICE_IN_NAMF = .TRUE.
                         ID_NMFDYFLY = J
!
                      ELSE IF ( FCAL_NAMES(J) .NE. 'MTTWTFLY' ) THEN
                         WRITE ( 6, * ) ' I=',I,' J=',J
                         WRITE ( 6, * ) ' FCAL_NAMES(J) >>',FCAL_NAMES(J),'<<'
                         WRITE ( ERRSTR, '("Invalid flyby cal ",a8,'// &
     &                                   '" in GET_CALIB --  cannot apply")') &
     &                           FCAL_NAMES(J)
                         CALL FERR  ( INT2(201), ERRSTR, INT2(0), INT2(0) )
                         CALL FATAL ( ERRSTR )
                    END IF
                    IF ( TWICE_IN_NAMF ) THEN
                         WRITE ( ERRSTR, '("In apply_calib - a flyby cal ",'// &
     &                           '"appears in namfil twice for one db")' )
                         CALL FERR  ( INT2(1201), ERRSTR, INT2(0), INT2(0) )
                         CALL FATAL (       ERRSTR       )
                    END IF
                 END IF
                 I = I + 1
              END DO
           END DO
!
           IDB_SAVE = IDB
      END IF
      DO_PRCOR = 0
!
! --- Next generate the necessary values for this observation
!
      IF ( ID_CFAKBDRY.NE.0 .OR. ID_CFAJJDRY.NE.0 .OR. &
     &     ID_CFAKBWET.NE.0 .OR. ID_MTTDRYSS.NE.0 .OR. &
     &     ID_MTTDYFLY.NE.0 .OR. ID_NMFDYFLY.NE.0 .OR. &
     &     ID_IFADRYSS.NE.0 .OR. &
     &     ID_IFADYFLY.NE.0                             ) THEN
!
! -------- The subroutine which calculates the cfadry correction needs
! -------- to know whether it is calculating the kbdry or jjdry value
! -------- for the stations in this observation
!
            DO I = 1,2
               ISTAT = ITT(ISITE(I))
               ID_JJ(I) = 0
               MTT_SEAS_WET(I) = .FALSE.  ! default for mttwet is non-seasonal
               MTT_SEAS_DRY(I) = .FALSE.  ! default for mttdry is non-seasonal
               IFA_SEAS_WET(I) = .FALSE.  ! default for ifawet is non-seasonal
               IFA_SEAS_DRY(I) = .FALSE.  ! default for ifadry is non-seasonal

               IF ( ID_CFAJJDRY .NE.0 ) THEN
                 IF( KBIT(JCAFFL(1,ISTAT),ID_CFAJJDRY) ) ID_JJ(I) = 1
               ENDIF

               IF ( ID_MTTDRYSS .NE.0 ) THEN
                 IF(KBIT(JCAFFL(1,ISTAT),ID_MTTDRYSS) ) THEN
!
                    ID_JJ(I) = 2  ! use mttdry & seasonal mapping function.
                    MTT_SEAS_DRY(I) = .TRUE.
                 ENDIF
               END IF

              IF ( ID_MTTDYFLY .NE.0 ) THEN
                IF( KBIT(JCAFFL(1,ISTAT),ID_MTTDYFLY) ) THEN
!
                   ID_JJ(I) = 2  ! use mttdry & unseasonal mapping function.
                   MTT_SEAS_DRY(I) = .FALSE.
                END IF
              ENDIF
              IF ( ID_IFADRYSS .NE.0 ) THEN
                IF( KBIT(JCAFFL(1,ISTAT),ID_IFADRYSS) ) THEN
!
                  ID_JJ(I) = 3  ! use ifadry & seasonal mapping function.
                  IFA_SEAS_DRY(I) = .TRUE.
                END IF
              ENDIF

              IF ( ID_IFADYFLY .NE.0 ) THEN
                IF( KBIT(JCAFFL(1,ISTAT),ID_IFADYFLY) ) THEN
!
                  ID_JJ(I) = 3  ! use ifadry & unseasonal mapping function.
                  IFA_SEAS_DRY(I) = .FALSE.
                 END IF
              ENDIF

              IF ( ID_NMFDYFLY .NE.0 )THEN
                IF( KBIT(JCAFFL(1,ISTAT),ID_NMFDYFLY) ) THEN
!
                  ID_JJ(I) = 4  ! use nmfdry & unseasonal mapping function.
                  IFA_SEAS_DRY(I) = .FALSE.
                END IF
              ENDIF
!
              ID_WET(I)= -1 ! so that we don't use partials in cfacalc.
              IF ( STS_TRP == LOAD__TRP ) ID_JJ(I) = 4
              IF ( STS_TRP == LOAD__TRP ) CFAKBWET = 0.0D0
            END DO
!
            CALL CALCALC ( ISITE, ISITN, ISTAR, VSTARC, AZ, ELEV, PI_VAR, &
     &                     ATMPR, RELHU, TEMPC, VLIGHT, CFAKBDRY, CFAKBWET, &
     &                     ID_JJ, FLYBY_WARNING, LATS, HEIGHTS, AX_OFFS, &
     &                     AX_TYPES, BARO_CALS, BARO_HEIGHTS, APP, &
     &                     MTT_SEAS_DRY, MTT_SEAS_WET, IFA_SEAS_DRY, &
     &                     IFA_SEAS_WET, AXDIF )
      END IF
!
      IF ( STS_VTD == 0                                     .AND. & 
     &     STS_TRP == LOAD__TRP                             .AND. &
     &     ELEV(1) > EL_MIN                                 .AND. &
     &     ELEV(2) > EL_MIN                                 .AND. &
     &     ( TRP_USE == REQ__TRP  .OR.  TRP_USE == USE__TRP )     ) THEN
!
           CALL JD_TO_MJD_SEC  ( FJD, MJD_TAI_OBS, TDB_OBS )
           TDB_OBS = TDB_OBS + FRACTC*86400.0D0
           CALL TDB_TO_TAI ( MJD_TAI_OBS, TDB_OBS, TAI_OBS )
           IF ( TAI_OBS < 0.0D0 ) THEN
                TAI_OBS     = TAI_OBS + 86400.0D0
                MJD_TAI_OBS = MJD_TAI_OBS - 1
           END IF
           IF ( TAI_OBS > 86400.0D0 ) THEN
                TAI_OBS     = TAI_OBS - 86400.0D0
                MJD_TAI_OBS = MJD_TAI_OBS + 1
           END IF
           PRT_INIT(ISITE(1)) = .TRUE.
           PRT_INIT(ISITE(2)) = .TRUE.
!
           IND_TRP(1) = 0
           IND_TRP(2) = 0
           IND_SCA(1) = 0
           IND_SCA(2) = 0
           CALL LIB$MOVC3 ( 8, ISITN(1,ISITE(1)), STR1 )
           CALL LIB$MOVC3 ( 8, ISITN(1,ISITE(2)), STR2 )
!
! -------- Try to find the record with scan id. 
!
           DO 410 J1=1,TRP%N_SCA
              IF ( DABS( (TRP%SCA(J1)%MJD*86400.0D0 + TRP%SCA(J1)%TAI) - &
     &                   (MJD_TAI_OBS*86400.0D0 + TAI_OBS) ) < TIM_TLR) THEN
                   DO 420 J2=1,TRP%SCA(J1)%L_STA
                      IF ( TRP%STA(TRP%SCA(J1)%IND_STA(J2))%NAME == STR1 .AND. &
     &                     DABS( TRP%SCA(J1)%DAT(J2)%EL - ELEV(1)) < EL_TLR ) THEN
                           IND_SCA(1) = J1
                           IND_TRP(1) = J2
                      END IF
                      IF ( TRP%STA(TRP%SCA(J1)%IND_STA(J2))%NAME == STR2 .AND. &
     &                     DABS( TRP%SCA(J1)%DAT(J2)%EL - ELEV(2)) < EL_TLR ) THEN
                           IND_SCA(2) = J1
                           IND_TRP(2) = J2
                      END IF
 420               CONTINUE
              END IF
 410       CONTINUE 
!
           IF ( IND_TRP(1) == 0 ) THEN
                CALL CLRCH ( STR )
                STR = MJDSEC_TO_DATE ( MJD_TAI_OBS, TAI_OBS, -2 )
                WRITE ( 6, * ) 'Elev: ',SNGL(ELEV(1)/DEG__TO__RAD)
                CALL ERR_LOG ( 1711, -2, 'GET_CALIB', 'Cannot find '// &
     &              'a value of the external atmospheric path delay '// &
     &              'in file '//TRP%FILE_NAME(1:I_LEN(TRP%FILE_NAME))// &
     &              ' for observation at '//STR(1:23)//' TAI at station '// &
     &              STR1//' for experiment '//DBNAME_CH )
                CALL EXIT ( 1 )
           END IF
           IF ( IND_TRP(1) > TRP%SCA(IND_SCA(1))%L_STA ) THEN
                CALL CLRCH ( STR )
                STR = MJDSEC_TO_DATE ( MJD_TAI_OBS, TAI_OBS, -2 )
                WRITE ( 6, * ) ' IND_TRP(1) = ', IND_TRP(1), &
     &                         ' L_STA = ', TRP%SCA(IND_SCA(1))%L_STA 
                WRITE ( 6, * ) 'Elev: ',SNGL(ELEV(1)/DEG__TO__RAD)
                CALL ERR_LOG ( 1712, -2, 'GET_CALIB', 'Trap of internal '// &
     &              'control in processing observation at '//STR(1:23)// &
     &              ' TAI at station '//STR1//' for experiment '//DBNAME_CH// &
     &              ' in attempt to get the value from the '// &
     &              'external atmospheric path delay '// &
     &              TRP%FILE_NAME(1:I_LEN(TRP%FILE_NAME))// &
     &              ' the number of stations in this scan is less than '// &
     &              ' the requested station index' )
                CALL EXIT ( 1 )
           END IF
!
           IF ( IND_TRP(2) == 0 ) THEN
                CALL CLRCH ( STR )
                STR = MJDSEC_TO_DATE ( MJD_TAI_OBS, TAI_OBS, -2 )
                WRITE ( 6, * ) 'Elev: ',SNGL(ELEV(2)/DEG__TO__RAD)
                CALL ERR_LOG ( 1713, -2, 'GET_CALIB', 'Cannot find '// &
     &              'a value of the external atmospheric path delay '// &
     &              'in file '//TRP%FILE_NAME(1:I_LEN(TRP%FILE_NAME))// &
     &              ' for observation at '//STR(1:21)//' TAI at station '// &
     &              STR2//' for experiment '//DBNAME_CH )
                CALL EXIT ( 1 )
           END IF
!
           IF ( IND_TRP(2) > TRP%SCA(IND_SCA(2))%L_STA ) THEN
                CALL CLRCH ( STR )
                STR = MJDSEC_TO_DATE ( MJD_TAI_OBS, TAI_OBS, -2 )
                WRITE ( 6, * ) ' IND_TRP(2) = ', IND_TRP(2), &
     &                         ' L_STA = ', TRP%SCA(IND_SCA(2))%L_STA 
                WRITE ( 6, * ) 'Elev: ',SNGL(ELEV(2)/DEG__TO__RAD)
                CALL ERR_LOG ( 1714, -2, 'GET_CALIB', 'Trap of internal '// &
     &              'control in processing observation at '//STR(1:23)// &
     &              ' TAI at station '//STR2//' for experiment '//DBNAME_CH// &
     &              ' in attempt to get the value from the '// &
     &              'external atmospheric path delay '// &
     &              TRP%FILE_NAME(1:I_LEN(TRP%FILE_NAME))// &
     &              ' the number of stations in this scan is less than '// &
     &              ' the requested station index' )
                CALL EXIT ( 1 )
           END IF
!
           IF ( PRT_INIT(ISITE(1)) .AND. IND_SCA(1) > 0 ) THEN
                MAP_FUN(1) = -TRP%SCA(IND_SCA(1))%DAT(IND_TRP(1))%DER_ZEN
                DT = DT - TRP%SCA(IND_SCA(1))%DAT(IND_TRP(1))%DEL_TOT_SLANT*1.D6
           END IF
           IF ( PRT_INIT(ISITE(2)) .AND. IND_SCA(2) > 0 ) THEN
                MAP_FUN(2) =  TRP%SCA(IND_SCA(2))%DAT(IND_TRP(2))%DER_ZEN
                DT = DT + TRP%SCA(IND_SCA(2))%DAT(IND_TRP(2))%DEL_TOT_SLANT*1.D6
           END IF
      END IF
!
! --- Loop over stations and across the calibration bits in JCAFFL,
! --- and apply the calibrations where requested.  Signs have been selected
! --- in SDBH such that all calibrations must be ADDED here.
!
      ATM_ZENDEL_SAVE = ATM_ZENDEL
      DO I = 1, 2   ! Stations 1 and 2
         ISTAT = ITT(ISITE(I))
         ATM_ZENDEL(I) = 0.0D0
         DO J = 1, NFCAL    !loop over calibrations
            IF ( KBIT (JCAFFL(1,ISTAT), J) ) THEN
                 IF ( J .EQ. ID_CFAJJDRY ) THEN
                      ATM_ZENDEL(I) = ATM_ZENDEL(I) + CFAKBDRY(I,1)/APP(I,1)
                      DT = DT + CFAKBDRY(I,1) * 1.0D6
                      RT = RT + CFAKBDRY(I,2)
                   ELSE IF (J .EQ. ID_CFAKBDRY) THEN
                      ATM_ZENDEL(I) = ATM_ZENDEL(I) + CFAKBDRY(I,1)/APP(I,1)
                      DT = DT + CFAKBDRY(I,1) * 1.0D6
                      RT = RT + CFAKBDRY(I,2)
                   ELSE IF ( J .EQ. ID_MTTDRYSS .OR. J .EQ. ID_MTTDYFLY ) THEN
                      ATM_ZENDEL(I) = ATM_ZENDEL(I) + CFAKBDRY(I,1)/APP(I,1)
                      DT = DT + CFAKBDRY(I,1) * 1.0D6
                      RT = RT + CFAKBDRY(I,2)
                   ELSE IF (J .EQ. ID_IFADRYSS .OR. J .EQ. ID_IFADYFLY) THEN
                      ATM_ZENDEL(I) = ATM_ZENDEL(I) + CFAKBDRY(I,1)/APP(I,1)
                      DT = DT + CFAKBDRY(I,1) * 1.0D6
                      RT = RT + CFAKBDRY(I,2)
                   ELSE IF ( J .EQ. ID_NMFDYFLY ) THEN
                      ATM_ZENDEL(I) = ATM_ZENDEL(I) + CFAKBDRY(I,1)/APP(I,1)
                      DT = DT + CFAKBDRY(I,1) * 1.0D6
                      RT = RT + CFAKBDRY(I,2)
                   ELSE IF (J .EQ. ID_CFAKBWET) THEN
                      ATM_ZENDEL(I) = ATM_ZENDEL(I) + CFAKBWET(I,1)/APP(I,1)
                      DT = DT + CFAKBWET(I,1) * 1.0D6
                      RT = RT + CFAKBWET(I,2)
                   ELSE
                      WRITE ( 6, * ) ' I=',I,' J=',J,' NFCAL=',NFCAL
                      WRITE ( 6, * ) ' ID_NMFDYFLY = ', ID_NMFDYFLY ! %%%
                      CALL FERR ( INT2(202), &
     &                   ' - in GET_CALIB - fly cal app problem', INT2(0), &
     &                    INT2(0) )
                     CALL FATAL ( ' in GET_CALIB, fly cal app problem' )
                  END IF
            END IF ! This one applied
         END DO ! Calibrations loop
      END DO ! Stations loop
      IF ( ATM_ZENDEL(1) == 0.0D0  .AND.  ATM_ZENDEL(2) == 0.0D0 ) THEN
           ATM_ZENDEL = ATM_ZENDEL_SAVE
      END IF
!
! --- Add troposphere noise based on average atmosphere delay
! --- (roughly elevation dependent)
!
      IF ( KELDEP_NOISE ) THEN
!
! -------- TROP_FRACT converts the atmospheric turbulence and mapping function
! -------- parameter error constants to the formal error units.
! -------- The delay and rate constants are stored in the input elevation
! -------- dependent constants file as picosecs and femtosecs/sec respectively.
!
           TROP_FRACT(1) = 1.D-12  ! Atmospheric turbulence delay
           TROP_FRACT(2) = 1.D-15  ! Atmospheric turbulence rate
           TROP_FRACT(3) = 1.D-12  ! Map funct parm error delay
           TROP_FRACT(4) = 1.D-15  ! Map funct parm error rate
!
! -------- cal_fract converts the flyby atmosphere calibration to the formal
! -------- error units.
!
           CAL_FRACT(1) = 1.0D6 ! delay
           CAL_FRACT(2) = 1.0D0 ! rate
           DO ICTS = 1,2 ! Sites
!
! ----------- Terms 1-4 are:
! -----------       atmospheric turbulence for delays
! -----------       atmospheric turbulence for rates
! -----------       mapping function parameter error for delays
! -----------       mapping function parameter error for rates
!
              DO ICTT = 1,4
                  IF ( ICTT .LE. 2 .AND. &
     &                 AVG_ATM ( ICTT,ISITE(ICTS) ) .LT. 0.0D0 ) THEN
!
! -------------------- A negative atmospheric turbulence delay/rate constant
! -------------------- indicates that the user wants to apply a fraction
! -------------------- of the delay/rate part of the flyby atmosphere
! -------------------- calibration used in the solution.
! -------------------- (The assumption is that one and only one flyby
! -------------------- calibration is being applied.
! -------------------- If this is false, then if none is applied,
! -------------------- a calibration of 0 is added, and if multiple ones
! -------------------- are applied, the last calibration in the namfil
! -------------------- list is used.)
! -------------------- The fraction to be applied is
! -------------------- the atmospheric turbulence delay/rate "constant" itself.
!
                       ISTAT   = ITT ( ISITE(ICTS) )
                       CAL_ADD = 0.0D0
                       DO J = 1, NFCAL  ! Loop over calibrations
                          IF ( KBIT (JCAFFL(1,ISTAT), J) ) THEN
                               IF ( J .EQ. ID_CFAJJDRY ) THEN
                                    CAL_ADD = CFAKBDRY(ICTS,ICTT)
                                 ELSE IF ( J .EQ. ID_CFAKBDRY ) THEN
                                    CAL_ADD =  CFAKBDRY(ICTS,ICTT)
                                 ELSE IF ( J .EQ. ID_MTTDRYSS .OR. &
     &                                     J .EQ. ID_MTTDYFLY      ) THEN
                                    CAL_ADD =  CFAKBDRY(ICTS,ICTT)
                                 ELSE IF ( J .EQ. ID_IFADRYSS .OR. &
     &                                     J .EQ. ID_IFADYFLY      ) THEN
                                    CAL_ADD =  CFAKBDRY(ICTS,ICTT)
                                 ELSE IF ( J .EQ. ID_NMFDYFLY ) THEN
                                    CAL_ADD =  CFAKBDRY(ICTS,ICTT)
                                 ELSE IF ( J .EQ. ID_CFAKBWET ) THEN
                                    CAL_ADD =  CFAKBWET(ICTS,ICTT)
                               END IF
                          END IF ! This one applied
                       END DO ! Calibrations loop
!
                       ELN(ICTT,ICTS) = CAL_FRACT(ICTT)* &
     &                                  AVG_ATM(ICTT,ISITE(ICTS))*CAL_ADD
                    ELSE
                       ELN(ICTT,ICTS) = TROP_FRACT(ICTT)* &
     &                                  AVG_ATM(ICTT, &
     &                                  ISITE(ICTS))/((DSIN(ELEV(ICTS)))**ICTT)
                  ENDIF
               ENDDO
           ENDDO
!
           DERR = DSQRT ( DERR**2 + ELN(1,1)**2 + ELN(1,2)**2 + &
     &                              ELN(3,1)**2 + ELN(3,2)**2 )
           RERR = DSQRT ( RERR**2 + ELN(2,1)**2 + ELN(2,2)**2 + &
     &                              ELN(4,1)**2 + ELN(4,2)**2 )
      ENDIF
!
! --- Add ionosphere calibration and modify errors
!
! --- Apply GION
!
      IF ( EFFREQ_XS .GT. MIN__FRQ  ) THEN
           GROUP_F_RATIO_SQ = (EFFREQ_XS*EFFREQ_XS)/(EFFREQ*EFFREQ)
           IF ( EFFREQ .LT. XS__FRQ ) GROUP_F_RATIO_SQ = 1.D0/GROUP_F_RATIO_SQ
        ELSE
           GROUP_F_RATIO_SQ = 0.079D0
      ENDIF
!
      IF ( PHEFFREQ_XS .GT. MIN__FRQ  ) THEN
           PHASE_F_RATIO_SQ=(PHEFFREQ_XS*PHEFFREQ_XS)/(PHEFFREQ*PHEFFREQ)
           IF ( EFFREQ .LT. XS__FRQ ) PHASE_F_RATIO_SQ = 1.D0/PHASE_F_RATIO_SQ
        ELSE
           PHASE_F_RATIO_SQ = 0.079D0
      ENDIF
      TEMP_GROUP =  1.D0 + 2.D0*GROUP_F_RATIO_SQ
      TEMP_PHASE = -1.D0 - 2.D0*GROUP_F_RATIO_SQ
!
! --- X and S band have different signs for gion.
!
      IF ( ( EFFREQ .LT. XS__FRQ   .AND.  EFFREQ_XS .LT. MIN__FRQ ) .OR. &
     &     ( EFFREQ .GT. MIN__FRQ  .AND.  EFFREQ_XS .GT. MIN__FRQ   .AND. &
     &       EFFREQ .LT. EFFREQ_XS                                      ) ) THEN
!
! -------- This is a lower (probably S) band
!
           TEMP_GROUP = -TEMP_GROUP
           TEMP_PHASE = -TEMP_PHASE
      ENDIF
      IF ( .NOT.KIONO ) THEN
           TEMP_GROUP = 1.D0
      ENDIF
!
      IONO_APPLIED = .FALSE.
      IF (       KBIT ( JSITI(ISTA1), INT2(4) ) .AND. &
     &     .NOT. KBIT ( JSITI(ISTA1), INT2(5) ) .AND. &
     &           KBIT ( JSITI(ISTA2), INT2(4) ) .AND. &
     &     .NOT. KBIT ( JSITI(ISTA2), INT2(5) )       ) THEN
!
! -------- Make certain that the iono correction is good and that the
! -------- matching S-band has non-zero fringe detection.  (Compensates
! -------- for an old CNPLT bug (now fixed.))
!
           IF ( KBIT( ICORR, INT2(5) ) .OR. &
     &          KBIT( ICORR, INT2(6) )      ) THEN ! No good - downweighted
!
                NOGOOD = 1
              ELSE ! Good
                NOGOOD = 0
           END IF
!
! -------- Group delay data
!
! -------- Handle case where GIONSG is zero, by setting it to a high value
! -------- MWH - 5/6/93
!
           IF ( FUSED_STATUS == IONOV__UNDF ) THEN
                IF ( GIONSG(1) .EQ. 0  .AND. KIONO ) NOGOOD=1
                IF ( GIONSG(2) .EQ. 0  .AND. KIONO ) NOGOOD=1
           END IF
!
           IF ( DATYP_INQ ( IDATYP, GRPRAT__DTP )  .OR. &
     &          DATYP_INQ ( IDATYP, GRPONL__DTP )  .OR. &
     &          DATYP_INQ ( IDATYP, SNBRAT__DTP )  .OR. &
     &          DATYP_INQ ( IDATYP, SNBONL__DTP )        ) THEN
!
! ------------- Some cases of group delay or narrow-band delay
!
                DT   = DT + GION(1)
                RT   = RT + GION(2)
                IF ( DERR .LT. TAU_ERR__BAD ) THEN
                     DERR = DSQRT ( DABS( TEMP_GROUP*DERR**2 + GIONSG(1)**2 ) )
                     RERR = DSQRT ( DABS( TEMP_GROUP*RERR**2 + GIONSG(2)**2 ) )
                END IF
                IONO_APPLIED = .TRUE.
              ELSE IF ( DATYP_INQ ( IDATYP, PHSRAT__DTP )  .OR. &
     &                  DATYP_INQ ( IDATYP, PHSONL__DTP )        ) THEN
!
! ------------- Some cases of phase delay data
!
                DT    = DT - GION(1)*(EFFREQ**2)/(PHEFFREQ**2)
                RT    = RT + GION(2)*(EFFREQ**2)/(REFFREQ**2)
                IF ( DPHER .LT. TAU_ERR__BAD ) THEN
                     DPHER = DSQRT ( DPHER**2 + GIONSG(1)**2 )
                     RERR  = DSQRT ( DABS( TEMP_GROUP*RERR**2 + GIONSG(2)**2 ) )
                END IF
                IONO_APPLIED = .TRUE.
              ELSE IF ( DATYP_INQ ( IDATYP, RATONL__DTP ) ) THEN
!
! ------------ Rates only
!
                RT   = RT + GION(2)
                RERR = DSQRT ( DABS( TEMP_GROUP*RERR**2 + GIONSG(2)**2 ) )
                IONO_APPLIED = .TRUE.
           END IF
      END IF
!
! --- Want to apply PHION
!
      IF ( .NOT. KBIT( JSITI(ISTA1), INT2(4) ) .AND. &
     &           KBIT( JSITI(ISTA1), INT2(5) ) .AND. &
     &     .NOT. KBIT( JSITI(ISTA2), INT2(4) ) .AND. &
     &           KBIT( JSITI(ISTA2), INT2(5) )       ) THEN
!
           IF ( .NOT. KBIT( ICORR, INT2(11)) ) THEN ! PHION is not downweighted
                NOGOOD = 0
              ELSE  ! PHION is no good
                NOGOOD = 1
           END IF
!
           IF ( DATYP_INQ ( IDATYP, GRPRAT__DTP )  .OR. &
     &          DATYP_INQ ( IDATYP, GRPONL__DTP )  .OR. &
     &          DATYP_INQ ( IDATYP, SNBRAT__DTP )  .OR. &
     &          DATYP_INQ ( IDATYP, SNBONL__DTP )        ) THEN
!
! ------------- Apply to group delay data
!
                DT    = DT    + PHION*(PHEFFREQ**2)/(EFFREQ**2)
                RT    = RT    + GION(2)
                DERR  = DSQRT ( DERR**2 + PHIONS**2 )
                RERR  = DSQRT ( DABS( TEMP_GROUP*RERR**2 + GIONSG(2)**2 ) )
                IONO_APPLIED = .TRUE.
              ELSE IF ( DATYP_INQ ( IDATYP, PHSRAT__DTP )  .OR. &
     &                  DATYP_INQ ( IDATYP, PHSONL__DTP )        ) THEN
!
! ------------ Apply to phase delay data
!
                DT    = DT    - PHION
                RT    = RT    + GION(2)*(EFFREQ**2)/(REFFREQ**2)
                DPHER = DSQRT ( DPHER**2 + PHIONS**2 )
                RERR  = DSQRT ( DABS( TEMP_GROUP*RERR**2 + GIONSG(2)**2 ) )
                IONO_APPLIED = .TRUE.
              ELSE IF ( DATYP_INQ ( IDATYP, RATONL__DTP ) ) THEN
!
! ------------- Rates only
!
                RT    = RT + GION(2)
                RERR  = DSQRT ( DABS( TEMP_GROUP*RERR**2 + GIONSG(2)**2 ) )
                IONO_APPLIED = .TRUE.
           END IF
      END IF
!
      IF ( MCAPL .NE. 0 ) THEN
           FREQ_GR_X    = EFFREQ*1.D6
           FREQ_GR_S    = EFFREQ_XS*1.D6
           FREQ_PH_X    = PHEFFREQ*1.D6
           FREQ_PH_S    = PHEFFREQ_XS*1.D6
           FREQ_RATE_X  = REFFREQ*1.D6
           FREQ_RATE_S  = REFFREQ_XS*1.D6
!
! -------- We need apply some mode calibrations
!
           DO I=1,M_CLM
              IF ( KBIT ( MCAPL, I ) ) THEN
!
! ---------------- Apply mode calibration to delay in according with solution type
!
                   IF ( DATYP_INQ ( IDATYP, GRPRAT__DTP ) .OR. &
     &                  DATYP_INQ ( IDATYP, SNBRAT__DTP ) .OR. &
     &                  DATYP_INQ ( IDATYP, GRPONL__DTP ) .OR. &
     &                  DATYP_INQ ( IDATYP, SNBONL__DTP )      ) THEN
                        IF ( IONO_APPLIED ) THEN
                             DT = DT + ( CALIBM(MCL__GRX,I)*FREQ_GR_X**2 - &
     &                                   CALIBM(MCL__GRS,I)*FREQ_GR_S**2   )/ &
     &                                 ( FREQ_GR_X**2 - FREQ_GR_S**2 )*1.D6
                          ELSE
                             DT = DT + CALIBM(MCL__GRX,I) * 1.D6
                        END IF
                      ELSE IF ( DATYP_INQ ( IDATYP, PHSRAT__DTP ) .OR. &
     &                          DATYP_INQ ( IDATYP, PHSONL__DTP )      ) THEN
                        IF ( IONO_APPLIED ) THEN
                             DT = DT + ( CALIBM(MCL__PHX,I)*FREQ_PH_X**2 - &
     &                                   CALIBM(MCL__PHS,I)*FREQ_PH_S**2   )/ &
     &                                 ( FREQ_PH_X**2 - FREQ_PH_S**2 )*1.D6
                           ELSE
                             DT = DT + CALIBM(MCL__PHX,I) * 1.D6
                        END IF
                      ELSE IF ( DATYP_INQ ( IDATYP, G_GXS__DTP ) ) THEN
                        DT = DT + ( CALIBM(MCL__GRX,I)*FREQ_GR_X**2 - &
     &                              CALIBM(MCL__GRS,I)*FREQ_GR_S**2   )/ &
     &                            ( FREQ_GR_X**2 - FREQ_GR_S**2 ) * 1.D6
                      ELSE IF ( DATYP_INQ ( IDATYP, PX_GXS__DTP ) ) THEN
                         DT = DT + ( CALIBM(MCL__PHX,I) - &
     &                             ( CALIBM(MCL__GRX,I)*FREQ_GR_X**2 - &
     &                               CALIBM(MCL__GRS,I)*FREQ_GR_S**2   )/ &
     &                             ( FREQ_GR_X**2 - FREQ_GR_S**2 )     )*1.D6
                      ELSE IF ( DATYP_INQ ( IDATYP, PS_GXS__DTP ) ) THEN
                         DT = DT + ( CALIBM(MCL__PHS,I) - &
     &                             ( CALIBM(MCL__GRX,I)*FREQ_GR_X**2 - &
     &                               CALIBM(MCL__GRS,I)*FREQ_GR_S**2   )/ &
     &                             ( FREQ_GR_X**2 - FREQ_GR_S**2 )     )*1.D6
                      ELSE IF ( DATYP_INQ ( IDATYP, PX_GX__DTP ) ) THEN
                         DT = DT + ( CALIBM(MCL__PHX,I)*FREQ_PH_X**2 + &
     &                               CALIBM(MCL__GRX,I)*FREQ_GR_X**2   )/ &
     &                             ( FREQ_PH_X**2 + FREQ_GR_X**2       )*1.D6
                      ELSE IF ( DATYP_INQ ( IDATYP, PX_GS__DTP ) ) THEN
                         DT = DT + ( CALIBM(MCL__PHX,I)*FREQ_PH_X**2 + &
     &                            CALIBM(MCL__GRS,I)*FREQ_GR_S**2)   &
     &                          /( FREQ_PH_X**2 + FREQ_GR_S**2          ) * 1.D6
                      ELSE IF ( DATYP_INQ ( IDATYP, PS_GX__DTP ) ) THEN
                         DT = DT + ( CALIBM(MCL__PHS,I)*FREQ_PH_S**2 + &
     &                               CALIBM(MCL__GRX,I)*FREQ_GR_X**2   )/ &
     &                             ( FREQ_PH_S**2 + FREQ_GR_X**2       )*1.D6
                      ELSE IF ( DATYP_INQ ( IDATYP, PS_GS__DTP ) ) THEN
                         DT = DT + ( CALIBM(MCL__PHS,I)*FREQ_PH_S**2 + &
     &                               CALIBM(MCL__GRS,I)*FREQ_GR_S**2   )/&
     &                             ( FREQ_PH_S**2 + FREQ_GR_S**2       ) * 1.D6
                      ELSE IF ( DATYP_INQ ( IDATYP, P_PXS__DTP ) ) THEN
                         DT = DT + ( CALIBM(MCL__PHX,I)*FREQ_PH_X**2 - &
     &                               CALIBM(MCL__PHS,I)*FREQ_PH_S**2   )/ &
     &                             ( FREQ_PH_X**2 - FREQ_PH_S**2       )*1.D6
                      ELSE IF ( DATYP_INQ ( IDATYP, GX__DTP ) ) THEN
                         DT = DT + CALIBM(MCL__GRX,I) * 1.D6
                      ELSE IF ( DATYP_INQ ( IDATYP, GS__DTP ) ) THEN
                         DT = DT + CALIBM(MCL__GRS,I) * 1.D6
                      ELSE IF ( DATYP_INQ ( IDATYP, PX__DTP ) ) THEN
                         DT = DT + CALIBM(MCL__PHX,I) * 1.D6
                      ELSE IF ( DATYP_INQ ( IDATYP, PS__DTP ) ) THEN
                         DT = DT + CALIBM(MCL__PHS,I) * 1.D6
                   END IF
!
! ---------------- Apply mode calibration to delay rate in according with
! ---------------- solution type
!
                   IF ( DATYP_INQ ( IDATYP, RATE__DTP   ) .OR. &
     &                  DATYP_INQ ( IDATYP, PHSRAT__DTP ) .OR. &
     &                  DATYP_INQ ( IDATYP, SNBRAT__DTP ) .OR. &
     &                  DATYP_INQ ( IDATYP, RATONL__DTP )      ) THEN
                        IF ( IONO_APPLIED ) THEN
                             RT = RT + ( CALIBM(MCL__RTX,I)*FREQ_GR_X**2 - &
     &                                   CALIBM(MCL__RTS,I)*FREQ_GR_S**2   )/ &
     &                                 ( FREQ_RATE_X**2 - FREQ_RATE_S**2 )
                           ELSE
                             RT = RT + CALIBM(MCL__RTX,I)
                        END IF
                   END IF
              END IF
           END DO
      END IF
!
! --- Get the raw observation weights with ionosphere sigmas.
!
      DERR_RAW  = DERR
      RERR_RAW  = RERR
      DPHER_RAW = DPHER
!
! --- Add formal errors
!
! *** switched order 3/6/95  mwh
!
      IF ( ISTA1 .GT. ISTA2 ) THEN     ! swap stations
           I = ISTA1
           ISTA1 = ISTA2
           ISTA2 = I
      END IF
!
! --- Calculate index in packed table which points to error for this
! --- baseline. N is (max # stations per database - 2) = 32 - 2 = 30
!
      N = MAX_ARC_STA - 2
      J = ((ISTA1-1) * N) - (((ISTA1-1) * (ISTA1-2))/2) + ISTA2-1
      JJ = ITTB(J)
!
      IF ( IS_R8_NAN(ET(1,JJ)) ) ET(1,JJ) = 1.D-12
      IF ( IS_R8_NAN(ET(2,JJ)) ) ET(2,JJ) = 1.D-15
      IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
           DPHER = SQRT ( DPHER**2 + ET(1,JJ)**2 )
        ELSE
           DERR  = SQRT ( DERR**2  + ET(1,JJ)**2 )
      END IF
!
      RERR = SQRT ( RERR**2 + ET(2, JJ)**2 )
!
! --- Get source-specific weights and RSS them with current error
!
      IF ( SOURCE_WEIGHT_FILE .NE. ' ' ) THEN
         IF ( GET_SOURCE_WEIGHT ( ISTRN_CHR, SOURCE_WEIGHT_FILE, &
     &        CONSTANTS ) ) THEN
!
              CONSTANTS(1) = CONSTANTS(1)*1.D-12
              CONSTANTS(2) = CONSTANTS(2)*1.D-15
              CONSTANTS(3) = CONSTANTS(3)*1.D-12
              CONSTANTS(4) = CONSTANTS(4)*1.D-15
!
              IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
                   DPHER = DSQRT ( DPHER**2 + CONSTANTS(3)*CONSTANTS(3) )
                   RERR  = DSQRT ( RERR**2  + CONSTANTS(4)*CONSTANTS(4) )
                ELSE
                   DERR = DSQRT  ( DERR**2  + CONSTANTS(1)*CONSTANTS(1) )
                   RERR = DSQRT  ( RERR**2  + CONSTANTS(2)*CONSTANTS(2) )
              ENDIF
           ELSE
              IF ( SOURCE_WEIGHTS .EQ. 're' ) THEN
                   CALL FERR ( INT2(123), 'Source-dependent weight missing', &
     &                         INT2(0), INT2(0) )
              ENDIF
         ENDIF
      ENDIF
!
      IF ( RWT_EL_USE == SOLVE__YES ) THEN
           IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
                DPHER = DSQRT ( DPHER**2 + (SE(ISTA1)*MAP_FUN(1))**2 + &
     &                                     (SE(ISTA2)*MAP_FUN(2))**2   )
              ELSE
                DERR = DSQRT  ( DERR**2  + (SE(ISTA1)*MAP_FUN(1))**2 + &
     &                                     (SE(ISTA2)*MAP_FUN(2))**2   )
           ENDIF
         ELSE IF ( RWT_EL_USE == SOLVE__RW_EL_MULT_GLOB  ) THEN
           IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
                DPHER = DSQRT ( DPHER**2 + SE(1)**2* &
     &                                  ( (TROP_WZD(1)*MAP_FUN(1))**2 + &
     &                                    (TROP_WZD(2)*MAP_FUN(2))**2   ) )
              ELSE
                IF ( SOLVE_SE_QUAD .EQ. 0.0D0 ) THEN
                     DERR = DSQRT  ( DERR**2  + SE(2)**2* &
     &                                     ( (TROP_WZD(1)*MAP_FUN(1)) + &
     &                                       (TROP_WZD(2)*MAP_FUN(2)) )**2 )
                   ELSE
                     DERR = DSQRT  ( DERR**2  + SOLVE_SE_QUAD**2* &
     &                                         ( MAP_FUN(1)**8 + &
     &                                           MAP_FUN(2)**8   ) )
                END IF
           ENDIF
      END IF
!
      IF ( RWT_SRC_USE == SOLVE__YES ) THEN
           IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
                DPHER = DSQRT ( DPHER**2 + SS(ISTAR)**2 )
              ELSE
                DERR  = DSQRT ( DERR**2  + SS(ISTAR)**2 )
           ENDIF
      END IF
!
! --- Finished
!
      RETURN
      END  !#!  GET_CALIB  #!#
