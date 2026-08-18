       SUBROUTINE SOCAL (JCAPPL, JSITI, ITT, NOGOOD, ISITE, DT, RT, &
     &                  CALIBS, ICORR, GION, &
     &                  GIONSG, PHION, PHIONS, DERR, RERR, DPHER, ITTB, &
     &                  ET, CALIBB, OBCAPL, &
     &                  ISITN,ISTAR,VSTARC,AZ,ELEV,ATMPR,RELHU,TEMPC, &
     &                  DERR_RAW,RERR_RAW,DPHER_RAW,LATS, &
     &                  HEIGHTS,AX_OFFS,AX_TYPES,BARO_CALS,BARO_HEIGHTS, &
     &                  apx, JCAFFL, NFCAL, FCAL_NAMES, NAMSTA,IDB, &
     &                  effreq,pheffreq,reffreq,effreq_xs,pheffreq_xs, &
     &                  axdif,istrn_chr,source_weight_file, &
     &                  source_weights,avg_atm,keldep_noise)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!
! 1.  SOCAL PROGRAM SPECIFICATION
!
! 1.1 Apply the weather, cable and ionosphere calibrations to the
!     theoretical delays and rates.
!
! 1.2 REFERENCES:
!
! 2.  SOCAL INTERFACE
!     Updated to specificaly type integers which
!-------------------------------------------------
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      real*8 effreq,pheffreq,reffreq,effreq_xs,pheffreq_xs,axdif(*)
      real*8 avg_atm(4,*)
      logical*2 keldep_noise
      character*(*) istrn_chr,source_weight_file,source_weights
!
! 2.3 OUTPUT Variables:
!
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'glbc3.i'
      INCLUDE 'glbcm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: calcalc
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 ITTB(MAX_ARC_BSL),JCAPPL(MAX_ARC_STA),JSITI(MAX_ARC_STA)
      INTEGER*2 JCAFFL(7,MAX_ARC_STA), NFCAL,IDB
      character*8 FCAL_NAMES(*)
      INTEGER*2 ITT(MAX_ARC_STA), ISITE(2)
      REAL*8    CALIBS(2,2,15), GION(2), GIONSG(2), PHIONS
      REAL*8    CALIBB(2,15),ET(2,MAX_ARC_BSL)
      REAL*8    DT, RT, PHION,derr,rerr,constants(4)
      real*8    group_f_ratio_sq,phase_f_ratio_sq,trop_fract(4)
      INTEGER*2 OBCAPL
      LOGICAL*2 KBIT
      LOGICAL*2   MTT_SEAS_DRY(2), MTT_SEAS_WET(2)
      LOGICAL*2   IFA_SEAS_DRY(2), IFA_SEAS_WET(2)
      INTEGER*2 ISTAR,AX_TYPES(MAX_ARC_STA)
      INTEGER*2 ID_CFAKBDRY, ID_CFAJJDRY, ID_CFAKBWET, ID_LANYI
      INTEGER*2 ID_MTTDRYSS,ID_MTTDYFLY, ID_MTTWETSS, ID_MTTWTFLY
      INTEGER*2 ID_IFADRYSS,ID_IFADYFLY, ID_IFAWETSS, ID_IFAWTFLY
      INTEGER*2 ID_DRY_CHAO,id_nmfdyfly,id_nmfwtfly
      INTEGER*2 DO_LANWET, DO_PRCOR
      INTEGER*2 ISITN(4,MAX_STA)
      REAL*8 AZ(2),ELEV(2),ATMPR(2),RELHU(2),TEMPC(2),DERR_RAW,RERR_RAW
      REAL*8 DPHER_RAW,dpher,eln(4,2)
      REAL*8 VSTARC(2,MAX_SRC),AX_OFFS(MAX_ARC_STA)
      REAL*8 CFAKBDRY(2,2),CFAKBWET(2,2)
      REAL*8 LATS(MAX_ARC_STA),HEIGHTS(MAX_ARC_STA)
      REAL*8 BARO_CALS(MAX_ARC_STA), BARO_HEIGHTS(MAX_ARC_STA)
      real*8 dtlanyi(2),rtlanyi(2)
      real*8 app(2,2),apx(2,2)
      integer*2 NAMSTA,idb_save,i,icorr,ista1,ista2,istat,j,jj,n,nogood
      integer*2 id_jj(2), id_wet(3)
      logical*2 set_this_id,twice_in_namf
      character*100 errstr
      real*8 temp_group,temp_phase,group_f_sq,phase_f_sq
      LOGICAL*2 get_source_weight
      integer*2 icts,ictt
      real*8 cal_add,cal_fract(2)
      real*8 calibb_del, calibb_rate
      LOGICAL*4  DATYP_INQ
!
! JCAPPL - Bit flag of station cable & atmosphere calibrations to be applied
! JCAFFL - Bit array of flyby calibrations to be applied
! OBCAPL - Bit flag of contributions to be applied
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
     &     DO_LANWET,IDB_SAVE, ID_MTTDRYSS, ID_MTTDYFLY, ID_MTTWETSS, &
     &     ID_MTTWTFLY, MTT_SEAS_DRY, MTT_SEAS_WET, &
     &     ID_IFADRYSS, ID_IFADYFLY, ID_IFAWETSS, &
     &     ID_IFAWTFLY, IFA_SEAS_DRY, IFA_SEAS_WET,ID_DRY_CHAO, &
     &     id_nmfdyfly, id_nmfwtfly
!
      DATA IDB_SAVE /0/, TWICE_IN_NAMF /.FALSE./
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
!      1986 modification applies to station dependent calibrations/
!      contributions.  Now SOCAL is being modified to handle
!      observation dependent contributions.  These contributions are
!      in array CALIBB, and are applied or not applied according to
!      bit flag OBCAPL, where if bit J in OBCAPL is set, the
!      contribution stored in CALIBB(*,J) is applied to the delay and
!      rate values for the data base.  The actual source (LCODE) of
!      each element in CALIBB is given in the CORFIL.
!   JRR 861216: Fixed a mistake of my own making dealing with the
!             application of ionospheric corrections to phase data.
!
!   KDB 870817: Code to handle special calibrations (CFAWET and CFADRY,
!             which can be applied, even if the data base does not
!             contain their lcodes and they're therefore technically
!             unavailable.
!   JWR 870606  Modified to return the raw (not reweighted) delay and rate
!             errors.
!   KDB 910715 Use the latest namfil/corfil scheme, where flyby and regular
!              calibrations separate in namfil, namfil carries calib/contrib
!              names and only SDBH accesses corfil.
!   AEE 911120 Added MTT and IFADIS dry and wet mapping functions.
!   AEE 920212 Added CHAO DRY stuff.
!   AEE 920627 Removed partial stuff and placed them in ATMPART.F
!
!   JMG 930428 Corrected for Bad ionosphere calculation. Previous version
!              did not take into correlation between iono sigma and gruop/
!              phase sigma.  This error was re-discovered in April, 93.
!              Approximate fix uses constant ratio between X and S band freq.
!   kdb 950831 Add atmospheric turbulence rate constant and mapping function
!              parameter error delay and rate constants.
!   kdb 950905 Add alternate atmospheric turbulence constants scheme:
!              a negative delay or rate constant indicates the application of
!              a specified fraction of the delay or rate part of the
!              flyby atmosphere calibration used.
!   kdb 970401 Reverse the sign of the EQE CONT contribution (correction to the
!              equation of the equinox to fix a CALC error.) (But only in
!              calc versions < 8.3, the version in which this will be fixed.)
!   kdb 970418 Add a small fudge factor to the reversal of the sign of the
!              EQE CONT contribution to account for the ratio of universal
!              to sidereal time.
!   pet 980203 Substituted hard-coded test of solution type by DATYP_INQ
!   jmg 990210 Fix no ionosphere case.
!
!
! 5.  SOCAL PROGRAM STRUCTURE
!
!   INITIALIZE ION CORRECTION QUALITY FLAG: NOGOOD=0...USE THIS POINT
!                                           NOGOOD=1...DONT USE
      NOGOOD = 0
      ISTA1 = ITT(ISITE(1))
      ISTA2 = ITT(ISITE(2))
!
!   Apply the observation dependent contributions where requested.
!
      DO J = 1, 15
        IF (KBIT (OBCAPL, J)) THEN
          CALIBB_DEL = CALIBB(1,J)
          CALIBB_RATE = CALIBB(2,J)
          IF (J.EQ.IREVCONT) THEN
!           Kluge: reverse the sign of up to one observation dependent
!                  contribution selected by hard coding in ncort.
!                  Plus use a small fudge factor, the ratio of universal to
!                  sidereal time.)
            CALIBB_DEL  = -1.0027379 * CALIBB_DEL
            CALIBB_RATE = -1.0027379 * CALIBB_RATE 
          ENDIF
          DT = DT + CALIBB_DEL * 1.0E6
          RT = RT + CALIBB_RATE
        END IF
      END DO
!
!   Apply the selected non-flyby calibrations:
!   Loop over stations and across the bits in JCAPPL, and apply the
!   calibrations where requested.  Signs have been selected in SDBH
!   such that all calibrations must be ADDED here.
!
!     First loop over the stations (1 and 2)
!
      DO I = 1, 2
        ISTAT = ITT(ISITE(I))
        IF ( ISTAT .LE. 0  .OR. ISTAT .GT. 16384 ) THEN
             WRITE ( 6, * ) ' ISITE = ',ISITE(I),' ITT(ISITE) = ',ITT(ISITE(I))
             WRITE ( 6, * ) ' $$$  Station correspondence table ITT is spoiled '// &
     &              'or has not been loaded $$$'
             WRITE ( 6, * ) ' $$$  Such a case may occur when PARFIL has not '// &
     &              'been read  $$$'
             STOP '(SOCAL) Abnormal termination'
        END IF
        DO J = 1, 15  !across the calibrations
          IF (KBIT (JCAPPL(ISTAT), J)) THEN  !apply this one
            DT = DT + CALIBS (I, 1, J) * 1.0E6   !delays
            RT = RT + CALIBS (I, 2, J)           !rates
          END IF
        END DO
      END DO
!
!     Apply the selected flyby calibrations:
!
!     First, if this is a new data base,
!     locate specific calibrations within the namfil info
!     This was once done in ncort, but will now be done here, so
!     that as new flyby calibs are added, users will not have to
!     keep modifying socal's argument list and callers, to keep
!     passing new id_ pointers.
!
      IF (IDB .NE. IDB_SAVE) THEN
        ID_CFAJJDRY = 0
        ID_CFAKBDRY = 0
        ID_CFAKBWET = 0
        ID_LANYI = 0
        DO_LANWET = 0
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
          DO WHILE (I .LE. NAMSTA  .AND. SET_THIS_ID)
            IF (KBIT (JCAFFL(1,I),J)) THEN
              SET_THIS_ID = .FALSE.
              IF (FCAL_NAMES(J) .EQ. 'CFAJJDRY') THEN
                IF (ID_CFAJJDRY.NE.0) TWICE_IN_NAMF = .TRUE.
                ID_CFAJJDRY = J
              ELSE IF (FCAL_NAMES(J) .EQ. 'CFAKBDRY') THEN
                IF (ID_CFAKBDRY.NE.0) TWICE_IN_NAMF = .TRUE.
                ID_CFAKBDRY = J
              ELSE IF (FCAL_NAMES(J) .EQ. 'CFAKBWET') THEN
                IF (ID_CFAKBWET.NE.0) TWICE_IN_NAMF = .TRUE.
                ID_CFAKBWET = J
              ELSE IF (FCAL_NAMES(J) .EQ. 'MTTDRYSS') THEN
                IF (ID_MTTDRYSS.NE.0) TWICE_IN_NAMF = .TRUE.
                ID_MTTDRYSS = J
              ELSE IF (FCAL_NAMES(J) .EQ. 'MTTDRFLY') THEN
                IF (ID_MTTDYFLY.NE.0) TWICE_IN_NAMF = .TRUE.
                ID_MTTDYFLY = J
              ELSE IF (FCAL_NAMES(J) .EQ. 'IFADRYSS') THEN
                IF (ID_IFADRYSS.NE.0) TWICE_IN_NAMF = .TRUE.
                ID_IFADRYSS = J
              ELSE IF (FCAL_NAMES(J) .EQ. 'IFADRFLY') THEN
                IF (ID_IFADYFLY.NE.0) TWICE_IN_NAMF = .TRUE.
                ID_IFADYFLY = J
              ELSE IF (FCAL_NAMES(J) .EQ. 'NMFDRFLY') THEN
                IF (ID_NMFDYFLY.NE.0) TWICE_IN_NAMF = .TRUE.
                ID_NMFDYFLY = J
!
              ELSE if (fcal_names(j).ne.'MTTWTFLY') then
                write(errstr,'("invalid flyby cal ",a8," in socal --", &
     &            " cannot apply")') &
     &            FCAL_NAMES(J)
                call ferr( INT2(201), errstr, INT2(0), INT2(0) )
                call fatal(errstr )
              END IF
              IF (TWICE_IN_NAMF) THEN
                WRITE(ERRSTR,'("IN SOCAL - A FLYBY CAL ", &
     &               "APPEARS IN NAMFIL TWICE FOR ONE DB")')
                CALL FERR( INT2(1201), errstr, INT2(0), INT2(0) )
                CALL FATAL(ERRSTR )
              END IF
            END IF
            I = I + 1
          END DO
        END DO
        IDB_SAVE = IDB
      END IF
      DO_PRCOR = 0
!
!     Next generate the necessary values for this observation
!
      IF (ID_CFAKBDRY.NE.0 .OR. ID_CFAJJDRY.NE.0 .OR. &
     &    ID_CFAKBWET.NE.0 .OR. ID_MTTDRYSS.NE.0 .OR. &
     &    ID_MTTDYFLY.NE.0 .OR. ID_NMFDYFLY.NE.0 .OR. &
     &    ID_IFADRYSS.NE.0 .OR. &
     &    ID_IFADYFLY.NE.0) THEN
!
!           the subroutine which calculates the cfadry correction needs
!           to know whether it is calculating the kbdry or jjdry value
!           for the stations in this observation
!
            DO I = 1,2
              ISTAT = ITT(ISITE(I))
              ID_JJ(I) = 0
              MTT_SEAS_WET(I) = .FALSE.  ! default for mttwet is non-seasonal
              MTT_SEAS_DRY(I) = .FALSE.  ! default for mttdry is non-seasonal
              IFA_SEAS_WET(I) = .FALSE.  ! default for ifawet is non-seasonal
              IFA_SEAS_DRY(I) = .FALSE.  ! default for ifadry is non-seasonal
              IF ( ID_CFAJJDRY .NE.0 .AND. &
     &             KBIT(JCAFFL(1,ISTAT),ID_CFAJJDRY) ) THEN
                   ID_JJ(I) = 1
              END IF
              IF ( ID_MTTDRYSS .NE.0 .AND. &
     &             KBIT(JCAFFL(1,ISTAT),ID_MTTDRYSS) ) THEN
                   ID_JJ(I) = 2  ! use mttdry & seasonal mapping function.
                   MTT_SEAS_DRY(I) = .TRUE.
              END IF
              IF ( ID_MTTDYFLY .NE.0 .AND. &
     &             KBIT(JCAFFL(1,ISTAT),ID_MTTDYFLY) ) THEN
                   ID_JJ(I) = 2  ! use mttdry & unseasonal mapping function.
                   MTT_SEAS_DRY(I) = .FALSE.
              END IF
              IF ( ID_IFADRYSS .NE.0 .AND. &
     &             KBIT(JCAFFL(1,ISTAT),ID_IFADRYSS) ) THEN
                   ID_JJ(I) = 3  ! use ifadry & seasonal mapping function.
                   IFA_SEAS_DRY(I) = .TRUE.
              END IF
              IF ( ID_IFADYFLY .NE.0 .AND. &
     &             KBIT(JCAFFL(1,ISTAT),ID_IFADYFLY) ) THEN
                   ID_JJ(I) = 3  ! use ifadry & unseasonal mapping function.
                   IFA_SEAS_DRY(I) = .FALSE.
              END IF
              IF ( ID_NMFDYFLY .NE.0 .AND. &
     &             KBIT(JCAFFL(1,ISTAT),ID_NMFDYFLY) ) THEN
                   ID_JJ(I) = 4  ! use nmfdry & unseasonal mapping function.
                   IFA_SEAS_DRY(I) = .FALSE.
              END IF
              ID_WET(I)= -1 ! so that we don't use partials in cfacalc.
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
!   Loop over stations and across the calibration bits in JCAFFL,
!   and apply the
!   calibrations where requested.  Signs have been selected in SDBH
!   such that all calibrations must be ADDED here.
!
      DO I = 1, 2   !stations 1 and 2
        ISTAT = ITT(ISITE(I))
        DO J = 1, NFCAL    !loop over calibrations
          IF (KBIT (JCAFFL(1,ISTAT), J)) THEN
            IF (J .EQ. ID_CFAJJDRY) THEN
              DT = DT + cfakbdry(i,1) * 1.0e6
              RT = RT + cfakbdry(i,2)
            ELSE IF (J .EQ. ID_CFAKBDRY) THEN
              DT = DT + cfakbdry(i,1) * 1.0e6
              RT = RT + cfakbdry(i,2)
            ELSE IF (J .EQ. ID_MTTDRYSS .OR. J .EQ. ID_MTTDYFLY) THEN
              DT = DT + cfakbdry(i,1) * 1.0e6
              RT = RT + cfakbdry(i,2)
            ELSE IF (J .EQ. ID_IFADRYSS .OR. J .EQ. ID_IFADYFLY) THEN
              DT = DT + cfakbdry(i,1) * 1.0e6
              RT = RT + cfakbdry(i,2)
            ELSE IF (J .EQ. ID_NMFDYFLY) THEN
              DT = DT + cfakbdry(i,1) * 1.0e6
              RT = RT + cfakbdry(i,2)
            ELSE IF (J .EQ. ID_CFAKBWET) THEN
              DT = DT + CFAKBWET(I,1) * 1.0E6
              RT = RT + CFAKBWET(I,2)
            ELSE
              CALL FERR( INT2(202), ' - IN SOCAL - FLY CAL APP PROBLEM', &
     &             INT2(0), INT2(0) )
              CALL FATAL('IN SOCAL, FLY CAL APP PROBLEM' )
            END IF
          END IF !this one applied
        END DO !calibrations loop
      END DO !stations loop
!
!  Add troposphere noise based on average atmosphere delay
!     (roughly elevation dependent)
!
      if (keldep_noise) then
!       trop_fract converts the atmospheric turbulence and
!       mapping function parameter error constants to the formal error units
!       The delay and rate constants are stored in the input elevation
!       dependent constants file as picosecs and femtosecs/sec respectively.
        trop_fract(1) = 1.d-12       !atmospheric turbulence delay
        trop_fract(2) = 1.d-15       !atmospheric turbulence rate
        trop_fract(3) = 1.d-12       !map funct parm error delay
        trop_fract(4) = 1.d-15       !map funct parm error rate
!       cal_fract converts the flyby atmosphere calibration to the formal
!       error units.
        cal_fract(1) = 1.0e6 !delay
        cal_fract(2) = 1.0   !rate
        do icts = 1,2 !sites
!
!             Terms 1-4 are:
!                 atmospheric turbulence for delays
!                 atmospheric turbulence for rates
!                 mapping function parameter error for delays
!                 mapping function parameter error for rates
!
          do ictt = 1,4
              if (ictt.le.2 .and. &
     &             avg_atm(ictt,isite(icts)).lt.0.0D0) then
!
!               A negative atmospheric turbulence delay/rate constant indicates
!               that the user wants to apply a fraction of the delay/rate part
!               of the flyby atmosphere calibration used in the solution.
!               (The assumption is that one and only one flyby calibration
!               is being applied.  If this is false, then if none is applied,
!               a calibration of 0 is added, and if multiple ones are applied,
!               the last calibration in the namfil list is used.)
!               The fraction to be applied is
!               the atmospheric turbulence delay/rate "constant" itself.
!
                ISTAT = ITT(ISITE(ICTS))
                cal_add = 0.0D0
                DO J = 1, NFCAL    !loop over calibrations
                  IF (KBIT (JCAFFL(1,ISTAT), J)) THEN
                    IF (J .EQ. ID_CFAJJDRY) THEN
                      cal_add = cfakbdry(icts,ictt)
                    ELSE IF (J .EQ. ID_CFAKBDRY) THEN
                      cal_add =  cfakbdry(icts,ictt)
                    ELSE IF (J .EQ. &
     &                         ID_MTTDRYSS.OR. J .EQ. ID_MTTDYFLY) THEN
                      cal_add =  cfakbdry(icts,ictt)
                    ELSE IF (J .EQ. &
     &                         ID_IFADRYSS.OR. J .EQ. ID_IFADYFLY) THEN
                      cal_add =  cfakbdry(icts,ictt)
                    ELSE IF (J .EQ. ID_NMFDYFLY) THEN
                      cal_add =  cfakbdry(icts,ictt)
                    ELSE IF (J .EQ. ID_CFAKBWET) THEN
                      cal_add =  CFAKBWET(icts,ictt)
                    END IF
                  END IF !this one applied
                END DO !calibrations loop
                eln(ictt,icts) = cal_fract(ictt)* &
     &                           avg_atm(ictt,isite(icts))* &
     &                           cal_add
              else
                eln(ictt,icts) = trop_fract(ictt)* &
     &                    avg_atm(ictt, &
     &                    isite(icts))/((dsin(elev(icts)))**ictt)
              endif
          enddo
        enddo
        derr = dsqrt(derr**2+eln(1,1)**2+eln(1,2)** &
     &                 2+eln(3,1)**2+eln(3,2)**2)
        rerr = dsqrt(rerr**2+eln(2,1)**2+eln(2,2)** &
     &                 2+eln(4,1)**2+eln(4,2)**2)
      endif
!
!   ADD IONOSPHERE CALIBRATION AND MODIFY ERRORS
!
!   Apply GION
!
!
!    sband**2/xband**2
      if (effreq_xs.gt.0) then
        group_f_ratio_sq = (effreq_xs*effreq_xs)/(effreq*effreq)
        if (effreq.lt.5000.) group_f_ratio_sq = 1.d0/group_f_ratio_sq
      else
        group_f_ratio_Sq = 0.079d0
      endif
      if (pheffreq_xs.gt.0) then
        phase_f_ratio_sq=(pheffreq_xs*pheffreq_xs)/(pheffreq*pheffreq)
        if (effreq.lt.5000.) phase_f_ratio_sq = 1.d0/phase_f_ratio_sq
      else
        phase_f_ratio_sq = 0.079
      endif
      temp_group = 1.+2*group_f_ratio_sq
      temp_phase = -1. - 2*group_f_ratio_sq
! X and S band have different signs for gion.
      if(effreq .lt. 5000.) then
        temp_group=-temp_group
        temp_phase=-temp_phase
      endif
      if (.not.kiono) then
        temp_group = 1.D0
      endif
! JMG 980928  This fixes case of no ionosphere  calibration.
      if ( FUSED_STATUS == IONOV__UNDF .AND. gionsg(1) .eq. 0) then
         temp_group=1.
         temp_phase=1.
      endif
!
!
      IF (KBIT ( JSITI(ISTA1), INT2(4) ) .AND. .NOT. KBIT ( JSITI(ISTA1), &
     &INT2(5)) .AND.KBIT ( JSITI(ISTA2), INT2(4) ) .AND. .NOT. KBIT ( JSITI(ISTA2), &
     & INT2(5)))THEN
!
!       Make certain that the iono correction is good and that the
!       matching S-band has non-zero fringe detection.  (Compensates
!       for an old CNPLT bug (now fixed.))
        IF ( KBIT( ICORR, INT2(5)) .OR.KBIT( ICORR, INT2(6) ) )  THEN        !NO GOOD - DOWNWEIGHTED
          NOGOOD = 1
        ELSE                                    !good
          NOGOOD = 0
        END IF
!
!       Group delay data
!
!  Handle case where GIONSG is zero, by setting it to a high value
!   MWH - 5/6/93
!
!       if (gionsg(1).eq.0.and.kiono) gionsg(1) = 1.d-6
!       if (gionsg(2).eq.0.and.kiono) gionsg(2) = 1.d-6
        IF ( FUSED_STATUS == IONOV__UNDF ) THEN
             if (gionsg(1).eq.0.and.kiono) NOGOOD=1
             if (gionsg(2).eq.0.and.kiono) NOGOOD=1
        END IF
!
        IF ( DATYP_INQ ( IDATYP, GROUP__DTP )  .OR. &
     &       DATYP_INQ ( IDATYP, SINGL__DTP )       ) THEN
!
! ------- Group delay or narrow-band delay
!
          DT    = DT    + GION(1)
          RT    = RT    + GION(2)
          IF ( FUSED_STATUS == IONOV__UNDF ) THEN
               IF ( TEMP_GROUP > 0.0D0 ) THEN
                    DERR  =  SQRT(temp_group*DERR**2 + GIONSG(1)**2)
                    RERR  =  SQRT(temp_group*RERR**2 + GIONSG(2)**2)
                  ELSE 
                    DERR  =  1.D-8
                    RERR  =  1.D-9
               END IF
          END IF
!
! ------- Phase delay data
!
        ELSE IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
          IF ( FUSED_STATUS == IONOV__UNDF ) THEN
               DT    = DT    - GION(1)*(effreq**2)/(pheffreq**2)
               RT    = RT    + GION(2)*(effreq**2)/(reffreq**2)
               DPHER =  SQRT(DPHER**2 + GIONSG(1)**2)
               RERR  =  SQRT(temp_group*RERR**2 + GIONSG(2)**2)
          END IF
        ELSE IF ( DATYP_INQ ( IDATYP, RATONL__DTP ) ) THEN
!
! ------- Rates only
!
          rt    = rt    + gion(2)
          rerr  =  sqrt(temp_group*rerr**2 + gionsg(2)**2)
        else
          call ferr( INT2(217), 'illegal data type in socal', INT2(0), &
     &         INT2(0) )
        end if
!
      END IF
!
!     Want to apply PHION
!
      IF (.NOT. KBIT( JSITI(ISTA1), INT2(4) ) .AND. KBIT( JSITI(ISTA1), &
     &INT2(5)) .AND..NOT. KBIT( JSITI(ISTA2), INT2(4) ) .AND. KBIT( JSITI(ISTA2), &
     & INT2(5)) )THEN
!
        IF (.NOT. KBIT( ICORR, INT2(11))) THEN    ! PHION is not downweighted
          NOGOOD = 0
!
        ELSE                              ! PHION is no good
          NOGOOD = 1
        END IF
!
        IF ( DATYP_INQ ( IDATYP, GROUP__DTP )  .OR. &
     &       DATYP_INQ ( IDATYP, SINGL__DTP )       ) THEN
!
! ---------- Apply to group delay data
!
             DT    = DT    + PHION*(PHEFFREQ**2)/(EFFREQ**2)
             RT    = RT    + GION(2)
             DERR  =  SQRT(DERR**2 + PHIONS**2)
             IF ( FUSED_STATUS == IONOV__UNDF ) THEN
                  RERR  =  SQRT(temp_group*RERR**2 + GIONSG(2)**2)
             END IF
           ELSE IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! ---------- Apply to phase delay data
!
             DT    = DT    - PHION
             RT    = RT    + GION(2)*(EFFREQ**2)/(REFFREQ**2)
             DPHER =  SQRT(DPHER**2 + PHIONS**2)
             IF ( FUSED_STATUS == IONOV__UNDF ) THEN
                  RERR  =  SQRT(temp_group*RERR**2 + GIONSG(2)**2)
             END IF
           ELSE IF ( DATYP_INQ ( IDATYP, RATONL__DTP ) ) THEN
!
! ---------- Rates only
!
             RT    = RT    + GION(2)
             RERR  =  SQRT(TEMP_GROUP*RERR**2 + GIONSG(2)**2)
           ELSE
            CALL FERR ( INT2(218), 'illegal data type in socal', INT2(0), &
     &                 INT2(0) )
        END IF
!
      END IF
!
!     Get the raw observation weights with ionosphere sigmas.
      DERR_RAW = DERR
      RERR_RAW = RERR
      DPHER_RAW = DPHER
!
!   Add formal errors
!
!*** switched order 3/6/95  mwh
!
      IF (ISTA1 .GT. ISTA2) THEN     ! swap stations
!      IF (ISTA2 .GT. ISTA1) THEN     ! swap stations
        I = ISTA1
        ISTA1 = ISTA2
        ISTA2 = I
      END IF
!
!   Calculate index in packed table which points to error for this
!   baseline. N is (max # stations per database - 2) = 32 - 2 = 30
!
      N = MAX_ARC_STA - 2
      J = ((ISTA1-1) * N) - (((ISTA1-1) * (ISTA1-2))/2) + ISTA2-1
      JJ = ITTB(J)
!
      IF ( JJ .GE. 1  .AND.  JJ .LE. MAX_ARC_BSL ) THEN
           IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
                DPHER = SQRT (DPHER**2 + ET(1, JJ)*ET(1, JJ))
             ELSE
                DERR = SQRT (DERR**2 + ET(1, JJ)*ET(1, JJ))
           END IF
!
           RERR = SQRT (RERR**2 + ET(2, JJ)*ET(2, JJ))
      END IF
!
!   Get source-specific weights and RSS them with current error
!
      if (source_weight_file.ne.' ') then
      if (get_source_weight(istrn_chr,source_weight_file, &
     &   constants))then
        constants(1) = constants(1)*1.d-12
        constants(2) = constants(2)*1.d-15
        constants(3) = constants(3)*1.d-12
        constants(4) = constants(4)*1.d-15
        IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
               DPHER = DSQRT (DPHER**2 + constants(3)*constants(3))
               RERR = DSQRT (RERR**2 + constants(4)*constants(4))
           ELSE
               DERR = DSQRT (DERR**2 + constants(1)*constants(1))
               RERR = DSQRT (RERR**2 + constants(2)*constants(2))
        ENDIF
      else
        if (source_weights.eq.'RE') then
          call ferr( INT2(123), 'source-dependent weight missing', INT2(0), &
     &         INT2(0) )
        endif
      endif
      endif
!
!   FINISHED
!
      RETURN
      END
