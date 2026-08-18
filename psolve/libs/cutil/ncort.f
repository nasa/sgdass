      SUBROUTINE NCORT ( JSITN,  JSITI, &
     &                   JCAPPL, NUMSTA, ITT, IDB, &
     &                   IDATYP, ITTB, ET, SE, SS, OBCAPL, MCAPL, &
     &                   JCAVAL, &
     &                   LATS, HEIGHTS, AX_TYPES, AX_OFFS, BARO_CALS, &
     &                   BARO_HEIGHTS, JCAFFL, FCAL_NAMES, NFCAL, NSSS, &
     &                   CALCV )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  NCORT PROGRAM SPECIFICATION
!
! 1.1 Read in station names, calibration, contribution and
!     status arrays, baseline names and formal errors. Also
!     set up correspondence table between those stations and the
!     Updated to specificaly type integers which
!-------------------------------------------------
!     stations listed in PARFIL.
!
! 1.2 REFERENCES:
!
! 2.  NCORT INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 NUMSTA,IDB,IDATYP
      REAL*8 CALCV
!
! IDATYP - Data type from SOCOM
! IDB - Number of the database section
! NUMSTA - Number of stations
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 JSITN(4,MAX_ARC_STA), JCAPPL(MAX_ARC_STA)
      INTEGER*2 JCAFFL(7,MAX_ARC_STA)
      INTEGER*2 JSITI(MAX_ARC_STA), ITT(MAX_ARC_STA)
      INTEGER*2 ITTB(MAX_ARC_BSL)
      REAL*8    ET(2,MAX_ARC_BSL), SE(MAX_ARC_STA), SS(MAX_ARC_SRC)
      INTEGER*2 OBCAPL, JCAVAL(MAX_ARC_STA), MCAPL
      REAL*8 LATS(MAX_ARC_STA),HEIGHTS(MAX_ARC_STA)
      INTEGER*2 AX_TYPES(MAX_ARC_STA), NFCAL
      CHARACTER*8 FCAL_NAMES(*)
      REAL*8 AX_OFFS(MAX_ARC_STA)
      REAL*8 BARO_CALS(MAX_ARC_STA),BARO_HEIGHTS(MAX_ARC_STA)
      INTEGER*2 I,I1,I2,ICHEK,ICT,IERR,IST,J,JJ,N,NSSS,NUMLOOP
      INTEGER*4  IERR_I4, IND_STA, J1, J2, J3, J4
      INTEGER*4  OBCAVL, MCAVL
!
! AX_OFFS - Antenna axis offsets
! AX_TYPES - Antenna axis types
! BARO_CALS - Barometer calibrations
! BARO_HEIGHTS - Heights of barometers
! HEIGHTS - Site elevations
! JSITN  - NAMFIL data base names
! JSITI  - station ion calibration flags
! JCAPPL - station cable and atmosphere calibration application flags
! JCAFFL - flyby station calibration application flags
! NFCAL - number of flyby calibrations contained in namfil
! FCAL_NAMES - names of flyby calibrations
! ITT    - NAMFIL/PARFIL station correspondence table
! ISITN  - PARFIL data base names
! ITTB   - NAMFIL station/baseline correspondence table, packed
! ET     - baseline formal errors (from NAMFIL)
! JBUF   - buffer for reading NAMFIL
! OBCAPL - Bit flag of contributions to be applied
! JCAVAL - Bit flag of available calibrations
! MCAPL  - Bit flag of mode calibrations to be applied
! LATS   - Station latitudes
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'glbc3.i'
      INCLUDE 'bindisp.i'
      INCLUDE 'flyby.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: getcard,ferr,plh
!
! 3.  LOCAL VARIABLES
!
      REAL*8       EE(4), REA, RNOR
      PARAMETER  ( REA = 6378136.3D0 ) ! Earth's equatiorial radius
      INTEGER*2    JBASL(4,2)
      CHARACTER    BASL_CH(2)*8
      EQUIVALENCE ( JBASL, BASL_CH )
      REAL*8        VSITE(3),RDUM
      CHARACTER     ERRSTR*160, LCARD*4, QCONT_LCODES(16)*8, STR*32, JBUF*70
      INTEGER*2   ID_CFAJJDRY, NOBCAL, IDUM1, IDUM2, INT2_ARG
      INTEGER*4   INT4
      LOGICAL*2,  EXTERNAL :: KBIT
      LOGICAL*4,  EXTERNAL :: DATYP_INQ
      INTEGER*4,  EXTERNAL :: ILEN, I_LEN, LTM_DIF
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   WK   850206  Created
!   IS   851022  Set up baseline correspondence table;, ITTB, so that
!                we can do the reweighting here.  We also need IDATYP
!                so we know whether to use group or phase reweight constants
!                ITT,ISITN redimensioned to match new PARFIL structure.
!   IS   851122  Handle case where a baseline is given twice.
!   IS   860130  Handle new calibration scheme (new NAMFIL structure).
!                Got rid of ECC,MONNAM,MTYPE because tey're not used
!                in PROC or CRES -- just skip over them in NAMFIL
!   KDB  860303  Added read of NAMFIL to pick up OBCAPL. Part of new
!                scheme to handle observaton dependent contributions.
!   KDB  860422  Handle new NAMFIL structure, where GETCARD replaces
!                reads, etc.
!   KDB  870817  Handle two special calibrations (CFAWET and CFADRY, which
!                can be applied even for databases which do not contain
!                their lcodes)
!   KDB  880615  Calculate stations' geodetic latitudes and elevations
!                above the geoid
!   KDB  890215  Set up Jim Ray's antenna geometry correction to
!                CFAKBDRY as a separate calibration (CFAJJDRY)
!   AEE  910515  Enhanced error messages written to the error file.
!   KDB  910715  Use new corfil/namfil scheme (namfil carries calibration/
!                 contribution names and separates flyby & regular calibs.
!                 Corfil only accessed in SDBH.)
!   kdb  970401  Reverse the sign of the
!                EQE CONT contribution (correction to the equation of the
!                equinox to fix a CALC error.) (Only do this for calc versions
!                less than 8.3, the version in which this will be fixed.)
!   kdb  970417  Fix benign error; reading the number of regular calibrations
!                into the number of flyby calibrations.  (This was benign
!                because there were more regular calibrations than flyby ones
!                and the  extra false ones were just ignored because
!                they weren't turned on.)
!   pet  971124  Simplified comments. Removed unused variables and commented out
!                code.
!   pet  980203  Substituted hard-coded test of solution type by DATYP_INQ
!   pet  1999.11.17  Added formal argument MCAPL. Changed parsing card CONT
!                    in order to extract status of mode calibrations
!   pet  2001.08.21  Added the trap of internal control for the case when
!                    station's coordinates are zero
!   pet  2007.12.03  Added variables SE, SS
!   pet  2023.07.29  Fixed crash at ********
!
!
! 5.  NCORT PROGRAM STRUCTURE
!
!     Pull information out of the namfil.
!
!     First, the number of flyby calibrations
!
      FL_CAL_SAVE = .FALSE.
      CALL GETCARD( IDB, 'CLCT', INT2(1), JBUF, IERR )
      IF ( IERR .NE. 0 ) THEN
           WRITE ( ERRSTR, '("In sub ncort getting a CLCT card ")' )
           CALL FERR ( IERR, ERRSTR, INT2(0), INT2(0) )
        ELSE
           READ ( JBUF, "(4X,4(1X,I3),50X)", IOSTAT=IERR_I4 ) &
     &            IDUM1, NFCAL, IDUM2, NOBCAL
           IERR  = IERR_I4
           CALL FERR ( IERR, "Reading CLCT card", INT2(0), INT2(0) )
      END IF
!
! --- Next the names of the flyby calibrations
!
      LCARD = 'FCLN'
      CALL GET_CLN_CARD ( IDB, LCARD, NFCAL, FCAL_NAMES, IERR )
      IF ( IERR .NE. 0 ) THEN
           WRITE ( ERRSTR, '("In sub NCORT getting the FCLN cards ")')
           CALL FERR ( INT2(25627), ERRSTR, INT2(0), INT2(0) )
      END IF
!
! --- Next the bit array indicating which are applied
!
      IST = 0
      IERR = 0
      NSSS = 0
      DO WHILE ( IERR .EQ. 0 )
         IST = IST + 1
         IF ( IST .EQ. 1 ) THEN
              CALL GETCARD ( IDB, 'FCLS', INT2(1), JBUF, IERR )
           ELSE
             CALL GETCARD( IDB, 'FCLS', INT2(0), JBUF, IERR )
        END IF
        IF (IERR .NE. 0) THEN
          IF (IERR .EQ. 1) THEN
            GO TO 8000
          ELSE
            WRITE (errstr, '("IN SUB NCORT GETTING A FCLS CARD")')
            call ferr( IERR, errstr, INT2(0), INT2(0) )
            GO TO 8000
          END IF
        ELSE IF(IST.GT.MAX_ARC_STA) THEN
          CALL FERR( INT2(16), 'TOO MANY STATIONS IN NAMFIL IN NCORT', &
     &         INT2(0), INT2(0) )
        END IF
        READ (JBUF,"(13X,7(1X,I7),1X)",IOSTAT=IERR_I4) &
     &     (JCAFFL(ICT,IST),ICT=1,7)
        IERR = IERR_I4
        call ferr( ierr, "Reading FCLS card", INT2(0), INT2(0) )
!
        NSSS = NSSS + 1
 8000   CONTINUE
      END DO
      IF (IERR .LT. 0) GO TO 9001
!
! --- Now the station names and bit variable indicating which
! --- non-flyby calibrations are applied
!
      IST = 0
      IERR = 0
      NSSS = 0
      DO WHILE (IERR .EQ. 0)
         IST = IST + 1
         IF ( IST .EQ. 1 ) THEN
              CALL GETCARD( IDB, 'CALS', INT2(1), JBUF, IERR )
            ELSE
              CALL GETCARD( IDB, 'CALS', INT2(0), JBUF, IERR )
         END IF
         IF ( IERR .NE. 0 ) THEN
              IF ( IERR .EQ. 1 ) THEN
                   GOTO 9000
                ELSE
                   WRITE ( ERRSTR, '("In sub NCORT getting a CALS card ")')
                   CALL FERR ( IERR, ERRSTR, INT2(0), INT2(0) )
                   GOTO 9000
              END IF
           ELSE IF(IST.GT.MAX_ARC_STA) THEN
              CALL FERR ( INT2(16), 'Too many stations in namfil in NCORT', &
     &                    INT2(0), INT2(0) )
        END IF
        READ ( JBUF, 9100, IOSTAT=IERR_I4) (JSITN(J,IST),J=1,4), &
     &                     JSITI(IST), JCAVAL(IST),JCAPPL(IST)
        IERR = IERR_I4
        CALL FERR ( IERR, "Reading a CALS card", INT2(0), INT2(0) )
9100    FORMAT(5X, 4A2, 1X, 3I7, 35X)
!
        NSSS = NSSS + 1
 9000   CONTINUE
      END DO
      IF ( IERR .LT. 0 ) GO TO 9001
!
! --- Set name correspondence table
!
      IF ( NUMSTA .GT. MAX_ARC_STA ) THEN
           CALL FERR ( INT2(17), 'Too many stations in NCORT', INT2(0), &
     &                 INT2(0) )
      ENDIF
!
      DO I = 1, NUMSTA
         J = 1
         ITT(I) = 0
!
! ------ Search current stations
!
         DO WHILE ( ITT(I) .EQ. 0   .AND.   J .LE. NSSS )
            IF ( ICHEK(ISITN(1,I),JSITN(1,J)) .EQ. 1 ) THEN
                 ITT(I)=J
              ELSE
                 J=J+1
            END IF
         END DO
      END DO
!
! --- Pick up OBCAPL and MCAPL, to pass back to PROC, CRES, ELIM, PAMB
!
      CALL GETCARD ( IDB, 'CONT', INT2(1), JBUF, IERR )
      IF ( IERR .NE. 0) THEN
           WRITE ( ERRSTR, '("IN SUB NCORT GETTING THE CONT CARD ")' )
           CALL FERR ( IERR, ERRSTR, INT2(0), INT2(0) )
           GOTO 9001
      ENDIF
      READ ( JBUF, FMT=9101, IOSTAT=IERR_I4 ) OBCAVL, OBCAPL, MCAVL, MCAPL
      IERR = IERR_I4
 9101 FORMAT ( 5X, 2I7, 2I7, 37X)
      CALL FERR ( IERR, "ncort: Reading CONT card", INT2(0), INT2(0) )
!
! --- Kluge: pick up CNTI card to locate the correction to the equation of
! ---        the equinox and reverse its sign to fix a CALC error.
! ---        (This error should be fixed after calc 8.2.)
!
      IREVCONT = 0
      IF ( CALCV .LT. 8.201d0 ) THEN
           LCARD = 'CNTI'
           CALL GET_CLN_CARD(IDB,LCARD,NOBCAL,QCONT_LCODES,IERR )
           CALL FERR ( IERR, "Reading CNTI card", INT2(0), INT2(0) )
           DO ICT = 1,NOBCAL
              IF ( QCONT_LCODES(ICT) .EQ. 'EQE CONT' ) THEN
                   IREVCONT = ICT
              ENDIF
           ENDDO
      ENDIF
!
! --- Put baseline stuff here at the end of NCORT even though it
! --- preceeds calibration flags in NAMFIL.  This way, we can use
! --- ITT, taking advantage of the 16-station limit per data base,
! --- thus making the baseline corresp. table smaller.
!
      DO JJ = 1, MAX_ARC_BSL
        ITTB(JJ) = 0
      END DO
!
! --- Loop over baselines
!
      JJ = 0
      IERR = 0
      DO WHILE (IERR .EQ. 0)
         JJ = JJ + 1
         IF ( JJ .EQ. 1) THEN
             CALL GETCARD ( IDB, 'REWT', INT2(1), JBUF, IERR )
           ELSE
             CALL GETCARD ( IDB, 'REWT', INT2(0), JBUF, IERR )
         END IF
         IF ( IERR .NE. 0 ) THEN
              IF ( IERR .EQ. 1 ) THEN
                   GOTO 9002
                ELSE
                  WRITE (ERRSTR,'("NCORT ERROR GETTING A REWT CARD -- ")')
                  CALL FERR ( INT2(203), ERRSTR, INT2(0), INT2(0) )
                  GOTO 9002
              END IF
           ELSE IF ( JJ .GT. MAX_ARC_BSL ) THEN
              CALL FERR( INT2(45), 'Too may baselines in NCORT', &
     &                   INT2(0), INT2(0) )
         END IF
         IF ( JBUF(44:52) == '*********' ) JBUF(44:52) = '     0.00'
         IF ( JBUF(54:62) == '*********' ) JBUF(54:62) = '     0.00'
         IF ( JBUF(34:42) == '*********' ) JBUF(34:42) = '     0.00'
!
         READ ( JBUF, 1100, IOSTAT=IERR_I4 ) &
     &          ( (JBASL(I,J),I=1,4),J=1,2), (EE(J),J=1,4)
!
         IF ( ILEN(BASL_CH(1)) == 0 ) THEN
              IERR = 4601
              CALL CLRCH ( STR )
              CALL INCH  ( INT4(JJ), STR )
              CALL FERR  ( IERR, "(NCORT) Empty first station in REWT card "// &
     &                     STR(1:4), INT2(0), INT2(0) )
              CALL EXIT  ( 1 )
         END IF
!
         IF ( ILEN(BASL_CH(2)) == 0 ) THEN
              IERR = 4602
              CALL CLRCH ( STR )
              CALL INCH  ( INT4(JJ), STR )
              CALL FERR  ( IERR, "(NCORT) Empty second station in REWT card "// &
     &                     STR(1:4), INT2(0), INT2(0) )
              CALL EXIT  ( 1 )
         END IF
         IERR = IERR_I4
         IF ( IERR .NE. 0 ) THEN
              WRITE ( 6, FMT='("JJ=",I4," CARD: ",A)' ) JJ, JBUF
         END IF
         CALL FERR ( IERR, "(NCORT) Reading REWT card ", INT2(0), INT2(0) )
 1100    FORMAT (5X, 4A2, 1X, 4A2, 4F10.2, 8X)
!
! ------ Convert the units to seconds and seconds/second.
!
         EE(1) = EE(1)*1.D-12
         EE(2) = EE(2)*1.D-15
         EE(3) = EE(3)*1.D-12
         EE(4) = EE(4)*1.D-15
!
! ------ Get indices for baselines in terms of JSITN
! ------ Note that  ISITN(1,ITT(I)) = JSITN(1,I)
!
         I1 = 0
         I2 = 0
         DO I = 1, NSSS
            IF (ICHEK (JSITN(1,I), JBASL(1,1)) .EQ. 1) I1 = I
            IF (ICHEK (JSITN(1,I), JBASL(1,2)) .EQ. 1) I2 = I
            IF (I1*I2 .NE. 0) GO TO 100
         END DO
         GOTO 9002
!
! ------ Swap indices if necessary
!
  100  CONTINUE
!
! ---- Switching order 3/6/95  mwh
!
       IF ( I1 .GT. I2 ) THEN
            I = I1
            I1 = I2
            I2 = I
       END IF
!
! ---- Calculate index within ITTB according to packing formula
! ---- N is (number of stations - 2), currently 32 - 2 = 30.
! ---- If there is already something there, don't overwrite!
!
        N = MAX_ARC_STA-2
        J = ((I1-1) * N) - (((I1-1) * (I1-2)) / 2) + I2 - 1
        IF ( J .GT. MAX_ARC_BSL ) THEN
             CALL FERR ( INT2(120), 'NCORT: Table too small in ncort', &
     &            INT2(0), INT2(0) )
           ELSE IF ( J .LE. 0 ) THEN
             WRITE ( 6, * ) ' I1=',I1,' I2 = ',I2,' N=',N,' J=',J
             STOP 'NCORT'
!             CALL FERR ( 122, 'NCORT: Error in calculation index for ITTB',
!     #                         0, 0 )
!             STOP 'NCORT'
        ENDIF
        IF ( ITTB(J) .EQ. 0 ) THEN
             ITTB(J) = JJ
!
! ---------- Load appropriate part of EE into ET (depending on IDATYP)
!
             I = 1
             IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) I = 3
             ET(1,JJ) = EE(I)
             ET(2,JJ) = EE(I+1)
        END IF
 9002   CONTINUE
      END DO
!
! --- Set up tables of geodetic latitudes and elevations above the
! --- geoid, for this data base.
!
      NUMLOOP = NUMSTA
      IF (NUMLOOP .GT. MAX_ARC_STA) NUMLOOP = MAX_ARC_STA
      RNOR = 0.0D0
      DO I = 1,NUMLOOP
         DO J = 1,3
            VSITE(J) = VSITEC(J,I)
            RNOR = RNOR + VSITE(J)**2
         END DO
         RNOR = DSQRT ( RNOR )
         IF ( RNOR .LT. 0.9*REA ) THEN
              WRITE ( 6, * ) ' I=',I,' ISITN_CHR = ',ISITN_CHR(I), &
     &        ' Coord.: ',VSITE
              CALL FERR ( INT2(130), &
     &            'NCORT: Error of internal control: station '// &
     &            'coordinates are unrelaistic: '// &
     &            'the radius-vector indicates that it is '// &
     &            'an extraterrestrial station', INT2(0), INT2(0) )
              STOP 'NCORT Abnormal termination'
         END IF
         CALL PLH ( VSITE, LATS(I), RDUM, HEIGHTS(I) )
      END DO
!
! --- The CFAJJDRY calibration is the CFAKBDRY calibration, with a
! --- correction for the antenna geometry.  Get the axis offset and
! --- axis type for this correction.
! --- Stations which are not listed in the axis type table
! --- will default to type 0 (a correction of 0 will be applied to
! --- CFAKBDRY for that station).
!
      ID_CFAJJDRY = 0
      DO I = 1,NUMLOOP
         DO J = 1, NFCAL
           IF ( FCAL_NAMES(J) .EQ. 'CFAJJDRY' .AND. &
     &          KBIT (JCAFFL(1,I), J)                )  ID_CFAJJDRY = J
         END DO
      END DO
!
      IF ( RWT_EL_USE == SOLVE__YES ) THEN
!
! -------- Logic for handling post DEC2007 elevation dependent weights logic
!
           DO 410 J1=1,NUMSTA
              IND_STA = 0
!@              IF ( ISTASP > 0 ) THEN
!@                   IND_STA = LTM_DIF ( 1, INT4(ISTASP), STASUP_CHR, ISITN_CHR(J1) )
!@              END IF
!
              IF ( IND_STA == 0 ) THEN
!
! ---------------- If the station was not found in the station list, set global
! ---------------- reweighting
!
                   SE(J1) = RWT_EL_GLB
                 ELSE
                   IF ( KBIT ( CMPSUP(1,RWT_EL_STA__CNB), INT2(IND_STA) ) ) THEN
!
! --------------------- We found the station in the station list, and it is
! --------------------- marked as elevation-dep reweight station. Very good!
!
                        SE(J1) = RWT_EL_STA(IND_STA)
                      ELSE
!
! --------------------- If the station was found in the station list, but
! --------------------- was not marked. set global reweighting
!
                        SE(J1) = RWT_EL_GLB
                   END IF
              ENDIF
 410       CONTINUE
         ELSE IF ( RWT_EL_USE == SOLVE__RW_EL_MULT_GLOB ) THEN
           DO 420 J2=1,NUMSTA
              SE(J2) = RWT_EL_GLB
 420       CONTINUE 
         ELSE
           CALL NOUT_R8 ( INT4(NUMSTA), SE )
      END IF
 9001 CONTINUE
      RETURN
      END  !#!  NCORT  #!#
