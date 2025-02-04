      PROGRAM ACCOR
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  ACCOR PROGRAM SPECIFICATION
!
! 1.1
!     Depending on the option passed to it through ICALIBOPT (IPAR(3)),
!     this program has one of three purposes.
!
!     If the option passed is '1':
!
!     This program gathers the station/calibration data needed for
!     SELCOR, the subroutine which allows a user to view and change
!     the data via a terminal.  After the call to SELCOR, ACCOR also
!     stores the user's changes to the data.  ACCOR is only called by
!     OPTIN; SDBH contains the necessary code to set up for its call
!     to SELCOR and store the user's changes.
!
!     There are two types of calibrations, non-flyby and flyby, so
!     ACCOR will ask which type to work with.
!
!     If the option passed is '2':
!
!     This program gathers the observation dependent contribution data
!     needed for OBCOR, the subroutine which allows a user to view and
!     change the data via a terminal.  After the call to OBCOR, ACCOR
!     stores the user's changes to the data.  ACCOR is only called by
!     OPTIN; SDBH contains the necessary code to set up for its call
!     to OBCOR and store the user's changes.
!
!
! 1.2 REFERENCES:
!
! 2.  ACCOR INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'precm.i'
      INCLUDE 'glbc4.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: None
!       CALLED SUBROUTINES: selcor,obcor
!
! 3.  LOCAL VARIABLES
!
!        CHANGEMADE      LOGICAL        Flag indicating whether or not
!                                       the user made a change to the
!                                       station calibration data.
!                                       Controls whether code to write
!                                       user changes to permanent file
!                                       (NAMFIL) is skipped or
!                                       performed.
!        ERRSTAT         CHARACTER*1    If a runtime error occurs at
!                                       a file handling statement,
!                                       ERRSTAT will be printed in a
!                                       message on the terminal and
!                                       will identify the statement.
!        FIRSTDB         LOGICAL        Indicates whether or not the
!                                       current data base is the first
!                                       data base selected.
!        ICALIBOPT       INTEGER        Receives the value of IPAR(3),
!                                       enabling ACCOR to determine
!                                       whether the user wants to work
!                                       with station dependent cali-
!                                       brations (= 'C') or with
!                                       observation dependent contri-
!                                       butions (= 'R')
!        ICAPPL(32)      INTEGER        Stores original data about
!                                       which calibrations were
!                                       applied to which station while
!                                       SELCOR is called.  After the
!                                       call, ICAPPL is compared to
!                                       the new data, to see if the
!                                       user made any changes.
!        ICONT           INTEGER        Indicates which type of read
!                                       or write ACCOR will use to
!                                       access NAMFIL.
!        IERR            INTEGER        Receives the values of runtime
!                                       errors.
!        IOBCAPL         INTEGER        Saves the original values of
!                                       OBCAPL for comparison after
!                                       the call to OBCOR, to find
!                                       out whether the user wanted
!                                       to make a change.
!        ISQR            INTEGER        Passed to OPTIN via EXEC 14
!                                       to tell OPTIN whether or not
!                                       the user chose the least
!                                       squares option in SELCOR or
!                                       ACCOR. Can also be given values
!                                       that indicate that:
!                                         A. CORFIL had a discrepancy and
!                                            OPTIN should terminate.
!                                         B. The user chose CRES.
!        ISTAND          INTEGER        Not used by ACCOR.  Included
!                                       in call to SELCOR, because
!                                       SDBH, the other program which
!                                       calls SELCOR, uses it.
!        JBUF            CHARACTER*70   Buffer used to transfer data
!                                       to and from NAMFIL.
!        JCAPPL(32)      INTEGER        Passes original data concern-
!                                       ing which calibrations are
!                                       applied to which station,
!                                       between NAMFIL and SELCOR, so
!                                       that the user can make changes
!        JCAVAL(32)      INTEGER        Passes data concerning which
!                                       calibrations are available for
!                                       which stations, from NAMFIL to
!                                       SELCOR.
!        JNCAL           INTEGER        SELCOR can handle up to 16
!                                       calibrations.  Indicates how
!                                       many are actually used.
!        JNSTA           INTEGER        SELCOR can handle up to 32
!                                       stations.  Indicates how many
!                                       are actually used.
!        JSITI(32)       INTEGER        Ion cal status.  Not directly
!                                       used. Just saved until ACCOR
!                                       is ready to rewrite the NAMFIL
!                                       record which contains it.
!        JSITN(4,32)     INTEGER        List of stations that will
!                                       be handled by SELCOR.
!        LCARD           CHARACTER*4    Indicates which type of NAMFIL
!                                       card ACCOR will read or write.
!                                       not really used by ACCOR.
!        LCORF           CHARACTER*4    Contains generic part of corfil name
!        LDBNAM(5)       INTEGER        Name of data base being
!                                       processed.
!        LDISP(8)        INTEGER        Equivalenced to QDISP.
!        MAXSTATCHANGED  INTEGER        After call to SELCOR, receives
!                                       position in station list of
!                                       last station in the list with
!                                       changed data.
!        NNVER           INTEGER        Version of data base being
!                                       processed.
!        NOBCAL          INTEGER        OBCOR can handle up to 15
!                                       contributions.  NOBCAL is
!                                       number actually used.
!        NUMDB           INTEGER        NAMFIL may contain multiple
!                                       data bases. Indicates which
!                                       data base ACCOR is currently
!                                       working with.
!        OBCAPL          INTEGER        Bit I of OBCAPL indicates that
!                                       contribution I will be applied
!                                       to the data base.
!        OBCAVL          INTEGER        Bit I of OBCAVL indicates that
!                                       contribution I is available
!                                       for the data base.
!        PROGCOM         CHARACTER*1    On input to SELCOR, indicates
!                                       that ACCOR (and not SDBH)
!                                       is calling SELCOR.  No meaning
!                                       on input to OBCOR, since OBCOR
!                                       is only called by ACCOR.  On
!                                       output from SELCOR or OBCOR,
!                                       indicates whether the user
!                                       wants to see the screen for
!                                       the next data base or return
!                                       to the OPTIN menu.
!        QDCAL(MAX_CAL)  CHARACTER*8    List of calibrations which
!                                       SELCOR will handle.
!        QDISP           CHARACTER*8    Transfers calibration names
!                                       from CORFIL to QDCAL.
!        QSITN(32)       CHARACTER*8    Equivalenced to JSITN.
!
!     Files:
!
!        NAMFIL                         Contains names of stations and
!                                       data concerning which calibra-
!                                       tions are available for which
!                                       stations and which calibra-
!                                       tions are applied.
!
      LOGICAL*2   CHANGEMADE, FIRSTDB, KBIT
      CHARACTER*1 ERRSTAT
      INTEGER*2   ICAPPL(MAX_ARC_STA)
      integer*4   ICH
      INTEGER*4   I4P0, I4P1, I4P12, I4P21, I4P52
      CHARACTER*4 CCH
      EQUIVALENCE ( ICH, CCH )
      CHARACTER*70 JBUF
      character*79 bufstr
      INTEGER*2 JCAPPL(MAX_ARC_STA), JCAVAL(MAX_ARC_STA), &
     &   JSITI(MAX_ARC_STA), JSITN(4,MAX_ARC_STA), &
     &   ICAFFL(7,MAX_ARC_STA),JCAFFL(7,MAX_ARC_STA), &
     &   MCAVL, MCAPL
      CHARACTER*4 LCARD
      CHARACTER   CDBNAM*10
      INTEGER*2  LDBNAM(5), LDISP(8)
      EQUIVALENCE ( LDBNAM, CDBNAM )
      INTEGER*2 OBCAPL, OBCAVL,IACCOR(2)
      CHARACTER*1 PROGCOM
      CHARACTER*8 QDCAL(MAX_CAL), QDISP, QDOBCAL(MAX_CONT), &
     &    QSITN(MAX_ARC_STA)
      INTEGER*2 I,ICALIBOPT,IERR,IOBCAPL,ISQR,ISTAND,J,JNCAL,JNSTA, &
     &          MAXSTATCHANGED,NNVER,NOBCAL, &
     &          NUMDB,LMODE,NFCAL,MFCAL,NZCAL, JPART(7,16), MCAPL_ORIG, &
     &          trimlen
      LOGICAL*4  SBAND_PHCAL
      INTEGER*2 L_CLM
      CHARACTER MCALNAMS(M_CLM)*8
      CHARACTER*8 QDFCAL(112), PARTIAL(112)
!
      EQUIVALENCE (QSITN(1), JSITN(1,1)), (QDISP, LDISP(1))
      INTEGER*2 IUNIT,LIMIT,NUM_PARTIAL,IDUM,KERR
      CHARACTER DIR_PART*63, FILE_PART*63, ERRSTR*128, PHC_STR(MAX_ARC_STA)*2
!
      DATA I4P0,I4P1,I4P12,I4P21,I4P52 / 0,1,12,21,52 /
      INTEGER*4  IOS
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   KDB  860206  Created
!   KDB  860225  Modified to allow call to OBCOR
!   KDB  860417  Modified to support new NAMFIL structure and
!                 handle multiple data bases
!   KDB  910718  Modified to support newer NAMFIL and CORFIL scheme:
!                Namfil gets name of calibs, flyby and non-flyby info
!                separated.
!   AEE  920522  Added partial menu so we now have flyby, non-flyby, and
!                partial menus.
!   JLR  921215  Added I4Pn variables to replace nJ constants
!   kdb  961106  Fix bug in which the flyby calibration name list was updated
!                  in the namfil to a hard coded list when the user was
!                  updating the partials selection.
!                Remove setting of nfcal to 8 before the selcorf call.
!                  (Unnecessary, and confusing to programmers.)
!                No longer hard code the partials list.
!   pet 1999.11.16  Added reading MCAL card. Added call of modcor.
!                   Updated commentrs
!   pet 2000.07.04  Added printing status of phase cal.
!
! 5.  ACCOR PROGRAM STRUCTURE
!
!     Initialize.
!
      CALL PRE_PROG()
      INCLUDE 'accor_version.i' ! Set revision date of the current version
      CALL SET_SIGNAL_CTRLC ( 2 )
!
      iunit = 302
      limit = 112
      dir_part = pre_sav_dir(:pre_sv_len)
      file_part = AVAL_PART_FILE//CHAR(0)
      call atmavl_n(iunit,limit,dir_part,file_part, &
     &              partial,num_partial,idum,kerr )
      if (kerr.ne.0 .or. num_partial .eq. 0) then
        if (kerr.ne.0) then
          errstr = 'accor error:  from atmavl_n'
          call ferr( kerr, errstr, INT2(0), INT2(0) )
        else
          errstr = 'accor error: part cal avail file empty = '// &
     &       dir_part(1:trimlen(dir_part))//'/'//file_part
          call ferr( INT2(401), errstr, INT2(0), INT2(0) )
        end if
      endif
      call use_glbfil_4('OR' )
      do i= 1,num_partial
        part_array(i)= partial(i)
      end do
      num_part= num_partial
      call use_glbfil_4('WC' )
      call start_mn()
      call setcr_mn(I4P0,I4P0 )
!
      CALL USE_BUFFER( IACCOR, INT2(2), 'ORC' )
!
! *** The following line was commented out by MWH on 930823, disabling
! ***  a hidden feature which allowed the user to specify which database
! ***  to begin processing
!
!      NUMDB=IACCOR(1)
      ICALIBOPT =IACCOR(2)
!
!     If the user did not specify which data base he wants initially, via
!     the OPTIN menu, give him the first data base.
!
!  79  IF(NUMDB.EQ.0) NUMDB=1
  79  NUMDB=1
!
!     Begin processing for individual data bases.
!
      FIRSTDB = .TRUE.
 100  CONTINUE
!
!     Get data base name from NAMFIL, to later display it and
!     identify the data on the screen.
!
      ERRSTAT = 'A'
      CALL GETCARD ( NUMDB, 'INIT', INT2(1), JBUF, IERR )
      IF (IERR .EQ. -6) THEN
        IF (FIRSTDB) THEN
          WRITE(bufstr,"('SORRY, LESS THAN ',I1,' DATA BASES IN YOUR ', &
     &               'SCRATCH FILES')") NUMDB
        call ferr( INT2(123), bufstr, INT2(0), INT2(0) )
        END IF
        GOTO 90
      END IF
      IF (IERR .NE. 0) GOTO 4000
      FIRSTDB = .FALSE.
      ERRSTAT = 'M'
      READ (JBUF, 5004, IOSTAT=IOS) &
     &   (LDBNAM(J), J = 1, 5), NNVER
      CALL FERR ( INT2(IOS), "Reading INIT card", INT2(0), INT2(0) )
 5004 FORMAT (10X, 5A2, I4, 46X)
!
! --- Depending on the value in ICALIBOPT, gather data to be passed
! --- to SELCOR, SELCORF or OBCOR for this data base.  These subroutines will
! --- display the data and allow the user to make certain changes.
! --- Put the data in the format that SELCOR, SELCORF or OBCOR requires.
!
!     If ICALIBOPT = 1 (station dependent calibrations)
!                         (NON-FLYBY)
!
!     First gather the station names.  Also, for each station/
!     calibration combination, gather data for:
!
!          1.  Whether or not the calibration is available for the
!              station.
!
!          2.  Whether or not the calibration is applied to the
!              station.
!
!     All of this information is contained in NAMFIL.  Place the
!     list of station names in JSITN (equivalenced to QSITN), and
!     count the stations.  Place the availability data in JCAVAL.
!     Place the application data in JCAPPL.
!
!     Also get list of names for display and count of calibs.
!
!     If ICALIBOPT = 2 (observation dependent contributions)
!
!     For each calibration, gather data for:
!
!          1.  Whether or not the calibration is available for the
!              data base.
!
!          2.  Whether or not the calibration is applied to the
!              data base.
!
!     All of this information is contained in NAMFIL.  Place the
!     Place the availability data in OBCAVL.
!     Place the application data in OBCAPL.
!
!     Also get list of names for display and count of contribs.
!
!
!     If ICALIBOPT = 3 (station dependent calibrations)
!           (FLYBY)
!
!     First gather the station names.  Also, for each station/
!     calibration combination, gather data for:
!
!
!          1.  Whether or not the calibration is applied at the
!              station.
!
!     All of this information is contained in NAMFIL.  Place the
!     list of station names in JSITN (equivalenced to QSITN), and
!     count the stations.
!     Place the application data in JCAFFL.
!
!     Also get list of names for display and count of calibs.
!
!
      ERRSTAT = 'P'
      CALL GETCARD ( NUMDB, 'CLCT', INT2(1), JBUF, IERR )
      IF (IERR .NE. 0) GOTO 4000
      ERRSTAT = 'Q'
      READ (JBUF, "(4X,5(1X,I3),46X)", IOSTAT=IOS) &
     &     JNCAL, NFCAL, NZCAL, NOBCAL, L_CLM !need nzcal in case rewrite rec later
      CALL FERR ( INT2(IOS), "Reading CLCT card", INT2(0), INT2(0) )
!
      IF ( ICALIBOPT .EQ. 1  .OR.  ICALIBOPT .EQ. 3  .OR. &
     &     ICALIBOPT .EQ. 4                                ) THEN
!
! ------- Station dependent calibrations.  first get station names and
! ------- indication of which calibrations are applied/available there
!
          JNSTA = 0
          I = 1
!
          ERRSTAT = 'D'
          IF ( ICALIBOPT .EQ. 1 ) THEN
               CALL GETCARD ( NUMDB, 'CALS', INT2(1), JBUF, IERR )
             ELSE
               CALL GETCARD ( NUMDB, 'FCLS', INT2(1), JBUF, IERR )
          END IF
!
          IF (IERR .NE. 0) GOTO 4000
          DO WHILE (IERR .EQ. 0)
             ERRSTAT = 'E'
             IF ( ICALIBOPT .EQ. 1 ) THEN
                  READ (JBUF, FMT = 5000, IOSTAT=IOS) &
     &                 (JSITN(J, I), J = 1, 4), JSITI(I), JCAVAL(I), JCAPPL(I)
                  CALL FERR ( INT2(IOS), "ACCOR: Reading CALS card", INT2(0), &
     &                        INT2(0) )
 5000             FORMAT (5X, 4A2, 1X, 3I7, 35X)
               ELSE
                  READ ( JBUF, "(5X,4A2,7(1X,I7),1X)", IOSTAT=IOS) &
     &                 (JSITN(J, I), J = 1, 4), (JCAFFL(J,I),J= 1,7)
                  CALL FERR ( INT2(IOS), "ACCOR: Reading FCLS card", INT2(0), &
     &                        INT2(0) )
             END IF
!
             JNSTA = JNSTA + 1
             I = I + 1
             ERRSTAT = 'F'
             IF ( ICALIBOPT .EQ. 1 ) THEN
                  CALL GETCARD ( NUMDB, 'CALS', INT2(0), JBUF, IERR )
                ELSE
                  CALL GETCARD ( NUMDB, 'FCLS', INT2(0), JBUF, IERR )
              END IF
          END DO
          IF ( IERR .LE. -1 .AND. IERR .GE. -7 ) GOTO 4000
!
! ------- Now get list of calibration names, for display
!
          ERRSTAT = 'S'
          IF ( ICALIBOPT .EQ. 1 ) THEN
               LCARD = 'CALN'
               CALL GET_CLN_CARD ( NUMDB, LCARD, JNCAL, QDCAL, IERR )
             ELSE IF ( ICALIBOPT .EQ. 3 ) THEN  ! flyby calibration
               LCARD = 'FCLN'
               CALL GET_CLN_CARD ( NUMDB, LCARD, NFCAL, QDFCAL, IERR )
               IERR = 0
             ELSE IF ( ICALIBOPT .EQ. 4 ) THEN  ! partial
               LCARD = 'FCLN'
               IERR = 0
          END IF
          IF ( IERR .NE. 0 ) GOTO 4000
      END IF
!
      IF ( ICALIBOPT .EQ. 2 ) THEN
           ERRSTAT = 'G'
           CALL GETCARD ( NUMDB, 'CONT', INT2(1), JBUF, IERR )
           IF ( IERR .NE. 0 ) GOTO 4000
           ERRSTAT = 'H'
           READ ( JBUF, FMT=5001, IOSTAT=IOS ) OBCAVL, OBCAPL, MCAVL, MCAPL
           CALL FERR ( INT2(IOS), "ACCOR: Reading CONT card", INT2(0), INT2(0) )
 5001      FORMAT (5X, 2I7, 2I7, 37X)
!
! -------- Read card(s) with observation dependent calibration
!
           ERRSTAT = 'T'
           LCARD   = 'CNTN'
           CALL GET_CLN_CARD ( NUMDB, LCARD, NOBCAL, QDOBCAL, IERR )
           IF ( IERR .NE. 0 ) GOTO 4000
!
! -------- Read card with mode calibrations
!
           ERRSTAT = 'T'
           LCARD   = 'MCAL'
           CALL GET_CLN_CARD ( NUMDB, LCARD, L_CLM, MCALNAMS, IERR )
           IF ( IERR .NE. 0 ) GOTO 4000
      END IF
!
! --- Copy current application data into ICAPPL,ICAFFL or IOBCAPL, to
! --- detect any changes after SELCOR  or OBCOR or SELCORF finishes.
! --- If doing flyby cals, also note number of cals, in case SELCORF adds
! --- some
!
      CALL USE_COMMON ( 'ORC' )
      IF ( ICALIBOPT .EQ. 1 ) THEN
          SBAND_PHCAL = .FALSE.
          DO I = 1, JNSTA
             ICAPPL(I) = JCAPPL(I)
             PHC_STR(I) = '@@'
!
! ---------- Setting flags of phase calibration for X-band
!
             IF ( PHCAL_MODE(I) .EQ. PHC__UND ) THEN
                  PHC_STR(I)(1:1) = '?'
               ELSE IF ( PHCAL_MODE(I) .EQ. PHC__MAN ) THEN
                  PHC_STR(I)(1:1) = '-'
               ELSE IF ( PHCAL_MODE(I) .EQ. PHC__MSR ) THEN
                  PHC_STR(I)(1:1) = '*'
               ELSE IF ( PHCAL_MODE(I) .EQ. PHC__OFF ) THEN
                  PHC_STR(I)(1:1) = 'o'
               ELSE IF ( PHCAL_MODE(I) .EQ. PHC__MIX ) THEN
                  PHC_STR(I)(1:1) = 'x'
             END IF
!
! ---------- Setting flags of phase calibration for S-band
!
             IF ( PHCAL_MODE_S(I) .EQ. PHC__UND ) THEN
                  PHC_STR(I)(2:2) = '?'
               ELSE IF ( PHCAL_MODE_S(I) .EQ. PHC__MAN ) THEN
                  PHC_STR(I)(2:2) = '-'
                  SBAND_PHCAL = .TRUE.
               ELSE IF ( PHCAL_MODE_S(I) .EQ. PHC__MSR ) THEN
                  PHC_STR(I)(2:2) = '*'
                  SBAND_PHCAL = .TRUE.
               ELSE IF ( PHCAL_MODE_S(I) .EQ. PHC__OFF ) THEN
                  PHC_STR(I)(2:2) = 'o'
                  SBAND_PHCAL = .TRUE.
               ELSE IF ( PHCAL_MODE_S(I) .EQ. PHC__MIX ) THEN
                  PHC_STR(I)(2:2) = 'x'
                  SBAND_PHCAL = .TRUE.
             END IF
!
             IF ( .NOT. KBIT ( OPP_STATUS, OPP_SET2__BIT ) ) THEN
!
! --------------- Status has not been defined
!
                  PHC_STR(I)(2:2) = ' '
             END IF
         END DO
         IF ( .NOT. SBAND_PHCAL ) THEN
!
! ----------- If status of phase-cal at S-band for all station is not defined
! ----------- le'ts remove it entirely
!
              DO I = 1, JNSTA
                 PHC_STR(I)(2:2) = ' '
              END DO
         END IF
        ELSE IF (ICALIBOPT .EQ. 2) THEN
          IOBCAPL = OBCAPL
          MCAPL_ORIG = MCAPL
        ELSE IF (ICALIBOPT .EQ. 3) THEN  ! flyby
          DO I = 1,JNSTA
             DO J = 1,7
                ICAFFL(J,I) = JCAFFL(J,I)
             END DO
          END DO
          MFCAL = NFCAL
        ELSE IF (ICALIBOPT .EQ. 4) THEN  ! partial
      END IF
!
! --- If ICALIBOPT = 1 or 3, dealing with a calibration.
! --- Call SELCOR/SELCORF, the subs which present the user with a
! --- screen of calibration data and allows him to change whether or
! --- not calibrations are applied to stations for which they are
! --- available.  Or, if ICALIBOPT = 2, call OBCOR, which will allow
! --- the user to change whether or not available observation
! --- dependent contributions will be applied to the data base of
! --- observations.
!
! --- ACCOR does not use ISTAND at all;  this variable
! --- is INCLUDEd in the call to SELCOR for the benefit
! --- of SDBH, the other program which calls SELCOR.
!
      ISTAND = 1
!
777   CONTINUE
      IF ( ICALIBOPT .EQ. 1 ) THEN ! non-flyby
           PROGCOM = 'A'
           CALL SELCOR ( JCAVAL, JCAPPL, QDCAL, QSITN, JNSTA, JNCAL, ISTAND, &
     &                   PROGCOM, LDBNAM, NNVER, PHC_STR )
        ELSE IF ( ICALIBOPT .EQ. 2) THEN  ! observation dependent and
 710       CONTINUE
           CALL OBCOR ( OBCAVL, OBCAPL, QDOBCAL, NOBCAL, PROGCOM, LDBNAM, &
     &                  NNVER )
           IF ( PROGCOM .EQ. 'M' ) THEN
!
! ------------- Mode calibrations
!
                CALL MODCOR ( INT4(L_CLM), MCALNAMS, MCAVL, MCAPL, PROGCOM, &
     &               CDBNAM, INT4(NNVER) )
                IF ( PROGCOM .EQ. '+' ) GOTO 710
           END IF
        ELSE IF (ICALIBOPT .EQ. 3) THEN   ! flyby calibration
          PROGCOM = 'A'
          CALL SELCORF ( JCAFFL, QDFCAL, QSITN, JNSTA, NFCAL,  PROGCOM, LDBNAM, &
     &                   NNVER )
        ELSE IF (ICALIBOPT .EQ. 4) THEN   ! partial
          PROGCOM = 'A'
          CALL SELPART ( JPART, PARTIAL, QSITN, JNSTA, NUM_PART, PROGCOM, LDBNAM, &
     &                   NNVER )
          CALL USE_GLBFIL_4 ( 'ORC' )
      END IF
!
! --- Record any changes to the application data which the user
! --- requested in SELCOR or OBCOR.
!
! --- If ICALIBOPT = 1 or 3:
! --- Determine whether a change was made.  If a change was made, then
! --- to minimize file I/O, determine the last station for which the
! --- user changed application data, and don't rewrite any records
! --- after it.  All station dependent calibration records before
! --- that must be rewritten, even if they were not changed,
! --- because of the way PUTCARD is written.
!
! --- If ICALIBOPT = 2:
! --- Determine whether a change was made.
!
      CHANGEMADE = .FALSE.
!
      IF ( ICALIBOPT .EQ. 1 ) THEN
           MAXSTATCHANGED = 0
           DO I = 1, JNSTA
              IF ( JCAPPL(I) .NE. ICAPPL(I) ) THEN
                   MAXSTATCHANGED = I
              END IF
           END DO
           IF ( MAXSTATCHANGED .GT. 0 ) CHANGEMADE = .TRUE.
        ELSE IF ( ICALIBOPT .EQ. 2 ) THEN
           IF ( OBCAPL .NE. IOBCAPL    ) CHANGEMADE = .TRUE.
           IF ( MCAPL  .NE. MCAPL_ORIG ) CHANGEMADE = .TRUE.
        ELSE
           MAXSTATCHANGED = 0
           DO I = 1, JNSTA
              DO J = 1,7
                 IF ( JCAFFL(J,I) .NE. ICAFFL(J,I) ) THEN
                      MAXSTATCHANGED = I
                 END IF
              END DO
           END DO
           IF ( MAXSTATCHANGED .GT. 0 ) CHANGEMADE = .TRUE.
           IF ( NFCAL .GT. MFCAL ) CHANGEMADE = .TRUE.
      END IF
!
      IF ( .NOT. CHANGEMADE ) GOTO 89
!
! --- Enter the user's changes.
!
!     If ICALIBOPT = 1:
!     write new records for the last station for which the user
!     changed data and all stations before it.  Write all records
!     before it, even any stations for which no data was changed,
!     because of the way PUTCARD is written.
!
!     If ICALIBOPT = 2:
!     Then write the record for which the user changed data.
!
!
!     If ICALIBOPT = 3:
!
!     If calibration applications changed,
!     write new records for the last station for which the user
!     changed data and all stations before it.  Write all records
!     before it, even any stations for which no data was changed,
!     because of the way PUTCARD is written.
!     If number of calibrations changed, rewrite record which
!     counts the number of calibrations and records which store
!     the list of flyby calibrations.
!
      IF (ICALIBOPT .EQ. 2) THEN
        LCARD = 'CONT'
        ERRSTAT = 'I'
        WRITE ( JBUF, 5002, IOSTAT=IOS ) LCARD, OBCAVL, OBCAPL, MCAVL, MCAPL
 5002   FORMAT ( A4, 1X, 2I7, 2I7, 37X )
        ERRSTAT = 'J'
        CALL PUTCARD ( NUMDB, LCARD, INT2(1), JBUF, IERR )
        IF (IERR .NE. 0) GOTO 4000
      ELSE
        IF (ICALIBOPT .EQ. 1) THEN
          LCARD = 'CALS'
        ELSE
          LCARD = 'FCLS'
        END IF
        IERR = 0
        I = 1
        DO WHILE (I .LE. MAXSTATCHANGED .AND. IERR .EQ. 0)
          ERRSTAT = 'K'
          IF (ICALIBOPT .EQ. 1) THEN
            WRITE (JBUF, 5003, IOSTAT=IOS ) LCARD, &
     &        (JSITN(J, I), J = 1, 4), JSITI(I), JCAVAL(I), JCAPPL(I)
 5003       FORMAT (A4, 1X, 4A2, 1X, 3I7, 35X)
            IERR = IOS
          ELSE
            WRITE ( JBUF, "(A4,1X,4A2,7(1X,I7),1X)", IOSTAT=IOS) &
     &              LCARD, (JSITN(J, I), J = 1, 4),  (JCAFFL(J,I),J=1,7)
            IERR = IOS
          END IF
!
          IF (IERR .NE. 0) GOTO 110
          ERRSTAT = 'L'
          IF (I .EQ. 1) THEN
            CALL PUTCARD ( NUMDB, LCARD, INT2(1), JBUF, IERR )
          ELSE
            CALL PUTCARD ( NUMDB, LCARD, INT2(0), JBUF, IERR )
          END IF
          I = I + 1
  110     CONTINUE
        END DO
        IF (IERR .NE. 0) GOTO 4000
      END IF
!
      IF (ICALIBOPT .EQ. 3  .AND. &
     &     NFCAL .GT. MFCAL) THEN
!
!       User has added a flyby calibration.  Update number and
!       list of names
!
        LCARD = 'CLCT'
        ERRSTAT = 'B'
        WRITE ( JBUF, "(A4,5(1X,I3),46X)", IOSTAT=IOS ) LCARD, JNCAL, NFCAL, &
     &                                                  NZCAL, NOBCAL, L_CLM
        ERRSTAT = 'C'
        CALL PUTCARD ( NUMDB, LCARD, INT2(1), JBUF, IERR )
        IF (IERR .NE. 0) GOTO 4000
!
        LCARD = 'FCLN'
        LMODE = 1
        ERRSTAT = 'O'
        CALL PUT_CLN_CARD ( NUMDB, LCARD, LMODE, NFCAL, QDFCAL, IERR )
        IF ( IERR .NE. 0 ) GOTO 4000
      END IF
!
   89 CONTINUE
      IF (PROGCOM .EQ. 'N') THEN      !Next data base
        NUMDB = NUMDB + 1
        GOTO 100
      END IF
!
      IF (PROGCOM .EQ. 'P') THEN       !Previous data base
        NUMDB = NUMDB - 1
        IF (NUMDB .GE. 1) then
          GOTO 100
        else
          goto 777
        endif
      END IF
!
  90  CONTINUE
      CALL CLOSENAMFIL()
      GOTO 50
!
 4000 CONTINUE
      CALL SETCR_MN (I4P52,I4P21 )
      WRITE(bufstr, 5010) IERR, ERRSTAT
 5010 FORMAT ("RUN ERROR ", I3, " STATEMENT ", A1)
      call addstr_f(bufstr )
      call nl_mn()
      CALL PAUSE ( 'ACCOR' ) 
!
 50   CONTINUE
      IF (PROGCOM .EQ. '@') THEN
        ISQR = 3
      ELSE IF (PROGCOM .EQ. 'Q') THEN
        ISQR = 1
      ELSE
        ISQR = 0
      END IF
        if (progcom .eq. 'F') then  ! (F)lyby
           icalibopt = 3
           goto 79
        end if
        if (progcom .eq. 'T') then  ! par(T)ial
           icalibopt = 4
           goto 79
        end if
        if (progcom .eq. 'D') then  ! (D)atabase
           icalibopt = 1
           goto 79
        end if
51    CONTINUE
!
      CALL END_MN()
      CALL USE_BUFFER( ISQR, INT2(1), 'OWC' )
      CALL END_PROG()
      END  !#!  ACCOR  #!#
