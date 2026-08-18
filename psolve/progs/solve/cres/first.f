      SUBROUTINE FIRST ( LBUF_LEN, LBUF, IPTR, PAGEWID )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
!
! 1.  FIRST PROGRAM SPECIFICATION
!
! 1.1 This routine does a number of tasks, including zeroing out
!     several arrays, opening files, reading site and star names,
!     getting and displaying cable, ionosphere and weather information,
!     and writing the header for observation printouts.
!
! 1.2 REFERENCES:
!
! 2.  FIRST INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'astro_constants.i'
      INCLUDE 'solve.i'
      INCLUDE 'fclib.i'
!
!
! 2.2 INPUT Variables: None
!
      INTEGER*4   LBUF_LEN, IPTR, PAGEWID
      CHARACTER   LBUF(LBUF_LEN)*120
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'oborg.i'
      INCLUDE 'crecm.i'
      INCLUDE 'buff2.i'
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'precm.i'
      INCLUDE 'sareq.i'
      INCLUDE 'vtd.i'
      INCLUDE 'fast.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: cres
!       CALLED SUBROUTINES: adql
!
! 3.  LOCAL VARIABLES
!
      CHARACTER   ERRSTR*80, PART_QLIN*80, NFLY_QLIN*80, NFLYBY(MAX_ARC_STA)*80
      CHARACTER   QLIN*160, OUTBUF*160 , QLIND*18, NFLY_QLIND*18
      EQUIVALENCE ( QLIN, QLIND ), ( NFLY_QLIN, NFLY_QLIND )
      CHARACTER   JBUF*70 , CALC_SOLVE_VER*256
      CHARACTER   QCAL*8, QCALS(15)*8, QCONS(15)*8, MCALNAMS(15)*8
      INTEGER*2   LDISP(8), IROT, IROTT, OBCAVL, MCAVL, MCAPL
      INTEGER*2   LDBNAM(5,15), IDBVER(15), NSS(15)
      LOGICAL*2   KBIT, NOT_DONE
      LOGICAL*1   FL_GLOBAL_L1
      CHARACTER   CBUF*128, CBUF1*256
      INTEGER*2   IBUF(64), IBUF1(128)
      EQUIVALENCE (IBUF(1),CBUF), (IBUF1(1),CBUF1)
      EQUIVALENCE (QCAL, LDISP)
      INTEGER*2   I, IDB, IERR, IST, J, K, N, NCAL, NCON, NSSS, NUMDD, &
     &            NN, NNN, EVST, L_CLM
      INTEGER*2   ICT, IDUM1, JCAFFL(7,MAX_ARC_STA), NFCAL
      CHARACTER   LCARD*4, QFCALS(112)*8, QCALIDS(15)*8
      INTEGER*4   IOS, LENN
      INTEGER*2  NAPL, ICONT_I2, IERR_I2
      INTEGER*2  TRIMLEN
      LOGICAL*2 K_MN
      LOGICAL*4  CHECK_STABIT
      INTEGER*4  L_ACM
      REAL*8     CLOOF_ACM(M_ACM), CLODR_ACM(M_ACM)
      CHARACTER  PART_NAME(MAX_ARC_STA)*8, APCON(MAX_ARC_STA)*8, &
     &           STAT_ACM(M_ACM)*8, RELEASE_DATE*10, REVISION_DATE*10, &
     &           OUT*160, FAST_STR*79, STR*128
      CHARACTER  STR_FAST*32, STR1*20, STR_COV*13, JSITN_CHR*8
      LOGICAL*1  TITLE_ANON 
      INTEGER*4  J1, J2, J3, IP1, IP2, IUER
!
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   CEK  890223  Need WRITE statement for WVR mask in spool file
!   CEK  890207  Site dependent elevation cutoff logic added
!   MWH  900212  CALC version output to screen and spool.
!   JWR  900625  Rotation epoch put in sarfile record type 1.
!   AEE  910712  Added code so that elevation cutoff is written to
!                the screen also.
!   KDB  910717  New namfil/corfil scheme.  For one thing, getting all
!                calib/contrib info out of namfil, which also stores
!                flyby and non-flyby calibs separately
!   AEE  910719  Added solve version number read from SOLVE_VERSION file
!                in save_files to the spool file and screen.
!   AEE  910815  Placed CALC and SOLVE versions on the same line.
!   AEE  910826  Removed SOLVE_VERSION file to /mk3/bin so that it is
!                accessable by both gemini and draco.
!   AEE  911204  changed note for using wet partials.
!   AEE  920204  Replaced hard coded "solve_version" file name with
!                "version_file" from solve.i
!   AEE  920212  Changed comment for default wet partial.
!   AEE  920402  Seperated Calibrations and Atmosphere partials into
!                two different groups (tables) for output.
!   AEE  920627  partials now come from partial_array of glbc3.i
!   jwr  950721  Delay and rate print units changed to ps and fs/s.
!   kdb  951206  fix rate label in residuals printout (ps -> fs).
!   pet  970128  Added stuff disclosing fast_mode
!   pet  970210  Forced to print condition number regardless of mode
!   pet  970712  Supress printout when it called from REWAY
!   pet  970717  Added stuff disclosing fast_cov
!   pet  971007  Added condition number warning
!   pet  971128  Fixed bug when too many calibration was applied and it cause
!                overflowing of the string and therefore SOLVE abend.
!   pet  971202  Added logic for bypassing deselected station
!   BA   971215  "ptr_nc" removed from fc_gethostname usage, in order
!                to get working with current (July 23, 1997) version of
!                fc_gethostname.  Also improved overall fc_gethostname
!                usage.
!   pet  980326  Added printing values of a priori clock model in the case
!                if it has been applied
!   pet  980505  Added option for listing title
!   pet  980508  Corrected bug in making name of alternative version file
!   pet  980708  Changed logic to support the situation when the first arc(s)
!                was skipped by PROC and the ARC which was actually processed
!                has number greater than 1
!   pet  980904  Fixed a minor bug: was trying to write information about
!                a priori clock model in the 23-th channel regardless
!                whether the spool file was openned or not.
!   pet  990315  Improved comments. Removed u nused variables.
!   pet  990416  Corrected a minor bug: information about a priori clock
!                was put on the screen in batch mode
!   pet  990528  Made LBUF_LEN, LBUF, IPTR, PAGEWID formal arguments,
!                eliminated common block cres_buf
!   pet  1999.11.17  Added printing mode calibratin status
!   pet  1999.11.17  Changed a concept of SOLVE version. Now Solve version is
!                    determined as the date of the latest version among the
!                    programs to have been executing in the context of SOLVE
!   pet  2000.05.31  Rem ??
!   pet  2000.09.18  Forced to print header of the listing before in batch
!                    independent solution
!   pet  2001.06.19  Removed code for header of residuals part of the listing
!   pet  2002.03.28  Added call LISTING_OPTIONS -- subroutine which prints
!                    listing options
!   pet  2002.05.23  Added printing a revision date just after Listing options
!                    (Request of Karen Baver)
!   pet  2016.01.04  Added printing experiment code togeather with the database name
!
! 5.  FIRST PROGRAM STRUCTURE
!
!
! --- Setting flag of printing the ouput information at the screen using
! --- "_MN" inbterface (curses)
!
      K_MN = KSCREEN .AND. KBIT ( PRE_IP ( 2 ), INT2(6) ) ! Interactide mode
      IF ( KBIT ( PRE_IP ( 3 ), INT2(12)) .AND. REWAY_VERBOSE )  &
     &     K_MN = .FALSE.  ! But supress printout
!                          !  in silent REWAY mode
      IF ( KSPOOL ) THEN
#ifdef GNU
           IF ( .NOT. KBATCH ) THEN
                BACKSPACE ( 23 )
                WRITE ( 23, '(A)' ) ""
           END IF
#endif
           WRITE ( STR(1:15), "( 1H1, 'Run ', I5, '-', I4 )" ) IRNCD
           CALL BLANK_TO_ZERO ( STR(6:15) )
           WRITE ( 23, '(A)' ) STR(1:15)
      ENDIF
      IF ( CRES_STYLE .EQ. CRES__PRE98 ) THEN
           TITLE_ANON = .TRUE.
         ELSE
           TITLE_ANON = .FALSE.
      END IF
      IF  ( KSPOOL .AND. .NOT. KBATCH .AND. CRES_STYLE .EQ. CRES__PRE98 ) THEN
!
! --------- PRE98-style of header line
!
            WRITE ( 23, '(A50)' ) RUN_STRING
            WRITE ( 23, '(1X)'  )
            CALL CLRCH ( CBUF  )
            CALL CLRCH ( CBUF1 )
            CBUF = SOLVE_PROG_DIR
            IERR = FC_GETHOSTNAME ( IBUF1, LENN )
            IF ( IERR.NE.0 ) THEN
                 WRITE ( 23, "('*** ERROR in fc_gethostname, called ', &
     &                         ' from Solve CRES/first: ',I6)" ) IERR
                 CBUF1 = '(unknown system)'
            ENDIF
            WRITE ( 23, '("Run done on ",A," with executables from ",A)') &
     &              CBUF1(1:I_LEN(CBUF1)), CBUF(1:I_LEN(CBUF))
        ELSE IF ( KSPOOL .AND. ( .NOT. KBATCH  .OR.  ISLTY2 .EQ. 'I' )  .AND. &
     &            CRES_STYLE .NE. CRES__PRE98 ) THEN
!
! -------- Post 04-MAY-98 style of header line
!
           FL_GLOBAL_L1 = .FALSE.
           CALL SOLUTION_IDENT ( CRL__ASC, 23, ' ', FL_GLOBAL_L1, TITLE_ANON )
      END IF
!
! --- Zero out the BW, BF, and IBAS arrays
!
      DO I = 1, MAX_ARC_BSL
         DO J = 1, 3
            BW(J,I) = 0.D0
            BF(J,I) = 0.D0
         END DO
         DO K = 1, 6
            IBAS(K,I) = 0
         END DO
      END DO
!
! --- Zero out the SW, SF, and ISRC arrays
!
      DO I = 1, MAX_ARC_SRC
         DO J = 1, 3
            SW(J,I) = 0.D0
            SF(J,I) = 0.D0
         END DO
         DO K = 1, 5
            ISRC(K,I) = 0
         END DO
      END DO
!
      DO I=1,MAX_ARC_STA
!
! ------ Reset GOOD_SITE to bad all sites, i.e., false
!
         GOOD_SITE(I) = .FALSE.
!
! ------ Set JCAPPL to zero
!
         JCAPPL(I)    = 0
      ENDDO
!
! --- Don't fool with the res file in the batch mode.
!
      IF ( KBIT ( PRE_IP(3), INT2(12)) .OR. .NOT. KBATCH ) THEN
           CALL ACS_RESFIL ( 'O' )
      END IF
!
! --- Initialize the archive scratch file and save a priori posn
! --- file name, ecc file name, monument file name if they exist.
! --- use SARFIL only if in a batch solution of a batch mode: if in first arc
! --- of back solution (NB: among the arcs actually processed), then reset,
! --- otherwise, open and read the rec counter
!
      IF ( KBACKSL .AND. CRES_WORKED ) THEN
           CALL ACS_SARFIL('OL' )
        ELSE IF ( ( KBACKSL .AND. .NOT. CRES_WORKED ) .OR. &
     &             .NOT. KBATCH                        .OR. &
     &             .NOT. KBACKSL                            ) THEN
           CALL ACS_SARFIL ( 'OI' )
      END IF
!
! --- Display cable, weather, and ion corr (Mallama, JAN. 17, 1984)
! --- Get cable, ionosphere and weather information
! --- Open and read namfil
!
      CALL DBPOX ( NUMDD, LDBNAM, IDBVER, IDBEND )
!
      DO I = 1,5
         EOPLOT_DBNAM(I) = LDBNAM(I,1)
      END DO
      EOPLOT_DBVER = IDBVER(1)
!
! --- Write archive record type 1
!
      ITPR = 1
      IABF(2) = IRNCD(1)
      IABF(3) = IRNCD(2)
      IABF(4) = NUMDD
      IABF(5) = IDATYP
      IABF(6) = IGLBLS
      IABF(7) = IARCS
!
! --- Put the first rotation epoch into the buffer or sar record type #1.
! --- If ut1 turned on, overstore the fist rotation epoch
!
      DABF(3) = TROT(1)
      IROT = 1
      NOT_DONE = .TRUE.
      DO WHILE ( IROT .LE. NROT   .AND.  NOT_DONE )
         IF ( IROTT( IROT, INT2(3), INT2(1), LROT) .EQ. 1 ) THEN ! Turned on epoch found.
              DABF(3)  = TROT(IROT)
              NOT_DONE = .FALSE.
           ELSE
              IROT = IROT+1
         ENDIF
      ENDDO
!
      CALL USE_SARFIL ( 'W', -1 )
!
! --- Loop over data bases
!
      DO IDB = 1, NUMDD
         IF ( KBIT(IDBSEL,IDB) ) THEN ! This DB in solution
!
! ----------- Getting some namfil info, to be printed.
!
! ----------- Read station names and status array (array telling which
! ----------- non-flyby calibrations are applied)
!
             IST = 0
             IERR = 0
             NSS(IDB) = 0
             DO WHILE ( IERR .EQ. 0 )
                IST = IST + 1
                IF ( IST .EQ. 1) THEN
                     CALL GETCARD ( IDB, 'CALS', INT2(1), JBUF, IERR )
                  ELSE
                     CALL GETCARD ( IDB, 'CALS', INT2(0), JBUF, IERR )
                END IF
!
                IF ( IERR .NE. 0 ) THEN
                     IF ( IERR .EQ. 1 ) THEN
                          GOTO 8050
                       ELSE
                          WRITE ( ERRSTR, "('CRES FIRST: GETTING NAMFIL CALS ', &
     &                                      'CARD -- IERR = ',I10)") IERR
                              CALL FERR ( INT2(140), ERRSTR, INT2(0), INT2(0) )
                          GOTO 8050
                     END IF
                END IF
!
                IF ( IST .GT. MAX_ARC_STA ) THEN
                     CALL FERR ( INT2(141), &
     &                   'CRES FIRST: TOO MANY STATIONS IN NAMFIL'// &
     &                   ' CALS SECTION', INT2(0), INT2(0) )
                ENDIF
!
                READ ( JBUF, 9100, IOSTAT=IOS ) (JSITN(J,IST),J=1,4), &
     &                                           JSITI(IST), JCAPPL(IST)
                IF ( IOS .NE. 0 ) THEN 
                     CALL FERR ( INT2(IOS), "CRES(FIRST) Reading "// &
     &                   "NAMFIL CALS card", INT2(0), INT2(0) )
                END IF
9100            FORMAT (5X, 4A2, 1X, I7, 7X, I7, 35X)
                NSS(IDB) = NSS(IDB) + 1
8050        CONTINUE
          END DO
!
! ------- Read observation dependent contributions application
! ------- status array
!
          IERR=0
          CALL GETCARD ( IDB, 'CONT', INT2(1), JBUF, IERR )
          IF ( IERR .EQ. 0 ) THEN
               READ ( JBUF, FMT=9101, IOSTAT=IOS ) OBCAVL, OBCAPL, MCAVL, MCAPL
               IF ( IOS .NE. 0 ) THEN
                    CALL FERR ( INT2(IOS), "CRES(FIRST) Reading "// &
     &                  "NAMFIL CONT card", INT2(0), INT2(0) )
 9101               FORMAT ( 5X, 2I7, 2I7, 37X)
               END IF
             ELSE
               WRITE ( ERRSTR, "('CRES FIRST: GETTING NAMFIL CONT CARD -- ', &
     &                            'IERR = ',I10)") IERR
               CALL FERR ( INT2(142), ERRSTR, INT2(0), INT2(0) )
          END IF
!
! ------- Read array telling which flyby calibrations are applied
!
          IST = 0
          IERR = 0
          DO WHILE ( IERR .EQ. 0 )
             IST = IST + 1
             IF ( IST .EQ. 1 ) THEN
                  CALL GETCARD ( IDB, 'FCLS', INT2(1), JBUF, IERR )
                ELSE
                  CALL GETCARD ( IDB, 'FCLS', INT2(0), JBUF, IERR )
             END IF
!
             IF ( IERR .NE. 0 ) THEN
                  IF ( IERR .EQ. 1 ) THEN
                       GOTO 8051
                    ELSE
                       WRITE ( ERRSTR,"('CRES FIRST: GETTING NAMFIL FCLS CARD ', &
     &                                  '-- ERROR = ',I10)") IERR
                           CALL FERR ( INT2(240), ERRSTR, INT2(0), INT2(0) )
                       GOTO 8051
                  END IF
             END IF
!
             IF ( IST .GT. MAX_ARC_STA ) THEN
                  CALL FERR ( INT2(241), &
     &                'CRES FIRST: EXCESS STATIONS IN NAMFIL FCLS SECTION', &
     &                 INT2(0), INT2(0) )
             ENDIF
             READ ( JBUF, "(13X,7(1X,I7),1X)", IOSTAT=IOS) &
     &              ( JCAFFL(ICT,IST), ICT=1,7 )
             IF ( IOS .NE. 0 ) THEN
                  CALL FERR ( INT2(IOS), "CRES(FIRST) Reading NAMFIL "// &
     &                "FCLS card", INT2(0), INT2(0) )
             END IF
8051         CONTINUE
          END DO
!
! ------- Read in things which used to be gotten from corfil:
! ------- counts and display names for
! ------- flyby and non-flyby calibrations and contributions
!
          CALL GETCARD ( IDB, 'CLCT', INT2(1), JBUF, IERR )
          IF ( IERR .EQ. 0 ) THEN
               READ ( JBUF, "(4X,5(1X,I3),46X)", &
     &                IOSTAT=IOS)NCAL, NFCAL, IDUM1, NCON, L_CLM
               CALL FERR ( INT2(IOS), "Reading NAMFIL CLCT card", INT2(0), &
     &              INT2(0) )
             ELSE
               WRITE ( ERRSTR, "('CRES FIRST: GETTING NAMFIL CLCT CARD -- ', &
     &                'ERROR = ',I10)") IERR
               CALL FERR ( INT2(242), ERRSTR, INT2(0), INT2(0) )
          END IF
!
          IF ( NCAL .GT. 15 ) CALL FERR ( INT2(138), &
     &        'CRES FIRST: too many reg calibs', INT2(0), INT2(0) )
          IF ( NCON .GT. 15 ) CALL FERR ( INT2(139), 'CRES FIRST: too many '// &
     &        'contributions', INT2(0), INT2(0) )
          IF ( NFCAL .GT. 112 ) CALL FERR ( INT2(238), &
     &        'CRES FIRST: too many flyby calibs', INT2(0), INT2(0) )
!
          LCARD = 'CALN'
          CALL GET_CLN_CARD ( IDB, LCARD, NCAL, QCALS, IERR )
          IF ( IERR .NE. 0 ) THEN
               WRITE ( ERRSTR, "('CRES FIRST: GETTING NAMFIL REG CAL ', &
     &                           'NAME LIST -- ERROR = ',I10)" ) IERR
               CALL FERR ( INT2(243), ERRSTR, INT2(0), INT2(0) )
          END IF
!
          LCARD = 'FCLN'
          CALL GET_CLN_CARD ( IDB, LCARD, NFCAL, QFCALS, IERR )
          IF ( IERR.NE.0 ) THEN
               WRITE ( ERRSTR, "('CRES FIRST: GETTING NAMFIL FLYBY CAL ', &
     &                'NAME LIST -- ERROR = ',I10)") IERR
               CALL FERR ( INT2(244), ERRSTR, INT2(0), INT2(0) )
          END IF
!
          LCARD = 'CNTN'
          CALL GET_CLN_CARD ( IDB, LCARD, NCON, QCONS, IERR )
          IF ( IERR .NE. 0 ) THEN
               WRITE ( ERRSTR, "('CRES FIRST: GETTING NAMFIL CONTRIB ', &
     &                           ' NAME LIST -- ERROR = ',I10)") IERR
               CALL FERR ( INT2(245), ERRSTR, INT2(0), INT2(0) )
          END IF
!
          IF ( L_CLM .GT. 0 ) THEN
               LCARD = 'MCAL'
               CALL GET_CLN_CARD ( IDB, LCARD, L_CLM, MCALNAMS, IERR )
               IF ( IERR .NE. 0 ) THEN
                    WRITE ( ERRSTR, "('CRES FIRST: GETTING NAMFIL CONTRIB ', &
     &                                 ' NAME LIST -- ERROR = ',I10)") IERR
                    CALL FERR ( INT2(246), ERRSTR, INT2(0), INT2(0) )
               END IF
          END IF
!
! ------- Cres (third) will pass adjst the position of the chao calibration
! ------- in the NAMFIL arrays.  Figure this out right now.
!
          LCARD = 'CALI'
          CALL GET_CLN_CARD ( IDB, LCARD, NCAL, QCALIDS, IERR )
          IF ( IERR .NE. 0 ) THEN
               WRITE ( ERRSTR, "('CRES FIRST: GETTING NAMFIL REG CAL ID LIST', &
     &                           ' -- ERROR = ',I10)" ) IERR
               CALL FERR ( INT2(247), ERRSTR, INT2(0), INT2(0) )
          END IF
!
          LCHAO = 0
          DO ICT = 1,NCAL
             IF ( QCALIDS ( ICT ) .EQ. 'ATM CONT' ) LCHAO = ICT
          END DO
!
! ----- DISPLAY HEADINGS
!
!
! ----- Get the string with Solve version
!
        IUER = -1
        CALL GET_SOLVE_VERSION ( RELEASE_DATE, REVISION_DATE, IUER )
        IF ( IUER .NE. 0 ) THEN
             CALL FERR ( INT2(2840), &
     &           'CRES(first) Error in attempt to learn SOLVE version', &
     &            INT2(0), INT2(0) )
             STOP 'CRES(first) Abnormal termination'
        END IF
!
        IF (  KSPOOL  .AND.  EVINT .GT. 1 ) THEN
             EVST = EVSTART
             IF ( EVST.EQ.0 ) EVST = EVINT
             WRITE ( 23, '(/,"This solution includes one point of every", &
     &             I2," starting with point #",I2)') evint,evst
        ENDIF
!
        IF ( RCOND .GT. COND_WARNING  .AND. COND_WARNING .GT. 1.0D0 ) THEN
!
! ---------- Printing warning about condition number.
!
             IF ( KSPOOL ) THEN
                  WRITE ( 23, 115 )
 115              FORMAT ( 'WARNING!!! Condition number is too large. ', &
     &                     'Results may be corrupted. WARNING!!!' )
             END IF
             IF ( K_MN ) THEN
                  IPTR=IPTR+1
                  WRITE ( LBUF(IPTR), '(1X)' )
                  CALL NL_MN()
                  IPTR = IPTR+1
                  WRITE ( LBUF(IPTR), 115 )
                  CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                  CALL NL_MN()
                  IPTR=IPTR+1
             END IF
        END IF
!
        IF ( KSPOOL ) THEN
             IF ( ILEN(EXP_CODE) == 0 ) EXP_CODE=DBNAME_CH(3:10)
             WRITE ( 23, "(/,1X,'Data base ',5A2,' Ver',I3,2X,A)") &
     &               ( LDBNAM(J,IDB),J=1,5), IDBVER(IDB), EXP_CODE(1:I_LEN(EXP_CODE))
             WRITE ( 23, "(/,' Matrix Condition Number = ',1PE23.15/)") RCOND
!
! ---------- Now prepare the string with listing options
!
             CALL LISTING_OPTIONS ( 23 )
!@             WRITE ( 23, '(A)' ) ' Solve_release:    '//RELEASE_DATE
!@             WRITE ( 23, '(A)' ) ' Solve_revision:   '//REVISION_DATE
!
! ---------- This trick is to circumvent HP FORTRAN90 bug
!
             WRITE ( 23, '(A,A)' ) ' Solve_release:    ', RELEASE_DATE
             WRITE ( 23, '(A,A)' ) ' Solve_revision:   ', REVISION_DATE
        ENDIF
!
! ----- Printing database name
!
        IF ( K_MN ) THEN
             IPTR=IPTR+1
             WRITE ( LBUF(IPTR), '(1X)' )
             IPTR = IPTR+1
             WRITE ( LBUF(IPTR), &
     &               "(2X,'Data base ',5A2,' Ver',I3)")( LDBNAM(J,IDB),J=1,5),IDBVER(IDB)
             CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
             CALL NL_MN()
             IPTR=IPTR+1
             IF ( RCOND .NE. 0.D0 ) THEN
                  WRITE ( LBUF(IPTR), "('  Matrix condition number = ', &
     &                                     1PE23.16)" ) RCOND
                  CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                  CALL NL_MN()
                  IPTR=IPTR+1
             ENDIF
        END IF
!
! ----- Reading NAMFILE ASM cards -- information about a priori clock model
!
        ICONT_I2 = 1
        DO 410 J1=1,M_ACM
           CALL CLRCH   ( JBUF )
           CALL GETCARD ( IDB, 'ACM ', ICONT_I2, JBUF, IERR_I2 )
           ICONT_I2 = 0
           IF ( IERR_I2 .EQ. -3 ) THEN
!
! ------------- There were no such a card in NAMFIL.
!
                STAT_ACM(J1)   = '        '
                CLOOF_ACM(J1)  = 0.0
                CLODR_ACM(J1)  = 0.0
                L_ACM = 0
                ICONT_I2 = 1
              ELSE IF  ( IERR_I2 .NE. 0 ) THEN
!
! ------------- Error in reading NAMFIL
!
                WRITE ( 6, * ) ' ierr_i2 = ',ierr_i2,' j1=',j1
                CALL FERR ( INT2(2823), 'CRES(first) Error in reading of '// &
     &              'ACM card', INT2(0), INT2(0) )
                STOP 'CRES -- Abnormnal termination'
              ELSE
!
! ------------- Decoding a card
!
                IF ( JBUF(15:15) == CHAR(0) ) THEN
!
! ------------------ Fix corrupted card
!
                     CALL CLRCH ( JBUF(15:22) )
                END IF 
                READ ( JBUF, &
     &                '(5X,I2,1X,I2,1X,I2,1X,A8,1X,D23.15,1X,D23.15)' ) &
     &               IP1, L_ACM, IP2, STAT_ACM(J1), CLOOF_ACM(J1), CLODR_ACM(J1)
                IF ( ILEN(STAT_ACM(J1)) .EQ. 0 ) THEN
!
! ------------------ Card is empty
!
                     CLOOF_ACM(J1)  = 0.0
                     CLODR_ACM(J1)  = 0.0
                END IF
!
! ------------- Fix crazy numbers
!
                IF ( DABS(CLOOF_ACM(J1)) < 1.D-16 ) THEN
                     CLOOF_ACM(J1) = 0.0D0
                END IF
                IF ( DABS(CLOOF_ACM(J1)) > 1.D5 ) THEN
                     CLOOF_ACM(J1) = 0.0D0
                END IF
                IF ( DABS(CLODR_ACM(J1)) < 1.D-20 ) THEN
                     CLODR_ACM(J1) = 0.0D0
                END IF
                IF ( DABS(CLODR_ACM(J1)) > 1.D5 ) THEN
                     CLODR_ACM(J1) = 0.0D0
                END IF
           END IF
!
           IF ( J1 .LE. L_ACM ) THEN
                IF ( J1 .EQ. 1 ) THEN
                     IPTR=IPTR+1
                     IF ( KSPOOL ) THEN
!@                          WRITE ( 23, '(A)'    ) '  Apriori clock model '// &
!@     &                                           'applied before estimation:'
                          WRITE ( 23, '(A,A)'    ) '  A priori clock model ', &
     &                                             'applied before estimation:'
                     END IF
!
!@                     WRITE ( LBUF(IPTR), '(A)' ) '  A priori clock model: '// &
!@     &                                           'applied before estimation:'
                     WRITE ( LBUF(IPTR), '(A,A)' ) '  A priori clock model: ', &
     &                                             'applied before estimation:'
                     IF ( K_MN ) THEN
                          CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                          CALL NL_MN()
                     END IF
                END IF
!
                CALL CLRCH ( OUT )
!
! ------------- Printing station name
!
                OUT(4:11)  = STAT_ACM(J1)
                OUT(13:13) = ':'
!
! ------------- Printing value of clock shift
!
                IF ( CLOOF_ACM(J1) .EQ. 0.0 ) THEN
                     OUT(15:17) = '0.0'
                   ELSE
                     WRITE ( OUT(15:36), FMT='(1PE22.15,0P)' ) CLOOF_ACM(J1)
                     CALL CHASHL ( OUT(15:36) )
                END IF
                OUT(38:43) = 'sec   '
!
! ------------- Printing value of clock drift
!
                IF ( CLODR_ACM(J1) .EQ. 0.0 ) THEN
                     OUT(44:46) = '0.0'
                   ELSE
                     WRITE ( OUT(44:65), FMT='(1PE22.15,0P)' ) CLODR_ACM(J1)
                      CALL CHASHL ( OUT(44:65) )
                END IF
                OUT(67:73) = 'sec/sec'
!
                IPTR=IPTR+1
                IF ( KSPOOL ) THEN
                     WRITE ( 23, '(A)' ) OUT(2:73)
                END IF
                WRITE ( LBUF(IPTR), '(A)' ) OUT(2:73)
                IF ( K_MN ) THEN
                     CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                     CALL NL_MN()
                END IF
           END IF
 410    CONTINUE
        IF ( L_ACM .GT. 0 ) THEN
             IPTR=IPTR+1
             IF ( K_MN ) THEN
                  CALL NL_MN()
             END IF
        END IF
        IF ( K_MN ) THEN
             WRITE(LBUF(IPTR),'("  Flyby Calibrations:")')
             CALL ADDSTR_F(LBUF(IPTR)(:PAGEWID) )
             CALL NL_MN()
        END IF
!
        IF ( KSPOOL .AND. KFULLOUT ) WRITE(23,9051)
        IF ( KSPOOL .AND. KFULLOUT ) WRITE(23,9052)
 9051        FORMAT(/2X,"Flyby Station Cals:",4X,"DB Station Cals:", &
     &              14X,"| DB Non-station Cals: | Atmosphere Partial:")
 9052        FORMAT(1X,"---------------------------------------------", &
     &                 "-----------------------------------------------------")
!
! ----- Fill display buffers  and archive buffers
!
! ----- Obviously, things are done differently now that we have a new
! ----- calibration scheme.  See &SNARK for a discussion of the way the
! ----- solution archive handles both old and new systems.
! ----- (Ilana Stern, Feb 1986)
! ----- This scheme has been revised, as of 7/91.
! ----- Flyby and regular calibrations now separated.  KDB
!
        DO I=1,MAX_ARC_STA
           PART_NAME(I) = ' '
           APCON(I)     = ' '
        ENDDO
!
        NAPL=0
        DO I=1,NCON
           IF ( KBIT(OBCAPL,I) ) THEN
                NAPL=NAPL+1
                APCON(NAPL) = QCONS(I)
           ENDIF
        ENDDO
        IF ( PART_APPLIED .NE. 0 ) PART_NAME(1)=PART_ARRAY(PART_APPLIED)
!
        NSSS = NSS(IDB)
        DO IST = 1,MAX(NAPL,NSSS) ! Station status
!
! ---------- First tell the user which calibrations are applied
! ---------- Station IST has cal I applied if the Ith bit of JCAPPL(IST)
! ---------- or JCAFFL(1,IST) is displayed.  JCAFFL is used for flyby
! ---------- calibrations, JCAPPL for non-flyby.
! ---------- N keeps track of our position in output buffer.
!
             CALL CLRCH (      QLIN )
             CALL CLRCH ( NFLY_QLIN )
             CALL CLRCH ( PART_QLIN )
             N   = 1
             NN  = 1
             NNN = 1
             DO I = 1, NCAL
                IF ( KBIT (JCAPPL(IST), I) ) THEN
!
! ------------------ put name of cal in output buffer
!
                     CALL ADQL ( NNN, NFLY_QLIN, QCALS(I)//'  ' )
                ENDIF
             END DO
!
             DO I = 1, NFCAL
                IF ( KBIT (JCAFFL(1,IST), I) ) THEN
                     IF ( INDEX ( QFCALS(I), 'CFAJJDRY' ) .NE. 0 .OR. &
     &                    INDEX ( QFCALS(I), 'CFAKBWET' ) .NE. 0 .OR. &
     &                    INDEX ( QFCALS(I), 'CFAKBDRY' ) .NE. 0 .OR. &
     &                    INDEX ( QFCALS(I), 'MTTDRYSS' ) .NE. 0 .OR. &
     &                    INDEX ( QFCALS(I), 'MTTDRFLY' ) .NE. 0 .OR. &
     &                    INDEX ( QFCALS(I), 'IFADRYSS' ) .NE. 0 .OR. &
     &                    INDEX ( QFCALS(I), 'NMFDRFLY' ) .NE. 0 .OR. &
     &                    INDEX ( QFCALS(I), 'IFADRFLY' ) .NE. 0      ) THEN
!
! ---------------------- Put name of cals in
!
                         CALL ADQL ( N, QLIN, QFCALS(I)//'  ' )
                       ELSE ! non-flyby stuff
!
! ---------------------- Put name of nfly in
!
                         CALL ADQL ( NNN, NFLY_QLIN, QFCALS(I)//'  ' )
                     END IF
                ENDIF ! output buffer
             END DO
!
! --------- Store -(NCAL) in IABF(6) (instead of old cable flag) to
! --------- indicate that calibrations are under the new scheme.
! --------- Store JCAPPL(IST) in IABF(8) (instead of old tropo flag).
! --------- No change to ionosphere handling.
!
            IABF(6) = -NCAL
            IABF(8) = JCAPPL(IST)
!
! --------- Now handle the ionosphere
!
            IF ( KBIT( JSITI(IST), INT2(4) ) .AND. .NOT. KBIT( JSITI(IST), &
     &           INT2(5)) ) THEN
                 CALL ADQL ( NNN, NFLY_QLIN, 'GION' ) ! apply GION
                 IABF(7) = 1
            END IF
!
            IF ( .NOT. KBIT( JSITI(IST), INT2(4) ) .AND. KBIT( JSITI(IST), &
     &           INT2(5)) ) THEN
                 CALL ADQL ( NNN, NFLY_QLIN, 'PHION' ) ! apply PHION
                 IABF(7) = 2
            END IF
!
! --------- No ionosphere correction
!
            IF ( .NOT. KBIT( JSITI(IST), INT2(4) ) .AND. .NOT. &
     &           KBIT( JSITI(IST), INT2(5)) ) THEN
                 IABF(7) = 0
            END IF
!
! --------- Remember last character
!
            N   = MAX(N-1,1)
            NN  = MAX(NN-1,1)
            NNN = MAX(NNN-1,1)
!
! --------- Display calibrations
!
            IF ( KFULLOUT .AND. K_MN ) THEN
                 IF ( ILEN(QLIN(1:N)) .GT. 0   .AND.  IST .LE. NUMSTA ) THEN
                      WRITE ( JSITN_CHR, '(4A2)' ) (JSITN(J,IST),J=1,4)
!
                      IPTR=IPTR+1
                      LBUF(IPTR) = '  '//JSITN_CHR//':  '//QLIN(1:N)
                      CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                      CALL NL_MN()
                 END IF
            ENDIF
!
            IF ( NNN .GT. LEN(NFLYBY(IST))-13 ) NNN = LEN(NFLYBY(IST))-13 ! pet
            WRITE ( NFLYBY(IST), 9074) (JSITN(J,IST),J=1,4), NFLY_QLIN(1:NNN)
            OUTBUF=' '
            IF ( IST .LE. NSSS ) THEN
                 WRITE(OUTBUF,9073) (JSITN(J,IST),J=1,4)
                 OUTBUF(12:) = QLIN(1:N)
                 OUTBUF(24:) = NFLY_QLIN(1:NNN)
            ENDIF
!
            OUTBUF(54:) = '| '//APCON(IST)
            OUTBUF(77:) = '| '//PART_NAME(IST)
            IF ( KSPOOL .AND. KFULLOUT ) WRITE(23,9076) outbuf(:100)
 9073       FORMAT(2X,4A2,":  ")
 9074       FORMAT (2X,4A2,":  ",A)
 9075       FORMAT (2X,4A2,":  ",A,10X,A)
 9076       FORMAT (2X,A)
        END DO !station status
!
        IF ( KSPOOL ) WRITE ( 23, 9052 )
!
! ----- Write non_flyby calibrations to screen and spool file:
!
        IF ( K_MN ) THEN
             IPTR=IPTR+1
             WRITE ( LBUF(IPTR), '(1X)' )
!
             CALL NL_MN()
             IPTR = IPTR+1
             WRITE ( LBUF(IPTR), '("  Database Calibrations:")' )
             CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
             CALL NL_MN()
        ENDIF
        DO IST=1,NSSS
           IF ( KFULLOUT .AND. K_MN ) THEN
                IPTR = IPTR+1
                WRITE ( LBUF(IPTR), '(A)' ) NFLYBY(IST)
                CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                CALL NL_MN()
           END IF
        END DO
!
! ----- Write partials to screen and spool file (?)
!
! ----- Test added by JMGipson to keep array checking from screwing up
!
        IF ( PART_APPLIED .NE. 0 ) THEN
             IF ( K_MN ) THEN
                  iptr=iptr+1
                  write(lbuf(iptr),'(1X)')
                  call nl_mn()
                  iptr=iptr+1
                  WRITE(lbuf(iptr),9090) part_array(part_applied)
                  call addstr_f(lbuf(iptr)(:pagewid) )
                  call nl_mn()
              ENDIF
        ENDIF    !if(part
 9090   FORMAT (2X, "Atmosphere Partial: ",A8)
!
! ----- Display which observation dependent contributions are applied.
!
! ----- The data base has contribution I applied if bit I of OBCAPL is set.
!
        QLIN(1:1)=' '
        N = 1
        DO I = 1, NCON
           IF ( KBIT ( OBCAPL, I) ) THEN
                CALL ADQL ( N, QLIN, QCONS(I)//'  ' )
           END IF
        END DO
        N=MAX(N-1,1)
!
        IF ( MCAPL .NE. 0 ) THEN
             IPTR=IPTR+1
             LBUF(IPTR) = '  Mode calibrations:'
             IF ( KFULLOUT .AND. K_MN ) THEN
                  CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                  CALL NL_MN()
             ENDIF
             IF ( KSPOOL ) WRITE ( 23, '(A)' ) LBUF(IPTR)
!
! ---------- Display mode calibrations
!
             DO I=1,L_CLM
                IF ( KBIT ( MCAPL, I ) ) THEN
                     IPTR = IPTR + 1
                     LBUF(IPTR) = '    '//MCALNAMS(I)
!
                     IF ( KFULLOUT .AND. K_MN ) THEN
                          CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                          CALL NL_MN()
                     ENDIF
                     IF ( KSPOOL ) WRITE ( 23, '(A)' ) LBUF(IPTR)
                END IF
             ENDDO
!
             IF ( KSPOOL ) WRITE ( 23, 9052 )
        END IF
!
! ----- Putting stuff in the string describing fast_mode
!
        CALL CLRCH ( STR_FAST )
        STR_FAST = 'unknown'
        DO 420 J2=1,FM_VAR
           IF ( FAST_MODE  .EQ. FM_VAL(J2) ) THEN
                CALL CLRCH ( STR_FAST )
                STR_FAST = FM_STR(J2)
           END IF
 420    CONTINUE
        IF ( SOLVE_EMULATION .NE. 0 ) THEN
             CALL CLRCH ( STR1 )
             CALL INCH  ( SOLVE_EMULATION, STR1 )
             STR_FAST(I_LEN(STR_FAST)+1:) = '/SOLVE_EMULATION='//STR1
        END IF
!
! ----- Putting stuff in the string describing fast_cov
!
        CALL CLRCH ( STR_COV )
        IF ( FAST_MODE  .NE. F__NONE ) THEN
           DO 430 J3=1,FC_VAR
              IF ( FAST_COV  .EQ. FC_VAL(J3) ) THEN
                   STR_COV = 'Fast_cov: '//FC_ABR(J3)
              END IF
 430       CONTINUE
        END IF
!
! ----- Formating line with versions of Calc and Solve
!
        CALL CLRCH ( FAST_STR )
        CALL CLRCH ( CALC_SOLVE_VER )
        IF ( ILEN(REVISION_DATE) .GT. 0       .AND. &
     &       REVISION_DATE .NE. RELEASE_DATE        ) THEN
             IF ( CALCV > 0.0 ) THEN
                  WRITE ( CALC_SOLVE_VER, 210 ) CALCV, &
     &                                    RELEASE_DATE(1:I_LEN(RELEASE_DATE)), &
     &                                    REVISION_DATE(1:I_LEN(REVISION_DATE))
 210              FORMAT ( " CALC Version: ", F6.2, "  SOLVE release: ", A, &
     &                     "  SOLVE revision: ", A )
                ELSE 
                  WRITE ( CALC_SOLVE_VER, 216 ) VTD__LABEL, &
     &                                          RELEASE_DATE(1:I_LEN(RELEASE_DATE)), &
     &                                          REVISION_DATE(1:I_LEN(REVISION_DATE))
 216              FORMAT ( 1X,A, "  SOLVE release: ",A, " SOLVE revision: ",A )
             END IF
           ELSE
!@             WRITE ( CALC_SOLVE_VER, 220 ) CALCV, &
!@     &                                     RELEASE_DATE(1:I_LEN(RELEASE_DATE))
!@ 220         FORMAT ( " CALC Version: ",F6.2,"  SOLVE release ",A )
               WRITE ( CALC_SOLVE_VER, 218 ) VTD__LABEL, &
     &                 RELEASE_DATE(1:I_LEN(RELEASE_DATE))
 218           FORMAT ( 1X,A, "  SOLVE release: ",A )
        END IF
        IF ( CALC_SOLVE_VER(21:21) .EQ. '0' ) CALC_SOLVE_VER(21:21) = ' '
!
! ----- Formatting the line with fast-mode
!
        FAST_STR = ' Fast_mode: '//STR_FAST(1:I_LEN(STR_FAST))//'  '// &
     &              STR_COV(1:I_LEN(STR_COV))
        IF ( KSPOOL ) THEN
             WRITE ( 23, FMT='(A)' ) CALC_SOLVE_VER(1:I_LEN(CALC_SOLVE_VER))
             IF ( CALCV < 0.0 ) THEN
                  WRITE ( 23, FMT='(A)' ) ' VTD_CONF: '// &
     &                                     VTD_CONF_SES(1:I_LEN(VTD_CONF_SES))
             END IF
             WRITE ( 23, FMT='(A)' ) FAST_STR(1:78)
        ENDIF
!
        IF ( K_MN ) THEN
!
! ---------- Printing the list of database calibrations in a short screen format
!
             IPTR=IPTR+1
             WRITE ( LBUF(IPTR), '(1X)' )
             CALL NL_MN()
!
             IPTR=IPTR+1
             WRITE ( LBUF(IPTR), '("  DATA BASE: ",A) ' ) QLIN(1:60)
             CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
             CALL NL_MN()
!
             IF ( ILEN(QLIN) .GT. 60 ) THEN
                  IPTR=IPTR+1
                  WRITE ( LBUF(IPTR), '("             ",A) ' ) QLIN(61:120)
                  CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                  CALL NL_MN()
             ENDIF
!
             IF ( ILEN(QLIN) .GT. 120 ) THEN
                  IPTR=IPTR+1
                  WRITE ( LBUF(IPTR), '("             ",A) ' ) QLIN(121:)
                  CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                  CALL NL_MN()
             ENDIF
!
             IPTR=IPTR+1
             WRITE ( LBUF(IPTR), FMT='(1X,A)' ) CALC_SOLVE_VER(1:79)
             CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
             CALL NL_MN()
!
             IPTR=IPTR+1
             WRITE ( LBUF(IPTR), FMT='(2X,A)' ) FAST_STR(1:79)
             CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
             CALL NL_MN()
        ENDIF
!
        DO I=1,NUMSTA
!
! -------- Test: was the station deselected?
!
           IF ( .NOT. CHECK_STABIT ( I ) ) THEN
!
! ------------- Station was deselcted
!
                IF ( K_MN ) THEN
                   IPTR=IPTR+1
                   WRITE ( LBUF(IPTR), '(" Station ",4A2," was deselected")') &
     &                    (ISITN(J,I),J=1,4)
                   CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                   CALL NL_MN()
                END IF
             ELSE
!
! ------------- Station was in solution
!
                IF ( K_MN ) THEN
                    IF ( ELVCUT(I) .GT. .001 ) THEN
                         IPTR=IPTR+1
                         WRITE ( lbuf(iptr), &
     &                          '(" Elevation cut for ",4A2," is ",F4.1)') &
     &                           (ISITN(J,I),J=1,4), ELVCUT(I)*180./PI__NUM
                         CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                         CALL NL_MN()
                     ENDIF
!
                     IF ( WVMASK(I) .NE. 0 ) THEN
                          IPTR=IPTR+1
                          WRITE ( LBUF(IPTR), &
     &                           '("   WVR Bitmask for ",4A2," is ",O7, &
     &                             " (octal) ")') &
     &                    (ISITN(J,I),J=1,4), wvmask(I)
                          CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                          CALL NL_MN()
                     ENDIF
                END IF
!
                IF ( KSPOOL ) THEN
                     IF ( ELVCUT(I) .gt. .001 ) THEN
                          WRITE(23, '(" Elevation cut for ",4A2," is ",F4.1)') &
     &                      (ISITN(J,I),J=1,4), ELVCUT(I)*180./PI__NUM
                     END IF
                     IF ( WVMASK(I).NE.0 ) THEN
                          WRITE(23,'("   WVR Bitmask for ",4A2," is ",O7, &
     &                               " (octal) ")') &
     &                                (ISITN(J,I),J=1,4), wvmask(I)
                     END IF
                END IF
           END IF
        END DO
!
! ----- Now save the calibration list (from CORFIL) to the solution
! ----- archive, so that we don't depend on the CORFIL remaining
! ----- unchanged.  This will be 1 or 2 records depending on NCAL.
!
       ELSE  ! This DB not in solution
          IF ( K_MN ) THEN
               IPTR=IPTR+1
               WRITE ( LBUF(IPTR), '(1X)' )
               CALL NL_MN()
               IPTR=IPTR+1
               WRITE ( LBUF(IPTR), 9055) (LDBNAM(J,IDB),J=1,5), IDBVER(IDB)
               CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
               CALL NL_MN()
          ENDIF
 9055     FORMAT (10X, 5A2, I3, 2X, "NOT IN SOLUTION")
          IF ( KSPOOL .AND. KFULLOUT ) WRITE(23, &
     &         9056)(LDBNAM(J,IDB),J=1,5), IDBVER(IDB)
 9056     FORMAT (/10X, 5A2, I3, 2X, "NOT IN SOLUTION")
        END IF
      END DO
!
      RETURN
      END  !#!  FIRST  #!#
