      SUBROUTINE GEROT ( LNAME, KFBDSP, YODER_COND )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GEROT PROGRAM SPECIFICATION
!
! 1.1 Read a UT1/PM data substitution (mod) file
!
! 1.2 REFERENCES:
!
! 2.  GEROT INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) LNAME
!
! LNAME - Name of Earth orientation mod file
! UT1INB - UT1 info from data base (1st JD, interval, # of epochs)
! WOBINB - Polar motion info fr data base (1st JD, interval, # of epochs)
!
! 2.3 OUTPUT Variables:
!
! UT1PTV - UT1 values from E.O. mod file
! UT1SIG - Sigmas of UT1 values
! WOBXXV - X-wobble values from mod file
! WBXSIG - Sigmas of X-wobble values
! WOBYYV - Y-wobble values from mod file
! WBYSIG - Sigmas of Y-wobble values
! WXUCOR - Correlation between X component and UT1
! WXYCOR - Correlation between X and Y component
! WYUCOR - Correlation between Y component and UT1
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INCLUDE 'socom.i'
      INCLUDE 'glbc4.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES:
!       CALLED SUBROUTINES: FERR, obstm, closenamfil
!
! 3.  LOCAL VARIABLES
!
      REAL*8 UT,DAY
      CHARACTER CARD*75, SIGNATURE*15, SIGN_LAST*15
      PARAMETER  ( SIGN_LAST = 'EOP-MOD Ver 2.0' )
      LOGICAL*2 KFBDSP,kbit, found
      INTEGER*2 NUM, I, J
      INTEGER*4 IOS, IOS_NEW
      INTEGER*4 NREC, NREC_BAD, LREC, J1
      REAL*8 FSTJD,INC,XP,YP,XPSG,YPSG,UTSG,XYCR,XUCR,YUCR
      real*8 fjdobs,ljdobs
      character*4 Yoder_cond
      CHARACTER   ERRSTR*130, BUFSTR*80, STR*76, EOP_TYPE*8
      INTEGER*2   UNIT40, ICOM, GETUNIT
      logical*2 mimic_calc
      data unit40 /0/
      INTEGER*2 INT2_ARG
!
!  Yoder_cond - Yoder short period status of mod file. "UT1 " = full
!               UT1 (short period terms left in); "UT1R" = short period
!               terms removed.
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   jwr  881109  Added status info to OPEN
!   DG   910701  Modified to read in UT1/PM series of arbitrary interval,
!                added common block GLBC4
!   DG   910702  Increased mod file arrays from 6 to 7 points (UT1PTV,
!                WOBXXV,WOBYYV,WBXSIG,WBYSIG,UT1SIG,WXYCOR,WXUCOR,WYUCOR)
!   DG   910823  Yoder_cond set for old mod files to UT1R
!   DG   910826  Special fix for short data bases that don't span a midnight.
!   :93.12.15:jwr: Modified to pick 15 points for cubic spline interpolation,
!                but will pick up 15 point for all types of interpolation.
!                the added points do not effect the linear or simple cubic
!                interpolation.
!   :94.01:28:jwr: Modified so that when calc used 15 or more points for interpolation,
!                  the list of points extracted for flyby interpolation is based on
!                  the starting point and number of points used by calc. When calc used
!                  less than 15, then this routine selects its own 15 points.  This
!                  guarentees that when the mod file is exactly the same file that
!                  calc used and when the ut1r/ut1s configuration and interpolator are
!                  the same, then using a mod file and no mod file will give identical
!                  results.
!     1999.01.08   pet  Updated comments
!     2000.10.04   pet  Added suppot of header comments in EOP mod file.
!                       Header comments are allowed just after the first line.
!                       Header comments are the lines with # the first character
!     2001.01.10   pet  Changed the logic for support of EOP-MOD ver 2.0 format
!                       The old format is not supported any more.
!     2001.01.11   pet  Checnged EOP type default. If the header is not "UT1 ",
!                       "UT1-", "UT1R" it is set to "UT1S"
!     2004.05.03   jwr  Changed some error handling to eliminate 'ferr'.
!
!   GEROT Program structure
!
!CCCC
!
! --- Get times of first and last observation from NAMFIL
!
      CALL OBSTM ( FJDOBS, LJDOBS )
      CALL CLOSENAMFIL()
!
! --- Open Earth orientation mod file.
!
      IF ( UNIT40 .EQ. 0 ) UNIT40 = GETUNIT()
      OPEN ( UNIT40, IOSTAT=IOS, FILE=LNAME, ACCESS='DIRECT', STATUS='OLD', &
     &       FORM='FORMATTED', RECL=76 )
      IF ( IOS .NE. 0 ) THEN
           ERRSTR = 'GEROT: Failure in opening eop-mod file '//LNAME
           CALL FERR ( INT2(179), ERRSTR, INT2(0), INT2(0) )
      ENDIF
!
      IF ( KFBDSP ) THEN
           IF ( KSPOOL ) WRITE(23,5514) LNAME
 5514      FORMAT ( " Earth orientation UT1/PM values from file ",A)
           IF ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6)) ) THEN
                WRITE ( BUFSTR, 5514 ) LNAME
                CALL ADDSTR_F ( BUFSTR(1:79) )
                CALL NL_MN()
           ENDIF
      END IF
!
! --- Read the first record of the substitution file.
!
      READ ( UNIT40, FMT=102, REC=1, IOSTAT=IOS ) SIGNATURE, FSTJD, INC, NUM, &
     &       EOP_TYPE, EOP_TS_MODF
 102  FORMAT ( A15, 2X, F9.1, 2X, F4.1, 2X, I5, 2X, A8, 2X, A8 )
      IF ( SIGNATURE .EQ. 'EOP-MOD Ver 2.0' ) THEN
           CONTINUE
         ELSE
           READ ( UNIT40, FMT='(A)', REC=1, IOSTAT=IOS ) STR
           WRITE ( 7, '(A)' ) 'Your mod-file is not in EOP-MOD ver 2.0 format'
           WRITE ( 7, '(A)' ) 'The first line is: '
           WRITE ( 7, '(A)' ) STR(1:75)
           WRITE ( 7, '(A)' ) 'Probably your file is too old. Try to use '// &
     &                        'program upgrade_eopmod'
           WRITE ( 7, '(A)' ) 'for transforming it to '//SIGN_LAST
           ERRSTR = 'GEROT: Unsupported format of eop-mod file '//LNAME
           CALL FERR ( INT2(218), ERRSTR, INT2(0), INT2(0) )
           STOP 'Abnormal termination'
      END IF
!
! --- Transform time-scale to the letters of upper registr
!
      CALL TRAN ( 11, EOP_TS_MODF, EOP_TS_MODF )
!
      IF ( EOP_TS_MODF .EQ. UTC__TS   .OR. &
     &     EOP_TS_MODF .EQ. UT1__TS   .OR. &
     &     EOP_TS_MODF .EQ. UT1R__TS  .OR. &
     &     EOP_TS_MODF .EQ. UT1S__TS  .OR. &
     &     EOP_TS_MODF .EQ. TDB__TS   .OR. &
     &     EOP_TS_MODF .EQ. TDT__TS   .OR. &
     &     EOP_TS_MODF .EQ. TAI__TS   .OR. &
     &     EOP_TS_MODF .EQ. TCG__TS   .OR. &
     &     EOP_TS_MODF .EQ. TCB__TS   .OR. &
     &     EOP_TS_MODF .EQ. UNDEF__TS      ) THEN
           CONTINUE
         ELSE
           ERRSTR = 'GEROT: wrong timescale: '//EOP_TS_MODF//' in mod-file '// &
     &               LNAME
           CALL FERR ( INT2(220), ERRSTR, INT2(0), INT2(0) )
           STOP 'Abnormal termination'
      END IF
      IF ( IOS .NE. 0 ) THEN
           NREC_BAD = 1
           GOTO 900
      END IF
      ICOM = 1
!
! --- Read line by line and determine how many lines we have to skip due to
! --- comments
!
      DO 410 J1=2,1024
         READ ( UNIT40, FMT='(A)', REC=J1, IOSTAT=IOS ) STR
!
! ------ Check whether the J1-th line is a comment of the line wth data
!
         IF ( STR(1:1) .NE. '#' .AND. STR(13:13) .EQ. '.'  .AND. &
     &        STR(21:21) .EQ. '.' ) THEN
              ICOM = INT2(J1-1)  ! Line with data. We should also skip
              GOTO 810           !                 the first line
         END IF
 410  CONTINUE
 810  CONTINUE
!
! --- Old mod files don't have the Yoder_cond flag. Default is UT1S
!
      IF ( EOP_TYPE(1:4) .EQ. 'UT1 ' .OR. &
     &     EOP_TYPE(1:4) .EQ. 'UT1-'      ) THEN
!
           YODER_COND = 'UT1 '
         ELSE IF ( EOP_TYPE(1:4) .EQ. 'UT1R' ) THEN
           YODER_COND = 'UT1R'
         ELSE IF ( EOP_TYPE(1:4) .EQ. 'UT1S' ) THEN
           YODER_COND = 'UT1S'
         ELSE
           YODER_COND = 'UT1S'
      ENDIF
!
! --- Compute starting position in mod file.
!
      IF ( UT1INB(3) .GT. 14 ) THEN ! Get the points with the same time as calc
           MIMIC_CALC = .TRUE.
           NREC = ( (UT1INB(1)+0.001D0 - FSTJD)/INC ) + ICOM
        ELSE ! Calc used less than 15, so can't match.
           MIMIC_CALC = .FALSE.
           NREC = (FJDOBS - FSTJD +1.D0)/INC - 8 + ICOM
      ENDIF
!
! --- Check for database times to early or too late for the mod file
! --- Too early:
!
      IF ( NREC .LT. ICOM+1 ) THEN
           WRITE ( 6, * ) ' NREC=',NREC, ' ICOM= ',ICOM
           WRITE ( 6, * ) ' UT1INB(1) = ', UT1INB(1)
           WRITE ( 6, * ) ' FSTJD = ', FSTJD
           ERRSTR = ' Database too early for mod file, in subroutine gerot. '// &
     &              ' Mod-file '//LNAME
           CLOSE ( UNIT40 )
           CALL FERR ( INT2(182), ERRSTR, INT2(0), INT2(0) )
      ENDIF
!
! --- Too late:
!
      IF ( MIMIC_CALC ) THEN
           LREC = NREC + UT1INB(3) - 1
         ELSE
           LREC = NREC + 14
      ENDIF
!
      IF ( LREC .GT. (NUM+ICOM) ) THEN
           ERRSTR = 'GEROT: Database too late for mod file '//LNAME
           CLOSE(UNIT40)
           IF ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6)) ) THEN
                CALL CLEAR_MN()
                CALL REFRESH_MN()
           ENDIF
           CALL FERR ( INT2(181), ERRSTR, INT2(0), INT2(0) )
      ENDIF
!
! --- Load mod file increment and number of points to be read
!
      UT1INV(2) = INC
      WOBINV(2) = INC
      IF ( MIMIC_CALC ) THEN
           UT1INV(3) = UT1INB(3)
           WOBINV(3) = UT1INB(3)
         ELSE
           UT1INV(3) = 15
           WOBINV(3) = 15
      ENDIF
!
! --- Make certain the tables are not too big for the array space
!
      IF ( UT1INV(3) .GT. MAX_FLYBY_EOP_VALUES  .OR. &
     &     UT1INV(3) .GT. MAX_FLYBY_EOP_VALUES        ) THEN
           ERRSTR = 'GEROT: Number of flyby eop values exceeds '// &
     &              'table size. Quitting!'
           CLOSE ( UNIT40 )
           CALL FERR ( INT2(183), ERRSTR, INT2(0), INT2(0) )
      ENDIF
!
! --- Start getting data from the mod file.
!
      FOUND = .FALSE.
      J = NREC
      IF ( MIMIC_CALC ) THEN
           DO WHILE ( .NOT. FOUND )
              READ ( UNIT40, FMT='(A)', REC=J, IOSTAT=IOS ) CARD
              READ ( CARD, *, IOSTAT=IOS ) DAY
              IF ( IOS .NE. 0 ) THEN
                   NREC_BAD = J
                   GOTO 900
              END IF
              IF ( ABS(DAY-UT1INB(1)) .LT. 0.1 ) THEN ! Match
                   NREC = J
                   FOUND = .TRUE.
                ELSE
                   IF ( DAY .GT. UT1INB(1) ) THEN
                        J = J-1
                      ELSE
                        J = J+1
                   ENDIF
              ENDIF
           ENDDO
      ENDIF
!
! --- Compute ending position
!
      IF ( MIMIC_CALC ) LREC = NREC + UT1INB(3) - 1
!
      I = 0
      DO J = NREC, LREC
         READ ( UNIT40, FMT='(A)', REC=J, IOSTAT=IOS ) CARD
         IF ( IOS .NE. 0 ) THEN
              NREC_BAD = J
              GOTO 900
         END IF
!
! ------ Please note that the following read is now list-directed, and so the
! ------ exact format is not prescribed.  However, each record must still be
! ------ exactly 76 characters long.  mwh, 10/18/94
!
         READ ( CARD, *, IOSTAT=IOS ) DAY, XP, YP, UT, XPSG, YPSG, UTSG, &
     &                                XYCR, XUCR, YUCR
         IF ( IOS .NE. 0 ) THEN
              NREC_BAD = J
              GOTO 900
         END IF
!
         I=I+1
         WOBXXV(I) = XP *100.D0
         WOBYYV(I) = YP *100.D0
         WBXSIG(I) = DABS(XPSG)
         WBYSIG(I) = DABS(YPSG)
         UT1PTV(I) = -UT / 1.D6     ! flip sign to get TAI-UT1
         UT1SIG(I) = DABS(UTSG)
         WXYCOR(I) = XYCR
         WXUCOR(I) = XUCR
         WYUCOR(I) = YUCR
         IF ( I .EQ. 1 ) THEN
              UT1INV(1)=DAY
              WOBINV(1)=DAY
         ENDIF
      ENDDO
      GOTO 999
!
! --- Error handling
!
  900 CONTINUE
      IF ( IOS.EQ.922) THEN
           CALL FERR ( INT2(IOS), 'GEROT: EOP mod file ends too early', &
     &                 INT2(0), INT2(0) )
         ELSE
           WRITE ( 6, * ) 'Gerot: NREC = ', NREC, ' LREC=',LREC
           WRITE ( 6, * ) 'Gerot: Line_number: ',NREC_BAD, ' IOS= ', IOS
           CALL CLRCH ( CARD )
           READ ( UNIT40, FMT='(A)', REC=NREC_BAD, IOSTAT=IOS_NEW ) CARD
           WRITE ( 6, * ) 'Line: ',CARD
           CALL FERR ( INT2(IOS_NEW), 'GEROT: Reading EOP mod file '//LNAME, &
     &                 INT2(0), INT2(0) )
           LNAME = 'NONE'
      ENDIF
!
! --- Close the substitution file.
!
  999 CONTINUE
      CLOSE ( UNIT40 )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!  write ( 6, * ) ' '         ! %%%%
!  write ( 6, * ) ' YODER_COND = ', YODER_COND  ! %%%%
!  write ( 6, * ) ' '         ! %%%%
!  call pause ( 'gerot' ) ! %%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      RETURN
      END  !#!  GEROT  #!#
