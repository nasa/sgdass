      SUBROUTINE STFLG ( ICONT, NPMAX, MAX_GCLOCK_DEG )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  STFLG PROGRAM SPECIFICATION
!
! 1.1 Display and set the site-dependent parameter flags.
!
! 1.2 REFERENCES:
!
! 2.  STFLG INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
      INCLUDE 'fclib.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 ICONT, IISTA
      INTEGER*4 MAX_GCLOCK_DEG, MAX_GCLOCK_DEG0, I5
!
! ICONT - Site number of first station to process
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'erm.i'
      INCLUDE 'socom.i'
      INCLUDE 'socom_plus.i'
      INCLUDE 'prfil.i'
      INCLUDE 'precm.i'
      INCLUDE 'glbc4.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: setfl
!       CALLED SUBROUTINES: dclk,datm,epoch,autoc,reset_b_cl,reset_b_at,autoa
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2 ICONTN, KBIT,FAIL, continue
      LOGICAL*2 DFUZZ
      REAL*8    TJD, TEPOC, HOURS, MINUTES, SEC, FJD8, LJD8, FJD_BEG, FJD_END
      INTEGER*2 EPOCH_INTERVAL, IERR, IDELCK, JCHAR, IYR, &
     &          JATM, NPCT, ISET, KLOC, IPOS, NLINE, KBITN, &
     &          IATM, ISKIP, ISNCSN, K, K1, ILAST, IMIN, IHR, ID, IM, KK, &
     &          JCLOCK, ITIME, I, IORD, IUHR, L, J, ICHR, IPAGEA, NUMPGA, &
     &          ISTART, IEND, LISTLEN, IPAGEC, NUMPGC, DECIMALTOINT, &
     &          NUM_TRYS
      INTEGER*4 NPMAX
      INTEGER*2 J1, J2, K2, ISEG
      INTEGER*4 IOS, IER
      INTEGER*4 IX, IY, ICH, NCLOCK, NCNT, IXX, IYY, IATLN, PAGELEN, PAGEWID
      INTEGER*4 XLAST,YLAST
      CHARACTER CCH*4, CCHAR*2 
      EQUIVALENCE (ICH,CCH)
      EQUIVALENCE (ICHR,CCHAR)
!
      INTEGER*2 IDSP(16), NUMBREAKS, BREAK(MAX_CLK)
      CHARACTER CDSP*32, MODA*1, MODC*1, BUFSTR*79, QUOTE*1, BLANK*1
      EQUIVALENCE (IDSP,CDSP)
!
      INTEGER*2    INT2_2AA
      PARAMETER  ( INT2_2AA = 2H"" )
      DATA QUOTE /'"'/
      DATA BLANK /' '/
      CHARACTER  JD_TO_DATE*23
      LOGICAL*4  STA_BRK(MAX_ARC_STA), CHECK_STABIT
      INTEGER*2    INT2_ARG
      INTEGER*4    INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      CHARACTER  STR*54, GET_VERSION*54
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
! 4.  HISTORY
!
!     J. RYAN - 81/9/30 TO PRINT YEAR, MONTH, DAY
!     B.R. SCHUPLER 820311 - MAKE CLOCK FLAG GUESSES 3 TERMS + SIN/COS
!     K.J.COAKLEY 83-3-3  FTN4 TO FTN4X LINE 1
!     K.J.COAKLEY 83-3-11 .GET. TO .GE. IN LINE 210 LOGICAL IF STMNT
!     J. RYAN - 83-10 31 CLOCK AND ATMOSPHERE INSERT AND DELETE INTERACTION
!        CLEANED UP. STATION MOTION REMOVED.
!     J. RYAN - 84-2-24 MORE WORK ON CLOCK AND ATMOSPHERE DELETES
!     M.K.  86-11-05  Continued atmospheres and atmosphere rates added.
!     JWR   87-02-05  Small change the initial cursor for deletes.
!     KDB   87-05-27  Uniform PARFIL access.
!     ejh   88.08.28  made a subroutine to SETFL
!     jwr   88.11.30  batch-mode print logic improved.
!     mwh   91.04.05  Allow resetting clocks, atmospheres for single station
!     mwh   91.07.23  Keep clock breaks when batch mode clocks are set up,
!                     Display breaks and allow their flags to be set in
!                     batch mode. (Breaks apply to individual stations)
!     kdb   95.05.18  The database merging mode sets up an automatic clock
!                     parameterization with multiple reference sites.
!                     Flag these on the site pages.
!     kdb   96.04.16  Pass the program version date from the caller to
!                     eliminate multiple dates requiring updates.
!     jwr   96.05.01  Logic added to handle cleaning the problem of
!                     multiple epoch (for different stations) at nearly
!                     the identical time.
!     kdb   96.12.23  Add ability to turn off gradients at all sites
!                     with a single keystroke (G).
!     pet   97.07.12  Changed defaults for max gclock polinom degree
!     pet   97.07.17  Made a lot of changes to fIx bugs with
!                     inserting/deleting clock breaks epochs
!     pet   97.08.14  Improved interface. Added Batch-clock,
!                     Batch-atmposphere as default. Added use of global
!                     variable from glbc4.i SETFL_MDEG
!     pet   97.08.19  Added capacity (M) to change max degree of global
!                     clock polynomials
!     pet   97.09.03  Bug fixed with setting CLK_BRK_STAT flag. Changed
!                     format of messages about clock breaks
!     pet   97.10.07  Epoch bug corrected.
!     pet   97.10.27  EPO_OCT97 bug fixed. I added re-setting auto-clock
!                     epochs after inserting or deleting a clock break.
!     jwr   97.11.20  Multiple reference clock stations added for use
!                     with merged superfiles.
!     pet   97.11.24  Updated comments. Corrected a bug connected with
!                     picking clock reference station.
!     pet   97.12.03  Added logic for bypassing deselected stations
!     pet   98.03.27  Corrected a bug: previous version forced PROC to abend
!                     when fast_mode was changed from B3D to FULL due to
!                     change of atmosphere/clock parameterization
!     pet   98.03.30  Corrected a bug: array sta_brk in some modes appeared
!                     to be not initialized
!     pet   98.08.06  Added printing start time tag, end time tag and duration
!                     of the experiment with precision up to 1 millisecond
!                     at the site page of SETFL.
!     pet   98.08.19  Corrected a bug in calcualtion start date, end data and
!                     duration of the experiment connected with rundung the
!                     lase minute.
!     pet   98.09.24  Corrected a minor bug in formatting nominal duration date.
!     pet   98.12.24  Added support of default duration of time span for linear
!                     spline for atmopshere and clocks in dialogue with user.
!     pet   2000.01.20  Changed format of information messages: leading zeroes
!                       are prionted in years/mounths/days/hours/minutes,
!                       f.e.  "00/01/20 09:01"  instead of  "  / 1/20  9: 1"
!     pet   2003.12.09  Replaced fc_gwinw, fc_gwinsz  calls with GET_TERMSIZE
!
! 5.  STFLG PROGRAM STRUCTURE
!
      ISET  = 0
      IDELCK = 0
      IPAGEA=1
      IPAGEC=1
      CALL GET_TERMSIZE ( PAGELEN, PAGEWID ) 
      IF ( PAGELEN .EQ. 0 ) PAGELEN=24
      LISTLEN=(PAGELEN-14)/2
!
! --- Dummy initialization of STA_BRK array. We assume that there are no
! --- clock breaks in non-batch mode clocks
!
      DO I=1,NUMSTA
         STA_BRK(I) = .FALSE.
      ENDDO
!
      IF(INDL.LT.0 .OR. INDL.GT.3) INDL = 2
!
  20  IF(ICONT.LE.0 .OR. ICONT.GT.NUMSTA) ICONT = 1
      DO WHILE (ICONT.GT.0 .AND. ICONT.LE.NUMSTA)
!
! --- DO BEGIN present site menus
!
      CONTINUE
!
! --- Clear the display
!
  50  CONTINUE
      CALL SET_CURSOR ( 0, 0, XLAST, YLAST )
      CALL CLEAR_MN()
!
! --- Write the site name
!
      IF ( CHECK_STABIT ( ICONT ) ) THEN
           WRITE ( BUFSTR, 75 ) (ISITN(L,ICONT),L=1,4), ICONT, NUMSTA, PRE_LETRS
           CALL ADDSTR_F ( BUFSTR )
           CALL NL_MN()
           WRITE ( BUFSTR, 76 )
   76      FORMAT ( 45X )
           CALL ADDSTR_F ( BUFSTR(:45) )
   75      FORMAT(1X,4A2," Parameter flags",19X,"Site # ",I2,'(',I2,')',3X, &
     &                   'User ',A2)
           STR = GET_VERSION()
           CALL SETCR_MN ( 79-I_LEN(STR), 0 )
           CALL REVERSE_ON_MN()
           CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
           CALL REVERSE_OFF_MN()
           CALL NL_MN()
           CALL NL_MN()
!
           DO J=1,3
              IDSP(J)=KBITN(LSITEC(1,J),ICONT)
           END DO
           WRITE ( BUFSTR, 125 ) (IDSP(L),L=1,3)
  125      FORMAT(" X Y Z                ",8X,3I2)
           CALL ADDSTR_F ( BUFSTR )
           CALL NL_MN()
           CALL NL_MN()
         ELSE
           CALL CLRCH ( BUFSTR )
           WRITE ( BUFSTR, 78 ) (ISITN(L,ICONT),L=1,4), ICONT, NUMSTA
 78        FORMAT ( 1X, 'Station  ', 4A2, '  ', I2, '(', I2, &
     &                  ')  is deselected' )
           CALL ADDSTR_F ( BUFSTR(:56) )
           STR = GET_VERSION()
           CALL SETCR_MN ( 79-I_LEN(STR), 0 )
           CALL REVERSE_ON_MN()
           CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
           CALL REVERSE_OFF_MN()
           CALL NL_MN()
           CALL NL_MN()
           CALL NL_MN()
           CALL NL_MN()
           CALL NL_MN()
           GOTO 710
      END IF
!
      IDSP(1)=KBITN(LAXOF,ICONT)
      HOURS = GRAD_INTERVAL
      IF ( NUMGRAD(ICONT) .EQ. 0 ) HOURS = 0.0D0
      IF ( HOURS .GT. 0.01d0 ) THEN
           WRITE ( bufstr, 140 ) IDSP(1), HOURS
  140      FORMAT ( "  Axis offset ",16X,I2, &
     &              "  Atmosphere Gradient Interval:",F6.2, " hours" )
         ELSE
           WRITE ( BUFSTR, 141 ) IDSP(1)
  141      FORMAT ( " Axis offset ",17X,I2," Atmosphere Gradients: NONE" )
      ENDIF
      CALL ADDSTR_F(BUFSTR )
      CALL NL_MN()
!
   55 CONTINUE
      CALL SET_CURSOR ( 0, 5, XLAST, YLAST )
      IF ( IDELCK .EQ. 0  .AND.  .NOT. BMODE_CL ) THEN
           CALL NL_MN()
           CALL ADDSTR_F ( " Clock polynomials" )
           CALL NL_MN()
           NLINE = 6
         ELSE
           NLINE = 5
      ENDIF
!
      NCLOCK=NLINE
      IF ( .NOT. BMODE_CL ) then ! Old style clocks
!
         NUMPGC = 1 + (NUMCLK(ICONT)-1)/LISTLEN
         ISTART = (IPAGEC-1)*LISTLEN+1
         IEND = MIN0 ( INT4(NUMCLK(ICONT)), ISTART+LISTLEN-1 )
!
! ------ Dummy initialization of STA_BRK array. We assume that there are no
! ------ clock breaks in non-batch mode clocks
!
         DO I=1,NUMSTA
            STA_BRK(I) = .FALSE.
         ENDDO
         DO IUHR = ISTART, IEND !running over the epochs for this station
            JCLOCK = IUHR + ICLSTR(ICONT)
!
! --------- Handle the polynomial coefficients
!
            DO IORD = 1,5
               IDSP(IORD) = KBITN(LCLK(JCLOCK),IORD)
            ENDDO
!
! --------- Handle the diurnal coefficients
!
            DO I=14,16
               STR(1:2) = '**'
               READ ( UNIT=STR(1:2), FMT='(A2) ') IDSP(I)
            END DO
            IF ( KBIT( LCLK(JCLOCK), INT2(14)) ) THEN
                 STR = '""'
                 READ ( UNIT=STR(1:2), FMT='(A2)' ) IDSP(14)
            END IF
            IF ( KBIT( LCLK(JCLOCK), INT2(15)) ) THEN
                 STR = 'CC'
                 READ ( UNIT=STR(1:2), FMT='(A2)' ) IDSP(15)
            END IF
            IF ( KBIT( LCLK(JCLOCK), INT2(15)) ) THEN
                 STR = 'SS'
                 READ ( UNIT=STR(1:2), FMT='(A2)' ) IDSP(16)
            END IF
!
           FJD8 = 0.5 + AINT ( FJDCL(JCLOCK) - 0.5 + 2.0D-5 )
           CALL MDYJL(IM,ID,IYR,ITIME,FJD8 )
           FJD8 = FJD8 - 2.0D-5
           IHR  = (FJDCL(JCLOCK)-FJD8) * 24.0D0 + .00000001D0
           IMIN = ((FJDCL(JCLOCK)-FJD8) - IHR/24.0D0) * 1440.0D0 + 0.00000001
!
           IF ( IDELCK.EQ.0 ) THEN ! Printing
              IF ( .NOT.KBIT( LCLK(JCLOCK), INT2(13)) ) THEN ! Not continued offset
                   WRITE ( BUFSTR, 175 ) IYR, IM, ID, IHR, IMIN, &
     &                                   (IDSP(KK),KK=1,5), &
     &                                   (IDSP(KK),KK=14,16)
                   CALL BLANK_TO_ZERO ( BUFSTR(2:9)   )
                   CALL BLANK_TO_ZERO ( BUFSTR(11:15) )
                   CALL ADDSTR_F ( BUFSTR )
                   CALL NL_MN()
                ELSE !continued offset
                   WRITE ( bufstr, 176) IYR, IM, ID, IHR, IMIN, QUOTE, &
     &                                  (IDSP(KK),KK=2,5), &
     &                                  (IDSP(KK),KK=14,16)
                   CALL BLANK_TO_ZERO ( BUFSTR(2:9)   )
                   CALL BLANK_TO_ZERO ( BUFSTR(11:15) )
                   CALL ADDSTR_F ( BUFSTR )
                   CALL NL_MN()
              END IF
           ENDIF ! Printing
  175      FORMAT(1X,2(I2,"/"),I2,1X,I2,":",I2,15X,5I2,1X,A1,1X,A1,1X,A1,8X,F7.4)
  176      FORMAT(1X,2(I2,"/"),I2,1X,I2,":",I2,16X,A1,4I2,1X,A1,1X,A1,1X,A1)
           NLINE = NLINE + 1
!
! -------- Save the line number where the atmosphere info starts.
!
           NCLOCK = NLINE+1
        ENDDO ! Running over the epochs for this station
      ENDIF ! Old style clocks
!
      IF ( BMODE_CL ) THEN
         NCLOCK=NCLOCK+2
         IF ( ICONT .EQ. BM_REF_CL ) THEN
              CALL NL_MN()
              CALL ADDSTR_F ( " Reference site for batch mode clocks" )
              CALL NL_MN()
           ELSE IF ( .NOT. (KBIT(ICLSTA(1,1),ICONT)) ) THEN
!
! ----------- The clock in position bm_ref_cl is the official reference site
! ----------- recognized by socom.
! ----------- However, databases set up in the sdbh database merging mode will
! ----------- also have unofficial reference sites set. This site is one of them
!
             CALL NL_MN()
             CALL ADDSTR_F ( " Unofficial clock reference site " )
             CALL NL_MN()
           ELSE
             MINUTES = CLOCK_INTERVAL * 60.d0
             CALL NL_MN()
             WRITE ( BUFSTR, "( &
     &              ' Batch mode clocks:            # Epochs =', I4, &
     &              '    Interval =',F8.1,' minutes')") NUMCLK(ICONT), MINUTES
             CALL ADDSTR_F ( BUFSTR )
             CALL NL_MN()
        ENDIF
!
        NLINE  = NCLOCK+1
        NUMBREAKS = 0
!!   write ( 6 ,* ) 'STFLG-346  icont= ', icont ! %%%%%
        DO IUHR = 2,NUMCLK(ICONT) !running over the epochs for this station
           JCLOCK = IUHR + ICLSTR(ICONT)
           IF ( .NOT. KBIT( LCLK(JCLOCK), INT2(13)) .AND. &
     &          KBIT(ICLSTA(1, JCLOCK),ICONT)             ) THEN
!
! ----------- Handle the polynomial coefficients
!
              NUMBREAKS = NUMBREAKS + 1
              BREAK(NUMBREAKS) = JCLOCK
              DO IORD = 1,5
                 IDSP(IORD) = KBITN(LCLK(JCLOCK),IORD)
              ENDDO
!
! ----------- Handle the diurnal coefficients
!
              DO I=14,16
                 STR(1:2) = '**'
                 READ ( UNIT=STR(1:2), FMT='(A2) ') IDSP(I)
              END DO
              IF ( KBIT( LCLK(JCLOCK), INT2(14)) ) THEN
                   STR = '""'
                   READ ( UNIT=STR(1:2), FMT='(A2)' ) IDSP(14)
              END IF
              IF ( KBIT( LCLK(JCLOCK), INT2(15)) ) THEN
                   STR = 'CC'
                  READ ( UNIT=STR(1:2), FMT='(A2)' ) IDSP(15)
              END IF
              IF ( KBIT( LCLK(JCLOCK), INT2(15)) ) THEN
                   STR = 'SS'
                   READ ( UNIT=STR(1:2), FMT='(A2)' ) IDSP(16)
              END IF
!
              FJD8 = 0.5D0 + AINT(FJDCL(JCLOCK) - 0.5D0 + 2.0D-5 )
              CALL MDYJL(IM,ID,IYR,ITIME,FJD8 )
              FJD8 = FJD8 - 2.D-5
              IHR  = (FJDCL(JCLOCK)-FJD8) * 24.0D0 + .00000001D0
              IMIN = ((FJDCL(JCLOCK)-FJD8) - IHR/24.0D0) * &
     &                1440.0D0+ 0.00000001
!
              IF ( IDELCK.EQ.0 ) THEN ! Printing
                 IF ( NUMBREAKS .EQ. 1 ) THEN
                      CALL ADDSTR_F ( " Clock breaks:" )
                      CALL NL_MN()
                 END IF
                 CALL OBSTM ( FJD_BEG, FJD_END )
                 IF ( BMODE_CL ) THEN
                      WRITE ( BUFSTR, 275 ) IYR, IM, ID, IHR, IMIN, &
     &                                      ( FJDCL(JCLOCK) - FJD_BEG )*24.0D0
  275                 FORMAT ( 1X, 2(I2,"/"), I2, 1X, I2, ":", I2, &
     &                         8X, F7.4, ' hours' )
                    ELSE
                      WRITE ( BUFSTR, 175 ) IYR, IM, ID, IHR, IMIN, &
     &                      ( IDSP(KK),KK=1,5 ), ( IDSP(KK),KK=14,16 ), &
     &                      ( FJDCL(JCLOCK) - FJD_BEG )*24.0D0
                 END IF
                 CALL BLANK_TO_ZERO ( BUFSTR(2:9)   )
                 CALL BLANK_TO_ZERO ( BUFSTR(11:15) )
                 CALL ADDSTR_F ( BUFSTR )
                 CALL NL_MN()
                 NLINE = NLINE + 1
              ENDIF ! Printing
!
! ----------- Save the line number where the atmosphere info starts.
!
           ENDIF
        ENDDO
        IF ( NUMBREAKS .GT. 0 ) THEN
             CLK_BRK_STAT = .TRUE.
           ELSE
             CLK_BRK_STAT = .FALSE.
             DO 410 J1=1,NUMSTA
                STA_BRK(J1) = .FALSE.
                DO 420 J2=2,NUMCLK(J1)
                   K2 = J2 + ICLSTR(J1)
!
! ---------------- Test bit of participation of the clocks of the j1-th
! ---------------- station in estimation
!
                   IF ( KBIT(ICLSTA(1,K2), J1) ) THEN
                        IF ( .NOT. KBIT( LCLK(K2), INT2(13)) .AND. &
     &                       KBIT(ICLSTA(1,K2),J1)        ) THEN
!
                             CLK_BRK_STAT = .TRUE.
                             STA_BRK(J1)  = .TRUE.
                        END IF
                   END IF
 420            CONTINUE
 410         CONTINUE
!
             IF ( NUMBREAKS .EQ. 0 .AND. CLK_BRK_STAT ) THEN
                  CALL ADDSTR_F ( " No clock breaks for this station" )
               ELSE IF ( NUMBREAKS .EQ. 0 .AND. .NOT. CLK_BRK_STAT ) THEN
                  CALL ADDSTR_F ( " No clock breaks for this database" )
             END IF
!
             CALL NL_MN()
        ENDIF
!
        NCLOCK = NLINE
      ENDIF
!
! --- Start atmosphere logic
!
  205 CONTINUE
      CALL SET_CURSOR ( 0, NCLOCK, XLAST, YLAST )
      NLINE = NCLOCK-1
      IF(IDELCK.EQ.0) then
        IF(.NOT.BMODE_AT) THEN
          call nl_mn()
          call addstr_f(" Atmosphere parameters " )
          call nl_mn()
          NLINE = NLINE + 2
        ELSE
          NLINE = NLINE + 1
        ENDIF
      ENDIF
      IATLN = NLINE
!
      IF ( .NOT. BMODE_AT ) THEN
           NUMPGA = 1 + (NUMATM(ICONT)-1)/LISTLEN
           ISTART=(IPAGEA-1)*LISTLEN+1
           IEND=MIN0( INT4(NUMATM(ICONT)), ISTART+LISTLEN-1 )
           DO IATM = istart,iend ! running the atmosphere epochs
              JATM = IATM+IATSTR(ICONT)
              CALL EPOC(IM,ID,IYR,IHR,IMIN,TATM(JATM) )
              IDSP(1) = KBITN(LATM(1,1),JATM)
              IDSP(2) = KBITN(LATM(1,2),JATM)
              IF ( IDELCK .EQ. 0 ) THEN  !printing
                   IF ( .NOT. KBIT(LATM(1,3),JATM) )  THEN  !not a continued epoch
                        WRITE ( BUFSTR, 245 ) IYR,IM,ID,IHR,IMIN,IDSP(1),IDSP(2)
                        CALL BLANK_TO_ZERO ( BUFSTR(2:9)   )
                        CALL BLANK_TO_ZERO ( BUFSTR(11:15) )
                        CALL ADDSTR_F(BUFSTR )
                        CALL NL_MN()
  245                   FORMAT(1X,2(I2,"/"),I2,1X,I2,":",I2,15X,2I2)
                     ELSE  !a continued epoch
                        WRITE(bufstr,246) IYR,IM,ID,IHR,IMIN,QUOTE,IDSP(2)
                        CALL BLANK_TO_ZERO ( BUFSTR(2:9)   )
                        CALL BLANK_TO_ZERO ( BUFSTR(11:15) )
                        CALL ADDSTR_F ( BUFSTR )
                        CALL NL_MN()
  246                   FORMAT(1X,2(I2,"/"),I2,1X,I2,":",I2,16X,A1,I2)
                   END IF
              END IF
              NLINE = NLINE + 1
           END DO
         ELSE
           IF ( NUMATM(ICONT) .LE. 1 ) THEN
                MINUTES = 0.D0
              ELSE
                MINUTES = ATMOS_INTERVAL*60.d0
           ENDIF
           CALL NL_MN()
!
           WRITE ( BUFSTR, "( &
     &            ' Batch mode atmospheres:       # Epochs =', &
     &            I4,'    Interval =',F8.1,' minutes')") &
     &            NUMATM(ICONT), MINUTES
           CALL ADDSTR_F ( BUFSTR )
           CALL NL_MN()
           NLINE = NLINE + 1
      ENDIF
!
! --- Set up the control line
!
      NCNT = NLINE + 4
      IF(IDELCK.EQ.0) then
        call nl_mn()
        if (bmode_cl) then
          write(bufstr,'( &
     &   "             (C)lock   (A)tmosphere   Troposphere (G)radients")')
          IF(INDL.EQ.0.and.IDELCK.EQ.0) call &
     &       addstr_f("(*)Insert:   (C)lock     (A)tmosphere   Troposphere (G)radients")
          IF(INDL.EQ.1.and.IDELCK.EQ.0) call &
     &       addstr_f("(*)Delete:   (C)lock     (A)tmosphere   Troposphere (G)radients")
          IF(INDL.EQ.2.and.IDELCK.EQ.0) call &
     &       addstr_f("(*)Automatic:(C)lock     (A)tmosphere   Troposphere (G)radients")
          IF(INDL.EQ.3.and.IDELCK.EQ.0) call &
     &       addstr_f("(*)Reset:    (C)lock     (A)tmosphere   Troposphere (G)radients")
        else
          call &
     &         addstr_f("             (C)lock     (A)tmosphere  (+)/(-):Next/Prev Clocks" )
        endif
        call nl_mn()
        IF ( BMODE_AT ) THEN
             WRITE ( BUFSTR(1:2), FMT='(I2)' ) MAX_GCLOCK_DEG
             CALL ADDSTR_F ( "Page:(N)ext  (P)revious  (S)ource       "// &
     &                       "(M)ax global clock poly degree:"//BUFSTR(1:2) )
          ELSE
             CALL ADDSTR_F ( "Page:(N)ext  (P)revious  (S)ource   "// &
     &                       "(>)/(<):Next/Prev Atm" )
        ENDIF
        GOTO 715
 710    CONTINUE
!
! -------- We came from deselected station place
!
           CALL ADDSTR_F ( "Page:(N)ext  (P)revious  (S)ource   " )
 715    CONTINUE
        CALL NL_MN()
        CALL ADDSTR_F ( "(O)ptin      (L)ast page (T)erminate    "// &
     &                  "Least s(Q)ares" )
        CALL NL_MN()
      ENDIF
!
      CALL SET_CURSOR ( 0, NCNT+2, XLAST, YLAST )
      CALL NL_MN()
      CALL PARCN()
!
      CALL OBSTM ( FJD8, LJD8 )
      ID   =  INT ( LJD8 - FJD8 + 1.D-8 )
      IHR  =  INT ( ( (LJD8 - FJD8)*86400.0 - ID*86400.0 )/3600.0 + 1.D-8 )
      IMIN =  INT ( ( (LJD8 - FJD8)*86400.0 - ID*86400.0 - IHR*3600.0 )/60.0 + &
     &                 1.D-8 )
      SEC = ( (LJD8 - FJD8)*86400.0 - ID*86400.0 - IHR*3600.0 - IMIN*60.0 )
      IF ( SEC .LT. 0.0 ) SEC = 0.0
      WRITE ( BUFSTR, '("Parms used / Parms available: ",i6," /",i6, &
     &                  "  Nominal duration: ",I2," ",I2,":",I2,":",F6.3)' ) &
     &        NPARAM, NPMAX, ID, IHR, IMIN, SEC
      CALL BLANK_TO_ZERO ( BUFSTR(69:79) )
      CALL SET_CURSOR ( 0, NCNT+1, XLAST, YLAST )
      CALL ADDSTR_F ( BUFSTR )
      CALL NL_MN()
!
      IF ( CHECK_STABIT ( ICONT ) ) THEN
!
! -------- Write experiment start time
!
           CALL OBSTM ( FJD8, LJD8 )
           STR = JD_TO_DATE ( FJD8, IER )
           CALL SET_CURSOR  ( 44, 2, XLAST, YLAST )
           CALL ADDSTR_F    ( 'Start time: '//STR(1:23) )
!
! -------- Write experiment end time
!
           CALL SET_CURSOR  ( 44, 3, XLAST, YLAST )
           STR = JD_TO_DATE ( LJD8, IER )
           CALL ADDSTR_F    ( 'Stop  time: '//STR(1:23) )
      END IF
!
! --- Reset the cursor to the top unless worked with the control line
! --- or clock/atm insert/delete and reset IDELCK to zero to unsupress printing
!
      IDELCK = 0
      IF ( ISET .EQ.0   .OR.  BMODE_CL )  THEN
           CALL SET_CURSOR ( 31, 2, XLAST, YLAST )
         ELSE
           IF ( ISET .EQ. 1 ) CALL SET_CURSOR ( IXX  ,       IYY , XLAST, YLAST)
           IF ( ISET .EQ. 2 ) CALL SET_CURSOR ( 31,       7, XLAST, YLAST)
           IF ( ISET .EQ. 3 ) CALL SET_CURSOR ( 31, IATLN+1, XLAST, YLAST)
           ISET =  0
      END IF
      IF ( .NOT. CHECK_STABIT ( ICONT ) ) THEN
!
! -------- Deselected station
!
           CALL SETCR_MN ( 6, 5 )
      END IF
!
 350  CONTINUE
 355  CONTINUE
      CALL SENKR_MN ( IX, IY, ICH )
      CCHAR(1:1) = CCH(4:4)
      IF ( .NOT. CHECK_STABIT ( ICONT ) ) THEN
           IF ( CCHAR(1:1) .EQ. ' ' ) CCHAR(1:1) = 'N'
           IF ( CCHAR(1:1) .NE. 'N'  .AND. &
     &          CCHAR(1:1) .NE. 'P'  .AND. &
     &          CCHAR(1:1) .NE. 'S'  .AND. &
     &          CCHAR(1:1) .NE. 'O'  .AND. &
     &          CCHAR(1:1) .NE. 'L'  .AND. &
     &          CCHAR(1:1) .NE. 'T'           ) GOTO 355
      END IF
      IF ( CCHAR(1:1).EQ.' ') THEN
         IF ( IY .EQ. NCNT-2 ) THEN
              IF ( IX.GE.13 .AND. IX.LE.19 ) CCHAR(1:1) = 'C'
              IF ( IX.GE.23 .AND. IX.LE.34 ) CCHAR(1:1) = 'A'
              IF ( IX.GE.37 .AND. IX.LE.59 ) CCHAR(1:1) = 'G'
            ELSE IF ( IY .EQ. NCNT-1 ) THEN
              IF ( IX.GE.5  .AND. IX.LE.10 ) CCHAR(1:1) = 'N'
              IF ( IX.GE.13 .AND. IX.LE.22 ) CCHAR(1:1) = 'P'
              IF ( IX.GE.25 .AND. IX.LE.32 ) CCHAR(1:1) = 'S'
              IF ( IX.GE.37 .AND. IX.LE.69 ) CCHAR(1:1) = 'M'
            ELSE IF ( IY .EQ. NCNT ) THEN
              IF ( IX.LE.6                 ) CCHAR(1:1) = 'O'
              IF ( IX.GE.10 .AND. IX.LE.20 ) CCHAR(1:1) = 'L'
              IF ( IX.GE.24 .AND. IX.LE.34 ) CCHAR(1:1) = 'T'
              IF ( IX.GE.38 .AND. IX.LE.52 ) CCHAR(1:1) = 'Q'
         ENDIF
      ENDIF
!
! --- Special feature to turn off gradients at all sites.
! --- (Make the processing loop throughout the convoluted code as the
! --- other gradient options do, to make sure all flags are properly set.)
!
      IF ( CCHAR(1:1).EQ.'G' ) GOTO 426
      IF ( CCHAR(1:1).EQ.'M' ) THEN
!
! -------- Changing max polinomial degree for global clocks
!
           CALL SETCR_MN ( 0, NCNT-1 )
           MAX_GCLOCK_DEG0 = SETFL_MDEG
           IF ( MAX_GCLOCK_DEG0 .EQ. 2 ) THEN
                CALL ADDSTR_F ( "Maximum degree of global clock polynomoial "// &
     &                          "(1,(2))? " )
             ELSE
                CALL ADDSTR_F ( "Maximum degree of global clock polynomoial "// &
     &                          "((1),2)? " )
           END IF
!
! -------- Reading the input
!
           CALL GETSTR_F ( BUFSTR )
           READ ( UNIT=BUFSTR(1:I_LEN(BUFSTR)), FMT='(I4)', &
     &                 IOSTAT=IOS ) MAX_GCLOCK_DEG
           IF ( IOS.NE.0 .OR. &
     &          MAX_GCLOCK_DEG.LT.1 .OR. &
     &          MAX_GCLOCK_DEG.GT.2      ) THEN
                MAX_GCLOCK_DEG = MAX_GCLOCK_DEG0
           END IF
!
! -------- Updating LCLK bits
!
           DO IISTA=1,NUMSTA
              IF ( NUMCLK(IISTA) .GT. 0 ) THEN
                   DO ISEG=1,NUMCLK(IISTA)
                      IF ( .NOT. KBIT( LCLK(ICLSTR(IISTA)+ISEG), &
     &                     INT2(13)).AND.KBIT( ICLSTA(1,ISEG),IISTA )            ) THEN
                           IF  ( MAX_GCLOCK_DEG .EQ. 0 ) THEN
                                 CALL SBIT ( LCLK(ICLSTR(IISTA)+ISEG), &
     &                                INT2(2), INT2(0) )
                                 CALL SBIT ( LCLK(ICLSTR(IISTA)+ISEG), &
     &                                INT2(3), INT2(0) )
                               ELSE IF ( MAX_GCLOCK_DEG .EQ. 1 ) THEN
                                 CALL SBIT ( LCLK(ICLSTR(IISTA)+ISEG), &
     &                                INT2(2), INT2(1) )
                                 CALL SBIT ( LCLK(ICLSTR(IISTA)+ISEG), &
     &                                INT2(3), INT2(0) )
                               ELSE IF ( MAX_GCLOCK_DEG .EQ. 2 ) THEN
                                 CALL SBIT ( LCLK(ICLSTR(IISTA)+ISEG), &
     &                                INT2(2), INT2(1) )
                                 CALL SBIT ( LCLK(ICLSTR(IISTA)+ISEG), &
     &                                INT2(3), INT2(1) )
                            ENDIF
                      ENDIF
                   ENDDO
              END IF
           ENDDO
!
! -------- Updating menu
!
           GOTO 50
      END IF
!
      ILAST = 0
!
! --- See if user wants to skip directly to another site page.
! --- Remember that ichr now has the character in the upper byte.
!
      IF ( CCHAR(1:1) .EQ. '.' ) THEN
           CALL SET_CURSOR ( 31, IY+1, XLAST, YLAST )
           GOTO 350
      END IF
!
! --- Jump if flipping insert/delete/auto/reset mode params
!
      IF ( CCHAR(1:1).EQ.'I' .OR. &
     &     CCHAR(1:1).EQ.'M' .OR. &
     &     CCHAR(1:1).EQ.'D' .OR. &
     &     CCHAR(1:1).EQ.'F'     ) GOTO 520
      IF ( CCHAR(1:1) .EQ. '0' ) THEN
           CALL SENKR_MN(IX,IY,ICH )
           CCHAR(1:1) = CCH(4:4)
           CALL SENKR_MN(IX,IY,ICH )
           CCHAR(2:2)=CCH(4:4)
           ICONT = DECIMALTOINT(CCHAR,IERR)
           CALL FERR ( IERR, "Invalid page number", INT2(0), INT2(0) )
           GOTO 20
      ENDIF
      IF ( .NOT. ( CCHAR(1:1) .LE. '0' .OR. CCHAR(1:1).GE.'9' ) ) THEN
           ICONT = jchar( ichr, INT2(1) ) - 48
           GOTO 20
      END IF
      IF ( CCHAR(1:1) .EQ. '+' ) then
           IF ( IPAGEC.LT.NUMPGC ) THEN
                IPAGEC=IPAGEC+1
                GOTO 50
          ENDIF
      ENDIF
      IF ( CCHAR(1:1) .EQ. '-' ) THEN
           IF ( IPAGEC.GT.1 ) THEN
                IPAGEC=IPAGEC-1
                GOTO 50
           ENDIF
      ENDIF
      IF ( CCHAR(1:1) .EQ. '>' ) THEN
           IF ( IPAGEA.LT.NUMPGA ) THEN
                IPAGEA=IPAGEA+1
                GOTO 50
           ENDIF
      ENDIF
      IF ( CCHAR(1:1) .EQ. '<' ) THEN
           IF ( IPAGEA .GT. 1 ) THEN
                IPAGEA=IPAGEA-1
                GOTO 50
           ENDIF
      ENDIF
!
! --- Code added here so the user can skip along the standard path
! --- in the menu without setting the flags by depressing "/".
!
      ISKIP = 0
      IF ( CCHAR(1:1).EQ. '/' ) THEN
           CCHAR(1:1) = ' '
           ISKIP = 1
      END IF
      IF ( CCHAR(1:1).EQ.' ' .OR. CCHAR(1:1) .EQ. '"' .OR. &
     &     CCHAR(1:1).EQ.'G' ) GOTO 570
      IF ( CCHAR(1:1) .EQ. '*' ) THEN
           CCHAR = '  '
           IY = NCNT-2
           IX = 1
      END IF
      IF ( CCHAR(1:1) .EQ. 'O') THEN
           ICONT = -7
           GOTO 600
      END IF
      IF ( CCHAR(1:1).EQ.'C' ) GOTO 530
      IF ( CCHAR(1:1).EQ.'A' ) GOTO 540
      IF ( CCHAR(1:1).EQ.'N' .OR. CCHAR(1:1).EQ.'P' .OR. &
     &     CCHAR(1:1).EQ.'I' .OR. CCHAR(1:1).EQ.'S' .OR. &
     &     CCHAR(1:1).EQ.'L' .OR. CCHAR(1:1).EQ.'B' .OR. &
     &     CCHAR(1:1).EQ.'X'                              ) GOTO 567
      GO TO 568
 567  ILAST = -1
      GO TO 570
 568  CONTINUE
      IF(CCHAR(1:1).EQ.'Q' .OR. CCHAR(1:1).EQ.'T') GO TO 560
      IF (CCHAR(1:1) .EQ. 'R') GO TO 50
      IF (CCHAR(1:1) .NE. ' ') GO TO 355
 570  IYY = IY
      IXX = IX
      IF (IY.GE.NCNT .OR. ILAST.EQ.-1)  THEN
!       THEN BEGIN process last control line
          IF(CCHAR(1:1).EQ. &
     &      'L')ICONT = -2
          IF(CCHAR(1:1).EQ.'S') ICONT = -1
          IF(CCHAR(1:1).EQ.'P') ICONT = ICONT-1
          IF(CCHAR(1:1).EQ.'N') ICONT = ICONT+1
          IF  (CCHAR(1:1).EQ.'B')  ICONT = -5
          IF  (CCHAR(1:1).EQ.'X')  ICONT = -6
          IF ( ICONT .GT. NUMSTA )  THEN
!
! ----------- Begin finished all sites
!
              ICONT = -1
          ENDIF
          GO TO 600
!
! ------- ENDT process last control line
!
      END IF
 560  CONTINUE
      IF ( IY .GE. NCNT-1  .OR.  CCHAR(1:1) .EQ. 'Q' .OR. &
     &     CCHAR(1:1) .EQ. 'T' )  THEN
!
! -------- Begin process second control line
!
          CALL set_cursor(0,0,xlast,ylast )
          CALL CLEAR_MN()
          IF(CCHAR(1:1).EQ.'T') THEN
!
!           THEN BEGIN terminate SOLVE option
!
              ICONT = -4
!
!             ENDT terminate SOLVE option
!
          ELSE if(cchar(1:1).eq.'Q') then
!
!           ELSE BEGIN run least squares option
!
              ICONT = -3
!
!             ENDE run least squares option
!
          END IF
          GO TO 600
!
!         ENDT process second control line
!
      END IF
      IF(IY.GE.NCNT-2) GO TO 500
      IF((IX/2)*2.EQ.IX.and.ix.lt.60) GO TO 350
      IPOS = (IX-30)/2 + 1
      IF(IPOS.LE.0) GO TO 50
      IF(IY.GE.IATLN) GO TO 475
      IF(IY.GE.6) GO TO 450
      IF(IY.EQ.4) GO TO 425
!
! === PROCESS SITE FLAGS
!
      IF(IY.NE.2) GO TO 350
      IF(IPOS.GT.4) GO TO 4101
      IF(ISKIP.EQ.1) GO TO 407
      CALL SWBIT(LSITEC(1,IPOS),ICONT )
      IDSP(1)=KBITN(LSITEC(1,IPOS),ICONT)
      CALL set_cursor(IX,IY,xlast,ylast )
      WRITE(bufstr,405) IDSP(1)
      call addstr_f(bufstr(:1) )
  405 FORMAT(I1)
  407 IX = IX + 2
      IF(IX.LE.35) GO TO 406
      IX = 31
      IY = 7
  406 CALL set_cursor(IX,IY,xlast,ylast)
      GO TO 350
!
 4101 GO TO 350
!
!**** PROCESS THE AXIS OFFSET FLAGS
!
  425 CONTINUE
      IF(IPOS.NE.1) GO TO 426
      CALL SWBIT(LAXOF,ICONT )
      IDSP(1)=KBITN(LAXOF,ICONT)
      CALL set_cursor(IX,IY,xlast,ylast )
      WRITE(bufstr,405) IDSP(1)
      call addstr_f(bufstr(:1) )
      CALL set_cursor(IX,IY,xlast,ylast )
      GOTO 350
!
!***  Process gradient flags
!
  426 CONTINUE
      CALL set_cursor(0,NCNT-2,xlast,ylast )
      CALL clrtobot_mn()
3115  CONTINUE 
      call addstr_f("Automatic gradient epoch insertion")
      call nl_mn()
      call addstr_f("Interval between epochs (hours, 0 for none)? " )
      call getstr_f(bufstr )
      IF ( INDEX ( BUFSTR, '.' ) .LE. 0 ) THEN
           BUFSTR = BUFSTR(1:I_LEN(BUFSTR))//'.0'
      END IF
      READ ( BUFSTR(1:I_LEN(BUFSTR)), FMT='(F8.4)', IOSTAT=IOS ) HOURS
      if ( HOURS.EQ.0 .OR. IOS.NE.0 ) THEN
           DO I = 1,NUMSTA
              NUMGRAD(I) = 0
           ENDDO
        ELSE
           CALL AUTGRAD ( HOURS*60.D0, FAIL )
      ENDIF
      CALL SET_CURSOR ( 62, IY, XLAST, YLAST )
      WRITE ( BUFSTR, '(F5.2)' ) HOURS
      CALL ADDSTR_F(BUFSTR(:5) )
      goto 50
!
!**** PROCESS THE CLOCK FLAGS
!
  450 CONTINUE
!
! --- Bail out if batchmode clock on and there are no clock breaks
!
      IF ( BMODE_CL .AND. NUMBREAKS.EQ.0 ) GO TO 350
      IF(ISKIP.EQ.1) GO TO 4517
!
!     Code modified here to handle in a retrofit mode which allows the
!     the user to set only the first 5 degrees of the polynomial and the
!     diurnal.  Diurnal flags still in bits 14-16.
!
      IF(IPOS.GT.5) IPOS = IPOS+8
      IF(IPOS.GT.16 .OR. IPOS.LT.1) GO TO 350
!
      IF ( BMODE_CL ) THEN
!
! -------- Disallow setting flags on already existing clock breaks in batch mode
!
!          K1 = IY-7
!          K = BREAK(K1)
!          IF ( K1.LT.1 .OR. K1.GT.NUMBREAKS ) GOTO 350
           GOTO 350
        ELSE
!
! ----- Handle non-batch mode the same as always
!
          K1= IY-6
          K = IY-6+ICLSTR(ICONT) + (ipagec-1)*listlen
          IF ( K1.LT.1 .OR. K1.GT.NUMCLK(ICONT) ) GOTO 350
      ENDIF
!
!     First handle polynomial coefficients
!
      IF (IPOS.LE.5)  THEN
!       THEN BEGIN polynomials
!
!         Look out for continued clock epoch, but make sure is is
!         not the 1st clock
!
          IF  (IPOS.EQ.1 .AND. CCHAR(1:1).EQ.'"' .AND. IY.EQ.7)GOTO 350
          IF (IPOS.EQ.1 .AND. CCHAR(1:1).EQ.'"' .AND. IY.NE.7)   THEN
!
!           THEN BEGIN continuation for polynomial
!
              CALL SBIT( LCLK(K), INT2(13), INT2(1) )
              CALL SBIT( LCLK(K), INT2(1), INT2(0) )
              ICONTN = .TRUE.
              IDSP(1) = INT2_2AA
!
!             ENDT continuation for polynomial
!
          ELSE
!
!           ELSE BEGIN no continuity for polynomial
!
              CALL   SWBIT(LCLK(K),IPOS )
              CDSP(1:2) = '0 '
              IF(KBIT(LCLK(K),IPOS)) CDSP(1:2) = '1 '
              ICONTN = .FALSE.
              IF ( BMODE_CL ) THEN
                   CALL SBIT ( LCLK(K-1), INT2(1), KBITN(LCLK(K), INT2(1)) )
              ENDIF
!
!             ENDE no continuity for polynomial
!
          END IF
!
          IF (IPOS.EQ.1 .AND. .NOT.ICONTN)  THEN
!
!           THEN BEGIN be sure bit 13 is off
!
              CALL SBIT( LCLK(K), INT2(13), INT2(0) )
              CALL set_cursor(IX-1,IY,xlast,ylast )
              WRITE(bufstr,1684) BLANK
            call addstr_f(bufstr(:1) )
 1684         FORMAT(A1)
!
!             ENDT be sure bit 13 is off
!
          END IF
          CALL set_cursor(IX,IY,xlast,ylast )
          WRITE(bufstr,1684) IDSP(1)
          call addstr_f(bufstr(:1) )
!
!         ENDT polynomials
!
!     Handle diurnals
!
!     First handle the continue flag, but make sure it is not the
!     epoch.
!
      END IF
      IF (IPOS.EQ.14 .AND. IY.NE.7) THEN
!
!       THEN BEGIN handle the continue diurnal flag
!
          IF (.NOT.KBIT( LCLK(K), INT2(14)))  THEN
!
!           THEN BEGIN try to turn it on
!
!             Make certain the previous diurnal is on
!
              IF(KBIT( LCLK(K-1), INT2(15)))  THEN
!
!               THEN BEGIN the previous diurnal is on
!
                  CALL SBIT( LCLK(K), INT2(14), INT2(1) )
                  CALL SBIT( LCLK(K), INT2(15), INT2(1) )
                  CALL SBIT( LCLK(K), INT2(16), INT2(1) )
                  CALL set_cursor(IX,IY,xlast,ylast )
                  WRITE(bufstr,1690) ''''
                  call addstr_f(bufstr(:5) )
 1690             FORMAT(A1," C S")
!
!                 ENDT the previous diurnal is on
!
              END IF
!
!             ENDT try to turn it on
!
          ELSE
!
!           ELSE BEGIN turn it off
!
              CALL SBIT( LCLK(K), INT2(14), INT2(0) )
              CALL set_cursor(IX,IY,xlast,ylast )
              WRITE(bufstr,1684) '*'
              call addstr_f(bufstr(:1) )
!
!             ENDE turn it off
!
          END IF
!
!         ENDT handle the continue diurnal flag
!
      END IF
      IF (IPOS.EQ.15 .OR. IPOS.EQ.16) THEN
!
!       THEN BEGIN handle the diurnals themselves
!
          CALL SWBIT( LCLK(K), INT2(15) )
          IF (KBIT( LCLK(K), INT2(15))) THEN
!
!           THEN BEGIN just flipped it on
!
              CALL SBIT( LCLK(K), INT2(16), INT2(1) )
              IF(IPOS.EQ.16) IX = IX-2
              CALL set_cursor(IX,IY,xlast,ylast )
              call addstr_f("S C" )
!
!             ENDT just flipped it on
!
          ELSE
!
!           ELSE BEGIN just flipped it off
!             Make certain the continue is off
!
              CALL SBIT( LCLK(K), INT2(14), INT2(0) )
              CALL SBIT( LCLK(K), INT2(16), INT2(0) )
              IF(IPOS.EQ.16) IX = IX-2
              CALL set_cursor(IX-2,IY,xlast,ylast )
              call addstr_f("* * *" )
!
!             ENDE just flipped it off
!
          END IF
!
!         ENDT handle the diurnals themselves
!
!     Change clock flag guesses to 3 terms + Sin/Cos
!
      END IF
 4517 IX = IX + 2
      IF (IX.GE.37) THEN
!
!       THEN BEGIN start
!
          ISNCSN = 0
          IF((IX .GE. 37) .AND. (IX .LE. 39)) ISNCSN = 1
          IF(ISNCSN .EQ. 1) IX = 43
          IF(ISNCSN .EQ. 1) GO TO 455
          IX = 31
          IY = IY+1
          IF(IY.GE.IATLN-1) IY = IY+2
!
!         ENDT start
!
      END IF
 455  CALL set_cursor(IX,IY,xlast,ylast)
      GO TO 350
!
!**** PROCESS THE ATMOSPHERE FLAGS
!
  475 CONTINUE
!
!     Bail out if batchmode atmospheres
!
      IF(BMODE_AT) GO TO 350
      IF(IPOS.LT.1) GO TO 350
      IF(IPOS.GT.2) GO TO 350
      IF (ISKIP.NE.1) THEN
!
!       THEN BEGIN not skipping past this flag
!         Get the atmosphere epoch number
!
          IATM = IY - IATLN
          IF(IATM.LT.1 .OR. IATM.GT.NUMATM(ICONT)) GO TO 350
          JATM = IATM + IATSTR(ICONT) + (ipagea-1)*listlen
          IF (IPOS.EQ.1 .AND. CCHAR(1:1).EQ.'"'.AND.IATM.EQ.1)GO TO 350
          IF(IPOS.EQ.1 .AND. CCHAR(1:1).EQ.'"' .AND. IATM.NE.1)  THEN
!
!           THEN BEGIN continuation for atmosphere polynomial
!
             CALL SBIT( LATM(1,3), JATM, INT2(1) )
             CALL SBIT( LATM(1,1), JATM, INT2(0) )
             ICONTN=.TRUE.
             IDSP(1) = INT2_2AA
             CALL set_cursor(IX,IY,xlast,ylast )
             WRITE(bufstr,1684)IDSP(1)
             call addstr_f(bufstr(:1) )
!
!           ENDT continuation for atmosphere polynomial
!
          ELSE
!
!           ELSE BEGIN no continuation
!
             CALL SWBIT(LATM(1,IPOS),JATM )
             IDSP(1)=KBITN(LATM(1,IPOS),JATM)
             ICONTN=.FALSE.
             WRITE(bufstr,405)IDSP(1)
             call addstr_f(bufstr(:1) )
!
!           ENDE no continuation
!
        END IF
          IF(IPOS.EQ.1 .AND. .NOT.ICONTN) THEN
!
!           THEN BEGIN be sure continuation bit is off
!
             CALL SBIT( LATM(1,3), JATM, INT2(0) )
             CALL set_cursor(IX-1,IY,xlast,ylast )
             WRITE(bufstr,1684)BLANK
             call addstr_f(bufstr(:1) )
!
!           ENDT be sure continuation bit is off
!
        END IF
!
!         ENDT not skipping past this flag
!
      END IF
      CALL set_cursor(IX+2,IY,xlast,ylast )
      IF(IPOS.EQ.1)GO TO 350
      IY = IY + 1
      IF(IY.LE.NLINE) GO TO 480
      IX = 0
      IY = IY + 3
  480 CALL set_cursor(IX-2,IY,xlast,ylast)
      GO TO 350
!
!**** Process clock and atmosphere epoch insert/delete line
!
  500 CONTINUE
      ISET = 1
      IPOS = (IX-3)/10 + 1
      NPCT = NCNT - 1
      GO TO (520,530,540),IPOS
!
!     Handle flipping the INSERT/DELETE/AUTO/BATCH OFF clocks and atm flags.
!
  520 CONTINUE
      IF ( CCHAR(1:1) .EQ. 'M' ) INDL=1
      IF ( CCHAR(1:1) .EQ. 'I' ) INDL=3
      IF ( CCHAR(1:1) .EQ. 'D' ) INDL=0
      IF ( CCHAR(1:1) .EQ. 'F' ) INDL=2
      CALL SET_CURSOR ( 0, NCNT-2, XLAST, YLAST )
      IF ( INDL .EQ. 0 ) THEN
           INDL = 1
           CALL ADDSTR_F   ( "(*)Delete:   " )
           CALL SET_CURSOR ( IX, IY, XLAST, YLAST )
           GOTO 350
      ENDIF
!
      IF(INDL.EQ.1) THEN
        INDL = 2
        call addstr_f("(*)Automatic:" )
        CALL set_cursor(IX,IY,xlast,ylast )
        GO TO 350
      ENDIF
      IF(INDL.EQ.2) THEN
        INDL = 3
        call addstr_f("(*)Reset:    " )
        CALL set_cursor(IX,IY,xlast,ylast )
        GO TO 350
      ENDIF
      IF(INDL.EQ.3) THEN
        INDL = 0
        call addstr_f("(*)Insert:   " )
        CALL set_cursor(IX,IY,xlast,ylast )
        GO TO 350
      ENDIF
!
! --- Handle insert/delete clock epochs
!
  530 CONTINUE
      ISET = 2
      IF ( INDL.EQ.0 ) THEN
         CALL SET_CURSOR ( 0, 6, XLAST, YLAST )
         CALL CLRTOBOT_MN()
         CALL REFRESH_MN()
         CALL ADDSTR_F ( " YY MM DD HR MN (RETURN TO QUIT)" )
         CALL NL_MN()
532      CONTINUE
         CALL ADDSTR_F ( "?" )
         CALL GET_EPOCH ( TJD, NCLOCK )
         IF ( DFUZZ ( TJD, 0.0D0) ) THEN
              IF ( BMODE_CL ) THEN
                   EPOCH_INTERVAL = CLOCK_INTERVAL *60.D0
                   GOTO 810
                 ELSE
                   CALL SET_CURSOR ( 0, 6, XLAST, YLAST )
                   CALL CLRTOBOT_MN()
                   GOTO 50
              END IF
         ENDIF
         IF ( TJD .LT. FJD8   .OR.   TJD .GT. LJD8 ) THEN
!              type *,'  TJD =',TJD
!              type *,' FJD8 =',FJD8
!              type *,' LJD8 =',LJD8
              CALL FERR ( INT2(692), &
     &            "Epoch outside range of experiment; try again", INT2(0), &
     &             INT2(0) )
              GOTO 50
         ENDIF
         NPCT = NPCT + 1
         NUM_TRYS = 0
         CONTINUE = .TRUE.
         DO WHILE ( CONTINUE .AND. NUM_TRYS.LT.30 )
            NUM_TRYS = NUM_TRYS+1
            DO I=ICLSTR(ICONT)+1,ICLSTR(ICONT)+NUMCLK(ICONT)
               IF ( DABS(TJD-FJDCL(I) ) .LT. 1.D0/86400.D0 ) THEN
                   IF ( NUM_TRYS .GT. 30 ) THEN
                        CALL FERR ( INT2(543), "Epoch already exists", &
     &                       INT2(0), INT2(0) )
                        GOTO 50
                     ELSE
                        TJD = TJD + 0.2D0/86400.D0
                   ENDIF
                 ELSE
                   CONTINUE = .FALSE.
                ENDIF
            ENDDO
         ENDDO
!
         CALL INCLK ( ICONT, TJD, KLOC, FALSE__L2 )
         IYY = IYY+1
         GOTO 532
      ENDIF
!
      IF ( INDL .EQ. 1 ) THEN  ! Delete epochs
           IF ( NUMCLK(ICONT) .EQ. 1 ) GOTO 350
           CALL DCLK ( ICONT, IPAGEC, LISTLEN )
           IYY = IYY-1
           IF ( BMODE_CL ) THEN
                EPOCH_INTERVAL = CLOCK_INTERVAL *60.D0
                GOTO 810
              ELSE
                GOTO 50
           ENDIF
      ENDIF
!      IF(INDL.EQ.1.AND.BMODE_CL) THEN!i hate compc structure
!          CALL set_cursor(1,5,xlast,ylast)
!          CALL clrtobot_mn
!          call addstr_f("Cannot delete in batch mode")
!       call nl_mn
!          call addstr_f(" Hit RETURN to continue. ")
!       call getstr_f(bufstr)
!          READ(bufstr,*)
!          CALL set_cursor(1,5,xlast,ylast)
!          call addstr_f("                             ")
!       call nl_mn
!          call addstr_f("                          ")
!       call nl_mn
!          GO TO 55
!      ENDIF
!
!   Handle automatic clock epochs
!
      IF ( INDL .EQ. 2 ) THEN ! Handle automatic epoch insertion
           CALL SET_CURSOR ( 0, NCNT-2, XLAST, YLAST )
           CALL CLRTOBOT_MN()
!
3112       CONTINUE
           CALL ADDSTR_F ( "Automatic clock epoch insertion" )
           CALL NL_MN()
!
! -------- Reformating the string with default
!
           WRITE ( UNIT=STR, FMT='(I4)' ) NINT ( DEF_CLOSPAN/60.0 + 0.001 )
           CALL CHASHL ( STR )
           IF ( STR(1:1) .EQ. '*' ) THEN
                STR(1:4) = '60  '
           END IF
!
           CALL ADDSTR_F ( "Interval between epochs (in minutes, [1, 5000]  <"// &
     &          STR(1:I_LEN(STR))//">  ) ? " )
!
! -------- Reading the string from keyboard
!
           CALL GETSTR_F ( BUFSTR )
!
! -------- If the string is empty, use default
!
           IF ( ILEN(BUFSTR) .EQ. 0 ) BUFSTR = STR
           IF ( INDEX ( BUFSTR, '.' ) .LE. 0 ) THEN
                BUFSTR = BUFSTR(1:I_LEN(BUFSTR))//'.0'
           END IF
           READ ( BUFSTR(1:I_LEN(BUFSTR)), FMT='(F8.4)', ERR=3112 ) MINUTES
           IF ( MINUTES .LE. 0.9999  .OR.  MINUTES .GT. 5000.0 ) THEN
!
! ------------- Try once more
!
                GOTO 3112
              ELSE
!
! ------------- Request to enter maximnal degree of global clock polynomial
!
                MAX_GCLOCK_DEG0 = SETFL_MDEG
                CLOCK_INTERVAL = MINUTES/60.D0
                EPOCH_INTERVAL = MINUTES
                IF ( MAX_GCLOCK_DEG0 .EQ. 2 ) THEN
                     CALL ADDSTR_F ( "Maximum degree of global clock "// &
     &                               "polynomoial (1,(2))? " )
                  ELSE
                     CALL ADDSTR_F ( "Maximum degree of global clock "// &
     &                               "polynomoial ((1),2)? " )
                END IF
                CALL GETSTR_F ( BUFSTR )
                READ ( BUFSTR(1:I_LEN(BUFSTR)), FMT='(I4)', &
     &                 IOSTAT=IOS ) MAX_GCLOCK_DEG
                IF ( IOS.NE.0 .OR. &
     &               MAX_GCLOCK_DEG .LT. 1 .OR. &
     &               MAX_GCLOCK_DEG .GT. 2      ) THEN
!
                     MAX_GCLOCK_DEG = MAX_GCLOCK_DEG0
                END IF
        END IF
!
3113    CONTINUE
        CALL ADDSTR_F ( "(B)atch mode or (T)his station only  <B> ? " )
        CALL CLRCH ( BUFSTR ) 
        CALL GETSTR_F ( BUFSTR )
        MODC = BUFSTR(1:1)
        CALL CASEFOLD ( MODC )
        IF ( MODC .EQ. ' '  .OR.  MODC .EQ. CHAR(0)  ) MODC = 'B'
        IF ( MODC .NE. 'B'  .AND.  MODC .NE. 'T' ) GOTO 3113
        IF ( MINUTES .GT. 0.0 ) THEN
!
           IF ( MODC.EQ.'T' .AND. .NOT.BMODE_CL ) THEN
                CALL AUTOC ( ICONT, EPOCH_INTERVAL, MAX_GCLOCK_DEG, FAIL )
           ENDIF
!
           IF ( ( MODC .EQ. 'T' .OR. MODC .EQ. 't' ) .AND. BMODE_CL ) THEN
                CALL SET_CURSOR(1,5,XLAST,YLAST )
                CALL CLRTOBOT_MN()
                WRITE(bufstr,"('Cannot AUTO PARAMETERIZE this station', &
     &                           ' with batch mode already set up.')")
                call ADDSTR_F(BUFSTR )
                call NL_MN()
                call ADDSTR_F(" hIT return TO CONTINUE. " )
                call GETSTR_F(BUFSTR )
                CALL SET_CURSOR(1,5,XLAST,YLAST )
                WRITE(bufstr,"('                                     ', &
     &        '                                ')")
                call ADDSTR_F(BUFSTR )
                call NL_MN()
                call ADDSTR_F("                          " )
                call NL_MN()
                GOTO 55
          ENDIF
!
          IF ( MODC.EQ.'B' .OR. MODC.EQ.'b' ) THEN
               CLOCK_REF_BITS = 0 
               iclsta = 0 !  ?????????????????????????????????????????
               IERR=-1
               CALL ADDSTR_F ( "Stations:" )
               CALL NL_MN()
               DO IISTA = 1,NUMSTA
                  IF ( CHECK_STABIT ( IISTA ) ) THEN
                       WRITE ( BUFSTR, '(I5," = ",A8)' ) IISTA, ISITN_CHR(IISTA)
                       CALL ADDSTR_F(BUFSTR )
                       CALL NL_MN()
                  END IF
               ENDDO
!
! ----------- The following clock reference station logic has been modified to
! ----------- support more than one clock reference site (for merged dbs). The
! ----------- information is stored in a bit array (clock_ref_bits), which
! ----------- replaces bm_ref_cl for the long term storage of such information.
! ----------- However, for normal dbs requiring only one reference station
! ----------- bm_ref_cl is still well defined and carrys the same information.
! ----------- The routines below this routine use the .._mult_ref.f versions
! ----------- which trigger on the bit array.
!
! ----------- However, the real 'bottom line' information used in partl.f to
! ----------- estimate clocks is stored in the arrays ICLSTA, ICLSTR, JDATE_CLO,
! ----------- and NPL_CLO.
!
              CALL PICK_REFCLO ( NUMSTA, ISITN_CHR, STA_BRK, BM_REF_CL, &
     &                           CLOCK_REF_BITS )
 810          CONTINUE
              IF ( BM_REF_CL .LT. 0  .OR.  BM_REF_CL .GT. NUMSTA ) BM_REF_CL = 0
              CALL OPENNAMFIL()
              CALL AUTC_MULT_REF ( 'A', CLOCK_REF_BITS, EPOCH_INTERVAL, FAIL )
!
! ----------- AUTC turns on a 2nd order clock regardless, so turn of the
! ----------- extra terms if needed.
!
! ----------- Updating LCLK bits
!
              DO IISTA=1,NUMSTA
                 IF ( NUMCLK(IISTA) .GT. 0 ) THEN
                   DO ISEG=1,NUMCLK(IISTA)
                      IF ( .NOT. KBIT( LCLK(ICLSTR(IISTA)+ISEG), &
     &                     INT2(13)).AND.KBIT( ICLSTA(1,ISEG),IISTA )            ) THEN
                           IF  ( MAX_GCLOCK_DEG .EQ. 0 ) THEN
                                 CALL SBIT ( LCLK(ICLSTR(IISTA)+ISEG), &
     &                                INT2(2), INT2(0) )
                                 CALL SBIT ( LCLK(ICLSTR(IISTA)+ISEG), &
     &                                INT2(3), INT2(0) )
                               ELSE IF ( MAX_GCLOCK_DEG .EQ. 1 ) THEN
                                 CALL SBIT ( LCLK(ICLSTR(IISTA)+ISEG), &
     &                                INT2(2), INT2(1) )
                                 CALL SBIT ( LCLK(ICLSTR(IISTA)+ISEG), &
     &                                INT2(3), INT2(0) )
                               ELSE IF ( MAX_GCLOCK_DEG .EQ. 2 ) THEN
                                 CALL SBIT ( LCLK(ICLSTR(IISTA)+ISEG), &
     &                                INT2(2), INT2(1) )
                                 CALL SBIT ( LCLK(ICLSTR(IISTA)+ISEG), &
     &                                INT2(3), INT2(1) )
                            ENDIF
                      ENDIF
                   ENDDO
                 END IF
              ENDDO
!
              CLOCK_INTERVAL = EPOCH_INTERVAL / 60.D0
              OLD_CLOCKS     = .FALSE.
              CALL CLOSENAMFIL()
              BMODE_CL       = .TRUE.
          ENDIF
          IF ( FAIL ) CALL FERR ( INT2(261), 'Setting automatic clock epochs', &
     &         INT2(0), INT2(0) )
        ENDIF
      ENDIF
!
! --- The following code changed 4/5/91 by MWH in order to reset only
! --- the current station, unless we're in batch mode clocks
!
      IF ( INDL.EQ.3 ) THEN !Reset batch mode flag
           IF ( BMODE_CL ) THEN
                CALL RESET_B_CL( INT2(6) )
             ELSE
                CALL SET_CURSOR ( 0, 5, XLAST, YLAST )
                CALL CLRTOBOT_MN()
                CALL CLCLK(ICONT )
           ENDIF
           GOTO 50
      ENDIF
      GOTO 600
!
! --- Handle insert/delete atmosphere epochs
!
  540 CONTINUE
      ISET = 3
      IF ( INDL .EQ. 0 ) THEN
           IF ( BMODE_AT ) THEN ! I hate compc structure
                CALL SET_CURSOR ( 1, 5, XLAST, YLAST )
                CALL CLRTOBOT_MN()
                CALL ADDSTR_F ( "Cannot insert in batch mode," )
                CALL NL_MN()
                CALL ADDSTR_F ( " Hit RETURN to continue. " )
                CALL GETSTR_F ( BUFSTR )
                CALL SET_CURSOR ( 1, 5, XLAST, YLAST )
                CALL ADDSTR_F ( "                             " )
                CALL NL_MN()
                CALL ADDSTR_F ( "                          " )
                CALL NL_MN()
                GOTO 55
           ENDIF
!
           CALL SET_CURSOR ( 0, IATLN, XLAST, YLAST )
           CALL CLRTOBOT_MN()
           CALL REFRESH_MN()
           CALL ADDSTR_F ( " YY MM DD HR MN (RETURN TO QUIT)" )
           CALL NL_MN()
542        CONTINUE
           CALL ADDSTR_F ( "?" )
           CALL GET_EPOCH ( TJD, INT2(NCNT-2) )
           IF ( DFUZZ(TJD,0.0D0) ) THEN
                CALL SET_CURSOR ( 0, IATLN, XLAST, YLAST )
                CALL CLRTOBOT_MN()
                GOTO 50
           ENDIF
           NPCT = NPCT + 1
           TEPOC = TJD
           CALL INATM(ICONT,TEPOC,JATM )
           IYY=IYY+1
           GOTO 542
      ENDIF
!
      IF(INDL.EQ.1.AND..NOT.BMODE_AT) THEN !Handle delete epochs
        IF(NUMATM(ICONT).EQ.1) GO TO 350
        CALL DATM(ICONT,IATLN,ipagea,listlen )
        IYY = IYY-1
        GOTO 50
      ENDIF
      IF(INDL.EQ.1.AND.BMODE_AT) THEN!i hate compc structure
          CALL set_cursor(1,5,xlast,ylast )
          call addstr_f("Cannot delete in batch mode," )
          call nl_mn()
          call addstr_f(" Hit RETURN to continue. " )
          call getstr_f(bufstr )
          CALL set_cursor(1,5,xlast,ylast )
          call addstr_f("                             " )
        call nl_mn()
          call addstr_f("                          " )
        call nl_mn()
          GO TO 55
      ENDIF
!
!
      IF ( INDL .EQ. 2 ) THEN ! Handle automatic epoch insertion
           CALL SET_CURSOR ( 0, NCNT-2, XLAST, YLAST )
           CALL CLRTOBOT_MN()
3111       CONTINUE
           CALL ADDSTR_F ( "Automatic atmosphere epoch insertion" )
           CALL NL_MN()
!
! -------- Reformating the string with default
!
           WRITE ( UNIT=STR, FMT='(I4)' ) NINT ( DEF_ATMSPAN/60.0 + 0.001 )
           CALL CHASHL ( STR )
           IF ( STR(1:1) .EQ. '*' ) THEN
                STR(1:4) = '60  '
           END IF
!
           CALL ADDSTR_F ( "Interval between epochs (in minutes, [1, 5000]  <"// &
     &          STR(1:I_LEN(STR))//">  ) ? " )
!
! -------- Reading the string from keyboard
!
           CALL GETSTR_F ( BUFSTR )
!
! -------- If the string is empty, use default
!
           IF ( ILEN(BUFSTR) .EQ. 0 ) BUFSTR = STR
           IF ( INDEX ( BUFSTR, '.' ) .LE. 0 ) THEN
                BUFSTR = BUFSTR(1:I_LEN(BUFSTR))//'.0'
           END IF
           READ ( BUFSTR(1:I_LEN(BUFSTR)), '(F8.4)', ERR=3111 ) MINUTES
           IF ( MINUTES .LE. 0.9999  .OR.  MINUTES .GT. 5000.0 ) THEN
                GOTO 3111
           END IF
!
3114       CONTINUE
           CALL ADDSTR_F ( "(B)atch mode or (T)his station only  <B> ? " )
           CALL CLRCH ( BUFSTR ) 
           CALL GETSTR_F ( BUFSTR )
           MODA = BUFSTR(1:1)
           IF ( MODA(1:1) .EQ. ' '  .OR.  MODA .EQ. CHAR(0) ) MODA = 'B'
           CALL CASEFOLD ( MODA )
           IF ( MODA.NE. 'B'   .AND.  MODA .NE. 'T') GOTO 3114
!
           IF ( MINUTES .GT. 0.0 ) THEN
                ATMOS_INTERVAL = MINUTES/60.D0
                EPOCH_INTERVAL = MINUTES
                IF ( ( MODA .EQ. 'T'  .OR.  MODA .EQ. 't' ) .AND. &
     &                 .NOT. BMODE_AT                              ) THEN
                     CALL AUTOA ( ICONT, EPOCH_INTERVAL, FAIL )
                     BMODE_AT = .FALSE.
                ENDIF
!
                IF ( ( MODA.EQ.'T' .OR. MODA.EQ.'t' ) .AND. BMODE_AT ) THEN
                     CALL SET_CURSOR ( 1, 5, XLAST, YLAST )
                     WRITE ( BUFSTR, "('Cannot auto parameterize THIS STATION', &
     &                      ' with batch mode already set up.')")
                     CALL ADDSTR_F ( BUFSTR )
                     CALL NL_MN()
                     CALL ADDSTR_F ( " Hit RETURN to continue. " )
                     CALL GETSTR_F ( BUFSTR )
                     CALL SET_CURSOR ( 1, 5, XLAST, YLAST )
                     CALL CLRCH ( BUFSTR ) 
                     CALL ADDSTR_F ( BUFSTR )
                     CALL NL_MN()
                     CALL ADDSTR_F ( "                          " )
                     CALL NL_MN()
                     GOTO 55
                ENDIF
                IF ( MODA.EQ.'B' .OR. MODA.EQ.'b' ) THEN
                     CALL OPENNAMFIL()
                     CALL AUTA ( EPOCH_INTERVAL, FAIL )
                     OLD_ATMS = .FALSE.
                     CALL CLOSENAMFIL()
                     BMODE_AT =.TRUE.
                 ENDIF
                 IF ( FAIL ) CALL FERR ( INT2(262), &
     &               'Setting automatic atmosphere '//'epochs', INT2(0), INT2(0) )
           ENDIF
      ENDIF
!
! --- The following code changed 4/5/91 by MWH in order to reset only
! --- the current station, unless we're in batch mode atmospheres
!
      IF(INDL.EQ.3) THEN
        if (bmode_at) then
          CALL RESET_B_AT(IATLN )
        else
          call set_cursor(0,5,xlast,ylast,xlast,ylast )
          call clrtobot_mn()
          call clatm(icont )
        endif
        GO TO 50
      ENDIF
!
  600 CONTINUE
!
! --- ENDW present site menus
!
      END DO
!
!**** Write the common block back onto the disk
!
      SOCOM_PLUS_FIRST = SPL__INIT
      CALL SOCOM_EXT()
      CALL USE_COMMON  ( 'OWC' )
      IER = 0
      CALL FAST_BYPASS ( IER )
!
      CALL USE_COMMON ( 'ORC' )
      CGM_TYPE = .FALSE.  !  Set once more session-type of socom
      CALL SOCOM_EXT()
!
  900 CONTINUE
32767 CONTINUE
      END  !#!  STFLG  #!#
