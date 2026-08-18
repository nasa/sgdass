      SUBROUTINE THIRD ( JD_DUR_NOM, JD_DUR_ACT, SUWSQ_TAU, LBUF_LEN, LBUF, &
     &                   IPTR, PAGEWID )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  THIRD PROGRAM SPECIFICATION
!
! 1.1 In this routine we calculate and print residual statistics,
!     write out archive records, baseline statistics and source
!     statistics.
!
! 1.2 REFERENCES:
!
! 2.  THIRD INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
      INTEGER*4   LBUF_LEN, IPTR, PAGEWID
      CHARACTER   LBUF(LBUF_LEN)*120
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'oborg.i'
      INCLUDE 'sareq.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'crecm.i'
      INCLUDE 'buff2.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: cres
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2  KBIT, TEST, K_MN
      REAL*8     DSIG, RSIG, DPARAM, CHID, CHIR, CHID_FLOOR, SHARE
      REAL*8     DEL_ERROR, RAT_ERROR
      REAL*8     SITE_WTS(2,MAX_STA), DPDUM2(2)
      REAL*8     JD_DUR_NOM, JD_DUR_ACT, SUWSQ_TAU, JD_DUR, FJDOBS, LJDOBS, SEC
      INTEGER*4  IMIN, IHR, ID, JDEL_ERROR, JRAT_ERROR
      INTEGER*2  IMIN2, IHR2, ID2, IM2, IY2
      INTEGER*2  ICT, JCT, LCT, NCT, IOUTS, IOUTF, NUM_IN_ROW, NUM_FULL_ROWS, &
     &           NUM_CHARS_PER_SITE
      INTEGER*2  LDBNAM(5,15), IDBVER(15)
      CHARACTER  C_LDBNAM(15)*10
      EQUIVALENCE ( LDBNAM, C_LDBNAM )
      REAL*8     DSIG_PRI, RSIG_PRI
      CHARACTER  SITE_BUFFER*120, WEIGHTING_TYPE_TEMP*2
      INTEGER*2  I, IEXC, I_LET, J, K, MSTA1, MSTA2, IERR, ISITX(2), NUMDD
      INTEGER*4  IOS, KC1, NC1, COUNT, JISIG, ihold(2)
      CHARACTER  SOURCE_LETTERS*26, STR*512, DUR_STR*80, BEG_STR*80
      INTEGER*4  IPTR_ST, IPTR_USE
      CHARACTER  FNAME*(NAME_SIZE)
      LOGICAL*4  DATYP_INQ, IS_R8_NAN
      CHARACTER  DATYP_LINE*21, SUPMET_LINE*16, TOT_STRING*5, DUR_TYP*18
      CHARACTER  JD_TO_DATE*23
      DATA       SOURCE_LETTERS / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' /
      INTEGER*2  INT2_RA, INT2_DE, INT2_CO
      PARAMETER  ( INT2_RA = 2HRA )
      PARAMETER  ( INT2_DE = 2HDE )
      PARAMETER  ( INT2_CO = 2HCO )
      INTEGER*4  RSIG_I4
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE   910712 Included residual rates in output even when rates not
!                used. Left obs. count = 0 to indicate non-use.
!   MWH   910903 Include sources with no good observations in source
!                statistics table
!   jwr   940110 Chi-square values printed with four digits after the
!                decimal point.
!   jwr   950801 Error in handle the dsig and rsig for simulations fixed.
!   kdb   951207 Integer*4 number of observations
!   kdb   970204 If site weights are applied, print these in addition to the
!                baseline form of the site weights.
!   kdb   970522 Fix formatting error in new site statistics
!                site weight record.  (Overflow occurred if a full set of sites
!                was printed in a line.)
!   pet   970712 Supress printout when it called from REWAY
!   pet   980203 Substituted hard-coded test of solution type by DATYP_INQ.
!                Changed a bit format of printing solution type
!   pet   980330 Added printing extended statistics section about the number
!                of parameters used
!   pet   980429 Changed format of extended statistics section
!   pet   980430 Added printing suppression method line
!   pet   980505 Changed the outout format
!                1) when global  wrms is larger 1 microsec it is printed
!                   in NANOSECONDS
!                2) when global  wrms is larger 1 millisec it is printed
!                   in MICROSECONDS
!                3) when wrms for baseline, source or total is less than
!                   10 picoseconds it is printed with decimal part.
!   pet   980708 Added writing the flag that CRES has processed at least one
!                arc
!   pet   980920 Forced CRES to have flexible format of printing w.r.m.s only
!                in proper CRES-mode and leaving it fixed when CRES is called
!                CRES_PRE98 mode.
!   pet   980920 CRES prints duration of the session (in both format: seconds
!                and dd hh:mm:ss.sss) when CRES is called in its porper mode
!   pet   980924 corrected a minor bug in formating actual duration date
!   pet   990426 added pritning start time of the session in both FJD and
!                yy/mm/dd hh:mm:ss.sss  format
!   pet   1999.05.10 Changed format from yy/mm/ss to yyyy.mm.ss
!   pet   1999.05.28 Made LBUF_LEN, LBUF, IPTR, PAGEWID formal arguments,
!                    eliminated common block cres_buf
!   pet  2000.01.25   Added support of NORATE_FLAG. Rate statistic is bypassed
!                     and not computed when NORATE_FLAG is .TRUE. and the
!                     program runs faster.
!   pet  2001.01.11   Fixed the old-old bug and forced the routine to compute
!                     CHISQR -- Ratio of sum of squares of weighted residuals
!                     to their mathematical expectation, correctly
!   pet  2001.04.16   Corrected a bug: dividing on zero.
!   pet  2001.04.20   Added logic for bypassing an attempt to divide on zero
!   pet  2001.05.09   Added logic for bypassing integer overflow
!   pet  2001.07.10   Added logic for checking whether RSIG is not-a-number
!   pet  2002.05.06   Added extractin of statistical information and putting
!                     it in glbc4
!   pet  2002.07.22   Fixed a bug: argument -1 for USE_SARFIL was accidentally
!                     replaced with +1 during the last updated. Restored
!                     orogonal logic.
!   jwr  2004.04.17   jdel_sig and jrat_sig to make format statements work in LINUX.
!   jwr  2004.05.24   ihold introduced to make 'Site Stat' formats work.
!   pet  2004.07.18   Added support of a kludge environment variable &
!                     as a temporary measure
!   pet  2005.05.27   Added code for preventing integer overflow when &
!                     rsig is too big
!   pet  2016.01.04   Changed format: added exp_code in SRC_STAT: string
!   pet  2017.10.23   Updated format: romved nrd+15ps and kept reporting rate &
!                     statistics only if rate was used in the solution
!   pet  2021.06.01   Added support of LISTING_OPTIONS SRC_POST2021_SPOOL__FMT
!                     That option changes format
!   pet  2022.04.03   Changed format of records with deselected baselines
!   pet  2024.07.09   Added support for computation of scan statistics and weighted epoch
!                     for a given source
!
! --- Setting flag of printing the ouput intformation at the screen using
! --- "_MN" inbterface (curses)
!
      K_MN = KSCREEN                      .AND. KBIT ( PRE_IP ( 2 ), INT2(6))           ! Interactide mode
      IF ( KBIT ( PRE_IP ( 3 ), INT2(12)) .AND. REWAY_VERBOSE ) THEN
           K_MN = .FALSE.  ! But supress printout
      END IF
!!                                              !  in silent REWAY mode
!!                                              !  in silent REWAY mode
      IF ( KSPOOL .AND. NLINE .GE. 55 .AND. KFULLOUT ) WRITE ( 23, 4020 )
 4020 FORMAT("1")
!
! --- Encoding line with beginning the session
!
      CALL DBPOX ( NUMDD, LDBNAM, IDBVER, IDBEND )
      CALL CLRCH ( BEG_STR )
      CALL OBSTM ( FJDOBS, LJDOBS )
      CALL EPOC  ( IM2, ID2, IY2, IHR2, IMIN2, FJDOBS )
      SEC = ( (FJDOBS-0.5D0) - INT(FJDOBS - 0.5D0) )*86400.0 - &
     &        ( IMIN2*60.D0 + IHR2*3600.D0 )
      IF ( SEC .LE. -0.001 ) THEN
           CALL EPOC  ( IM2, ID2, IY2, IHR2, IMIN2, FJDOBS-1.D0/1440.0 )
           SEC = ( (FJDOBS-0.5D0) - DINT(FJDOBS - 0.5D0) )*86400.0 - &
     &           ( IMIN2*60.D0 + IHR2*3600.D0 )
      END IF
!
      IF ( IY2 .GT. 70 ) THEN
           IY2 = IY2 + 1900
         ELSE
           IY2 = IY2 + 2000
      END IF
      WRITE ( BEG_STR, '(A,1X,F14.6,"   ",I4,".",I2,".",I2," ",I2,":",I2,":", &
     &        F6.3,"  UTC")' ) ' Session started on:   ', FJDOBS, IY2, IM2, &
     &        ID2, IHR2, IMIN2, SEC
      IF ( BEG_STR(47:47) .EQ. ' ' ) BEG_STR(47:47) = '0'
      IF ( BEG_STR(48:48) .EQ. ' ' ) BEG_STR(49:49) = '0'
      IF ( BEG_STR(50:50) .EQ. ' ' ) BEG_STR(50:50) = '0'
      IF ( BEG_STR(53:53) .EQ. ' ' ) BEG_STR(53:53) = '0'
      IF ( BEG_STR(56:56) .EQ. ' ' ) BEG_STR(56:56) = '0'
      IF ( BEG_STR(59:59) .EQ. ' ' ) BEG_STR(59:59) = '0'
      IF ( BEG_STR(60:60) .EQ. ' ' ) BEG_STR(60:60) = '0'
!
! --- Preparing the line with duration of the sessions
!
      IF ( JD_DUR_ACT .LE. -0.001 ) THEN
           JD_DUR  = JD_DUR_NOM
           DUR_TYP = ' Nominal duration:'
         ELSE
           JD_DUR  = JD_DUR_ACT
           DUR_TYP = ' Actual duration: '
      END IF
!
! --- Encoding line with duration of the session
!
      CALL CLRCH  ( DUR_STR )
      ID   =  INT ( JD_DUR + 1.D-8 )
      IHR  =  INT ( ( JD_DUR*86400.0 - ID*86400.0 )/3600.0 + 1.D-8 )
      IMIN =  INT ( ( JD_DUR*86400.0 - ID*86400.0 - IHR*3600.0 )/60.0 + 1.D-8 )
      SEC = ( JD_DUR*86400.0 - ID*86400.0 - IHR*3600.0 - IMIN*60.0 )
      IF ( SEC .LT. 0.0 ) SEC = 0.0
!
      WRITE ( DUR_STR, '(A,4X,F11.3,"  sec",11X,I2," ",I2,":",I2,":",F6.3, &
     &                   "  sec")' ) DUR_TYP, JD_DUR*86400.D0, &
     &                               ID, IHR, IMIN, SEC
!
      IF ( DUR_STR(50:50) .EQ. ' ' ) DUR_STR(50:50) = '0'
      IF ( DUR_STR(53:53) .EQ. ' ' ) DUR_STR(53:53) = '0'
      IF ( DUR_STR(56:56) .EQ. ' ' ) DUR_STR(56:56) = '0'
      IF ( DUR_STR(59:59) .EQ. ' ' ) DUR_STR(59:59) = '0'
      IF ( DUR_STR(60:60) .EQ. ' ' ) DUR_STR(60:60) = '0'
!
! --- Calculate and print the residual statistics
!
      IF ( NUMBER .EQ. 0 ) THEN
           DSIG=0.D0
           RSIG=0.D0
           CHISQR(1)=0.0D0
           CHISQR(2)=0.0D0
           CHISQR(3)=0.0D0
           WRMS(1)=0.0D0
           WRMS(2)=0.0D0
           WRMS(3)=0.0D0
           NC1=0
           KC1=0
           CHIDNMG=0
         ELSE
           IF ( .NOT. DATYP_INQ ( IDATYP, RATE__DTP ) ) THEN
!
! ------------- Rate was not used in solution
!
                IF ( ME_CHI .GT. 1.D-12 ) THEN
                     CHISQR(1)= WRMSI(1)/ME_CHI
                     CHISQR(2)= WRMSI(2)/ME_CHI ! to write rate chisqr anyway.
                END IF
                CHISQR(3) = CHISQR(1)
                CHINMRG   = WRMSI(1)
                CHIDNMG   = NC - NPARAM
                IF ( FACT(1) .GT. 1.D-15 ) THEN
                     DSIG=DSQRT ( WRMSI(1)/FACT(1) )*1.0D9
                   ELSE
                     DSIG = 0.0
                END IF
                IF ( FACT(2) .GT. 1.D-15 ) THEN
!
! ------------------ write rate weighted RMS anyway.
!
                     RSIG=DSQRT ( WRMSI(2)/FACT(2) )*1.0D12
                   ELSE
                     RSIG=0.0D0
                END IF
                IF ( NC .GT. 0 ) THEN
                     WRMS(1) = DSQRT ( WRMSI(1)/NC )
                   ELSE
                     WRMS(1) = 0.0D0
                END IF
                IF ( KC .GT. 0 ) THEN
!
! ------------------ to write rate normalized RMS anyway.
!
                     WRMS(2) = DSQRT ( WRMSI(2)/KC )
                   ELSE
                     WRMS(2) = 0.0D0
                END IF
                WRMS(3)=WRMS(1)
                NC1=NC
                KC1=0
             ELSE IF ( DATYP_INQ ( IDATYP, RATONL__DTP ) ) THEN
!
! ------------- Rate only
!
                CHISQR(1)=0.0
                IF ( ME_CHI .GT. 1.D-12 ) THEN
                     CHISQR(2)=WRMSI(2)/ME_CHI
                END IF
                CHISQR(3) = CHISQR(2)
                CHINMRG   = WRMSI(2)
                CHIDNMG   = KC-NPARAM
                DSIG      = 0.0D0
                IF ( FACT(2) .GT. 1.D-15 ) THEN
                     RSIG = DSQRT(WRMSI(2)/FACT(2))*1.0D12
                   ELSE
                     RSIG = 0.0D0
                END IF
                WRMS(1)=0.0D0
                IF ( KC .GT. 0 ) THEN
                     WRMS(2) = DSQRT ( WRMSI(2)/KC )
                END IF
                WRMS(3)=WRMS(2)
                NC1=0
                KC1=KC
              ELSE
!
! ------------- Both, delay and rate
!
                CHISQR(1)=0.0
                CHISQR(2)=0.0
                IF ( KC +ME_CHI .GT. 1.D-12 ) THEN
                     CHISQR(3) = (WRMSI(1)+WRMSI(2))/(KC+ME_CHI)
                END IF
                CHINMRG=WRMSI(1)+WRMSI(2)
                CHIDNMG=KC+NC-NPARAM
                IF ( (KC+NC) .GT. 0 ) THEN
                     DPARAM = NC *  FLOAT(NPARAM) / (KC + NC)
                   ELSE
                     DPARAM = 0.D0
                END IF
                IF ( (NC - DPARAM) .GT. 0 ) THEN
                     CHISQR(1) = WRMSI(1) / (NC-DPARAM)
                   ELSE
                     CHISQR(1) = 0.0D0
                END IF
                IF ( (KC+NC) .GT. 0 ) THEN
                     DPARAM = KC * FLOAT(NPARAM) / (KC + NC)
                   ELSE
                     DPARAM = 0.0D0
                END IF
                IF ( (KC-DPARAM) .GT. 0 ) THEN
                      CHISQR(2) = WRMSI(2) / (KC-DPARAM)
                   ELSE
                      CHISQR(2) = 0.0D0
                END IF
                IF ( FACT(1) .GT. 1.D-15 ) THEN
                     DSIG = DSQRT(WRMSI(1)/FACT(1))*1.0D9
                   ELSE
                     DSIG = 0.0D0
                END IF
                IF ( FACT(2) .GT. 1.D-15 ) THEN
                     RSIG = DSQRT(WRMSI(2)/FACT(2))*1.0D12
                   ELSE
                     RSIG = 0.0D0
                END IF
                IF ( NC .GT. 0 ) THEN
                     WRMS(1) = DSQRT(WRMSI(1)/NC)
                   ELSE
                     WRMS(1) = 0.0D0
                END IF
                IF ( KC .GT. 0 ) THEN
                     WRMS(2) = DSQRT(WRMSI(2)/KC)
                   ELSE
                     WRMS(2) = 0.0D0
                END IF
                IF ( KC+NC .GT. 0 ) THEN
                     WRMS(3) = DSQRT((WRMSI(1)+WRMSI(2))/(KC+NC))
                   ELSE
                     WRMS(3) = 0.0D0
                END IF
                NC1=NC
                KC1=KC
           ENDIF
      ENDIF
!
      IF ( DATYP_INQ ( IDATYP, DELAY__DTP ) )  THEN
           CALL USE_GLBFIL_4 ( 'OR'  )
!
! -------- Keep statistics. They will be needed for writing them in
! -------- SINEX listing
!
           STAT_NUMOBS = NC
           STAT_NUMUNK = NPARAM
           STAT_SQUOC  = SUWSQ_TAU
           STAT_SQURES = WRMSI(1)
           STAT_VARFAC = CHISQR(1)
           STAT_WRMS   = DSIG*1.D-9
           CALL USE_GLBFIL_4 ( 'WC' )
      END IF
!
      CALL DATYP_SHOW ( IDATYP, DATYP_LINE )
      IF ( KSPOOL ) THEN
           WRITE(23,15005) IRNCD,NUMBER
15005      FORMAT(/" Run ",I5,"-",I4,"   ",I10,' Observation Pairs Available ')
!
           IF ( KSPOOL .AND. CRES_STYLE .NE. CRES__PRE98 ) THEN
!
! ------------- Printing beginning and duration the session
!
                WRITE ( 23, FMT='(A)' ) BEG_STR(1:I_LEN(BEG_STR))
                WRITE ( 23, FMT='(A)' ) DUR_STR(1:I_LEN(DUR_STR))
           END IF
           IF ( FL_EQUAL_EFF_FREQ ) THEN
                WRITE ( 23, FMT='(1X,"Solution type: ",A)' ) DATYP_LINE//'  EQUAL_EFF_FREQ'
              ELSE
                WRITE ( 23, FMT='(1X,"Solution type: ",A)' ) DATYP_LINE
           END IF
!
           IF ( KFULLOUT ) WRITE ( 23, 5005 )
 5005      FORMAT(/, &
     & ' Data Type     Number of   Weighted RMS    Normalized RMS   Chi', &
     & ' Square',/, &
     & '             Observations    Residual         Residual      (precis)',/, &
     & '                 Used')
!
      END IF
!
      IF ( K_MN ) THEN
         IPTR=IPTR+1
         WRITE ( LBUF(IPTR), '(1X)' )
         CALL NL_MN()
         IPTR=IPTR+1
!
         WRITE ( LBUF(IPTR), 15006 ) IRNCD,NUMBER
15006    FORMAT ( " Run ", I5, "-", I4, "   ", I10, &
     &            " Observation Pairs Available " )
         CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
         CALL NL_MN()
!
         IF ( CRES_STYLE .NE. CRES__PRE98 ) THEN
!
! ----------- Printing beginning the session
!
              IPTR = IPTR + 1
              CALL CLRCH ( LBUF(IPTR) )
              LBUF(IPTR) = BEG_STR
              CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
              CALL NL_MN()
!
! ----------- Printing duration of the session
!
              IPTR = IPTR + 1
              CALL CLRCH ( LBUF(IPTR) )
              LBUF(IPTR) = DUR_STR
              CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
              CALL NL_MN()
         END IF
!
! ------ New style (03-FEB-98) printing solution type at the screen
!
         IPTR = IPTR + 1
         WRITE ( UNIT=LBUF(IPTR), FMT='(1X,"Solution type: ",A)' ) DATYP_LINE
         CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
         CALL NL_MN()
!
         IPTR=IPTR+1
         WRITE ( LBUF(IPTR), '(1X)' )
         CALL NL_MN()
!
         IPTR=IPTR+1
         WRITE ( LBUF(IPTR), '(" Data Type     Number of   Weighted RMS", &
     &      "    Normalized RMS   Chi Square")')
         CALL ADDSTR_F(LBUF(IPTR)(:PAGEWID) )
         CALL NL_MN()
!
         IPTR=IPTR+1
         LBUF(IPTR)= &
     &   "             Observations    Residual         Residual      (precis)"
         CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
         CALL NL_MN()
!
         IPTR=IPTR+1
         WRITE ( LBUF(IPTR), '("                 Used")' )
         CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
         CALL NL_MN()
      END IF
!
      IF ( NDIFF .NE. 0    .AND.   K_MN ) THEN
           IPTR=IPTR+1
           WRITE ( LBUF(IPTR), 5722 ) NDIFF
           CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
           CALL NL_MN()
      ENDIF
!
      IF ( KSPOOL .AND. NDIFF .ne. 0  .and. KFULLOUT ) WRITE(23,5722) NDIFF
 5722 FORMAT(1X,I5," Points were differenced observation pairs")
!
! --- If the database is a simulation case, reset wrms(3) to 1 so
! --- that the scaled errors will be same as unscaled errors in the
! --- downstream progams (ADJST and BASFE).
!
      IF ( SIMULATION_TEST ) THEN
         WRMS(3) = 1.D0
         IF ( K_MN ) THEN
              call addstr_f("WARNING:" )
              call &
     &             addstr_f(" Simulation - Normalized residual set to 1 for scaling errors" )
              call nl_mn()
              call &
     &             addstr_f("      Weighted RMS residual also set to 1" )
              CALL NL_MN()
         ENDIF
         IF ( KSPOOL ) WRITE ( 23, "('WARNING:', &
     &       ' Simulation - Normalized residual set to 1 for scaling errors'/ &
     &       '      Weighted RMS residual also set to 1')")
      ENDIF
!
! --- Writing total W.R.M.S in different format in according with its value
!
      IPTR = IPTR+1
      IF ( CRES_STYLE == CRES__PRE98 ) THEN
!
! -------- Fixed format of printing w.r.m.s in mode compatibility with old
! -------- CRES
!
           WRITE ( LBUF(IPTR), 5012 ) NC1, NINT(DSIG*1000.D0), WRMS(1), &
     &                                CHISQR(1)
 5012      FORMAT ( "   Delay",2X, I10, 7X, I6, " ps", 8X, F9.2, F15.4 )
         ELSE IF ( CRES_STYLE == CRES__PRE03 ) THEN
!
! ------- Flexible format of printing w.r.m.s. in POST98 format
!
          IF ( DSIG .GT. 1.D6 ) THEN
               WRITE ( LBUF(IPTR), 5015 ) NC1, DSIG*1.D-9, WRMS(1), CHISQR(1)
 5015          FORMAT ( "   Delay",2X, I10, 7X, G11.4, " SEC. ", F9.2, F15.4, &
     &                  ' !!!' )
            ELSE IF ( NINT(DSIG*1000.D0) .LT. 10 ) THEN
               WRITE ( LBUF(IPTR), 5011 ) NC1, DSIG*1000.D0, WRMS(1), CHISQR(1)
 5011          FORMAT ( "   Delay",2X, I10, 7X, F6.1, " ps", 8X, F9.2, F15.4 )
            ELSE IF ( NINT(DSIG*1000.D0) .LE. 999999 ) THEN
               WRITE ( LBUF(IPTR), 5012 ) NC1, NINT(DSIG*1000.D0), WRMS(1), &
     &                                    CHISQR(1)
            ELSE IF ( NINT(DSIG) .LE. 999999 ) THEN
               WRITE ( LBUF(IPTR), 5013 ) NC1, NINT(DSIG), WRMS(1), CHISQR(1)
 5013          FORMAT ( "   Delay",2X, I10, 7X, I6, " NANOSECOND", F9.2, &
     &                  F15.4, ' !!!' )
            ELSE IF ( NINT(DSIG*0.001D0) .LE. 999999 ) THEN
               WRITE ( LBUF(IPTR), 5014 ) NC1, NINT(DSIG*0.001D0), WRMS(1), &
     &                 CHISQR(1)
 5014          FORMAT ( "   Delay",2X, I10, 7X, I6, " MICROSEC. ", F9.2, F15.4, &
     &                  ' !!!' )
            ELSE
          END IF
         ELSE 
!
! --------- Post August 2003 style ("current")
!
            WRITE  ( LBUF(IPTR), 5016 ) NC1, DSIG*1.D3, WRMS(1), CHISQR(1)
 5016       FORMAT ( "   Delay ",2X, I6, 2X, F17.3, " ps ", 7X, F10.2, 1X, &
     &                   F12.4 )
      END IF
      IF ( K_MN ) THEN
           CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
           CALL NL_MN()
      ENDIF
      IF ( KSPOOL ) THEN
           WRITE ( 23, '(" ")' )
           WRITE ( 23, '(A)'   ) LBUF(IPTR)(1:I_LEN(LBUF(IPTR)))
      ENDIF
!
      IF ( IS_R8_NAN ( RSIG ) ) THEN
           RSIG_I4 =  2000000000
         ELSE IF ( RSIG >  2.0D6 ) THEN
           RSIG_I4 =  2000000000
         ELSE IF ( RSIG < -2.0D6 ) THEN
           RSIG_I4 = -2000000000
         ELSE 
           RSIG_I4  = NINT(RSIG*1000.0D0)
      END IF 
!
      IF ( K_MN .AND. .NOT. NORATE_FLAG ) THEN
           IPTR=IPTR+1
           IF ( CRES_STYLE .EQ. CRES__PRE98 .OR. &
     &          CRES_STYLE .EQ. CRES__PRE03      ) THEN
                WRITE ( LBUF(IPTR), 5101 ) KC1, RSIG_I4, WRMS(2), &
     &                  CHISQR(2)
 5101           FORMAT ( "   Rate ",2X,I10,7X,I6," fs/s",6X,F9.2,F15.4)
              ELSE 
                IF ( KC1 > 0 ) THEN
                     WRITE ( LBUF(IPTR), 5102 ) KC1, RSIG_I4, WRMS(2), &
     &                       CHISQR(2)
 5102                FORMAT ( "   Rate ",3X,I6,7X,I12," fs/s",7X,F9.2,1X,F12.4)
                   ELSE
                     WRITE ( LBUF(IPTR), 5102 ) KC1, 0, 0.0, 0.0
                END IF 
           END IF
           CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
           CALL NL_MN()
         ELSE IF ( K_MN .AND. NORATE_FLAG ) THEN
           IPTR=IPTR+1
           LBUF(IPTR) = "   No rate"
           CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
           CALL NL_MN()
      ENDIF
      IF ( KSPOOL ) THEN
           IF ( .NOT. NORATE_FLAG ) THEN
                IF ( CRES_STYLE .EQ. CRES__PRE98 .OR. &
     &               CRES_STYLE .EQ. CRES__PRE03      ) THEN
                     WRITE ( 23, 5101 ) KC1, RSIG_I4, WRMS(2), CHISQR(2)
                   ELSE 
                     IF ( KC1 > 0 ) THEN
                          WRITE ( 23, 5102 ) KC1, RSIG_I4, WRMS(2), CHISQR(2)
                       ELSE
                          WRITE ( 23, 5102 ) KC1, 0, 0.0, 0.0
                     END IF
                END IF
              ELSE
                WRITE ( 23, '(A)' ) "   No rate"
           END IF
      END IF
!
! --- Store back zeros if rate was not used:
!
      IF ( .NOT. DATYP_INQ ( IDATYP, RATE__DTP ) ) THEN
           CHISQR(2) = 0.0D0
           RSIG      = 0.0D0
           WRMS(2)   = 0.0D0
      END IF
!
      IF ( K_MN   .AND. .NOT. NORATE_FLAG ) THEN
           IPTR=IPTR+1
           IF ( CRES_STYLE .EQ. CRES__PRE98 .OR. &
     &          CRES_STYLE .EQ. CRES__PRE03      ) THEN
!
                WRITE ( LBUF(IPTR), 5113 ) NC1+KC1, WRMS(3), CHISQR(3)
 5113           FORMAT ( "Combined ", I11, 24X, F9.2, F15.4 )
!@              ELSE
!@                WRITE ( LBUF(IPTR), 5114 ) NC1+KC1, WRMS(3), CHISQR(3)
!@ 5114           FORMAT ( "Combined ", 2X, I6, 31X, F9.2, 1X, F12.4 )
           END IF
           CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
           CALL CLRCH ( LBUF(IPTR+1) )
           CALL NL_MN()
      ENDIF
      IF ( KSPOOL .AND. .NOT. NORATE_FLAG ) THEN
           IF ( CRES_STYLE .EQ. CRES__PRE98 .OR. &
     &          CRES_STYLE .EQ. CRES__PRE03      ) THEN
!
                WRITE ( 23, 5113 ) NC1+KC1, WRMS(3), CHISQR(3)
!              ELSE
!                WRITE ( 23, 5114 ) NC1+KC1, WRMS(3), CHISQR(3)
           END IF
      END IF
!
! --- Write archive records type 4 to the SAR file
!
      COUNT = MAX(NC1,KC1)
      ITPR = 4
      NABF(1) = INT2_DE
      N4BF(2) = COUNT
      RABF(1) = DSIG*1000.
      RABF(2) = WRMS(1)
      RABF(3) = CHISQR(1)
      RABF(4) = FACT(1)
      CALL USE_SARFIL ( 'W', -1 )
!
      NABF(1) = INT2_RA
      N4BF(2) = COUNT
      RABF(1) = RSIG*1000.
      RABF(2) = WRMS(2)
      RABF(3) = CHISQR(2)
      RABF(4) = FACT(2)
      CALL USE_SARFIL ( 'W', -1 )
!
      ITPR = 4
      NABF(1) = INT2_CO
      N4BF(2) = 2*COUNT
      RABF(1) = 0.0D0
      RABF(2) = WRMS(3)
      RABF(3) = CHISQR(3)
      NABF(11)= NPARAM
      CALL USE_SARFIL ( 'W', -1 )
!
! --- Write out relevent info to GLBFxx
!
      IF ( .NOT. CRES_WORKED ) THEN
           CWRMS(1) = NC1 * (WRMS(1) * WRMS(1))
           CWRMS(2) = KC1 * (WRMS(2) * WRMS(2))
           CFACT(1) = FACT(1)
           CFACT(2) = FACT(2)
           CNCSUM   = NC1
           CKCSUM   = KC1
           CSHARE   = 0
         ELSE
           CWRMS(1) = CWRMS(1) + NC1 * (WRMS(1) * WRMS(1))
           CWRMS(2) = CWRMS(2) + KC1 * (WRMS(2) * WRMS(2))
           IF(NC1.GT.0) CFACT(1) = CFACT(1) + FACT(1)
           IF(KC1.GT.0) CFACT(2) = CFACT(2) + FACT(2)
           CNCSUM   = CNCSUM + NC1
           CKCSUM   = CKCSUM + KC1
      END IF
!
! --- Set flag: CRES processed at least one arc
!
      CRES_WORKED = .TRUE.
      CALL USE_GLBFIL ( 'OWC' )
!
! --- Extended statistics about number of observation
!
      IPTR    = IPTR + 2
      IPTR_ST = IPTR
      LBUF(IPTR) = '-------------------------------------------------'// &
     &             '----------------------'
      IPTR    = IPTR + 1
!
      CALL CLRCH ( STR )
      IF ( CRES_STYLE .EQ. CRES__PRE98 ) THEN
           STR = 'PRE98 compatibility'
         ELSE IF ( CRES_STYLE .EQ. CRES__PRE03 ) THEN
           STR = 'PRE03 compatibility'
         ELSE
           STR = '      POST03'
      END IF
!
      WRITE  ( UNIT=LBUF(IPTR), FMT=110 ) STR(1:I_LEN(STR))
 110  FORMAT ( ' CRES mode: ',29X, A )
      IPTR    = IPTR + 1
!
      CALL SUPMET_SHOW ( SUPMET, SUPMET_LINE )
      WRITE  ( UNIT=LBUF(IPTR), FMT=120 ) SUPMET_LINE
 120  FORMAT ( ' Suppression method in use: ',19X,A )
      IPTR    = IPTR + 1
!
      WRITE  ( UNIT=LBUF(IPTR), FMT=130 ) QUALCODE_GOOD_LIM
 130  FORMAT ( ' Used quality_code_limit: ',33X,I1 )
      IPTR    = IPTR + 1
!
      WRITE  ( UNIT=LBUF(IPTR), FMT=140 ) ITOTRC_OBS
 140  FORMAT ( ' Number of potentially recoverable observations: ',5X,I6 )
      IPTR    = IPTR + 1
!
      WRITE  ( UNIT=LBUF(IPTR), FMT=150 ) QUALCODE_GOOD_LIM, ITOTCG_OBS
 150  FORMAT ( ' Number of potentially good observations with QC ',I1, &
     &         '-9: ',I6)
      IPTR    = IPTR + 1
!
      IF ( ITOTRC_OBS .GT. 0 ) THEN
           SHARE = FLOAT(ITOTUS_OBS)/FLOAT(ITOTRC_OBS)
         ELSE
           SHARE = 0.0D0
      END IF
      WRITE  ( UNIT=LBUF(IPTR), FMT=160 ) ITOTUS_OBS, 100.0*SHARE
 160  FORMAT ( ' Number of used observations: ',24X,I6,'  (',F6.2,'%)  ' )
      IPTR    = IPTR + 1
!
      IF ( ITOTUS_OBS+ITOTSU_OBS .GT. 0 ) THEN
           SHARE = FLOAT(ITOTSU_OBS)/FLOAT(ITOTUS_OBS+ITOTSU_OBS)
         ELSE
           SHARE = 0.0D0
      END IF
      WRITE  ( UNIT=LBUF(IPTR), FMT=170 ) ITOTSU_OBS, 100.0*SHARE
 170  FORMAT ( ' Number of suppressed observations: ',18X,I6, &
     &         '  (',f6.2,'%)  ')
      IPTR    = IPTR + 1
!
      LBUF(IPTR) = '-------------------------------------------------'// &
     &             '----------------------'
!
      IF ( KSPOOL ) THEN
           DO IPTR_USE = IPTR_ST, IPTR
              WRITE ( 23, FMT='(A)' ) LBUF(IPTR_USE)(1:72)
           END DO
      END IF
      IF ( K_MN ) THEN
           DO IPTR_USE = IPTR_ST, IPTR
              CALL ADDSTR_F ( LBUF(IPTR_USE)(:PAGEWID) )
              CALL NL_MN()
           END DO
      END IF
!
! --- Write out baseline statistics and archive records type 5
!
      IF ( CRES_STYLE == CRES__PRE98 ) THEN
           TOT_STRING = 'total'
         ELSE
           TOT_STRING = 'recov'
      END IF
!
      IF ( K_MN ) THEN
           IPTR=IPTR+1
           WRITE ( LBUF(IPTR), '(1X)' )
           CALL NL_MN()
!
           IPTR=IPTR+1
           WRITE ( LBUF(IPTR), '(1X)' )
           CALL NL_MN()
!
           IPTR=IPTR+1
           WRITE ( LBUF(IPTR), '(" Baseline Statistics ")' )
           CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
           CALL NL_MN()
!
           IPTR=IPTR+1
           IF ( CRES_STYLE .EQ. CRES__PRE98 .OR. &
     &          CRES_STYLE .EQ. CRES__PRE03      ) THEN
                WRITE ( LBUF(IPTR), '( &
     &             "      Baseline      # W.Obs   W.RMS Del N.R.D. N.R.D. ", &
     &             "WRMSRate N.R.R. D.RW R.RW ")')
              ELSE
                IF ( SRC_LISTING_STYLE == SRC_POST2021_SPOOL__FMT .OR. &
     &               SRC_LISTING_STYLE == SRC_POST2024_SPOOL__FMT      ) THEN
                     WRITE ( LBUF(IPTR), '( &
     &                      "      Baseline      Numb.  Obs  W.RMS Del    Chi/ndf  Database   Exp Code   Nominal mid. epoch")' )
                   ELSE
                     WRITE ( LBUF(IPTR), '( &
     &                      "      Baseline      # W.Obs   W.RMS Del N.R.D.")' )
                END IF
           END IF
           CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
           CALL NL_MN()
!
           IPTR=IPTR+1
           IF ( CRES_STYLE .EQ. CRES__PRE98 .OR. &
     &          CRES_STYLE .EQ. CRES__PRE03      ) THEN
                WRITE ( LBUF(IPTR), "( "//&
     &                 "'                  used/',a,'     ps  standard (',I3, "//&
     &                 "'ps+i))  fs/s              ps  fs/s  ')" ) TOT_STRING, &
     &                                            NINT(REWEIGHT_FLOOR*1.E12)
              ELSE
                IF ( SRC_LISTING_STYLE == SRC_POST2024_SPOOL__FMT ) THEN
                     WRITE ( LBUF(IPTR), "( "//&
     &                       "'                    used ',a,'     ps  ')" ) TOT_STRING
                  ELSE IF ( SRC_LISTING_STYLE == SRC_POST2021_SPOOL__FMT ) THEN
                     WRITE ( LBUF(IPTR), "( "//&
     &                       "'                    used ',a,'     ps  ')" ) TOT_STRING
                   ELSE
                     WRITE ( LBUF(IPTR), "( "//&
     &                       "'                  used/',a,'     ps  ')" ) TOT_STRING
                END IF
           END IF
           CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
           CALL NL_MN()
      ENDIF
!
      IF ( KSPOOL .AND. KFULLOUT ) THEN
           IF ( CRES_STYLE .EQ. CRES__PRE98 .OR. &
     &          CRES_STYLE .EQ. CRES__PRE03      ) THEN
                WRITE ( 23, 5500 ) TOT_STRING, NINT(REWEIGHT_FLOOR*1.E12)
              ELSE
                IF ( SRC_LISTING_STYLE == SRC_POST2021_SPOOL__FMT .OR. &
     &               SRC_LISTING_STYLE == SRC_POST2024_SPOOL__FMT ) THEN
                     WRITE ( 23, 5600 ) TOT_STRING
                   ELSE
                     WRITE ( 23, 5700 ) TOT_STRING
                END IF
           END IF
 5500      FORMAT(//," Baseline Statistics ",/, &
     &         '      Baseline      # W.Obs   W.RMS Del   N.R.D.   N.R.D. ', &
     &         'W.RMS Rate   N.R.R.  D.RW   R.RW ',/, &
     &         '                  used/',a,'     ps     standard (',I3,'ps+i)', &
     &         '  fs/s                ps    fs/s  ',/)
 5600      FORMAT(//," Baseline Statistics ",/, &
     &               '      Baseline      Numb.  Obs  W.RMS Del    Chi/ndf  Database   Exp Code   Nominal mid. epoch'/ &
     &               '                    used ',a,'     ps' )
 5700      FORMAT(//," Baseline Statistics ",/, &
     &               '      Baseline      # W.Obs   W.RMS Del   ', &
     &               'N.R.D.  Database  Exp Code  Nominal mid. epoch'/ &
     &               '                  used/',a,'     ps             ')
      END IF
!
      ITPR = 5
      IEXC = 0
      DO MSTA1 = 1,NUMSTA-1
         DO MSTA2 = MSTA1+1,NUMSTA
            TEST = .TRUE.
            I = 0
            DO WHILE ( TEST  .AND.  I .LT. IBCNT )
               I = I+1
               IF ( ( MSTA1 .EQ. IBAS(1,I) .AND. MSTA2 .EQ. IBAS(2,I)) .OR. &
     &              ( MSTA2 .EQ. IBAS(1,I) .AND. MSTA1 .EQ. IBAS(2,I))    ) THEN
!
                  TEST = .FALSE.
                  IF ( IBAS(3,I) .LE. 0 ) THEN
                       IEXC = 1
                      ELSE
                       IF ( DABS(BF(1,I)) .GT. 1.D-30 ) THEN
                            DSIG = DSQRT ( BW(1,I)/BF(1,I) )*1.0D12
                          ELSE
                            DSIG = 0.0D0
                       END IF
                       CHID = DSQRT ( BW(1,I)/ABS(IBAS(3,I)) )
                       CHID_FLOOR = DSQRT ( BW(3,I)/ABS(IBAS(3,I)) )
                       IF ( DABS(BF(2,I)) .GT. 1.D-30 ) THEN
                            RSIG = DSQRT ( BW(2,I)/BF(2,I) )*1.0D15
                          ELSE
                            RSIG = 0.0D0
                       END IF
                       CHIR = DSQRT ( BW(2,I)/ABS(IBAS(4,I)) )
                  END IF
!
                  ISITX(1) = MSTA1
                  ISITX(2) = MSTA2
                  CALL GET_ADDED_NOISE ( ISITX, ET, ITT, ITTB, DEL_ERROR, &
     &                                   RAT_ERROR )
                  IF ( SIMULATION_TEST ) THEN
                       WRMS(3) = 1.D0
                  ENDIF
!
                  IF ( DSIG .LT. 1.D9  .AND.  DSIG .GT. -1.D9 ) THEN
                       JISIG = NINT(DSIG)
                       DSIG_PRI = DSIG
                     ELSE
                       IF ( DSIG .LT. 0 ) THEN
                            JISIG = -1000000000
                            DSIG_PRI = -99999.999
                       END IF
                       IF ( DSIG .GE. 0 ) THEN
                            JISIG =  1000000000
                            DSIG_PRI = 999999.999
                       END IF
                  END IF
!
                  JDEL_ERROR = DEL_ERROR
                  JRAT_ERROR = RAT_ERROR
!
                  IF ( IS_R8_NAN ( RSIG ) ) THEN
                       RSIG_I4 =  2000000000
                       RSIG_PRI = 999999.999
                     ELSE IF ( RSIG >  2.0D9 ) THEN
                       RSIG_I4 =  2000000000
                       RSIG_PRI = 999999.999
                     ELSE IF ( RSIG < -2.0D9 ) THEN
                       RSIG_I4 = -2000000000
                       RSIG_PRI = -99999.999
                     ELSE 
                       RSIG_I4  = NINT(RSIG)
                       RSIG_PRI = RSIG
                  END IF 
!
                  IF ( IBAS(3,I) .GT. 0  .AND. KFULLOUT .AND. KSPOOL ) THEN
                       IF ( CRES_STYLE .EQ. CRES__PRE98 .OR. &
     &                      CRES_STYLE .EQ. CRES__PRE03      ) THEN
                            IF ( JISIG .LT. 10 ) THEN
                                 WRITE ( 23, 5501 ) ( (ISITN_CHR(IBAS(K,I)) ),K=1,2), &
     &                           IBAS(3,I), IBAS(5,I), DSIG, CHID, CHID_FLOOR, &
     &                           RSIG_I4, CHIR, JDEL_ERROR, JRAT_ERROR
                               ELSE IF ( JISIG .LT. 10000000 ) THEN
                                 WRITE ( 23, 5502 ) ( (ISITN_CHR(IBAS(K,I)) ),K=1,2), &
     &                           IBAS(3,I), IBAS(5,I), JISIG, CHID, &
     &                           CHID_FLOOR, &
     &                           RSIG_I4, CHIR, JDEL_ERROR, JRAT_ERROR
                               ELSE
                                 WRITE ( 23, 5506 ) ( (ISITN_CHR(IBAS(K,I)) ),K=1,2), &
     &                           IBAS(3,I), IBAS(5,I), DSIG, CHID, &
     &                           CHID_FLOOR, &
     &                           RSIG_I4, CHIR, JDEL_ERROR, JRAT_ERROR
                            END IF
                          ELSE IF ( SRC_LISTING_STYLE == SRC_POST2021_SPOOL__FMT .OR. &
     &                              SRC_LISTING_STYLE == SRC_POST2024_SPOOL__FMT      ) THEN
                            IF ( ILEN(EXP_CODE) == 0 ) EXP_CODE=DBNAME_CH(3:10)
                            IF ( JISIG .LT. -99999 .OR. JISIG .GT. 999999 ) THEN
                                 WRITE ( 23, 5602 ) ( (ISITN_CHR(IBAS(K,I)) ),K=1,2), &
     &                                       IBAS(3,I), IBAS(5,I), DSIG, CHID, &
     &                                       DBNAME_CH(1:10), EXP_CODE, &
     &                                       JD_TO_DATE( (FJDOBS+LJDOBS)/2.0D0, -3 )
                              ELSE
                                 WRITE ( 23, 5601 ) ( (ISITN_CHR(IBAS(K,I)) ),K=1,2), &
     &                                       IBAS(3,I), IBAS(5,I), DSIG_PRI, CHID, &
     &                                       DBNAME_CH(1:10), EXP_CODE, &
     &                                       JD_TO_DATE( (FJDOBS+LJDOBS)/2.0D0, -3 )
                            END IF
                          ELSE
                            IF ( ILEN(EXP_CODE) == 0 ) EXP_CODE=DBNAME_CH(3:10)
                            IF ( JISIG .LT. 10 ) THEN
                                 WRITE ( 23, 5611 ) ( (ISITN_CHR(IBAS(K,I)) ),K=1,2), &
     &                                       IBAS(3,I), IBAS(5,I), DSIG, CHID, &
     &                                       DBNAME_CH(1:10), EXP_CODE, &
     &                                       JD_TO_DATE( (FJDOBS+LJDOBS)/2.0D0, -3 )
                               ELSE IF ( JISIG .LT. 10000000 ) THEN
                                 WRITE ( 23, 5612 ) ( (ISITN_CHR(IBAS(K,I)) ),K=1,2), &
     &                                       IBAS(3,I), IBAS(5,I), JISIG, CHID, &
     &                                       DBNAME_CH(1:10), EXP_CODE, &
     &                                       JD_TO_DATE( (FJDOBS+LJDOBS)/2.0D0, -3 )
                               ELSE
                                 WRITE ( 23, 5616 ) ( (ISITN_CHR(IBAS(K,I)) ),K=1,2), &
     &                                       IBAS(3,I), IBAS(5,I), DSIG, CHID, &
     &                                       DBNAME_CH(1:10), EXP_CODE, &
     &                                       JD_TO_DATE( (FJDOBS+LJDOBS)/2.0D0, -3 )
                            END IF
                       END IF
 5501                  FORMAT (1X,A,"-",A,I4,"/",I4,1X,F7.1,4X,F7.2,3X,F7.2,I8, &
     &                         4X,F8.2,2I7, " BAS_STAT" )
 5502                  FORMAT (1X,A,"-",A,I4,"/",I4,1X,I7,4X,F7.2,3X,F7.2,I8, &
     &                         4X,F8.2,2I7, " BAS_STAT" )
 5506                  FORMAT (1X,A,"-",A,I4,"/",I4,1X,1PD9.2,2X,F7.2,3X,F7.2,I8, &
     &                         4X,F8.2,2I7, " BAS_STAT" )
 5601                  FORMAT (1X,A,1X,A,1X,I5,1X,I5,1X,F10.3,  4X,F7.2, 2X, A, 2X, A, 2X, A21, 3X, 'BAS_STAT' )
 5602                  FORMAT (1X,A,1X,A,1X,I5,1X,I5,1X,1PD10.3,2X,F7.2, 2X, A, 2X, A, 2X, A21, 3X, 'BAS_STAT' )
 5611                  FORMAT (1X,A,"-",A,I4,"/",I4,1X,F7.1,  4X,F7.2, 2X, A, 2X, A, 2X, A21, 3X, 'BAS_STAT' )
 5612                  FORMAT (1X,A,"-",A,I4,"/",I4,1X,I7,    4X,F7.2, 2X, A, 2X, A, 2X, A21, 3X, 'BAS_STAT' )
 5616                  FORMAT (1X,A,"-",A,I4,"/",I4,1X,1PD9.2,2X,F7.2, 2X, A, 2X, A, 2X, A21, 3X, 'BAS_STAT' )
                  END IF
!
                  IF ( IBAS(3,I) .GT. 0  .AND.  K_MN ) THEN
                       IF ( CRES_STYLE .EQ. CRES__PRE98 .OR. &
     &                      CRES_STYLE .EQ. CRES__PRE03      ) THEN
                            IPTR=IPTR+1
                            IF ( JISIG .LT. 10 ) THEN
                                 WRITE ( LBUF(IPTR), 5503) &
     &                             ( (ISITN_CHR(IBAS(K,I))),K=1,2), IBAS(3,I), &
     &                                IBAS(5,I), DSIG, CHID, CHID_FLOOR, &
     &                                RSIG_I4, CHIR, jDEL_ERROR, jRAT_ERROR
                               ELSE IF ( JISIG .LT. 10000000 ) THEN
                                 WRITE ( LBUF(IPTR), 5504) &
     &                             ( (ISITN_CHR(IBAS(K,I))),K=1,2), IBAS(3,I), &
     &                               IBAS(5,I), JISIG, CHID, CHID_FLOOR, &
     &                               RSIG_I4, CHIR, jDEL_ERROR, jRAT_ERROR
                               ELSE 
                                 WRITE ( LBUF(IPTR), 5505) &
     &                             ( (ISITN_CHR(IBAS(K,I))),K=1,2), IBAS(3,I), &
     &                               IBAS(5,I), JISIG, CHID, CHID_FLOOR, &
     &                               RSIG_I4, CHIR, jDEL_ERROR, jRAT_ERROR
                            END IF
                          ELSE
                            IPTR=IPTR+1
                            IF ( JISIG .LT. 10 ) THEN
                                 WRITE ( LBUF(IPTR), 5603) &
     &                             ( (ISITN_CHR(IBAS(K,I))),K=1,2), IBAS(3,I), &
     &                                IBAS(5,I), DSIG, CHID
                               ELSE IF ( JISIG .LT. 10000000 ) THEN
                                 WRITE ( LBUF(IPTR), 5604) &
     &                             ( (ISITN_CHR(IBAS(K,I))),K=1,2), IBAS(3,I), &
     &                               IBAS(5,I), JISIG, CHID
                               ELSE 
                                 WRITE ( LBUF(IPTR), 5605) &
     &                             ( (ISITN_CHR(IBAS(K,I))),K=1,2), IBAS(3,I), &
     &                               IBAS(5,I), DSIG, CHID
                            END IF
                       END IF
 5503                  FORMAT ( 1X,A,"-",A,I4,"/",I4,1X,F7.1,1X,F7.2,1X,F7.2,I8, &
     &                          1X, F8.2, 2I5, " BAS_STAT" )
 5504                  FORMAT ( 1X,A,"-",A,I4,"/",I4,1X,I7,1X,F7.2,1X,F7.2,I8, &
     &                          1X, F8.2, 2I5, " BAS_STAT" )
 5505                  FORMAT ( 1X,A,"-",A,I4,"/",I4,1X,1PD8.1,1X,F7.2,1X,F7.2,I8, &
     &                          1X, F8.2, 2I5, " BAS_STAT" )
 5603                  FORMAT ( 1X,A,"-",A,I4,"/", I4, 1X, F7.1,  1X, F7.2, 3X, "BAS_STAT" )
 5604                  FORMAT ( 1X,A,"-",A,I4,"/", I4, 1X, I7,    1X, F7.2, 3X, "BAS_STAT" )
 5605                  FORMAT ( 1X,A,"-",A,I4,"/", I4, 1X, 1PD8.1,1X, F7.2, 3X, "BAS_STAT" )
!
                       CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                       CALL NL_MN()
                   ENDIF
!
                    DO J = 1, 4
                       NABF(J)   = ISITN(J,IBAS(1,I))
                       NABF(J+4) = ISITN(J,IBAS(2,I))
                    END DO
!
                    NABF( 9) = IBAS(3,I)
                    NABF(10) = IBAS(5,I)
                    IF ( IBAS(3,I) .GT. 0 ) THEN  ! The are some good obs.
                         RABF(4) = DSIG
                         RABF(5) = CHID
                         RABF(6) = RSIG
                         RABF(7) = CHIR
                      ELSE                       ! No good obs.
                         RABF(4) = 0.
                         RABF(5) = 0.
                         RABF(6) = 0.
                         RABF(7) = 0.
                    ENDIF
!
                    NABF(19)= IBAS(4, I)
!
! ----------------- This good_site logic keeps sites with no GOOD obs on any
! ----------------- baseline from the SARFIL
!
                    IF ( GOOD_SITE(IBAS(1,I)) .AND. GOOD_SITE(IBAS(2,I)) ) THEN
                         CALL USE_SARFIL ( 'W', -1 )
                    ENDIF
                ENDIF  ! MSTA ...
                IF ( TEST .AND. I .EQ. IBCNT ) Then ! Baseline with no obs of any kind
                     IF ( K_MN ) THEN
                          IPTR=IPTR+1
                          WRITE ( LBUF(IPTR), 5541 )  (ISITN(J,MSTA1),J=1,4), &
     &                                                (ISITN(J,MSTA2),J=1,4)
                          CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                          CALL NL_MN()
                     ENDIF
!
                     IF ( KFULLOUT .AND. KSPOOL ) THEN
                          WRITE(23,5541)  ( ISITN(J,MSTA1),J=1,4 ), &
     &                                    ( ISITN(J,MSTA2),J=1,4 )
 5541                     FORMAT (1X,4A2,"-",4A2,"  No Data")
                          DO J = 1, 4
                             NABF(J) = ISITN(J,MSTA1)
                             NABF(J+4) = ISITN(J,MSTA2)
                          END DO
!
                          NABF( 9) = 0
                          NABF(10) = 0
                          RABF(4)  = 0.
                          RABF(5)  = 0.
                          RABF(6)  = 0.
                          RABF(7)  = 0.
                          NABF(19) = 0
!
! ----------------------- This good_site logic keeps sites with no GOOD obs
! ----------------------- on any baseline from the SARFIL.
!
                          IF ( GOOD_SITE(MSTA1) .AND. GOOD_SITE(MSTA2) ) THEN
                               CALL USE_SARFIL ( 'W', -1 )
                          ENDIF
                     ENDIF  ! kfullout ...
                ENDIF  ! test...
            END DO  ! while test...
          ENDDO ! msta2
      ENDDO ! msta1
!
! --- Check for excluded baselines and print stats -- Ilana  10 Aug 84
!
      IF ( KMINOUT ) GOTO 5599
      IF ( IEXC .EQ. 1 ) THEN
           IF ( KSPOOL) WRITE(23, 5530)
           IF ( K_MN ) THEN
                IPTR=IPTR+1
                WRITE ( LBUF(IPTR), '(1X)' )
                CALL NL_MN()
!
                IPTR=IPTR+1
                WRITE ( LBUF(IPTR),'(1X)' )
                CALL NL_MN()
!
                IPTR=IPTR+1
                WRITE ( LBUF(IPTR), '(" Not included:")' )
                CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                CALL NL_MN()
!
                IPTR=IPTR+1
                WRITE(LBUF(IPTR),'(1x)')
                CALL NL_MN()
            ENDIF
!
 5530       FORMAT (//" Not included:"/)
           DO MSTA1 = 1,NUMSTA-1
               DO MSTA2 = MSTA1+1,NUMSTA
                  TEST = .TRUE.
                  I = 0
                  DO WHILE ( TEST .AND. I .LT. IBCNT )
                     I = I+1
                     IF ( (MSTA1 .EQ.IBAS(1,I) .AND. MSTA2 .EQ.IBAS(2,I)) &
     &                    .OR.(MSTA2 .EQ.IBAS(1,I) .AND. MSTA1 .EQ.IBAS(2,I)) ) THEN
!
                         TEST = .FALSE.
                         IF ( IBAS(3,I) .EQ. 0 ) THEN
!
! --------------------------- The number of used observation is zero
! --------------------------- We print only baseline name and number of
! --------------------------- potentially recoverable observatins
!
                              IPTR = IPTR+1
                              WRITE ( LBUF(IPTR), 5551 ) (ISITN_CHR(IBAS(K,I)), &
     &                                           K=1,2), 0, IBAS(5,I), C_LDBNAM(1), TRIM(EXP_CODE)
 5551                         FORMAT ( 1X,A," ",A,I4,"/",I4,"    deselected baseline    ",A, 2X, A )
                              IF ( K_MN ) THEN
                                   CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                                   CALL NL_MN()
                              END IF
                              IF ( KSPOOL ) THEN
                                   WRITE ( 23, '(A)' ) LBUF(IPTR)
                              END IF
                           ELSE IF ( IBAS(3,I) .LT. 0 ) THEN
!
! --------------------------- The number of used observation is negative.
! --------------------------- This indicate on necessaty to print statistics
!
                              DSIG       = DSQRT(BW(1,I)/BF(1,I))*1.0D12
                              CHID       = DSQRT(BW(1,I)/ABS(IBAS(3,I)))
                              CHID_FLOOR = DSQRT(BW(3,I)/ABS(IBAS(3,I)))
                              RSIG       = DSQRT(BW(2,I)/BF(2,I))*1.0D15
                              CHIR       = DSQRT(BW(2,I)/ABS(IBAS(4,I)))
                              IF ( KSPOOL ) THEN
                                   IF ( CRES_STYLE .EQ. CRES__PRE98 .OR. &
     &                                  CRES_STYLE .EQ. CRES__PRE03      ) THEN
                                        WRITE ( 23, 5501 ) (ISITN_CHR(IBAS(K,I)), K=1,2), &
     &                                         -IBAS(3,I), IBAS(5,I), NINT(DSIG), CHID, &
     &                                         CHID_FLOOR, NINT(RSIG), CHIR
                                     ELSE
                                        WRITE ( 23, 5601 ) (ISITN_CHR(IBAS(K,I)), K=1,2), &
     &                                         -IBAS(3,I), IBAS(5,I), NINT(DSIG), CHID
                                   END IF
                              END IF
!
                              IF ( K_MN ) THEN
                                   IPTR=IPTR+1
                                   IF ( CRES_STYLE .EQ. CRES__PRE98 .OR. &
     &                                  CRES_STYLE .EQ. CRES__PRE03      ) THEN
                                        WRITE ( LBUF(IPTR), 5501) &
     &                                         (ISITN_CHR(IBAS(K,I)),K=1,2), -IBAS(3,I), &
     &                                          IBAS(5,I),  NINT(DSIG), CHID, &
     &                                          CHID_FLOOR, NINT(RSIG), CHIR
                                      ELSE
                                        WRITE ( LBUF(IPTR), 5601) &
     &                                         (ISITN_CHR(IBAS(K,I)),K=1,2), -IBAS(3,I), &
     &                                          IBAS(5,I),  NINT(DSIG), CHID
                                   END IF
                                   CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                                   CALL NL_MN()
                              ENDIF ! k_mn
                         ENDIF ! ibas
                     ENDIF ! msta
                  ENDDO
               ENDDO
            END DO
      END IF  ! IEXC
!
      IF ( KSPOOL ) WRITE ( 23, 5540 )
 5540 FORMAT(/)
 5599 CONTINUE
      IF(KMINOUT) GOTO 8600
!
! --- Write out the site statistics.
! --- Currently only the site weight statistics are printed, and these are
! --- only printed if the selected weights are actually site weights.
!
      WEIGHTING_TYPE_TEMP = WEIGHTING_TYPE
      IF ( WEIGHTING_TYPE_TEMP .NE. 'ST' ) WEIGHTING_TYPE = REWAY_TYPE
!
      IF ( WEIGHTING_TYPE_TEMP .EQ. 'ST' ) THEN
           IF ( KSCREEN .AND. K_MN ) THEN
                IPTR=IPTR+1
                WRITE ( LBUF(IPTR), '(1X)' )
                CALL NL_MN()
!
                IPTR=IPTR+1
                WRITE ( LBUF(IPTR), '(1X)' )
                CALL NL_MN()
!
                IPTR=IPTR+1
                WRITE ( LBUF(IPTR), '(" Site Statistics ", &
     &                                "(site_name  D.RW_in_ps  R.RW_in_fs/s)")')
                CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                CALL NL_MN()
           ENDIF
!
           IF ( KSPOOL ) THEN
                WRITE ( 23, '(A)' ) ' Site Statistics '// &
     &                              '(site_name  D.RW_in_ps  R.RW_in_fs/s)'
           END IF
           CALL WTS_BL_OTH ( ET, INT2(1), NUMSTA, SITE_WTS, DPDUM2 )
           DO ICT = 1,NUMSTA
              SITE_WTS(1,ICT) = SITE_WTS(1,ICT)*1.E12
              SITE_WTS(2,ICT) = SITE_WTS(2,ICT)*1.E15
           ENDDO
!
! -------- Dump as many sites (and their weights) as will form full rows of
! -------- 7 sites
!
           NUM_IN_ROW = 7
           NUM_FULL_ROWS = NUMSTA/NUM_IN_ROW
           NUM_CHARS_PER_SITE = 17
           IF ( NUM_FULL_ROWS .GT. 0 ) THEN
                DO NCT = 1,NUM_FULL_ROWS
!
! ---------------- Build a buffer for the current row
!
                   SITE_BUFFER = ' '
                   IOUTS = 2
                   IOUTF = 1+NUM_CHARS_PER_SITE
                   DO LCT = 1, NUM_IN_ROW
!
! ------------------- Add into the buffer as many sites as will fit into a full row
!
                      ICT = NUM_IN_ROW*(NCT-1)+LCT ! Pointer to current site in isitn
                      IHOLD(1) = SITE_WTS(1,ICT)
                      IHOLD(2) = SITE_WTS(2,ICT)
                      WRITE ( SITE_BUFFER(IOUTS:IOUTF), "(1X,4A2,I4,I4)") &
     &                      ( ISITN(JCT,ICT),JCT=1,4 ), &
     &                      ( IHOLD(JCT),JCT=1,2)
                      IOUTS = IOUTS + NUM_CHARS_PER_SITE
                      IOUTF = IOUTF + NUM_CHARS_PER_SITE
                   ENDDO
!
                   IOUTF = IOUTF - NUM_CHARS_PER_SITE
!
! ---------------- Dump the buffer to the spool file and/or terminal
!
                   IF ( KSPOOL ) WRITE ( 23, '(A)' ) SITE_BUFFER(1:IOUTF)
                   IF ( KSCREEN .AND. K_MN ) THEN
                        IPTR = IPTR + 1
                        WRITE ( LBUF(IPTR), '(A)' ) SITE_BUFFER(1:IOUTF)
                        CALL ADDSTR_F(LBUF(IPTR)(:PAGEWID) )
                        CALL NL_MN()
                   ENDIF
                ENDDO
           END IF ! num_full_rows .gt. 0
!
! -------- Dump any leftover sites that don't form a full row of 7
!
           IF ( NUMSTA/NUM_IN_ROW*NUM_IN_ROW .NE. NUMSTA ) THEN
                SITE_BUFFER = ' '
                IOUTS = 2
                IOUTF = 1+NUM_CHARS_PER_SITE
                DO ICT = NUM_FULL_ROWS*NUM_IN_ROW+1,NUMSTA
                   IHOLD(1) = SITE_WTS(1,ICT)
                   IHOLD(2) = SITE_WTS(2,ICT)  
                   WRITE ( SITE_BUFFER(IOUTS:IOUTF), "(1X,4A2,I4,I4)") &
     &                     ( ISITN(JCT,ICT),JCT=1,4 ), &
     &                       IHOLD(1), IHOLD(2)
!!!     &                     ( SITE_WTS(JCT,ICT),JCT=1,2)
                   IOUTS = IOUTS + NUM_CHARS_PER_SITE
                   IOUTF = IOUTF + NUM_CHARS_PER_SITE
                ENDDO
!
                IOUTF = IOUTF - NUM_CHARS_PER_SITE
!
! ------------- Dump the buffer to the spool file and/or terminal
!
                IF ( KSPOOL ) WRITE ( 23, '(A)' ) SITE_BUFFER(1:IOUTF)
                IF ( KSCREEN .AND. K_MN ) THEN
                     IPTR = IPTR + 1
                     WRITE ( LBUF(IPTR), '(A)' ) SITE_BUFFER(1:IOUTF)
                     CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                     CALL NL_MN()
                ENDIF
           ENDIF  !  numsta/num_in_row*num_in_row
           IF ( KSPOOL ) WRITE(23,"(/)")
      ENDIF
 8600 CONTINUE
!
! --- Write out source statistics  and archive records type 6
!
      IF ( ISCNT .EQ. 1 ) GOTO 5699
      ITPR = 6
      IF ( K_MN ) THEN
           IPTR=IPTR+1
           WRITE ( LBUF(IPTR), '(1X)' )
           CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
           CALL NL_MN()
!
           IPTR=IPTR+1
           WRITE ( LBUF(IPTR), '(" Source Statistics ")' )
           CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
           CALL NL_MN()
!
           IPTR=IPTR+1
           IF ( SRC_LISTING_STYLE == SRC_POST2024_SPOOL__FMT      ) THEN
                WRITE ( LBUF(IPTR), '(A)' ) '           B-name    J-name       #use  #rec  #tot   '// &
     &                                      'wrms (ps) Usca Tsca Database   Exp Code   Date'
              ELSE IF ( SRC_LISTING_STYLE == SRC_POST2021_SPOOL__FMT ) THEN
                WRITE ( LBUF(IPTR), '(A)' ) '           B-name    J-name       #use  #rec  #tot'// &
     &                                      '   wrms (ps)  Database   Exp Code'
              ELSE
                WRITE ( LBUF(IPTR), '( &
     &            "     Source         # W.Obs   W.RMS Del   N.R.D.   N.R.D. ", &
     &            "W.RMS Rate   N.R.R. ")')
           END IF
           CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
           CALL NL_MN()
!
           IPTR=IPTR+1
           IF ( SRC_LISTING_STYLE == SRC_POST2024_SPOOL__FMT .OR. &
     &          SRC_LISTING_STYLE == SRC_POST2021_SPOOL__FMT      ) THEN
                LBUF(IPTR) = ' '
              ELSE
                WRITE ( LBUF(IPTR), '("                                 ps     ", &
     &                  "standard  (",I3,"ps)   fs/s   ")') NINT(REWEIGHT_FLOOR*1.D12)
           ENDIF
           CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
           CALL NL_MN()
      ENDIF
!
      IF ( KSPOOL .AND. KFULLOUT ) THEN
           WRITE ( 23, '(A)' ) ' Source Statistics '
           IF ( SRC_LISTING_STYLE == SRC_SHORT_SPOOL__FMT ) THEN
                WRITE ( 23, '(A)' ) ' Let   Source      #use #rec #tot     wrms (ps)'
              ELSE IF ( SRC_LISTING_STYLE == SRC_LONG_SPOOL__FMT ) THEN
                WRITE ( 23, '(A)' ) '           Source     #use #rec #tot  '// &
     &                              'wrms (ps)  Database    Exp Code  Nominal mid. epoch'
              ELSE IF ( SRC_LISTING_STYLE == SRC_POST2021_SPOOL__FMT ) THEN
                WRITE ( 23, '(A)' ) '           B-name    J-name       #use  #rec  #tot'// &
     &                              '   wrms (ps)  Database   Exp Code   Nominal mid. epoch'
              ELSE IF ( SRC_LISTING_STYLE == SRC_POST2024_SPOOL__FMT ) THEN
                WRITE ( 23, '(A)' ) '           B-name    J-name       #use  '// &
     &                              '#rec  #tot   wrms (ps) Usca Tsca Database'// &
     &                              '   Exp Code   Nominal mid. epoch'
              ELSE ! PRE2004
                WRITE(23,5510) NINT(REWEIGHT_FLOOR*1.E12)
 5510 FORMAT (  &
     &          '     Source         # W.Obs   W.RMS Del   N.R.D.   N.R.D. ', &
     &          'W.RMS Rate   N.R.R. ',/, &
     &          '                                 ps     stand', &
     &          'ard  (',I3,'ps)   fs/s   ')
           END IF
           WRITE ( 23, '(A)' ) ' '
      END IF
!
      IF ( SIMULATION_TEST) THEN
           IF ( K_MN ) THEN
                IPTR=IPTR+1
                LBUF(IPTR) = 'WARNING: Simulation -  Weighted RMS '// &
     &                       'residuals set to 1'
                CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                CALL NL_MN()
           ENDIF
           IF ( KSPOOL )  WRITE( 23, '(A)' ) 'WARNING:  Simulation -- '// &
     &                    'Weighted RMS residuals set to 1'
      ENDIF
!
      DO I = 1, ISCNT
         IF ( ABS(ISRC(2,I)) .NE. 0 ) THEN
              IF ( DABS(SF(1,I)) .GT. 1.D-30 ) THEN
                   DSIG  = DSQRT(SW(1,I)/SF(1,I))*1.0D12
                 ELSE
                   DSIG  = 0.0D0
              END IF
              CHID       = DSQRT(SW(1,I)/ABS(ISRC(2,I)))
              CHID_FLOOR = DSQRT(SW(3,I)/ABS(ISRC(2,I)))
            ELSE
              DSIG =0.0D0
              CHID =0.0D0
              CHID_FLOOR =0.0D0
         ENDIF
!
         IF ( ABS(ISRC(3,I) ) .NE. 0 ) THEN
              IF ( DABS(SF(2,I)) .GT. 1.D-30 ) THEN
                   RSIG = DSQRT(SW(2,I)/SF(2,I))*1.0D15
                 ELSE
                   RSIG = 0.0D0
              END IF
              CHIR      = DSQRT(SW(2,I)/ABS(ISRC(3,I)))
            ELSE
              RSIG = 0.0D0
              CHIR = 0.0D0
         ENDIF
         I_LET      = MOD(I-1,26) + 1
         IF ( SIMULATION_TEST ) THEN
              WRMS(3) = 1.D0
         ENDIF
!
         IF ( ISRC(2,I) .GE. 0  ) THEN
              IPTR=IPTR+1
              IF ( IS_R8_NAN ( DSIG ) ) DSIG = 2.0D9
              IF ( DSIG .LT. 1.D9  .AND.  DSIG .GT. -1.D9 ) THEN
                   JISIG = NINT(DSIG)
                 ELSE
                   IF ( DSIG .LT. 0 ) JISIG = -1000000000
                   IF ( DSIG .GE. 0 ) JISIG =  1000000000
              END IF
!
              IF ( IS_R8_NAN ( RSIG ) ) RSIG = 2.0D9
              IF ( RSIG .LT. 1.D9  .AND.  RSIG .GT. -1.D9 ) THEN
                   RSIG_I4 = NINT(RSIG)
                 ELSE
                   IF ( RSIG .LT. 0 ) RSIG_I4 = -1000000000
                   IF ( RSIG .GE. 0 ) RSIG_I4 =  1000000000
              END IF
!
              IF ( SRC_LISTING_STYLE == SRC_SHORT_SPOOL__FMT ) THEN
                   WRITE ( LBUF(IPTR), 5511 ) SOURCE_LETTERS(I_LET:I_LET), &
     &                                   ISTRN_CHR(ISRC(1,I)), ISTAT_SRC(1,I), &
     &                                   ISTAT_SRC(2,I), ISTAT_SRC(3,I), DSIG
 5511              FORMAT ( 3X,A1, 3X, A, 3X,3(I4,"/"), 1X, F12.3 )
                 ELSE IF ( SRC_LISTING_STYLE == SRC_LONG_SPOOL__FMT ) THEN
                   IF ( ILEN(EXP_CODE) == 0 ) EXP_CODE=DBNAME_CH(3:10)
                   IF ( DSIG < 999999.9D0 ) THEN
                         WRITE ( LBUF(IPTR), 5512 ) ISTRN_CHR(ISRC(1,I)), &
     &                           ISTAT_SRC(1,I), ISTAT_SRC(2,I), ISTAT_SRC(3,I), &
     &                           DSIG, DBNAME_CH(1:10), EXP_CODE, &
     &                           JD_TO_DATE( (FJDOBS+LJDOBS)/2.0D0, -3 )
                     ELSE 
                         WRITE ( LBUF(IPTR), 5522 ) ISTRN_CHR(ISRC(1,I)), &
     &                           ISTAT_SRC(1,I), ISTAT_SRC(2,I), ISTAT_SRC(3,I), &
     &                           DSIG, DBNAME_CH(1:10), EXP_CODE, &
     &                           JD_TO_DATE( (FJDOBS+LJDOBS)/2.0D0, -3 )
                   END IF
 5512              FORMAT ( 'SRC_STAT:', 2X, A, 2X,3(I4,1X), 1X, F10.3, &
     &                      2X,A, 2X,A, 2X, A )
 5522              FORMAT ( 'SRC_STAT:', 2X, A, 2X,3(I4,1X), 1X, 1PD10.3, &
     &                      2X,A, 2X,A, 2X, A  )
                   LBUF(IPTR)(93:101) = '         '
                 ELSE IF ( SRC_LISTING_STYLE == SRC_POST2021_SPOOL__FMT ) THEN
                   IF ( ILEN(EXP_CODE) == 0 ) EXP_CODE=DBNAME_CH(3:10)
                   IF ( DSIG < 999999.9D0 ) THEN
                        WRITE ( LBUF(IPTR), 5513 ) ISTRN_CHR(ISRC(1,I)), JNAME(ISRC(1,I)), &
     &                          ISTAT_SRC(1,I), ISTAT_SRC(2,I), ISTAT_SRC(3,I), &
     &                          DSIG, DBNAME_CH(1:10), EXP_CODE, &
     &                          JD_TO_DATE( (FJDOBS+LJDOBS)/2.0D0, -3 )
                     ELSE 
                        WRITE ( LBUF(IPTR), 5523 ) ISTRN_CHR(ISRC(1,I)), JNAME(ISRC(1,I)), &
     &                          ISTAT_SRC(1,I), ISTAT_SRC(2,I), ISTAT_SRC(3,I), &
     &                          DSIG, DBNAME_CH(1:10), EXP_CODE, &
     &                          JD_TO_DATE( (FJDOBS+LJDOBS)/2.0D0, -3 )
                   END IF
 5513              FORMAT ( 'SRC_STAT:', 2X, A, 2X, A, 2X,3(I5,1X), 1X, F10.3, &
     &                      2X,A, 2X,A, 2X, A )
 5523              FORMAT ( 'SRC_STAT:', 2X, A, 2X, A, 2X,3(I5,1X), 1X, 1PD10.3, &
     &                      2X,A, 2X,A, 2X, A  )
                 ELSE IF ( SRC_LISTING_STYLE == SRC_POST2024_SPOOL__FMT ) THEN
                   IF ( ILEN(EXP_CODE) == 0 ) EXP_CODE=DBNAME_CH(3:10)
                   IF ( DSIG < 999999.9D0 ) THEN
                        WRITE ( LBUF(IPTR), 5514 ) ISTRN_CHR(ISRC(1,I)), JNAME(ISRC(1,I)), &
     &                          ISTAT_SRC(1,I), ISTAT_SRC(2,I), ISTAT_SRC(3,I), &
     &                          DSIG, NSCA_USED(ISRC(1,I)), NSCA_TOT(ISRC(1,I)), &
     &                          DBNAME_CH(1:10), EXP_CODE, &
     &                          JD_TO_DATE( (FJDOBS+LJDOBS)/2.0D0, -3 )
                     ELSE 
                        WRITE ( LBUF(IPTR), 5524 ) ISTRN_CHR(ISRC(1,I)), JNAME(ISRC(1,I)), &
     &                          ISTAT_SRC(1,I), ISTAT_SRC(2,I), ISTAT_SRC(3,I), &
     &                          DSIG, NSCA_USED(ISRC(1,I)), NSCA_TOT(ISRC(1,I)), &
     &                          DBNAME_CH(1:10), EXP_CODE, &
     &                          JD_TO_DATE( (FJDOBS+LJDOBS)/2.0D0, -3 )
                   END IF
 5514              FORMAT ( 'SRC_STAT:', 2X, A, 2X, A, 2X,3(I5,1X), 1X, F10.3, &
     &                      2X, I3, 1X, I3, 2X,A, 2X,A, 2X, A )
 5524              FORMAT ( 'SRC_STAT:', 2X, A, 2X, A, 2X,3(I5,1X), 1X, 1PD10.3, &
     &                      2X, I3, 1X, I3, 2X,A, 2X,A, 2X, A )
                 ELSE 
                   IF ( JISIG .LT. 10 ) THEN
                        WRITE ( LBUF(IPTR), 5515 ) ISTRN_CHR(ISRC(1,I)), &
     &                          SOURCE_LETTERS(I_LET:I_LET), ISRC(2,I), &
     &                          ISRC(4,I), DSIG, CHID, CHID_FLOOR, &
     &                          RSIG_I4, CHIR
 5515                   FORMAT ( 5X,A,1X,A1,3X,I4,"/",I4,1X,F7.1,5X, &
     &                           F7.2,3X,F6.2,I8, 5X,F7.2)
                     ELSE
                        WRITE ( LBUF(IPTR), 5516 ) ISTRN_CHR(ISRC(1,I)), &
     &                          SOURCE_LETTERS(I_LET:I_LET), ISRC(2,I), &
     &                          ISRC(4,I), JISIG, CHID, CHID_FLOOR, &
     &                          RSIG_I4, CHIR
 5516                   FORMAT ( 5X,A,1X,A1,3X,I4,"/",I4,1X,I7,5X,F7.2,3X, &
     &                           F6.2,I8, 5X,F7.2)
                   END IF
              END IF
!
              IF ( K_MN ) THEN
                   IF ( SRC_LISTING_STYLE == SRC_POST2021_SPOOL__FMT .OR. &
     &                  SRC_LISTING_STYLE == SRC_POST2024_SPOOL__FMT      ) THEN
                        STR = LBUF(IPTR)
                        CALL CLRCH ( STR(84:) )
                        CALL ADDSTR_F ( STR(1:PAGEWID) )
                      ELSE 
                        CALL ADDSTR_F ( LBUF(IPTR)(1:PAGEWID) )
                   END IF
                   CALL NL_MN()
              ENDIF
              IF ( KFULLOUT .AND. KSPOOL ) THEN
                   WRITE ( 23, '(A)' ) LBUF(IPTR)(1:I_LEN(LBUF(IPTR)))
              END IF
         END IF
!
         DO J = 1, 4
            NABF(J) = ISTRN(J,ISRC(1,I))
         END DO
         JNAME_SARS = JNAME(I)
!
         NABF(5) = ISRC(2,I)
         NABF(6) = ISRC(4,I)
         RABF(4) = DSIG
         RABF(5) = CHID
         RABF(6) = RSIG
         RABF(7) = CHIR
         NABF(19)= ISRC(3, I)
         NSCA_TOT_SARS  = NSCA_TOT(I)
         NSCA_USED_SARS = NSCA_USED(I)
         WEIGHTED_EPOCH_SARS = WEIGHTED_EPOCH(I)
         WEIGHT_SUM_SARS     = WEIGHT_SUM(I)
!
         CALL USE_SARFIL ( 'W', -1 )
      END DO
 5699 CONTINUE
      IF ( K_MN ) THEN
           CALL REFRESH_MN()
      ENDIF
      IF ( KBIT( PRE_IP(3), INT2(12)) .OR. .NOT. KBATCH ) THEN
           CALL ACS_RESFIL ('C' )
      END IF
!
! --- Turn on and schedule program ADJST without wait and terminate.
! --- Write out common.
!
      SOL_AVAIL = .TRUE.
!
      CALL ACS_SARFIL ( 'SC'  )
      IF ( K_MN ) THEN
!
! -------- Write saved screen output to file to be used by adjust to scroll
! -------- back display
!
           FNAME = PRE_SCR_DIR(:PRE_SD_LEN)//'CRBF'//PRE_LETRS
           OPEN ( 34, FILE=FNAME )
           DO I=1,IPTR
              WRITE ( 34, '(A120)', IOSTAT=IOS ) LBUF(I)
              IERR = IOS
              CALL FERR ( IERR, "Writing CRES screen display buffer", INT2(0), &
     &             INT2(0) )
           ENDDO
           CLOSE ( 34 )
      END IF
      IF ( KSCREEN ) CALL END_MN
!
      RETURN
      END  !#!  THIRD  #!#
