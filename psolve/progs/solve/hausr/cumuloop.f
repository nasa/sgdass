      SUBROUTINE   CUMULOOP ( ICNTRL )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  CUMULOOP PROGRAM SPECIFICATION
!
! 1.1 Set up the arc/global arrays by reading the GLBFxx common
!
! 1.2 REFERENCES:
!
! 2.  CUMULOOP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'corpar.i'
!
! 2.2 INPUT Variables:
!
      integer*2 ICNTRL
!
! ICNTRL -  Zero      - HAUSR is being run interactively
!           None zero - HAUSR ib being run in batch mode
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'sareq.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: hausr
!       CALLED SUBROUTINES: bline, sourc
!
! 3.  LOCAL VARIABLES
!
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc2.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'fast.i'
      INCLUDE 'hausr.i'
!
      INTEGER*4    IOS, LOUT
      INTEGER*4    GLBMINSUP, MAXTOTS
      INTEGER*4    NRECORDS
      CHARACTER    BUF1*63, BUF2*63
!
!
      REAL*8       CHISQR_CUM(3), DSIG_CUM, RSIG_CUM, DPARAM, WRMS_CUM(3), TEMP
      REAL*8       CHISQR_CUM_SAVE
      INTEGER*4    NUM, J, NCKCSUM, TOTAL_PARMS, TOTAL_ARC_PARMS, NUMSTATS, &
     &             NUM_RATES
      INTEGER*2    IERR, I, LOOP, LL, ARCS, KK, START, STOP, TOTS
      lOGICAL*2    KEXIST
      DIMENSION    ARCS(MAX_ARCS), TOTS(MAX_ARCS)
      CHARACTER    FNAME*(NAME_SIZE) 
      CHARACTER    STR*20, STR1*20, STR8*8
      INTEGER*2    INT2_ARG, INT2_DE, INT2_RA
      PARAMETER  ( INT2_RA = 2HRA )
      PARAMETER  ( INT2_DE = 2HDE )
      INTEGER*4    INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4,   EXTERNAL :: GET_UNIT, I_LEN
!
! 4.  HISTORY
!  WHO  WHEN   WHAT
!
!  kdb 951207  Integer*4 number of observations
!  pet 970713  Added include block corpar. Made parameter MAX_ARC a global one
!  pet 970717  Suprerssed printing Chi-square in the case when it was not
!              FAST_COV F__SEG mode
!  pet 990417  Increased number of digits in output
!  pet 2000.01.10   Re-defined arrays ARCS and TOTS as MAX_ARCS instead of
!                   MAX_ARC
!  pet 2001.06.14   Activated the traps of internal control of float operations
!  pet 2002.10.01   Fixed a bug which lieved 209 years: reported chi-sq(All)
!                   was incorrect.
!  pet 2002.10.01   Added code for writing statistivs to the end of Sinex
!                   listing if making the listing in sinex format was requested.
!  pet 2014.09.03   Added more space for print out the number of delays (more than 10 millions)
!  pet 2023.11.17   Added more space for print out the number of arc and the arc index
!  pet 2024.07.09   Added support for computation of scan statistics and weighted epoch
!                   for a given source
!
! 5.  CUMULOOP PROGRAM STRUCTURE
!
!   calculate cumulative statistics
!
! --- Read first record of archive file for number of records,
! --- location of statistics record, if stats previously calculated,
! --- and flag indicating wether or not the file has been updated.
!
      CALL ACS_SARFIL('O' )
!
      NUM = 1  ! first record!
!
      CALL USE_SARFIL ( 'R', NUM )
!
! --- Data structure for first record:
!
      NRECORDS = N4BF(1) - 2
      NUMSTATS = N4BF(2)   ! Double integer rep of numstats:
!
! --- Note that globals are added only once and as of 900727,
! --- suppressed globals are not counted. (MWH)
!
      GLBMINSUP   = TGLBLS  - GLBSUP
      TOTAL_PARMS = CNPARAM + GLBMINSUP
      NCKCSUM     = CNCSUM  + CKCSUM
      NEXCPT      = NACSTA  + NACSRC
!
      IF ( CFACT(1) .GT. 1.D-30 ) THEN
           DSIG_CUM = DSQRT ( CWRMS(1)/CFACT(1) ) * 1.0D9
         ELSE
           DSIG_CUM = 0.0
      END IF
      IF ( CFACT(2) .GT. 1.D-30 ) THEN
           RSIG_CUM = DSQRT ( CWRMS(2)/CFACT(2) ) * 1.0D12
         ELSE
           RSIG_CUM = 0.0D0
      END IF
      IF ( CKCSUM+CNCSUM-TOTAL_PARMS .LT. 0.0D0 ) THEN
           CHISQR_CUM(1)=0.D0
         ELSE
           CHISQR_CUM(3) = ( CWRMS(1) + CWRMS(2)) &
     &                     /( CKCSUM + CNCSUM - TOTAL_PARMS + CSHARE )
      ENDIF
!
      DPARAM = (TOTAL_PARMS-CSHARE)
      IF ( CNCSUM-DPARAM .LE. 0.0D0 ) THEN
           CHISQR_CUM(1)=0.0D0
        ELSE
           CHISQR_CUM(1) = CWRMS(1)/(CNCSUM - DPARAM)
      ENDIF
!
      CHISQR_CUM(1)  = CWRMS(1)/(CNCSUM - DPARAM)
      CHISQR_CUM_SAVE = CHISQR_CUM(1) ! save it for future
      NUM_RATES = CKCSUM              ! save it for ftuure
!
      IF ( CKCSUM + CNCSUM > 0 ) THEN
           DPARAM = CKCSUM * (TOTAL_PARMS-CSHARE)/(CKCSUM + CNCSUM)
         ELSE
           DPARAM = 0.0D0
      END IF
      IF ( CKCSUM-DPARAM .LE. 0.0D0 ) THEN
           CHISQR_CUM(2) = 0.0D0
         ELSE
           CHISQR_CUM(2) = CWRMS(2)/(CKCSUM - DPARAM)
      ENDIF
!
      IF ( CKCSUM+CNCSUM .LE. 0.0D0 ) THEN
           WRMS_CUM(3) = 0.0D0
        ELSE
           IF ( CKCSUM + CNCSUM > 0 ) THEN
                WRMS_CUM(3) = DSQRT( (CWRMS(1) + CWRMS(2))/(CNCSUM + CKCSUM) )
              ELSE
                WRMS_CUM(3) = 0.0D0
           END IF
      ENDIF
      IF ( CKCSUM .LE. 0.0D0 ) THEN
           WRMS_CUM(2)= 0.0D0
         ELSE
           WRMS_CUM(2) = DSQRT ( CWRMS(2)/CKCSUM )
      ENDIF
      IF ( CNCSUM .LE. 0.0D0 ) THEN
           WRMS_CUM(1)= 0.0D0
        ELSE
           WRMS_CUM(1) = DSQRT ( CWRMS(1)/CNCSUM )
      ENDIF
!
      WRITE  ( 23, 5001 )
 5001 FORMAT ( /,"1Overall Solution Statistics Summary")
      WRITE  ( 23, 5005 )
 5005 FORMAT (/, &
     & " Data Type     Number of   Weighted RMS    Normalized RMS   Chi", &
     & " Square",/, &
     & "             Observations    Residual         Residual",/, &
     & "                 Used")
      IF ( FAST_MODE .EQ. F__NONE  .OR.  FAST_COV .EQ. F__SEG ) THEN
           WRITE ( 23, 5010 ) CNCSUM, DSIG_CUM*1000., WRMS_CUM(1), &
     &                        CHISQR_CUM(1)
 5010      FORMAT ( /,"   Delay",5X,I8,1X,F12.3," ps",7X,F9.3,F15.3)
           WRITE  ( 23, 5020 ) CKCSUM,RSIG_CUM*1000., WRMS_CUM(2), &
     &                         CHISQR_CUM(2)
 5020      FORMAT ( "   Rate ",5X,I8,1X,F12.3," fs/s",5X,F9.3,F15.3)
           WRITE  ( 23, 5030 ) NCKCSUM, WRMS_CUM(3), CHISQR_CUM(3)
 5030      FORMAT ( "   Combined", 2X, I8, 23X, F9.3, F15.3 )
         ELSE
           WRITE  ( 23, 5011 ) CNCSUM, DSIG_CUM*1000., WRMS_CUM(1)
 5011      FORMAT ( /,"   Delay",5X,I8,1X,F12.3," ps",7X,F9.3,'  N/A  ' )
           WRITE  ( 23, 5021 ) CKCSUM,RSIG_CUM*1000., WRMS_CUM(2)
 5021      FORMAT ( "   Rate ",5X,I8,1X,F12.3," fs/s",5X,F9.3,'  N/A  ' )
           WRITE  ( 23, 5031 ) NCKCSUM, WRMS_CUM(3)
 5031      FORMAT ( "   Combined", 2X, I8, 23X, F9.3, '  N/A  ' )
      END IF
!
! --- Loop through archive file
!
      ARC_NUMBER      = 0
      TOTAL_ARC_PARMS = 0
      IBNDX           = 0
      ISNDX           = 0
      MAXTOTS         = 0
!
      FNAME=PRE_SCR_DIR(1:PRE_SD_LEN)//'HAUS'//PRE_LETRS
      CALL BIN_EXIST ( FNAME, KEXIST )
      IF ( KEXIST ) CALL BIN_UNLINK ( FNAME, IERR )
      CALL FTN_OPEN ( INT2(19), FNAME, ' ' )
      WRITE ( 19, 1002, IOSTAT=IOS )
      CALL FERR ( INT2(IOS), 'Writing '//FNAME, INT2(0), INT2(0) )
 1002 FORMAT &
     &  ('arc#        NC1     WRMS(1)           FACT(1)      DSIG(ps)', &
     & 6X,'       KC1     WRMS(2)           FACT(2)      RSIG(ps)')
      DSIG_CUM = 0.D0
      RSIG_CUM = 0.D0
      CWRMS(1) = 0.D0
      CWRMS(2) = 0.D0
      CFACT(1) = 0.D0
      CFACT(2) = 0.D0
      CHISQR_CUM(1) = 0.D0
      CHISQR_CUM(2) = 0.D0
      CNCSUM  = 0
      CKCSUM  = 0
      SOURCES = 0
      DO J = 1, NRECORDS
!
! ------ Physical file record number NUM is J + 1
!
          NUM = J + 1
!
          CALL USE_SARFIL ( 'R', NUM )
!
! ------- Keep track of arc parms per arc
!
          IF ( ITPR .EQ. 1 ) THEN
              ARC_NUMBER = ARC_NUMBER + 1
              IF ( ARC_NUMBER .GT. MAX_ARCS ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( INT4(ARC_NUMBER), STR )
                   CALL CLRCH ( STR1 )
                   CALL INCH  ( INT4(MAX_ARCS), STR1 )
                   CALL ERR_LOG ( 6101, -1, 'CUMULOOP', 'Number '// &
     &                 'of arcs '//STR(1:I_LEN(STR))//' exceeded the limit: '// &
     &                  STR1(1:I_LEN(STR1))//' -- MAX_ARCS (defined in '// &
     &                 'solve.i)' )
                   CALL FERR ( INT2(123), &
     &                 'CUMULOOP(HAUSR) Exceeded maximum number '//'of arcs', &
     &                  INT2(0), INT2(0) )
                   STOP 'HAUSR Abnormal termination'
              ENDIF
              ARCS(ARC_NUMBER) = NABF(6)
              TOTS(ARC_NUMBER) = NABF(6) + NABF(5)
              IF ( TOTS(ARC_NUMBER) .GT. MAXTOTS ) THEN
                   MAXTOTS = TOTS(ARC_NUMBER)
              END IF
              TOTAL_ARC_PARMS = TOTAL_ARC_PARMS + NABF(6)
            ELSE IF ( ITPR .EQ. 4 ) THEN
               IF ( NABF(1) .EQ. INT2_DE ) THEN
                    TEMP = DSQRT(1.D0/RABF(4))*1.D9
                    WRITE ( BUF1, 1001 ) ARC_NUMBER, N4BF(2), RABF(2), RABF(4), &
     &                                   RABF(1)
 1001               FORMAT ( I5, I10, 3G16.8 )
                    CWRMS(1) = CWRMS(1) + N4BF(2)*(RABF(2)*RABF(2))
                    CFACT(1) = CFACT(1) + RABF(4)
                    CNCSUM = CNCSUM+N4BF(2)
                  ELSE IF ( NABF(1) .EQ. INT2_RA ) THEN
                    WRITE ( BUF2, 1003 ) N4BF(2), RABF(2), RABF(4), RABF(1)
 1003               FORMAT ( I10, 3G16.8 )
                    WRITE ( 19, '(A63,2X,A63)' ) BUF1, BUF2
                    CWRMS(2) = CWRMS(2) + N4BF(2)*(RABF(2)*RABF(2))
                    CFACT(2) = CFACT(2) + RABF(4)
                    CKCSUM = CKCSUM+N4BF(2)
                ENDIF
            ELSE IF ( ITPR .EQ. 5 ) THEN
              CALL BLINE ( NABF )
              IF ( IBNDX .GT. (MAX_STA*(MAX_STA-1)/2) ) THEN
                  WRITE ( *, * ) ' CUMULOOP: IBNDX ERROR . . .  IBNDX=',IBNDX
                  CALL PAUSE ( 'CUMULOOP' )
              END IF
            ELSE IF ( ITPR .EQ. 6 ) THEN
              CALL SOURC ( NABF )
              CALL MEMCPY ( STR8, NABF )
              IF ( ISNDX .GT. 4*MAX_SRC ) THEN
                   WRITE (*,*) 'ISNDX ERROR . . . ISNDX= ', ISNDX
                   CALL PAUSE ( 'CUMULOOP' )
              END IF
          END IF
      END DO
!
      CALL ACS_SARFIL ( 'C' )
!
! --- Write summary delay, rate info for all data, whether used or not
!
      IF ( CFACT(1) .GT. 1.D-30 ) THEN
           DSIG_CUM = DSQRT (CWRMS(1)/CFACT(1)) * 1.0D9
         ELSE
           DSIG_CUM = 0.0D0
      END IF
      IF ( CFACT(2) .GT. 1.D-30 ) THEN
           RSIG_CUM = DSQRT ( CWRMS(2)/CFACT(2) ) * 1.0D12
         ELSE
           RSIG_CUM = 0.0D0
      END IF
      IF ( ABS(CKCSUM + CNCSUM) .GT. 1.D-30 ) THEN
           IF ( NUM_RATES .EQ. 0 ) THEN
                CHISQR_CUM(1) = CHISQR_CUM_SAVE
              ELSE
                DPARAM = CNCSUM * (TOTAL_PARMS-CSHARE)/(CKCSUM + CNCSUM)
                CHISQR_CUM(1) = CWRMS(1)/(CNCSUM - DPARAM)
           END IF
           DPARAM = CKCSUM * (TOTAL_PARMS-CSHARE)/(CKCSUM + CNCSUM)
           CHISQR_CUM(2) = CWRMS(2)/(CKCSUM - DPARAM)
         ELSE
           DPARAM = 0.0
           CHISQR_CUM(1) = 0.0
           DPARAM = 0.0
           CHISQR_CUM(2) = 0.0
      END IF
!
      IF ( ABS(CNCSUM) .GT. 1.D-30 ) THEN
           WRMS_CUM(1) = DSQRT (CWRMS(1)/CNCSUM)
         ELSE
           WRMS_CUM(1) = 0.0D0
      END IF
      IF ( ABS(CKCSUM) .GT. 1.D-30 ) THEN
           WRMS_CUM(2) = DSQRT (CWRMS(2)/CKCSUM)
         ELSE
           WRMS_CUM(2) = 0.0D0
      END IF
      WRITE ( 23, 5014 ) CNCSUM, DSIG_CUM*1000., WRMS_CUM(1), &
     &                   CHISQR_CUM(1)
      WRITE ( 23, 5015 ) CKCSUM, RSIG_CUM*1000., WRMS_CUM(2), &
     &                   CHISQR_CUM(2)
 5014 FORMAT(/,"   Delay(All) ",I8,1X,F11.3," ps",7X,F9.3,F15.3)
 5015 FORMAT(  "   Rate(All)  ",I8,1X,F11.3," fs/s",5X,F9.3,F15.3)
!
! --- Spew the arc matrix:  lists the local parms/arc
!
      LOOP = (ARC_NUMBER)/10
      LL   = ARC_NUMBER
      If (LL .gt. 10) LL = 10
!
      WRITE (23, 1101) (I, I=1, LL)
 1101 FORMAT (/, " Local parameters per arc", / ,"     :", 10I6)
      Write (23, 1102) ('--', I=1, LL*3)
 1102 FORMAT ("------", 30A2)
!
      DO I = 0, LOOP
         START = I*10+1
         STOP  = START+9
         IF ( STOP .GT. ARC_NUMBER ) STOP = ARC_NUMBER
         WRITE (23, 1103) I*10,(ARCS(KK), KK=START, STOP)
 1103    FORMAT (I5," :",10I6)
      END DO
!
      WRITE (23, 1102) ('--', I=1, LL*3)
      WRITE (23, 1104)
 1104 FORMAT (/)
!
! --- Spew the tots matrix:  lists the local+global parms/arc
!
      LOOP = (ARC_NUMBER)/10
      LL   = ARC_NUMBER
      If (LL .gt. 10) LL = 10
!
      WRITE (23, 1105) (I, I=1, LL)
 1105 FORMAT (/, " Total parameters per arc", / ,"     :", 10I6)
      WRITE (23, 1102) ('--', I=1, LL*3)
!
      DO I = 0, LOOP
         START = I*10+1
         STOP  = START+9
         IF ( STOP .GT. ARC_NUMBER ) STOP = ARC_NUMBER
         WRITE ( 23, 1103 ) I*10, ( TOTS(KK), KK= START, STOP )
      END DO
!
      WRITE  (23, 1102) ('--', I=1, LL*3)
      WRITE  (23, 1104)
!
      WRITE  (23, 1106) maxtots
 1106 FORMAT ( "Maximum total parameters in a single arc : ", I5 )
!
      WRITE (23, 6969)  GLBMINSUP, TOTAL_ARC_PARMS, TOTAL_PARMS, CSHARE, &
     &                  NCKCSUM-TOTAL_PARMS+CSHARE
 6969 FORMAT (/,'       Global Parameters:  ', I7, &
     &        /,' Total Arc Parameters   :  ', I7, &
     &        /,' Total Parameters       :  ', I7, &
     &        /,' Constraints            :  ', F12.1, &
     &        /,' Degrees of Freedom     :  ', F12.1)
!
      IF ( FL_SINEX_MAKE  .AND.  FL_SINEX_GLO ) THEN
!
! -------- Aga: it was requested to make a listing in sinex format in global
! -------- mode.
!
           LOUT = GET_UNIT()
!
! -------- OPen the Sinex listing in append mode
!
           OPEN ( UNIT=LOUT, FILE=SINEX_REALNAME, STATUS='OLD', &
     &            ACCESS='APPEND', IOSTAT=IOS )
           IF ( IOS .NE. 0 ) THEN
                CALL ERR_LOG ( 8302, -1, 'CUMULOOP', 'Error in '// &
     &              'an attempt to open the listing of the solution in '// &
     &              'sinex format' )
                STOP 'HAUSR(cmuloop) Abnormal termination'
           END IF
!
! ======== SOLUTION/STATISTICS block
!
           WRITE ( UNIT=LOUT, FMT='(A)' ) '+SOLUTION/STATISTICS'
           WRITE ( LOUT, '(A)' )          '* Units for WRMS: sec'
           WRITE ( UNIT=LOUT, FMT=180 ) 'NUMBER OF OBSERVATIONS        ', &
     &                                   CNCSUM
 180       FORMAT ( 1X,A30, 1X,I12 )
           WRITE ( UNIT=LOUT, FMT=180 ) 'NUMBER OF UNKNOWNS            ', &
     &                                   TOTAL_PARMS
           WRITE ( UNIT=LOUT, FMT=190 ) 'SQUARE SUM OF RESIDUALS (VTPV)', &
     &                                   CWRMS(1)
 190       FORMAT ( 1X,A30, 1X,1PD21.14 )
           WRITE ( UNIT=LOUT, FMT=190 ) 'VARIANCE FACTOR               ', &
     &                                   CHISQR_CUM(1)
           WRITE ( UNIT=LOUT, FMT=190 ) 'WRMS OF POSTFIT RESIDUALS     ', &
     &                                   DSIG_CUM*1.D-9
!
           WRITE ( UNIT=LOUT, FMT='(A)' ) '-SOLUTION/STATISTICS'
           WRITE ( LOUT, '(A)' ) '*'
           WRITE ( LOUT, '(A)' ) '* --------------------------------------'// &
     &                           '---------------------------------------'
!
! -------- Write the final line. Deal done!
!
           WRITE ( LOUT , FMT='(A)' ) '%ENDSNX'
      END IF
!
      RETURN
      END  !#!  CUMULOOP   #!#
