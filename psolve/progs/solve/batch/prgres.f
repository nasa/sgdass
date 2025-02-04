      SUBROUTINE PRGRES ( ARCNUM, IPASS, SOLTY2, DBNAME, VER, KLAST, ID, &
     &                    RESTRT, ETIME0, ETIMP0, KFAIL, SOLTYP )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  PRGRES PROGRAM SPECIFICATION
!
! 1.1 Update the SOLVE progress file (PRGFxx).
!
! 1.2 REFERENCES:
!
! 2.  PRGRES INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*1 SOLTY2, SOLTYP
      CHARACTER*(*) DBNAME,ID(10)
      INTEGER*2 ARCNUM,IPASS,VER
      INTEGER*4 ETIME0,ETIMP0
      LOGICAL*2 KLAST,RESTRT,KFAIL
!
! ARCNUM - Number of the current arc
! DBNAME - Name of the current database
! ETIME0 - Time at start of processing for this arc
! ETIMP0 - Time at start of processing for this pass
! ID - Run identification string
! KFAIL - Flag indicating that processing has failed
! KLAST - True if this is the last arc
! RESTRT - True if we are recovering from an interrupted run
! SOLTYP - Solution type
! SOLTY2 - Solution type of current pass (FORWARD or BACK)
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: prces
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*2 CMIN
      CHARACTER*4 HRS,PERCENT
      CHARACTER*5 SECS
      CHARACTER*20 MESS
      SAVE MESS
      CHARACTER*11 TYP
      CHARACTER*13 NUMTIM, NUMSEC
      CHARACTER*63 LPRGNAM,LTIMNAM
      CHARACTER    STR*128
      INTEGER*2    TRIMLEN, IL, I
      INTEGER*4    IOS
      INTEGER*4    ETIME, ETIME1, LCLSEC, SECONDS
!
      INTEGER*4 ETIMER
!
! 4.  HISTORY
!   WHO   WHEN      WHAT
!   jmg  960803     Time tag feature.
!   PVT July 2 1999 Renamed MIN to CMIN in order to avoid conflict with
!                   intrinsic function
!   pet 05-JUL-99   Removed unused variables
!   pet 2001.12.13  Added support of a new type of solutions: GLOBAL_ONLY
!                   ( SOLTYP = 'G' )
!   pet 2002.03.19  Removed check whether the total number of parameters
!                   exceeded the limit. It is not an appropriate place for such
!                   a check.
!
!
! 5.  PRGRES PROGRAM STRUCTURE
!
      LPRGNAM=PRE_SCR_DIR(1:PRE_SD_LEN)//'PRGF'//PRE_LETRS
      LTIMNAM=PRE_SCR_DIR(1:PRE_SD_LEN)//'TIME'//PRE_LETRS
!
! --- Open file without update, for first write to file, so that an eof
! --- will be written at the end and wipe out the data from the previous
! --- run.  for subsequent writes, open with update, to avoid wiping out
! --- previous data from the current run.
!
80    CONTINUE
      IF ( (ARCNUM .EQ. 0 .AND. SOLTYP .NE. 'C') .OR. &
     &     (ARCNUM .EQ. 0 .AND. SOLTYP .EQ. 'C' .AND. SOLTY2 .EQ. 'F' ) .OR. &
     &     (ARCNUM .EQ. 0 .AND. SOLTYP .EQ. 'G' .AND. SOLTY2 .EQ. 'F' )   ) THEN
           CALL FTN_OPEN( INT2(12), LPRGNAM, ' ' )
!
! -------- open and close "TIMExx" file to reset it.
!
           CALL FTN_OPEN( INT2(122), LTIMNAM, ' ' )
           WRITE ( 122, * ) "Starting"
           CLOSE ( 122 )
        ELSE
          CALL FTN_OPEN( INT2(12), LPRGNAM, 'A' )
      END IF
      CALL USE_GLBFIL_4 ( 'ORC' )
!
! --- Get the elapsed time
!
      ETIME = ETIMER ( LCLSEC )
      SECONDS = LCLSEC - ETIME
      CALL NUMERICTIME ( LCLSEC, NUMTIM )
!
! --- For a recover, record the fact, fix the pass time to compensate
! --- For reading the control file
!
      IF ( RESTRT ) THEN
           ETIMP0=ETIMP0+(ETIME-ETIME0)
           CALL NUMERICTIME ( SECONDS+ETIME0, NUMSEC )
           WRITE ( 12, 9853, IOSTAT=IOS, ERR=7000 ) NUMSEC
9853       FORMAT(" RECOVER  ",53X,A)
           GOTO 8000
      ENDIF
!
! ----For the zeroth arc of pass 1 write the id
!
      IF ( ARCNUM .EQ. 0  .AND.  IPASS .EQ. 1 ) THEN
           CALL NUMERICTIME ( SECONDS, NUMSEC )
            DO I=1,10
               IF ( ID(I) .NE. ' ' ) then
                    WRITE ( 12, 9854, IOSTAT=IOS, ERR=7000 ) ID(I), NUMSEC
9854                FORMAT ( " ", A, "  ", A )
               ENDIF
            ENDDO
      ENDIF
!
! --- Write the direction info for the zeroth arc
!
      IF ( ARCNUM .EQ. 0 ) THEN
           ETIMP0=ETIME
           IF ( SOLTY2 .EQ. 'F' ) THEN
               TYP='FORWARD'
             ELSE IF ( SOLTY2 .EQ. 'B' ) THEN
               TYP='BACK'
             ELSE
               TYP='INDEPENDENT'
           END IF
           WRITE ( 12, 9857, IOSTAT=IOS, ERR=7000 ) TYP, NUMTIM
9857       FORMAT ( " ", A, " SOLUTION START   ", 33X, A)
      ENDIF
!
! --- Did this arc fail
!
      MESS=' '
      IF ( KFAIL ) MESS = 'FAILED'
!
! --- For each arc write the elapsed time since the last arc
!
      IF ( ARCNUM .GT. 0 ) THEN
           ETIME1=(ETIME-ETIME0)*100
           CALL TIMEF(ETIME1,ETIME1,HRS,CMIN,SECS,PERCENT )
           IL=MAX(INT2(1),TRIMLEN(MESS))
           IF ( RCOND .EQ. 0.D0 ) THEN
                WRITE ( STR, 9859, IOSTAT=IOS, ERR=7000 ) ARCNUM, DBNAME, VER, HRS, &
     &                   CMIN, SECS, NUMTIM, MESS(1:IL)
              ELSE
                WRITE ( STR, 9858, IOSTAT=IOS, ERR=7000 ) ARCNUM, DBNAME, VER, HRS, &
     &                  CMIN, SECS, RCOND, NUMTIM, MESS(1:IL)
           ENDIF
9858       FORMAT(" ",I5," ",A10," ",I3,"  ARC TIME: ", &
     &            A4,":",A2,":",A2,"  cond# = ",1PG9.3,2X,A,' ',A)
9859       FORMAT(" ",I5," ",A10," ",I3,"  ARC TIME: ", &
     &            A4,":",A2,":",A2," ",20X,A,' ',A)
           CALL BLANK_TO_ZERO ( STR(37:43) )
           WRITE ( 12, '(A)' ) TRIM(STR)
      ENDIF
!
! --- Write the pass time when we are done, and if this is the second
! --- pass write the total run time as well
!
      IF ( KLAST ) THEN
           ETIME1=(ETIME-ETIMP0)*100
           CALL TIMEF ( ETIME1, ETIME1, HRS, CMIN, SECS, PERCENT )
           IF ( SOLTY2 .EQ. 'F' ) THEN
                TYP='FORWARD'
             ELSE IF ( SOLTY2 .EQ. 'B' ) THEN
                TYP='BACK'
             ELSE
                TYP='INDEPENDENT'
            ENDIF
            WRITE ( STR, 9866, IOSTAT=IOS, ERR=7000 ) TYP, HRS, CMIN, SECS, &
     &                                               NUMTIM
9866        FORMAT ( " ", A, " SOLUTION RUN TIME: ", A4,":",A2,":",A2," ",20X,A)
            CALL BLANK_TO_ZERO ( STR(35:42) )
            WRITE ( UNIT=12, FMT='(A)' ) TRIM(STR)
            IF ( IPASS .EQ. 2 ) THEN
                 CALL TIMEF ( ETIME*100, ETIME*100, HRS, CMIN, SECS, PERCENT )
                 TYP='TOTAL'
                WRITE ( 12, 9866, IOSTAT=IOS, ERR=7000 ) TYP, HRS, CMIN, SECS, &
     &                                                   NUMTIM
            ENDIF
      ENDIF
      GOTO 8000
!
!  Error return
!
      IOS = 0
7000  CONTINUE
      CALL FERR ( INT2(IOS), "writing progress file", INT2(0), INT2(0) )
8000  CONTINUE
!
! --- CLEAN UP
!
      ETIME0=ETIME
      CLOSE(12)
!
      RETURN
      END  !#!  PRGRES  #!#
