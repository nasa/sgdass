      PROGRAM CORRN
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  CORRN PROGRAM SPECIFICATION
!
! 1.1 CORRN ALLOWS CHANGES TO BE MADE IN THE IONOSPHERE CALIBRATION
!
!     SUBROUTINE DBPOX READS THE NECESSARY INFORMATION ABOUT THE DATA
!     BASES IN OBSFIL: # OF DATA BASES, DATA BASE NAMES, DATA BASE
!     VERSIONS AND POSITIONS IN THE OBSFIL.
!
!     SUBROUTINE IONX DISPLAYS THE STATUS OF EACH DATA BASE IN SEQUENCE
!     AND RECORDS THE CHANGES TO BE MADE, IF ANY.  IT CHECKS THAT THE
!     SELECTED CALIBRATIONS ARE AVAILABLE BEFORE ALLOWING THE CHANGE.
!
!     THE FOLLOWING SPECIFIES THE SIGNIFICANCE OF THE BITS IN JSITI(I)
!    BIT- 1 ON = GION available for this station
!    BIT- 2 ON = PHION available for this station
!    BIT- 4 ON = Apply GION in next solution
!    BIT- 5 ON = Apply PHION in next solution
!                (NOTE: Bits 4 and 5 are mutually exclusive)
!
!     THE FOLLOWING SPECIFIES THE SIGNIFICANCE OF THE BITS IN ICORR
!    BIT- 2 ON = No matching observation for GION
!    BIT- 3 ON = GION based on downweighted data
!    BIT- 4 ON = GION available in OBSFIL
!    BIT- 5 ON = Dont use this obs. in solutions with GION applied
!                 (cannot be turned off if BIT-2 is on)
!    BIT- 8 ON = No matching observation for PHION
!    BIT- 9 ON = PHION based on downweighted data
!    BIT-10 ON = PHION available in OBSFIL
!    BIT-11 ON = Dont use this obs. in solutions with PHION applied
!                 (cannot be turned off if BIT-8 is on)
!
! 1.2 REFERENCES:
!
! 2.  CORRN INTERFACE
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
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: none
!     CALLED SUBROUTINES: ionx
!
! 3.  LOCAL VARIABLES
!
!     LDBNAM - DATA BASE NAMES
!     IDBVER - DATA BASE VERSIONS
!     IDBEND - LAST OBSERVATIONS IN OBSFIL BY DATA BASE
!
!     NUMDD  - TOTAL NUMBER OF DATA BASES IN OBSFIL
!     NSSS   - NUMBER OF STATIONS IN CURRENT DATA BASE
!
!     LCOMM  - INDICATES WHETHER USER WANTS TO SEE THE NEXT DATA BASE,
!              RETURN TO THE OPTIONS MENU OR PERFORM LEAST SQUARES.
!     ISITI(16) CONTAINS THE INITIAL STATUS.
!     JSITI(16) CONTAINS THE FINAL STATUS.
!
      INTEGER*2 LDBNAM(5,15), IDBVER(15)
      INTEGER*2 JSITN(4,MAX_ARC_STA), ISITI(MAX_ARC_STA), &
     &          JSITI(MAX_ARC_STA), JAVAL(MAX_ARC_STA), &
     &          JAPPL(MAX_ARC_STA)
      CHARACTER*70 JBUF
      character*80 bufstr
      CHARACTER*4 LCARD
      CHARACTER*1 ERRSTAT
      CHARACTER*100 errstr
      INTEGER*4  IOS
      INTEGER*2 NOBSF (  3 )
      INTEGER*2 NCOMM (  3 )
      INTEGER*2 IDB,IERR,ISQR,IST,J,LCOMM,MAXCHANGED,NSSS,NUMDD
!
      DATA NOBSF  /2HOB,2HSF,2HIL/
      DATA NCOMM  /2HCO,2HMM,2HON/
!
      INTEGER*4 I4P0
      DATA I4P0 /0/
!
! 4.  HISTORY
!  WHO   WHEN   WHAT
!  KJC 83-6-9 REDIMENSION IBUF FROM 6 TO 27
!            AND DIMENSION ECC AND MONNAM FOR ECCENTRICITIES AND
!            MONUMENT NAMES
!  MALLAMA, FEBRUARY 1984, ENABLED DOWN-WEIGHTING AND RE-WEIGHTING
!
!  MALLAMA, JULY 1984:   MADE COMPATIBLE WITH MULTI-USER SOLVE.
!  BAVER,    4/28/86 :   CHANGED TO SUPPORT NEW NAMFIL STRUCTURE,
!                        CONVERTED TO FORTRAN 77, REWROTE TO ALLOW
!                        THE USER TO SEE NEXT DATA BASE OR RETURN
!                        TO THE OPTIONS MENU.
!  BAVER,    5/6/86  :   ADDED CODE TO ALLOW USER TO INITIATE LEAST
!                        SQUARES.
!  BAVER,   5/26/87  :   REMOVED OBSOLETE PARFIL OPEN AND READ
!  AEE    910515         Enhanced error messages written to the error file.
!  KDB    950810         Convert to handle 32 sites.
!  pet    2004.03.09     Fixed a bug.
!
! 5.  CORRN  PROGRAM STRUCTURE
!
      CALL PRE_PROG()
      INCLUDE 'corrn_version.i' ! Set revision date of the current version
!
      CALL START_MN()
      CALL SETCR_MN ( I4P0, I4P0 )
      CALL SET_SIGNAL_CTRLC ( 2 )
!
! --- Open and read common.
!
      CALL USE_COMMON('ORC' )
!
! --- Open and read namfil
!
      CALL DBPOX(NUMDD,LDBNAM,IDBVER,IDBEND )
!
! --- Loop over data bases
!
      DO IDB = 1, NUMDD   !Data bases
!
! ----- Read station names and status array
!
        IST = 1
        IERR = 0
        NSSS = 0
        DO WHILE (IERR .EQ. 0)  !Read station and status
!
! -------- Read in NAMFIL site variables
!
           IF ( IST .EQ. 1 ) THEN
                ERRSTAT = 'A'
                CALL GETCARD ( IDB, 'CALS', INT2(1), JBUF, IERR )
             ELSE
                ERRSTAT = 'B'
               CALL GETCARD ( IDB, 'CALS', INT2(0), JBUF, IERR )
           END IF
           IF ( IERR .NE. 0 ) GOTO 500
           ERRSTAT = 'C'
           READ ( JBUF, 5000, IOSTAT=IOS, ERR=500) &
     &           (JSITN(J,IST), J = 1, 4), ISITI(IST), JAVAL(IST), &
     &          JAPPL(IST)
 5000      FORMAT (5X, 4A2, 1X, 3I7, 35X)
!
! -------- Duplicate status array.
!
           JSITI(IST) = ISITI(IST)
           NSSS = NSSS + 1
           IST = IST + 1
           IERR = IOS
       END DO
  500  CONTINUE
       IF (IERR .NE. 1) GO TO 4000
!
! ---- Present status and select options
!
       CALL IONX(NSSS,LDBNAM(1,IDB),IDBVER(IDB),JSITN,JSITI,LCOMM )
!
! ---- Compare original and new status
!
       MAXCHANGED = 0
       DO IST = 1, NSSS  ! Compare statuses
          IF ( JSITI(IST) .NE. ISITI(IST) ) MAXCHANGED = IST
       END DO            ! Compare statuses
!
! ---- Modify NAMFIL if change of status
!
       IF ( MAXCHANGED .GT. 0 ) THEN  !Change ionosphere
!
!        Write NAMFIL ionosphere status lines
!
         LCARD = 'CALS'
         DO IST = 1, MAXCHANGED   !Write station and status
           ERRSTAT = 'D'
           WRITE (JBUF, 5001, IOSTAT=IOS) &
     &         LCARD, (JSITN(J, IST), J = 1, 4), JSITI(IST), &
     &         JAVAL(IST), JAPPL(IST)
 5001        FORMAT (A4, 1X, 4A2, 1X, 3I7, 35X)
           IF (IST .EQ. 1) THEN
             ERRSTAT = 'E'
             CALL PUTCARD ( IDB, 'CALS', INT2(1), JBUF, IERR )
           ELSE
             ERRSTAT = 'F'
             CALL PUTCARD ( IDB, 'CALS', INT2(0), JBUF, IERR )
           END IF
           IF (IERR .NE. 0) GO TO 4000
         END DO                   !Write station and status
       END IF                    !Change ionosphere
       IF (LCOMM .EQ. 0) THEN    !User wants to return to options
         GO TO 98
       END IF
       IF (LCOMM .EQ. 2) THEN    !User wants to do least squares
         GO TO 98
       END IF
      END DO                      !Data bases
!
!     Indicate that all data bases were processed successfully.
!
      IF (IDB .GT. NUMDD) GO TO 98
!
!     Abnormal termination message.
!
 4000 CONTINUE
      IERR = IOS
      IF (IERR .NE. 0) THEN
        WRITE(bufstr, &
     &         "('CORRN ERROR ', I10, ' AT STATEMENT ', A1)")IERR, ERRSTAT
        call addstr_f(bufstr )
        call nl_mn()
        call refresh_mn()
        WRITE(errstr, &
     &         "('CORRN ERROR ', I10, ' AT STATEMENT ', A1)")IERR, ERRSTAT
        call ferr( INT2(257), errstr, INT2(0), INT2(0) )
      END IF
!
  98  CONTINUE
      CALL CLOSENAMFIL()
      IF (LCOMM .EQ. 2) THEN
        ISQR = 1             !Tell OPTIN to do least squares
      ELSE
        ISQR = 0
      END IF
      call end_mn()
      CALL USE_BUFFER( ISQR, INT2(1), 'OWC' )
      CALL END_PROG()
      END
