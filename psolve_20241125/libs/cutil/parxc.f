      SUBROUTINE PARXC ( CCH, KERR )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  PARXC PROGRAM SPECIFICATION
!
! 1.1 Selects all stations parameters or all earth orientation
!      parameters, exclusively.  Westford, however, is never selected.
!
! 1.2 REFERENCES:
!
! 2.  PARXC INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER  CCH*1
!
! CCH - Character which determines whether to turn on station position
!         parameters (!) or earth orientation parameters (#).
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 KERR
!
! KERR - Returns 0 if station position parameters selected, 1 if earth
!        orientation parameters selected
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INCLUDE 'socom.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'prfil.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: rmflg
!       CALLED SUBROUTINES: rotin,rotfl
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2     IDSP, J, I, MASTER_ISTA, IMASTR(4),IROTF, IDUM2(2)
      INTEGER*4     IX, IY, IDUM
      CHARACTER     BUFSTR*179, FNAME*49, MASTER*8
      EQUIVALENCE ( MASTER, IMASTR )
      INTEGER*2     ISTA, TRIMLEN
      INTEGER*4     IOS
      LOGICAL*2     KBIT, KB
      REAL*8        RDUM(3), TMIN, TMAX
      LOGICAL*4     CHECK_STABIT, IS_CURLIB_ON
!
      INTEGER*4     I4P0, I4P2
      DATA          I4P0, I4P2 / 0, 2 /
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   mwh  940602  Require response from user only if a reference station
!                wasn't found in the station pick file
!   kdb  960522  Moved to cutil for the benefit of optin's < feature.  No
!                actual changes.
!   kdb  971212  Adedd more comments. Added support of case when the candidate
!                to master appeared to be desleceted
!   pet  990412  Added check of flag of curses before using curses routines.
!                Got free from unused variables.
!
! 5.  PARXC PROGRAM STRUCTURE
!
!CCCCC
      KERR = 0
      IF ( .NOT.  CCH .EQ. '#' ) THEN
!
! -------- Mode: Turn on station position parameters
!
! -------- Find master station
!
           FNAME = PRE_SAV_DIR(:PRE_SV_LEN)//STATION_PICK_FILE
           OPEN ( UNIT=67, FILE=FNAME(:TRIMLEN(FNAME)), IOSTAT=IOS )
           CALL FERR ( INT2(IOS), "Opening Station PICK file", INT2(0), &
     &          INT2(0) )
!
           DO WHILE ( .TRUE. )
              READ (67, '(A8)', END=810 ) MASTER
              DO ISTA=1,NUMSTA
!!                 IF ( EQUAL(IMASTR(1), 1, ISITN(1,ISTA),1,8)) THEN
                 IF ( MASTER .EQ. ISITN_CHR(ISTA)  .AND. &
     &                CHECK_STABIT ( ISTA )               ) THEN
!
                      MASTER_ISTA = ISTA
                      GOTO 820
                 ENDIF
              ENDDO
           ENDDO
  810      CONTINUE
!
! -------- No master station found. In that case we should set it forcible.
! -------- It will be the first selected stations (station with minimal number,
! -------- that means the first in alphabetic order. If all stations appeared
! -------- to be deselected then the first station will be set as a master
! -------- station. Such a choice is senseless but it will not bring abend.
!
           MASTER_ISTA = 1
           DO ISTA=NUMSTA,1,-1
              IF ( CHECK_STABIT ( ISTA ) ) THEN
                   MASTER_ISTA = ISTA
              END IF
           ENDDO
           IF ( IS_CURLIB_ON() ) THEN
                CALL CLEAR_MN()
                CALL SETCR_MN ( I4P0, I4P0 )
              ELSE
                WRITE ( 6, '(A)' ) ' '
           END IF
           WRITE ( BUFSTR, '("No master station found in ",A)' ) FNAME
           CALL ADDSTR_F ( BUFSTR(1:79) )
           CALL NL_MN()
           CLOSE ( UNIT=67 )
           WRITE ( BUFSTR, 519 ) ISITN_CHR(MASTER_ISTA)
  519      FORMAT ( "Nevertheless, master station for position estimation ", &
     &              "is set ", A )
           CALL ADDSTR_F ( BUFSTR(1:79) )
           CALL NL_MN()
           IF ( IS_CURLIB_ON() ) THEN
                CALL ADDSTR_F ( "Hit any key to acknowledge and continue" )
                CALL SENKR_MN ( IX, IY, IDUM )
           END IF
!
 820       CONTINUE
!
           DO 100 I = 1,NUMSTA
              DO 50 J = 1,3
!
! -------------- Setting bits of position estimation for all stations
! -------------- (even for deslected), excluding master one
!
                 IF ( I .NE. MASTER_ISTA )  CALL SBIT ( LSITEC(1,J), I, &
     &                INT2(1))
                 IF ( I .EQ. MASTER_ISTA )  CALL SBIT ( LSITEC(1,J), I, &
     &                INT2(0))
   50         CONTINUE
  100      CONTINUE
!
! -------- Turn off earth orientation parameters, except UT1-rate bit. It keeps
! -------- its value
!
           KB = KBIT ( LROT(1,3), INT2(2) )
           EOP_STYLE(1) = 0
           EOP_STYLE(2) = 0
           DO 200 I = 1,ROT_BIT_WORDS
              DO 150 J = 1,3
                 LROT(I,J) = 0
  150         CONTINUE
  200      CONTINUE
           IF ( KB ) CALL SBIT ( LROT(1,3), INT2(2), INT2(1) )
           CLOSE ( UNIT=67 )
           RETURN
      END IF
!
      IF ( CCH .EQ. '#' ) THEN
!
! -------- Turn on earth orientation parameters
!
! -------- Start out with old style of parameterization (list of epochs with
! -------- four orders each for x-wobble, y-wobble and UT1-TAI.
!
           EOP_STYLE(1) = 0
           EOP_STYLE(2) = 0
!
! -------- Delete first epoch
!
           NROT = 1
           CALL OBSTM ( TMIN, TMAX )
           TROT(1) = (TMIN+TMAX) / 2.0D0
           CALL INTRP_EOVR ( TROT(1), ROTAP(1,1), ROTAP(1,2), ROTAP(1,3), &
     &          ROTAP(1,4), .FALSE., RDUM(1), RDUM(2), RDUM(3), IDUM2 )
!
! -------- Now process parameters
!
           DO 300 I=1,ROT_BIT_WORDS
              DO 250 J=1,3
                 LROT(I,J) = 0
  250         CONTINUE
  300      CONTINUE
!
           DO J=1,3
              IDSP = IROTF ( INT2(1), J, INT2(1), LROT)
           END DO
           IDSP = IROTF ( INT2(1), INT2(3), INT2(2), LROT)
!
! -------- Turn off station position parameters
!
           DO I=1,STA_BIT_WORDS
              LSITEC(I, 1) = 0
              LSITEC(I, 2) = 0
              LSITEC(I, 3) = 0
           END DO
           KERR = 1
!
           CLOSE ( UNIT=67 )
      END IF
!
      RETURN
      END  !#!  PARXC  #!#
