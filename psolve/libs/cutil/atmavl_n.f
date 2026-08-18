      SUBROUTINE ATMAVL_N ( IUNIT, LIMIT, DIR_NAME, FILE_NAME, FLAV_NAMES, &
     &                      NUM_FLAV, IDEFAULT, KERR )
!
      IMPLICIT NONE
!
!
! 1.  ATMAVL_N PROGRAM SPECIFICATION
!
! 1.1 Read the appropriate file to get a list of the calibrations
!     (partial or flyby) which are theoretically
!     available in this copy of SOLVE.  A calibration is available
!     if socal knows how to calculate it.  If a user wants to make a
!     calibration available or unavailable, he must add/delete it both
!     here and in socal.  If he only updates one of these two places, this
!     subroutine will not report the
!     correct status of the calibration.
!
!
! 1.2 REFERENCES:
!
! 2.  ATMAVL_N INTERFACE
!
! 2.1 Parameter File
!
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IUNIT,LIMIT
      CHARACTER*(*) DIR_NAME,FILE_NAME
!
!     iunit - unit number the calling program wants to assign to the
!             file of available cals
!
!     limit - maximum number of calibrations the user wants to pull
!             from the file
!
!     dir_name - name of input directory where input file_name lives
!     file_name - name of input file of calibration names
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*8 FLAV_NAMES(*)
      INTEGER*2 NUM_FLAV,KERR,IDEFAULT
!
! FLAV_NAMES - list of available calibrations, according to the file
! NUM_FLAV - number of available calibrations, according to the file
! IDEFAULT - default calibration to be turned on (position in list)
! KERR - error return
!        0 = ok (read the entire availability file)
!       -1 = open error
!       -2 = read error, when still trying to get requested number of calibs
!        1 = more in file than requested
!        2 = read error, when doing final read to see if more in file than
!            requested (may or may not be more in file than requested)
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 IERR
      INTEGER*2 TRIMLEN
      CHARACTER FNAME*128, BUF*128, TOKEN*8
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   KDB  961112  Created based on flyavl_n.f.
!   pet  2000.04.26  Increased the length of fname to 128 symbols
!
! 5.  ATMAVL_N PROGRAM STRUCTURE
!
!     The file of theoretically available calibrations lists the
!     calibrations, one per line.
!     Just read the file and pull out the calibration names, up to the
!     limit the user requested.
!     If one of the calibrations is the default, the word DEFAULT must follow
!     that calibration on the calibration's line, as the second field.
!
      FNAME=DIR_NAME(1:TRIMLEN(DIR_NAME))//FILE_NAME
      OPEN ( IUNIT, FILE=FNAME, IOSTAT=IERR, ERR=100, STATUS='OLD', &
     &       ACCESS='SEQUENTIAL', FORM='FORMATTED' )
 100  CONTINUE
      IF ( IERR .NE. 0 ) THEN
           KERR = -1
           GO TO 900
      END IF
      NUM_FLAV = 0
      DO WHILE (NUM_FLAV.LT.LIMIT)
         READ (IUNIT,'(A)',IOSTAT=IERR,ERR=200,END=300) buf
         NUM_FLAV = NUM_FLAV + 1
         CALL SPLITSTRING ( BUF, TOKEN, BUF )
         FLAV_NAMES(NUM_FLAV) = TOKEN
         CALL SPLITSTRING ( BUF, TOKEN, BUF )
         IF ( TOKEN(1:7) .EQ. 'DEFAULT' ) THEN
              IDEFAULT = NUM_FLAV
         ENDIF
      END DO
!
! --- The sub stopped reading because it reached the limit requested
! --- by the user.  See if there were more left, to warn the user.
!
      READ ( IUNIT, '(A8)', IOSTAT=IERR, ERR=201, END=301 )
      CLOSE(IUNIT)
      KERR = 1
      GO TO 900
 201  CLOSE(IUNIT)
      KERR = 2
      GO TO 900
 301  CLOSE(IUNIT)
      KERR = 0
      GO TO 900
!
! --- Error and eof from normal read to pull off list
!
 200  CLOSE(IUNIT)
      KERR = -2
      GO TO 900
 300  CLOSE (IUNIT)
      KERR = 0
 900  CONTINUE
!
      RETURN
      END  !#!  ATMAVL_N  #!#
