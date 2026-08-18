      SUBROUTINE FLYAVL_N (IUNIT,LIMIT,FLAV_NAMES,NUM_FLAV,KERR,jcaff1)
!
      implicit none
!
!
! 1.  FLYAVL_N PROGRAM SPECIFICATION
!
! 1.1 Read the appropriate file to get a list of the flyby calibrations
!     which are theoretically
!     available in this copy of SOLVE.  A flyby calibration is available
!     if socal knows how to calculate it.  If a user wants to make a
!     flyby calibration available or unavailable, he must add/delete it both
!     here and in socal.  If he only updates one of these two places, this
!     subroutine will not report the
!     correct status of the calibration.
!
! 1.2 REFERENCES:
!
! 2.  FLYAVL_N INTERFACE
!
! 2.1 Parameter File
!
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IUNIT,LIMIT
!
!     iunit - unit number the calling program wants to assign to the
!             file of available flyby cals
!
!     limit - maximum number of calibrations the user wants to pull
!             from the file
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*8 FLAV_NAMES(*)
      INTEGER*2 NUM_FLAV,KERR,jcaff1(*)
!
! FLAV_NAMES - list of available flyby calibrations, according to the file
! NUM_FLAV - number of flyby available calibrations, according to the file
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
      INTEGER*2  IERR
      CHARACTER  ERRSTR*100 
      CHARACTER  FNAME*256, BUF*256 
      CHARACTER  TOKEN*8
      INTEGER*4  IERR4
!
! 4.  HISTORY
!   WHO   WHEN       WHAT
!   KDB  910801      Created
!   KDB  2006.06.15  Increased phe lenfth of the file names from 50 to 256 &
!        character
!
! 5.  FLYAVL_N PROGRAM STRUCTURE
!
!     The file of theoretically available flyby calibrations lists the
!     calibrations, one per line.
!     Just read the file and pull out the calibration names, up to the
!     limit the user requested.
!
      FNAME=PRE_SAV_DIR(:PRE_SV_LEN)//AVAL_FCAL_FILE
      OPEN ( IUNIT, FILE=FNAME, IOSTAT=IERR4, ERR=100, &
     &       STATUS='OLD', ACCESS='SEQUENTIAL', FORM='FORMATTED' )
 100  CONTINUE 
      IF ( IERR4 .NE. 0 ) THEN
           WRITE ( 6, * ) 'Failure in reading file '//TRIM(FNAME)
           KERR = -1
           GOTO 900
      END IF
      NUM_FLAV = 0
      DO WHILE (NUM_FLAV.LT.LIMIT)
        READ (IUNIT,'(A)',IOSTAT=IERR4,ERR=200,END=300) buf
        NUM_FLAV = NUM_FLAV + 1
        call splitstring(buf,token,buf)
        FLAV_NAMES(NUM_FLAV) = token
        call splitstring(buf,token,buf)
!       if (token(1:7).eq.'DEFAULT') then
!         call sbit(jcaff1(1),num_flav,1)
!       endif
      END DO
!
!     The sub stopped reading because it reached the limit requested
!     by the user.  See if there were more left, to warn the user.
!
      READ(IUNIT,'(A8)',IOSTAT=IERR4,ERR=201,END=301)
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
!     error and eof from normal read to pull off list
!
 200  CLOSE(IUNIT)
      KERR = -2
      GO TO 900
 300  CLOSE (IUNIT)
      KERR = 0
!
 900  RETURN
      END
