      SUBROUTINE SSTAP ( LSINAM, SUBXYZ, SITDIF, LNAME, KFBDSP, NVSITEC )
      IMPLICIT NONE
!
! 1.  SSTAP PROGRAM SPECIFICATION
!
! 1.1 Call subroutines GSTAP and DSTAP to read a site position
!     substitution file and to calculate site position differences.
!
! 1.2 REFERENCES:
!
! 2.  SSTAP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      LOGICAL*2 KFBDSP
      CHARACTER*(*) LNAME
!
! KFBDSP - True if flyby info is to be displayed
! LNAME - Name of station mod file
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 LSINAM(4,*)
      REAL*8 SUBXYZ(3,*),SITDIF(3,*),NVSITEC(3,*)
!
! LSINAM - Array of station names
! NVSITEC - Site coordinates after substitutions
! SITDIF - Site coordinate differences
! SUBXYZ - Alternate site coordinates from mod file
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'bindisp.i'
      INCLUDE 'flyby.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: flyby_init
!       CALLED SUBROUTINES: gstap,dstap
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 NSITE
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! NSITE - Number of stations with alternate positions.
!
! 4.  HISTORY
!   WHO   WHEN        WHAT
!   pet   2003.08.15  Re-wrote. Enabled support of keeping substituion arrays 
!                     in flyby.i, so the file is read only once
!
! 5.  SSTAP PROGRAM STRUCTURE
!
!  GET SUBSTITUTIONS
!
      IF ( SIT_SUBSTITUTE_INIT ) THEN
!
! -------- Aaa. Site position substitution file has been already read. 
! -------- Then we need to merely copy it from the save place
!
           NSITE = NSIT_KEEP 
           IF ( NSIT_KEEP .GT. 0 ) THEN
                CALL LIB$MOVC3 ( 8*INT4(NSIT_KEEP),   LSITNM_KEEP, LSINAM )
                CALL LIB$MOVC3 ( 3*8*INT4(NSIT_KEEP), SITCO_KEEP,  SUBXYZ )
           END IF
         ELSE 
!
! -------- Aga. We did not yet read source substitution file. Let's do it
!
           CALL GSTAP ( LSINAM, NSITE, SUBXYZ, LNAME, KFBDSP )
!
! -------- Set the flag that we have done it
!
           SIT_SUBSTITUTE_INIT = .TRUE.
!
! -------- Keep arrays which we have read
!
           NSIT_KEEP = NSITE
           IF ( NSIT_KEEP .GT. 0 ) THEN
                CALL LIB$MOVC3 ( 8*INT4(NSIT_KEEP),   LSINAM, LSITNM_KEEP )
                CALL LIB$MOVC3 ( 3*8*INT4(NSIT_KEEP), SUBXYZ, SITCO_KEEP  )
           END IF
      END IF
!
! --- Do substitutions
!
      CALL DSTAP ( LSINAM, NSITE, SUBXYZ, NUMSTA, ISITN, &
     &             VSITEC, SITDIF, FLYBY_WARNING, NVSITEC )
!
      RETURN
      END  SUBROUTINE SSTAP 
