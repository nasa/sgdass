      SUBROUTINE SVELP ( LSINAM, VSUBXYZ, SITDIF, VELOCITY_FILE_NAME, KFBDSP, &
     &                   NVSITEV, TIME0X, NVSITEC )
      IMPLICIT NONE
!
! 1.  SVELP PROGRAM SPECIFICATION
!
! 1.1 Call subroutines GVSTAP and DVSTAP to read a site position
!     velocity substitution file and to calculate site position differences.
!
! 1.2 REFERENCES:
!
! 2.  SVELP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      LOGICAL*2 KFBDSP
      CHARACTER VELOCITY_FILE_NAME*(*)
      REAL*8    TIME0X
!
! KFBDSP - True if flyby info is to be displayed
! VELOCITY_FILE_NAME - Name of station velocity mod file
! TIME0X - Site ref date parameter
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 LSINAM(4,*)
      REAL*8 VSUBXYZ(3,*),SITDIF(3,*),NVSITEV(3,*),NVSITEC(3,*)
!
! LSINAM - Array of station names
! NVSITEV - Site coordinates after substitutions
! SITDIF - Site coordinate differences
! VSUBXYZ - Velocity coordinates from mod file
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'bindisp.i'
      INCLUDE 'flyby.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: flyby_init
!       CALLED SUBROUTINES: gvelp,dvelp
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 NSITE
      INTEGER*2 INT2_ARG
      INTEGER*4 INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! NSITE - Number of stations with alternate positions.
!
! 4.  HISTORY
!   WHO   WHEN        WHAT
!   AEE   03/14/91    First version
!   pet   2000.11.28  some updates
!   pet   2003.08.15  Re-wrote. Enabled support of keeping substituion arrays 
!                     in flyby.i, so the file is read only once
!
! 5.  SVELP PROGRAM STRUCTURE
!
      IF ( VEL_SUBSTITUTE_INIT ) THEN
!
! -------- Aaa. Site position substitution file has been already read. 
! -------- Then we need to merely copy it from the save place
!
           NSITE = NVEL_KEEP 
           IF ( NVEL_KEEP .GT. 0 ) THEN
                CALL LIB$MOVC3 ( 8*INT4(NVEL_KEEP),   LVELNM_KEEP, LSINAM  )
                CALL LIB$MOVC3 ( 3*8*INT4(NVEL_KEEP), VELCO_KEEP,  VSUBXYZ )
           END IF
           TIME0X = TIME0X_KEEP
         ELSE 
!
! -------- Aga. We did not yet read source substitution file. Let's do it
!
           CALL GVELP ( LSINAM, NSITE, VSUBXYZ, VELOCITY_FILE_NAME, KFBDSP, &
     &                  TIME0X )
!
! -------- Set the flag that we have done it
!
           VEL_SUBSTITUTE_INIT = .TRUE.
!
! -------- Keep arrays which we have read
!
           NVEL_KEEP = NSITE 
           IF ( NVEL_KEEP .GT. 0 ) THEN
                CALL LIB$MOVC3 ( 8*INT4(NVEL_KEEP),   LSINAM,  LVELNM_KEEP )
                CALL LIB$MOVC3 ( 3*8*INT4(NVEL_KEEP), VSUBXYZ, VELCO_KEEP  )
           END IF
           TIME0X_KEEP = TIME0X 
      END IF
!
! --- Do substitutions
!
      CALL DVELP ( LSINAM, NSITE, VSUBXYZ, NUMSTA, ISITN, TIME0X, SITDIF, &
     &             FLYBY_WARNING, NVSITEV, NVSITEC )
!
      RETURN
      END  !#!  SVELP  #!#
