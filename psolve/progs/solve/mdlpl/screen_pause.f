      SUBROUTINE SCREEN_PAUSE ( CONTROL_FILE_NAME )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!     Sees if the user wants to copy to the laser jet
!
      CHARACTER     CONTROL_FILE_NAME*30, DEV_TYPE*10 
      INTEGER*4     ICHR, IX, IY, FPLOT, IERR4
      CHARACTER*4   CCHAR
      EQUIVALENCE ( CCHAR, ICHR )
!
! 4.  HISTORY
!  WHO  WHEN        WHAT
!  JWR  880527      Original version
!  jwr  90:07:30    Modified for pgplot
!  JLR  921216      replaced   0J with I4P0
!  pet  2003.09.02  replaced I4P0 with 0
!
!
      CALL RESET_SCREEN ( 0 )
      CALL ADDSTR_F ( "Control: Space bar to skip on, O to return to "// &
     &                "OPTIN, C to copy?" )
      CALL SENKR_MN ( IX, IY, ICHR )
      IF ( CCHAR(4:4) .EQ. 'O' ) THEN
           CALL RUN_PROG( 'OPTIN', 'PASS', INT2(0) )
      ENDIF
!
      IF ( CCHAR(4:4) .EQ. 'C' ) THEN
           DEV_TYPE = '/hpglp    '
           IERR4 = FPLOT ( CONTROL_FILE_NAME, DEV_TYPE )
      ENDIF
!
      CALL RESET_SCREEN ( 0 )
!
      RETURN
      END  !#!  SCREEN_PAUSE  #!#
