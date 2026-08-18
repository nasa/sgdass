      SUBROUTINE DOION ( IONCTL, IONFLG )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  DOION PROGRAM SPECIFICATION
!
! 1.1 Set up ionosphere calibration control.
!
! 1.2 REFERENCES:
!
! 2.  DOION INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) IONCTL
      INTEGER*2 IONFLG
!
! IONCTL - Ionosphere control variable (ON,OFF or DEFAULT)
! IONFLG - Ionosphere control flag
!
! 2.3 OUTPUT Variables:
!
! IONFLG - Ionosphere control flag
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: docali
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2 KBIT
!
! 4.  HISTORY
!   WHO  WHEN        WHAT
!   pet  1999.11.18  Changed logic: forced it to apply ionosphere if it is
!                    available when IONFLG is ON
!   pet  2000.02.16  Changed logic: forbid to try toapply PHION even if it is
!                    available
!
! 5.  DOION PROGRAM STRUCTURE
!
!
! ---  If DEFAULT, then we're finished
!
      IF ( IONCTL(1:7) .EQ. 'DEFAULT' ) RETURN
!
! If ON, then make sure required calibration is applied
!
      IF ( IONCTL(1:2) .EQ. 'ON' ) THEN
           IF ( KBIT ( IONFLG, INT2(1) ) ) THEN
!
! ------------- GION is available
!
                CALL SBIT ( IONFLG, INT2(4), INT2(1) )
                CALL SBIT ( IONFLG, INT2(5), INT2(0) )
           END IF
        ELSE IF ( IONCTL(1:3) .EQ. 'OFF' ) THEN
!
! -------- If OFF, then turn off calibration flag bits
!
           CALL SBIT ( IONFLG, INT2(4), INT2(0) )
           CALL SBIT ( IONFLG, INT2(5), INT2(0) )
        ELSE
!
! --------  Error for invalid value of IONCTL
!
           CALL FERR ( INT2(20020), 'BATCH(doion): Unknown ion control flag', &
     &          INT2(0), INT2(0) )
      ENDIF
!
      RETURN
      END  !#!  DOION  #!#
