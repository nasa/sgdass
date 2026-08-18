      SUBROUTINE STECTX ( LNAME, NVSITEV, TIME0X, PLATE_FACT )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  PROGRAM SPECIFICATION
!
! 1.1 Calculate site displacements using tectonic plate model
!       AM0-2 or NUVEL.
!
! 1.2 REFERENCES:
!
! 2.  STECT INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) LNAME
      REAL*8 TIME0X, PLATE_FACT
!
! LNAME - Name of station mod file
! TIME0X - Site ref date parameter
!
! 2.3 OUTPUT Variables:
!
      REAL*8 NVSITEV(3,*)
!
! NVSITEV - SItee velocities after substitutions
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: flyby_init
!       CALLED SUBROUTINES: sitpl,absmo,absmo_nuvel
!
! 3.  LOCAL VARIABLES
!
      REAL*8 T,T0,XPM,YPM,ZPM
      CHARACTER*4 PLATES(MAX_STA)
      INTEGER*2 I
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   MWH  900620  Use site motion reference date provided in control file
!                (If none provided use default - 1980 Oct 17  00UT)
!   PET  2006.06.05  Fixed a  bug: the last argument for ABSMO_NUVEL was &
!                    missed.
!
! 5. STECT PROGRAM STRUCTURE
!
      IF ( LNAME(1:5) .NE. 'AM0-2' .AND. &
     &     LNAME(1:5) .NE. 'NUVEL'       ) THEN
           CALL FERR ( INT2(219), 'STECTX: Unknown plate motion model', &
     &                 INT2(0), INT2(0) )
      END IF
!
      T0 = TIME0X
      T  = TATM(1)/365.25D0
      CALL SITPL ( ISITN, NUMSTA, PLATES )
      DO I=1,NUMSTA
         IF ( LNAME(1:5) .EQ. 'AM0-2' ) THEN
              CALL ABSMO ( PLATES(I), T0, VSITEC(1,I), VSITEC(2,I), &
     &                     VSITEC(3,I), T, XPM, YPM, ZPM, PLATE_FACT )
           ELSE IF ( LNAME(1:5) .EQ. 'NUVEL' ) THEN
              CALL ABSMO_NUVEL ( PLATES(I), T0, VSITEC(1,I), VSITEC(2,I), &
     &                           VSITEC(3,I), T, XPM, YPM, ZPM, PLATE_FACT, &
     &                           'NONE' )
         ENDIF
!
         NVSITEV(1,I) = NVSITEV(1,I) + (XPM-VSITEC(1,I))/(T-T0)
         NVSITEV(2,I) = NVSITEV(2,I) + (YPM-VSITEC(2,I))/(T-T0)
         NVSITEV(3,I) = NVSITEV(3,I) + (ZPM-VSITEC(3,I))/(T-T0)
      END DO
!
      RETURN
      END  SUBROUTINE  STECTX  !#!#
