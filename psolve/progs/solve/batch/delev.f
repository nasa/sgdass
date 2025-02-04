      SUBROUTINE DELEV()
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  DELEV PROGRAM SPECIFICATION
!
! 1.1 Set elevation cut array.
!
! 1.2 REFERENCES:
!
! 2.  DELEV INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'astro_constants.i'
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'belev.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: arcset
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2  I,J
      LOGICAL*2  EQUAL
      INTEGER*4  J1
      REAL*8     EL
!
! 4.  HISTORY
!   WHO   WHEN        WHAT
!   pet   2008.02.25  Added support arc-dependent elevation cutoff
!
! 5.  DELEV PROGRAM STRUCTURE
!
! Loop over stations
!
      DO I=1,NUMSTA
!
! ----- Start with default elevation cutoff
!
        EL=ELDEF
!
! ------ Loop over elevation cutoff requests
!
        DO J=1,NEL
!
! ------- If station name matches that of request, use new elevation cutoff
!
          IF(EQUAL( IELNM(1,J), INT2(1), ISITN(1,I), INT2(1), INT2(8) )) THEN
            EL=ELC(J)
            GO TO 220
          ENDIF
        ENDDO
220     CONTINUE
!
! ----- Convert to radians
!
        ELVCUT(I)=EL*PI__NUM/180.0D0
      ENDDO
!
      SITE_DEP_EL_CUT=.TRUE.
!
      IF ( NEL_ARC > 0 ) THEN
          DO I=1,NUMSTA
             DO 410 J1=1,NEL_ARC
                IF ( ELNM_ARC_CHR(J1) == 'ALL     '   .OR.  &
                     ELNM_ARC_CHR(J1) == ISITN_CHR(I)       ) THEN
!
                     ELVCUT(I) = ELVAL_ARC(J1)*PI__NUM/180.0D0
                END IF
 410         CONTINUE 
          END DO
      END IF
!
      RETURN
      END  SUBROUTINE  !#!#  DELEV
