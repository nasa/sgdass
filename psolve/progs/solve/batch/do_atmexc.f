      SUBROUTINE DO_ATMEXC ( ISTAD )
      IMPLICIT NONE
!
! 1.  DO_ATMEXC PROGRAM SPECIFICATION
!
! 1.1 Delete atmospheres at specified stations.
!
! 1.2 REFERENCES:
!
! 2.  DO_ATMEXC INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 ISTAD(*)
!
! ISTAD - Station data flag
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'batme.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: sflags
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I, II, J
      LOGICAL*2 KBIT
!
! 4.  HISTORY
!     WHO   WHEN        WHAT
!     pet   2001.12.28  Starting from today do_atmexc also excludes troposphere
!                       gradients in the case if excludes estimation of
!                       atmopshere parameters
!     pet   2004.03.15  Updated the code for the case when estimation of &
!                       atmosphere is switched off for more than one station
!     pet   2004.03.16  Added support of the feature which allows to switch &
!                       off gradients for some stations
!     pet   2005.08.23  Cleaned logic
!     pet   2006.05.12  Added support of "station name" ALL
!
! 5.  DO_ATMEXC PROGRAM STRUCTURE
!
      DO I=1,NUMSTA
         IF ( .NOT. KBIT(ISTAD,I) ) THEN
!
! ----------- If data is turned off for this station, then no atmospheres
!
              NUMATM(I)  = 0
              NUMGRAD(I) = 0
           ELSE
             IF ( NUM_ATMOFF .GT. 0 ) THEN
                  DO II=1,NUM_ATMOFF
                     IF ( ISITN_CHR(I) .EQ. LIST_ATMOFF(II) ) THEN
!
! ----------------------- Set estimation of only amtosphere offset
!
                          NUMATM(I)  = 1
                     ENDIF
                     IF ( LIST_ATMOFF(II)(1:4) == 'ALL ' ) THEN
                          NUMATM(I)  = 1
                     END IF
                  END DO
             ENDIF
!
             IF ( NUM_ATMEXC .GT. 0 ) THEN
                  DO II=1,NUM_ATMEXC
                     IF ( ISITN_CHR(I) .EQ. LIST_ATMEXC(II) ) THEN
!
! ----------------------- Delete atmospheres if this station is on the list 
! ----------------------- to do so
!
                          NUMATM(I)  = 0
                          NUMGRAD(I) = 0
                     ENDIF
                  END DO
             ENDIF
!
             DO J=1,NATMEX
                 IF ( ATMEX_CHR(J) .EQ. ISITN_CHR(I) ) THEN
                      NUMATM(I)  = 0
                      NUMGRAD(I) = 0
                 ENDIF
             ENDDO
         ENDIF
220      CONTINUE
      ENDDO
!
      RETURN
      END  !#!  DO_ATMEXC  #!#
