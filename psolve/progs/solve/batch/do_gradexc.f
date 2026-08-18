      SUBROUTINE DO_GRADEXC ( ISTAD )
      IMPLICIT NONE
!
! 1.  DO_GRADEXC PROGRAM SPECIFICATION
!
! 1.1 Delete gradients at specified stations.
!
! 1.2 REFERENCES:
!
! 2.  DO_GRADEXC INTERFACE
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
      INCLUDE 'ba2cm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: sflags
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I, J, II, NUM
      CHARACTER GRADFLG_USE*1
      LOGICAL*2 KBIT
!
! 4.  HISTORY
!     WHO   WHEN        WHAT
!     pet   2006.05.12  Added support of "station name" ALL
!
! 5.  DO_GRADEXC PROGRAM STRUCTURE
!
      NUM = NUMGRAD(1)
      GRADFLG_USE = GRADFLG
      IF ( GRAD_INTV_TYP .NE. ' ' ) GRADFLG_USE = GRAD_INTV_TYP
!
      DO I=1,NUMSTA
        IF ( .NOT. KBIT(ISTAD,I) ) THEN
!
! ---------- If data is turned off for this station, then no gradients
!
             NUMGRAD(I)=0
           ELSE IF ( GRADFLG_USE .EQ. 'Y'  .OR.  GRADFLG_USE .EQ. 'A' ) THEN
             DO J=1,NGRADEX
                IF ( GRADEX_CHR(J) .EQ. ISITN_CHR(I) ) THEN
                     NUMGRAD(I)=0
                     GOTO 220
                ENDIF
             ENDDO
           ELSE
             NUMGRAD(I) = 0
             DO J=1,NGRADEX
                IF ( GRADEX_CHR(J) .EQ. ISITN_CHR(I) ) THEN
                     NUMGRAD(I) = NUM
                     GOTO 220
                ENDIF
             ENDDO
        ENDIF
220     CONTINUE

        IF ( NUM_GRADOFF .GT. 0 ) THEN
             DO II=1,NUM_GRADOFF
                IF ( ISITN_CHR(I) .EQ. LIST_GRADOFF(II) ) THEN
!
! ------------------ Clear the flag of estimation of atmosphere gradients
! ------------------ for this station
!
                     NUMGRAD(I) = 0
                 ENDIF
                 IF ( LIST_GRADOFF(II)(1:4) == 'ALL ' ) THEN
                      NUMGRAD(I) = 0
                 END IF
             END DO
        END IF
!
        IF ( NUM_ATMOFF .GT. 0 ) THEN
             DO II=1,NUM_ATMOFF
                IF ( ISITN_CHR(I) .EQ. LIST_ATMOFF(II) ) THEN
!
! ------------------ Clear the flag of estimation of atmosphere gradients
! ------------------ for this station
!
                     NUMGRAD(I)  = 0
                 ENDIF
!
                 IF ( LIST_ATMOFF(II)(1:4) == 'ALL ' ) THEN
                      NUMGRAD(I) = 0
                 END IF
             END DO
         ENDIF
!
         IF ( GRAD_INTV_TYP .EQ. 'N' ) THEN
              NUMGRAD(I) = 0
         END IF
      ENDDO
!
      RETURN
      END  !#!  DO_GRADEXC  #!#
