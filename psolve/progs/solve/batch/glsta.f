      SUBROUTINE GLSTA ( IGLSTA, I1, I2, NUMSTA, ISTAD, ISITN )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GLSTA PROGRAM SPECIFICATION
!
! 1.1 How many global stations in this solution?
!
! 1.2 REFERENCES:
!
! 2.  GLSTA INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IGLSTA,NUMSTA,ISTAD(*),ISITN(4,*)
!
! IGLSTA - Number of global stations
! ISITN - Array of station names
! ISTAD -  Station data flag
! NUMSTA - Number of stations
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 I1,I2
!
! I1,I2 - Station list positions of first two global stations
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'glbc2.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: arcset
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I ,J
      LOGICAL*2 KCARRY, KBIT, EQUAL
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  GLSTA PROGRAM STRUCTURE
!
      IGLSTA=0
!
! --- Loop over stations
!
      DO I=1,NUMSTA
         IF ( KBIT(ISTAD,I) ) THEN
!
! ----------- Set primary value for station carry flag
!
              KCARRY=KCSTA
              DO J=1,NACSTA
!
! -------------- Check for EXCEPTions
!
                 IF ( EQUAL ( ISELAR((J-1)*4+IACSTA), INT2(1), ISITN(1,I), &
     &                INT2(1), INT2(8)) ) THEN
                      KCARRY=.NOT.KCARRY
                      GOTO 100
                 ENDIF
              ENDDO
100           CONTINUE
              IF ( KCARRY ) THEN
!
! ---------------- Increment count of global stations
!
                   IGLSTA=IGLSTA+1
                   IF ( IGLSTA .EQ. 1 ) I1=I
                   IF ( IGLSTA .EQ. 2 ) I2=I
              ENDIF
         ENDIF
      ENDDO
!
      RETURN
      END  !#!  GLSTA  #!#
