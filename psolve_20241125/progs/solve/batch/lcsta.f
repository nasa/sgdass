      SUBROUTINE LCSTA ( ILCSTA, NUMSTA, ISTAD, ISITN )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  LCSTA PROGRAM SPECIFICATION
!
! 1.1 How many local stations in this arc?
!
! 1.2 REFERENCES:
!
! 2.  LCSTA INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 NUMSTA,ISTAD(*),ISITN(4,*)
!
! ISITN - Array of station names
! ISTAD - Station data flag
! NUMSTA - Total number of stations
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 ILCSTA
!
! ILCSTA - Number of local stations
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'glbc2.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: arcset
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I, J
      LOGICAL*2 KCARRY, KBIT, EQUAL
!
! 4.  HISTORY
!   WHO   WHEN        WHAT
!   pet   2002.05.30  Improved comments
!
! 5.  LCSTA PROGRAM STRUCTURE
!
      ILCSTA=0
!
! Loop over all stations
!
      DO I=1,NUMSTA
!
! ------ If data is turned on for this station, set carry flag to default
!
         IF ( KBIT(ISTAD,I) ) THEN
              KCARRY=KCSTA
!
! ----------- Now check exception list to determine whether to reverse
! ----------- carry flag
!
              DO J=1,NACSTA
                 IF ( EQUAL( ISELAR((J-1)*4+IACSTA), INT2(1), ISITN(1,I), &
     &                INT2(1), INT2(8)) ) THEN
                      KCARRY=.NOT.KCARRY
                      GOTO 100
                 ENDIF
              ENDDO
100           CONTINUE
!
! ----------- If carry flag is not set, increment number of local stations
!
              IF ( .NOT. KCARRY ) ILCSTA=ILCSTA+1
         ENDIF
      ENDDO
!
      RETURN
      END  !#!  LCSTA  #!#
