      SUBROUTINE XEPAR (IPARM2,IX2T3,NPARM2,IPARM3,NPARM3,IWDS)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  XEPAR PROGRAM SPECIFICATION
!
! 1.1 Generate a cross reference list from IPARM2 to IPARM3.
!
! 1.2 REFERENCES:
!
! 2.  XEPAR INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IWDS,NPARM2,NPARM3
      INTEGER*2 IPARM2(IWDS,NPARM2),IPARM3(IWDS,NPARM3)
!
! IPARM2 - First parameter list
! IPARM3 - Second parameter list
! IWDS - Number of words per parameter
! NPARM2 - Number of parameters in first list
! NPARM3 - Number of parameters in second list
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 IX2T3(NPARM2)
!
! IX2T3 - Cross reference list from IPARM2 to IPARM3
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
!
      INTEGER*2 ICH,ICHCM,I,J
      CHARACTER  STR1*128, STR2*128
      INTEGER*2 INT2_ARG
      INTEGER*4 INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! I,J - Loop index
! ICH - Number of bytes per parameter
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   pet   2005.03.18  Got rid of ICHCM
!
! 5.  XEPAR PROGRAM STRUCTURE
!
      ICH=2*IWDS
      IF ( ICH > LEN(STR1)  .OR.  ICH .LE. 0 ) THEN
           WRITE ( 6, * ) ' ICH=',ICH
           STOP 'XEPAR: Fatal error'
      END IF
!
      DO 2000 J=1,NPARM2
         IX2T3(J)=0
         CALL LIB$MOVC3 ( INT4(ICH), IPARM2(1,J), %REF(STR1) )
         DO 1500 I=1,NPARM3
!@            IF ( ICHCM( IPARM2(1,J), INT2(1), IPARM3(1,I), INT2(1), ICH ) &
!@     &           .NE. 0 ) GOTO 1500
            CALL LIB$MOVC3 ( INT4(ICH), IPARM3(1,I), %REF(STR2) )
            IF ( STR1(1:ICH) .NE. STR2(1:ICH) ) GOTO 1500
            IX2T3(J)=I
            GOTO 2000
1500    CONTINUE
2000  CONTINUE
!
      RETURN
      END
