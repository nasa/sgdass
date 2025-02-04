      SUBROUTINE CXEPAR ( LPARM2, IX2T3, NPARM2, LPARM3, NPARM3 )
      IMPLICIT NONE
! character version of xepar
!
! 1.  XEPAR PROGRAM SPECIFICATION
!
! 1.1 Generate a cross reference list from LPARM2 to LPARM3.
!
! 1.2 REFERENCES:
!
! 2.  XEPAR INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 NPARM2, NPARM3
      CHARACTER LPARM2(NPARM2)*(*), LPARM3(NPARM3)*(*) 
!
! LPARM2 - First parameter list
! LPARM3 - Second parameter list
! IWDS - Number of words per parameter
! NPARM2 - Number of parameters in first list
! NPARM3 - Number of parameters in second list
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 IX2T3(NPARM2)
!
! IX2T3 - Cross reference list from LPARM2 to LPARM3
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
      INTEGER*2 i,J
!
! I,J - Loop index
! ICH - Number of bytes per parameter
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  XEPAR PROGRAM STRUCTURE
!
      DO 2000 J=1,NPARM2
        IX2T3(J)=0
        DO 1500 I=1,NPARM3
          if(lparm2(j) .eq. lparm3(i)) then
            IX2T3(J)=I
            GO TO 2000
          endif
1500      CONTINUE
2000    CONTINUE
!
      RETURN
      END
