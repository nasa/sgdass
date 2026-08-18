      SUBROUTINE CXEPAR2 ( LPARM2, IX2T3, NPARM2, LPARM3, NPARM3 )
      IMPLICIT NONE
! character version of xepar2
! modified July 29, 1996 JMGipson
! replaced LPARM2,LPARM3 by character strings
! pet 2017.11.06 converted to INTEGER*4  
!
      INTEGER*4 NPARM2, NPARM3, IX2T3(NPARM2)
      INTEGER*4 I, J
      INTEGER*2 IWDS
      CHARACTER LPARM2(NPARM2)*(*), LPARM3(NPARM3)*(*) 
!
!   This subroutine will generate a cross reference list
!   from LPARM2 to LPARM3, taking advantage of LPARM2 being a superset
!   and in the same order as LPARM3
!
      J=1
      DO I=1,NPARM3
         DO WHILE ( LPARM2(J) .NE. LPARM3(I) )
            IF ( J .GT. NPARM2 ) THEN
                 CALL FERR ( 108, 'More Parameters in LPARM3 than in LPARM2', 0, 0 )
            END IF
            IX2T3(J)=0
            J=J+1
         ENDDO
         IF ( J .GT. NPARM2 ) THEN
              CALL FERR ( 109, 'More Parameters in LPARM3 than in LPARM2',0,0)
         END IF
         IX2T3(J)=I
         J=J+1
      ENDDO
!
! --- Clean up
!
      DO I=J,NPARM2
         IX2T3(I)=0
      ENDDO
!
      RETURN
      END  SUBROUTINE  CXEPAR2  !#!#
