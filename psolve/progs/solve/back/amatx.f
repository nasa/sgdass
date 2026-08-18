      SUBROUTINE AMATX ( A1, A2, B1, B2, S1, S2, IXNT3, NPARM )
      IMPLICIT NONE
!
! 1.  AMATX PROGRAM SPECIFICATION
!
! 1.1 Add the global parameters from the COVFL to the saved arc
!     equations.  Map MAT2 to MAT1 via IXNT3 cross-reference list.
!
! 1.2 REFERENCES:
!
! 2.  AMATX INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4 IXNT3(*),NPARM
      REAL*8 A2(*),B2(*),S2(*)
!
! A2,B2,S2 - Elements of CGM (MAT2) to be moved to MAT1 slots
! IXNT3 - Cross-reference list
! NPARM - Number of parameters
!
! 2.3 OUTPUT Variables:
!
      REAL*8 A1(*),B1(*),S1(*)
!
! A1,B1,S1 - Corresponding ARC (MAT1) elements moved from CGM
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: mxcmb
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 I, J, IX
      INTEGER*8 INDX8
!
! 4. HISTORY
!   WHO   WHEN  WHAT
!
! 5.  AMATX PROGRAM STRUCTURE
!
      DO I=1,NPARM
         IF ( IXNT3(I) .NE. 0 ) THEN
              IX = IXNT3(I)
              IF ( IX .LE. M_GPA  .AND.  I .LE. M_GPA ) THEN
                   S1(IX)=S2(I)
                   B1(IX)=B2(I)
              ENDIF
              DO J=1,I
                 IF ( IXNT3(J).NE.0 ) THEN
                      A1(INDX8(IX,IXNT3(J))) = A2(INDX8(I,J))
                 ENDIF
              ENDDO
          ENDIF
      ENDDO
!
      RETURN
      END  !#!  AMATX  #!#
