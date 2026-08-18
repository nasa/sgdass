      LOGICAL*2 FUNCTION KCHEK2(IARR,IEMA,IC)
      IMPLICIT NONE
!
! 1.  KCHEK2 PROGRAM SPECIFICATION
!
! 1.1 Returns true if IARR and IEMA arrays match, false otherwise
!
! 1.2 REFERENCES:
!
! 2.  KCHEK2 INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IARR(*),IEMA(*),IC
!
! IARR - Clock or atmosphere
! IEMA - Array of Clock/atmosphere parameters
! IC - 9
!
! 2.3 OUTPUT Variables:
!
! KCHEK2 - True if IARR matches IEMA, False otherwise
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: ind_par
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I
!
! 4.  HISTORY
!  WHO  WHEN    WHAT
!
! 5.  KCHEK2 PROGRAM STRUCTURE
!
      KCHEK2=.FALSE.
      DO I=1,IC/2
         IF(IARR(I).NE.IEMA(I)) RETURN
      ENDDO
!
      IF ( MOD(IC,INT2(2)) .EQ. 1 ) THEN
           I=IC/2+1
           IF ( IBITS(IARR(I),INT2(8),INT2(8)) .NE. &
     &          IBITS(IEMA(I),INT2(8),INT2(8))        ) RETURN
      ENDIF
!
      KCHEK2=.TRUE.
      RETURN
      END
