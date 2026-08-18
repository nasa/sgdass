      SUBROUTINE UEN_VEL(VSITEV,VSITEC,NUMSTA)
      IMPLICIT NONE
!
! 1.  UEN_VEL PROGRAM SPECIFICATION
!
! 1.1 Calculate site velocity parameters in UEN coordinate frame.
!
! 1.2 REFERENCES:
!
! 2.  UEN_VEL INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      REAL*8 VSITEV(3,*),VSITEC(3,*)
      INTEGER*2 NUMSTA
!
! NUMSTA - Number of sites
! VSITEC - Site coordinate parameters in XYZ
! VSITEV - Site velocity parameters in XYZ
!
! 2.3 OUTPUT Variables:
!
! VSITEV - Site velocity parameters in UEN
!
! 2.4 COMMON BLOCKS USED
      REAL*8 MAT1(3,3)
      COMMON/ROTEMA/MAT1
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: a1jst
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I,ISTA,J
      REAL*8 XYZ(3),VUEN(3)
!
! 4.  HISTORY
!   WHO   WHEN  WHAT
!
! 5.  UEN_VEL PROGRAM STRUCTURE
!
! Loop over stations
!
      DO ISTA=1,NUMSTA
!
! Extract site coordinates for this station
!
        DO I=1,3
          XYZ(I)=VSITEC(I,ISTA)
        ENDDO
!
! Produce rotation matrix for rotation into UEN
!
        CALL UEN_ROT(XYZ,MAT1)
!
! Rotate velocities into UEN
!
        DO I=1,3
          VUEN(I)=0.0D0
          DO J=1,3
            VUEN(I)=VUEN(I)+MAT1(I,J)*VSITEV(J,ISTA)
          ENDDO
        ENDDO
!
! Transfer velocity values into output array
!
        DO I=1,3
          VSITEV(I,ISTA)=VUEN(I)
        ENDDO
      ENDDO
!
      RETURN
      END
