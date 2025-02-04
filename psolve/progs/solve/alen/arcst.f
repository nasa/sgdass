      SUBROUTINE ARCST (R1,D1,R2,D2, &
     &                  ER1,ED1,ER2,ED2, &
     &                  CR1D1,CR1R2,CR1D2,CD1R2,CD1D2,CR2D2, &
     &                  ARCDST,ARCERR)
!
      IMPLICIT NONE
!
! 1.  ARCST PROGRAM SPECIFICATION
!
! 1.1 Calculate arc distances between sources and corresponding
!     errors.
!
! 1.2 REFERENCES:
!
! 2.  ARCST INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      REAL*8 CD1D2,CD1R2,CR1D1,CR1D2, &
     &       CR1R2,CR2D2,D1,D2,ED1,ED2,ER1,ER2,R1,R2
!
!     R1    :RA  SOURCE 1 (RADIAN)
!     D1    :DEC SOURCE 1 (RADIAN)
!     R2    :RA  SOURCE 2 (RADIAN)
!     D2    :DEC SOURCE 2 (RADIAN)
!     ER1   :VARIANCE IN R1 (RADIAN)
!     ED1   :            D1
!     ER2   :            R2
!     ED2   :            D2
!     CR1D1 :NORM. CORR. BETWEEN R1 AND D1 (FROM CORR. MATRIX)
!     CR1R2 :                    R1     R2
!     CR1D2 :                    R1     D2
!     CD1R2 :                    D1     R2
!     CD1D2 :                    D1     D2
!     CR2D2 :                    R2     D2
!
! 2.3 OUTPUT Variables:
!
      REAL*8 ARCDST,ARCERR
!
!     ARCDST:ARC DISTANCE (RADIAN)
!     ARCERR:ARC DISTANCE VARIANCE (RADIAN)
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: alen
!       CALLED SUBROUTINES: dacs
!
! 3.  LOCAL VARIABLES
!
      REAL*8 A,B,C,CD1,CD2,CRA,DACS,DIFRA,SARC,SD1,SD2,SRA
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  ARCST PROGRAM STRUCTURE
!
      DIFRA=R2-R1
      CRA=DCOS(DIFRA)
      SRA=DSIN(DIFRA)
      CD1=DCOS(D1)
      SD1=DSIN(D1)
      CD2=DCOS(D2)
      SD2=DSIN(D2)
!
!     ARC DISTANCE
!
      ARCDST=DACS(CD1*CD2*CRA+SD1*SD2)
      SARC=DSIN(ARCDST)
!
!     FACTORS A, B, AND C FOR STATISTICS
!
      A=CD1*SD1-SD1*CD2*CRA
      B=SD1*CD2-CD1*SD2*CRA
      C=-(CD1*CD2*SRA)
!
!     ARC DISTANCE VARIANCE
!
      ARCERR=( A*A*ED1*ED1 &
     &        +B*B*ED2*ED2 &
     &        +C*C*ER1*ER1 &
     &        +C*C*ER2*ER2 &
     &        +2.D0*A*B*ED1*ED2*CD1D2 &
     &        +2.D0*A*C*ED1*ER1*CR1D1 &
     &        -2.D0*A*C*ED1*ER2*CD1R2 &
     &        +2.D0*B*C*ED2*ER1*CR1D2 &
     &        -2.D0*B*C*ED2*ER2*CR2D2 &
     &        -2.D0*C*C*ER1*ER2*CR1R2) / (SARC*SARC)
      RETURN
      END
