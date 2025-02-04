      SUBROUTINE HSORT(RA,RX,N)
      IMPLICIT NONE
!
! 1.  HSORT PROGRAM SPECIFICATION
!
! 1.1 Sort an I*2 and a REAL*8 array.
!
! 1.2 REFERENCES:
!
! 2.  HSORT INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 N,RX(N)
      REAL*8 RA(N)
!
! N - Number of elements in the arrays
! RA - The REAL*8 array
! RX - The integer array
!
! 2.3 OUTPUT Variables: None
!
! RA - sorted REAL*8 array
! RX - sorted integer array
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
      INTEGER*2 IR,I,J,K,L,BFRX
      REAL*8 BFRA
!
! BFRA - Temporary real value
! BFRX - Temporary integer value
! I,J,L,IR - Array indices
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  HSORT PROGRAM STRUCTURE
!
      L=N/2+1
      IR=N
!
10    CONTINUE
          IF(L.GT.1) THEN
              L=L-1
              BFRA=RA(L)
              BFRX=RX(L)
          ELSE
              BFRA=RA(IR)
              BFRX=RX(IR)
              RA(IR)=RA(1)
              RX(IR)=RX(1)
              IR=IR-1
              IF(IR.LE.1) THEN
                  RA(1)=BFRA
                  RX(1)=BFRX
                  RETURN
              ENDIF
          ENDIF
          I = L
          J = L + L
20        IF(J.LE.IR) THEN
              IF(J.LT.IR) then
                  If (RA(J).LT.RA(J+1)) J=J+1
              ENDIF
              IF(BFRA.LT.RA(J)) THEN
                  RA(I)=RA(J)
                  RX(I)=RX(J)
                  I=J
                  J=J+J
              ELSE
                  J=IR+1
              ENDIF
              GO TO 20
          ENDIF
          RA(I)=BFRA
          RX(I)=BFRX
      GO TO 10
      END
