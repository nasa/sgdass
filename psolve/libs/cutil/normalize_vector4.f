      SUBROUTINE NORMALIZE_VECTOR4 ( VEC, NLEN )
      IMPLICIT NONE                         !Added by IMP/jwr
      INTEGER*4 I
!
! --- normalize a vector
!
      REAL*8     VEC(*), wt
      INTEGER*4  NLEN
!
      WT=0.0
      DO I=1,NLEN
         WT = WT + VEC(I)*VEC(I)
      END DO
      WT=SQRT(WT)
      IF ( WT .GT. 0.0 ) THEN
           DO I=1,NLEN
              VEC(I)=VEC(I)/WT
           END DO
      END IF
!
      RETURN
      END  !#!  NORMALIZE_VECTO4R  #!#
