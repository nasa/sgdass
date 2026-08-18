      SUBROUTINE NORMALIZE_VECTOR ( VEC, NLEN )
      IMPLICIT NONE                         !Added by IMP/jwr
!
!-----The following type specification statements added automatically
!     by imp/jwr July 2002
!
      INTEGER*2 i
!-----END of imp added lines.
!
!
! --- normalize a vector
!
      REAL*8     VEC(*), wt
      INTEGER*2  NLEN
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
      END  !#!  NORMALIZE_VECTOR  #!#
