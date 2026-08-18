      SUBROUTINE DPPCO ( A, N, RCOND, Z )
      IMPLICIT   NONE
      INCLUDE   'matvec.i' ! Definnition of DB__INVMAT_MIN
      INTEGER*2  N
      REAL*8     A(*), RCOND, Z(*)
#if defined (SUN) || defined (GNU)
      INTEGER*4, EXTERNAL :: INT4
#endif
      INTEGER*4  IUER
!
      IUER = -1
!%
!%      IF ( INT4(N) .LE. DB__INVMAT_MIN ) THEN
!%           CALL DPPCO_VEC  ( INT4(N), A, COND__MAX, RCOND, Z, IUER )
!%         ELSE
!%           CALL DPPCO_BLAS ( INT4(N), A, COND__MAX, RCOND, Z, IUER )
!%      END IF
!%
       CALL DPPCO_BLAS ( INT4(N), A, COND__MAX, RCOND, Z, IUER )
!
      RETURN
      END  !#!  DPPCO  #!#
