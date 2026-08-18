      SUBROUTINE DPPCO_SOLVE ( A, N, SCAL, B, RCOND, Z )
      IMPLICIT   NONE
      INCLUDE   'matvec.i' ! Definnition of DB__INVMAT_MIN
      INTEGER*2  N
      REAL*8     A(*), SCAL(*), B(*), RCOND, Z(*)
#if defined (SUN) || defined (GNU)
      INTEGER*4, EXTERNAL :: INT4
#endif
      INTEGER*4  IUER
!
      IUER = -1
      IF ( INT4(N) .LE. DB__INVMAT_MIN ) THEN
           CALL DPPCO_SOLVE_VEC  ( INT4(N), A, RCOND, Z, SCAL, B, IUER )
         ELSE
           CALL DPPCO_SOLVE_BLAS ( INT4(N), A, RCOND, Z, SCAL, B, IUER )
      END IF
!
      RETURN
      END  !#!  DPPCO_SOLVE  #!#
