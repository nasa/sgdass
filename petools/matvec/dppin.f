      SUBROUTINE DPPIN ( A, N )
! ************************************************************************
! *                                                                      *
! *   Calculate inverse from a Cholesky factorization of a square        *
! *   symmetrical matrix in upper triangular representation.             *
! *   This is a double precision version of the LINPACK SPPDI routine    *
! *   with direct calls to BLAS and I*4 indexing.  Only the inversion    *
! *   part of the original LINPACK routine is here.                      *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i' ! Definnition of DB__INVMAT, MAX__DIM
      INTEGER*2  N
#if defined (SUN) || defined (GNU)
      INTEGER*4, EXTERNAL :: INT4
#endif
      REAL*8     A(*)
!    
#ifdef HPUX
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = JINT ( FLOATI(INT2_ARG) )
!
      IF ( INT4(N) .LE. DB__INVMAT_MIN ) THEN
           CALL DPPIN_VEC  ( INT4(N), A )
         ELSE
           CALL DPPIN_BLAS ( INT4(N), A )
      END IF
#else
      CALL DPPIN_BLAS ( INT4(N), A )
#endif
!
      RETURN
      END  !#!  DPPIN  #!#
