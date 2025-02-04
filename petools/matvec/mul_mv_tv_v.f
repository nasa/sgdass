#ifdef GEN_PREFIX
#define FUNC_MUL_MV_TV_V   GEN_MUL_MV_TV_V 
#else
#define FUNC_MUL_MV_TV_V   OPT_MUL_MV_TV_V 
#endif
#include <mk5_preprocessor_directives.inc>
      SUBROUTINE FUNC_MUL_MV_TV_V ( M1, N1, MAT, M2, VECI, M3, VECO, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  MUL_MV_TV_V  multiplies matrix by vector:              *
! *   VECO = MAT * VECI                                                  *
! *                                                                      *
! *  ###  12-Dec-96   MUL_MV_TV_V  v1.0  (c)  L. Petrov   12-Dec-96 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  M1, N1, M2, M3, IUER
      REAL*8     MAT(M1,N1), VECI(M2), VECO(M3)
      CHARACTER  STR*80
!
      REAL*8     S
      INTEGER*4  J1, J2
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      IF ( M1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. M2  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MV_TV_V', STR )
           RETURN
      END IF
!
      IF ( N1 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR = 'N1 .NE. M3  N1='
           CALL INCH ( N1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MV_TV_V', STR )
           RETURN
      END IF
!
#ifdef GEN_PREFIX
      DO 410 J1=1,N1
         S=0.0D0
         DO 420 J2=1,M1
            S = S + MAT(J2,J1)*VECI(J2)
 420     CONTINUE
         VECO(J1) = S
 410  CONTINUE
#endif
!
#ifdef HPUX
      CALL BLAS_$DGEMM ( 'T', 'N', N1, 1, M1, 1.0D0, MAT, M1, VECI, &
     &                   M2, 0.0D0, VECO, M3 )
#else
      CALL DGEMV ( 'T', M1, N1, 1.D0, MAT, M1, VECI, 1, 0.0D0, VECO, 1 )
#endif
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MUL_MV_TV_V  #!#
