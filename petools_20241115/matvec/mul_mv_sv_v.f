#ifdef GEN_PREFIX
#define FUNC_MUL_MV_SV_V   GEN_MUL_MV_SV_V 
#else
#define FUNC_MUL_MV_SV_V   OPT_MUL_MV_SV_V 
#endif
#include <mk5_preprocessor_directives.inc>
      SUBROUTINE FUNC_MUL_MV_SV_V ( M1, MAT, M2, VECI, M3, VECO, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  MUL_MV_SV_V  multiplies matrix by vector:              *
! *   VECO = MAT * VECI                                                  *
! *                                                                      *
! *  ###  10-DEC-96   MUL_MV_SV_V  v2.0  (c) L. Petrov  24-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i'
      INTEGER*4  M1, M2, M3, IUER
      REAL*8     MAT(*), VECI(M2), VECO(M3)
      CHARACTER  STR*80
!
      REAL*8     S
      INTEGER*4  J1, J2, K, LC, I, J, LOC
      REAL*8,    EXTERNAL :: DP_VV_V
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
      LOC(I,J)=min(I,J) +(max(I,J)*(max(I,J)-1))/2
!
      IF ( M1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. M2  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MV_SV_V', STR )
           RETURN
      END IF
!
      IF ( M1 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR = 'M1 .NE. M3  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MV_SV_V', STR )
           RETURN
      END IF
!
#ifdef GEN_PREFIX
      DO 410 J1=1,M1
         S=0.0D0
         DO 420 J2=1,M1
            LC = LOC(J1,J2)
            S = S + MAT(LC)*VECI(J2)
 420     CONTINUE
         VECO(J1) = S
 410  CONTINUE
#else
      IF ( M1 .EQ. 2 ) THEN
           VECO(1) = MAT(1)*VECI(1) + MAT(2)*VECI(2)
           VECO(2) = MAT(2)*VECI(1) + MAT(3)*VECI(2)
         ELSE IF ( M1 .EQ. 3 ) THEN
           VECO(1) = MAT(1)*VECI(1) + MAT(2)*VECI(2) + MAT(4)*VECI(3) 
           VECO(2) = MAT(2)*VECI(1) + MAT(3)*VECI(2) + MAT(5)*VECI(3) 
           VECO(3) = MAT(4)*VECI(1) + MAT(5)*VECI(2) + MAT(6)*VECI(3) 
         ELSE IF ( M1 .EQ. 4 ) THEN
           VECO(1) = MAT(1)*VECI(1) + MAT(2)*VECI(2) + MAT(4)*VECI(3) + &
     &               MAT(7)*VECI(4)
           VECO(2) = MAT(2)*VECI(1) + MAT(3)*VECI(2) + MAT(5)*VECI(3) + &
     &               MAT(8)*VECI(4)
           VECO(3) = MAT(4)*VECI(1) + MAT(5)*VECI(2) + MAT(6)*VECI(3) + &
     &               MAT(9)*VECI(4)
           VECO(4) = MAT(7)*VECI(1) + MAT(8)*VECI(2) + MAT(9)*VECI(3) + &
     &               MAT(10)*VECI(4)
         ELSE IF ( M1 .EQ. 5 ) THEN
           VECO(1) = MAT(1)*VECI(1)  + MAT(2)*VECI(2)  + MAT(4)*VECI(3)  + &
     &               MAT(7)*VECI(4)  + MAT(11)*VECI(5) 
           VECO(2) = MAT(2)*VECI(1)  + MAT(3)*VECI(2)  + MAT(5)*VECI(3)  + &
     &               MAT(8)*VECI(4)  + MAT(12)*VECI(5) 
           VECO(3) = MAT(4)*VECI(1)  + MAT(5)*VECI(2)  + MAT(6)*VECI(3)  + &
     &               MAT(9)*VECI(4)  + MAT(13)*VECI(5) 
           VECO(4) = MAT(7)*VECI(1)  + MAT(8)*VECI(2)  + MAT(9)*VECI(3)  + &
     &               MAT(10)*VECI(4) + MAT(14)*VECI(5)
           VECO(5) = MAT(11)*VECI(1) + MAT(12)*VECI(2) + MAT(13)*VECI(3) + &
     &               MAT(14)*VECI(4) + MAT(15)*VECI(5)
         ELSE IF ( M1 .EQ. 6 ) THEN
           VECO(1) = MAT(1)*VECI(1)  + MAT(2)*VECI(2)  + MAT(4)*VECI(3)  + &
     &               MAT(7)*VECI(4)  + MAT(11)*VECI(5) + MAT(16)*VECI(6) 
           VECO(2) = MAT(2)*VECI(1)  + MAT(3)*VECI(2)  + MAT(5)*VECI(3)  + &
     &               MAT(8)*VECI(4)  + MAT(12)*VECI(5) + MAT(17)*VECI(6) 
           VECO(3) = MAT(4)*VECI(1)  + MAT(5)*VECI(2)  + MAT(6)*VECI(3)  + &
     &               MAT(9)*VECI(4)  + MAT(13)*VECI(5) + MAT(18)*VECI(6) 
           VECO(4) = MAT(7)*VECI(1)  + MAT(8)*VECI(2)  + MAT(9)*VECI(3)  + &
     &               MAT(10)*VECI(4) + MAT(14)*VECI(5) + MAT(19)*VECI(6) 
           VECO(5) = MAT(11)*VECI(1) + MAT(12)*VECI(2) + MAT(13)*VECI(3) + &
     &               MAT(14)*VECI(4) + MAT(15)*VECI(5) + MAT(20)*VECI(6) 
           VECO(6) = MAT(16)*VECI(1) + MAT(17)*VECI(2) + MAT(18)*VECI(3) + &
     &               MAT(19)*VECI(4) + MAT(20)*VECI(5) + MAT(21)*VECI(6) 
         ELSE 
           IF ( M1 .LT. DB1__MUL_MV_SV_V ) THEN
!
! ------------- Small dimensions
!
                VECO(1) = MAT(1)*VECI(1)
                K = 2
                DO 410 J1=2,M1
                   VECO(J1) = 0.0D0
                   DO 420 J2=1,J1-1
                      VECO(J2) = VECO(J2) + MAT(K)*VECI(J1)
                      VECO(J1) = VECO(J1) + MAT(K)*VECI(J2)
                      K = K+1
 420               CONTINUE
                   VECO(J1) = VECO(J1) + MAT(K)*VECI(J1)
                   K = K+1
 410            CONTINUE
             ELSE
!
! ------------- Large dimenstions: blas is more efficient
!
                CALL DSPMV ( 'U', M1, 1.0D0, MAT, VECI, 1, 0.0D0, VECO, 1 )
           END IF
      END IF          
#endif
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MUL_MV_SV_V  #!#
