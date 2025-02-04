      SUBROUTINE MUL_MV_SV_V ( M1, MAT, M2, VECI, M3, VECO, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  MUL_MV_SV_V  multiplies matrix by vector:              *
! *   VECO = MAT * VECI                                                  *
! *                                                                      *
! *  ###  10-DEC-96   MUL_MV_SV_V  v2.0  (c) L. Petrov  24-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
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
      DO 410 J1=1,M1
         S=0.0D0
         DO 420 J2=1,M1
            LC = LOC(J1,J2)
            S = S + MAT(LC)*VECI(J2)
 420     CONTINUE
         VECO(J1) = S
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MUL_MV_SV_V  #!#
