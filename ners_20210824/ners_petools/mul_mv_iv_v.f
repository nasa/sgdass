      SUBROUTINE MUL_MV_IV_V ( M1, N1, MAT, M2, VECI, M3, VECO, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  MUL_MV_IV_V  multiplies matrix by vector:              *
! *   VECO = MAT * VECI                                                  *
! *                                                                      *
! * ###  10-DEC-96  MUL_MV_IV_V  v3.0  (c)  L. Petrov   22-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i'
      INTEGER*4  M1, N1, M2, M3, IUER
      REAL*8     MAT(M1,N1), VECI(M2), VECO(M3)
      CHARACTER  STR*80
!
      REAL*8     S
      INTEGER*4  J1, J2, J3, J4
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      IF ( N1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='N1 .NE. M2  N1='
           CALL INCH ( N1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MV_IV_V', STR )
           RETURN
      END IF
!
      IF ( M1 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR = 'M1 .NE. M3  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MV_IV_V', STR )
           RETURN
      END IF
!
#ifdef GEN_PREFIX
      DO 410 J1=1,M1
         S=0.0D0
         DO 420 J2=1,N1
            S = S + MAT(J1,J2)*VECI(J2)
 420     CONTINUE
         VECO(J1) = S
 410  CONTINUE
      IUER = 0
      RETURN
#endif
!
#ifdef HPUX
      CALL BLAS_$DGEMM ( 'N', 'N', M1, 1, N1, 1.0D0, MAT, M1, VECI, &
     &                   M2, 0.0D0, VECO, M3 )
#else
      IF ( M1 .LE. DB__MUL_MM_II_I  .OR.  N1 .LE. DB__MUL_MM_II_I ) THEN
           DO 430 J3=1,M1
              S=0.0D0
              DO 440 J4=1,N1
                 S = S + MAT(J3,J4)*VECI(J4)
 440          CONTINUE
              VECO(J3) = S
 430       CONTINUE 
         ELSE
           CALL DGEMV ( 'N', M1, N1, 1.D0, MAT, M1, VECI, 1, 0.0D0, VECO, 1 )
      END IF
#endif
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MUL_MV_IV_V  #!#
