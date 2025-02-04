      SUBROUTINE MUL_MV_IV_V ( M1, N1, MAT, M2, VECI, M3, VECO, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  MUL_MV_IV_V  multiplies matrix by vector:              *
! *   VECO = MAT * VECI                                                  *
! *                                                                      *
! *  ###  10-Dec-96   MUL_MV_IV_V  v2.0  (c)  L. Petrov   24-Dec-96 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  M1, N1, M2, M3, IUER
      REAL*8     MAT(M1,N1), VECI(M2), VECO(M3)
      CHARACTER  STR*80
!
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
      CALL BLAS_$DGEMM ( 'N', 'N', M1, 1, N1, 1.0D0, MAT, M1, VECI, &
     &                   M2, 0.0D0, VECO, M3 )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MUL_MV_IV_V  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MUL_MV_SV_V ( M1, MAT, M2, VECI, M3, VECO, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  MUL_MV_SV_V  multiplies matrix by vector:              *
! *   VECO = MAT * VECI                                                  *
! *                                                                      *
! *  ###  10-Dec-96   MUL_MV_IV_V  v2.0  (c)  L. Petrov   23-Dec-96 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i'
      INTEGER*4  M1, M2, M3, IUER
      REAL*8     MAT(*), VECI(M2), VECO(M3)
      CHARACTER  STR*80
      INTEGER*4   M, INM, NT0
      PARAMETER ( M=MAX__DIM, INM=1, NT0=1 )
      REAL*8     VECN(M), VEC_$DDOT
      INTEGER*4  J1, J3, KI, KB, IN(M), IN2(M)
      REAL*8,    EXTERNAL :: DP_VV_V
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
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
      IF ( M1 .LE. NT0 ) THEN
           KB=1
           DO 510 J1=1,M1
              KI=KB
              VECO(J1) = VEC_$DDOT ( MAT(KI), VECI, J1 )
              KI=(KB+J1-1)
              IF ( J1 .LT. M1 ) THEN
                 DO 530 J3=J1,M1-1
                    KI=KI+J3
                    VECO(J1) = VECO(J1) + MAT(KI)*VECI(J3+1)
 530             CONTINUE
              END IF
              KB=KB+J1
 510       CONTINUE
           RETURN
      END IF
!      IF ( M1 .GT. INM ) THEN
!           CALL VEC_$IINIT  ( IN2(INM+1), M1-INM, 1)
!           CALL VEC_$IREC1N ( IN2(INM+1), M1-INM, IN(INM) )
!      END IF
       IN2(1)=1
       CALL VEC_$IINIT  ( IN,  M1, 1   )
       CALL VEC_$IREC1N ( IN,  M1, IN2 )
       CALL VEC_$IREC1N ( IN2, M1, IN  )
!CCCCC
      KB=1
      DO 410 J1=1,M1
!
         CALL VEC_$DCOPY ( MAT(KB), VECN, J1 )
         IF ( J1.LT.M1 ) THEN
              CALL VEC_$DGATHER ( MAT(J1), IN(J1+1), M1-J1, VECN(J1+1) )
         END IF
         VECO(J1) = DP_VV_V   ( M1, VECN, VECI )
         KB=KB+J1
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MUL_MV_SV_V  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MUL_MV_TV_V ( M1, N1, MAT, M2, VECI, M3, VECO, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  MUL_MV_TV_V  multiplies matrix by vector:              *
! *   VECO = MAT * VECI                                                  *
! *                                                                      *
! *  ###  12-Dec-96   MUL_MV_TV_V  v2.0  (c)  L. Petrov   24-Dec-96 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  M1, N1, M2, M3, IUER
      REAL*8     MAT(M1,N1), VECI(M2), VECO(M3)
      CHARACTER  STR*80
!
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
      CALL BLAS_$DGEMM ( 'T', 'N', N1, 1, M1, 1.0D0, MAT, M1, VECI, &
     &                   M2, 0.0D0, VECO, M3 )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MUL_MV_TV_V  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MAT_SUMTRA ( L, MATI, MATO )
! ************************************************************************
! *                                                                      *
! *   Subroutime  MAT_SUMTRA  calculats summa MATI + MATI(T), where      *
! *   MATI is square martix in rectangular representation. MATO is       *
! *   square symmetric matrix in upper-diagonal representation.          *
! *                                                                      *
! *  ###  19-Dec-96   MAT_SUMTRA    V2.0 (c)  L. Petrov  18-DEC-96  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  L
      REAL*8     MATI(L,L), MATO(*)
!
      INTEGER*4  J1, J2, LC, I, J, LOC
      LOC(I,J) = min(I,J) +(max(I,J)*(max(I,J)-1))/2
!
      LC=0
      DO 410 J1=1,L
         DO 420 J2=1,J1
            LC=LC+1
            MATO(LC)=MATI(J1,J2) + MATI(J2,J1)
 420     CONTINUE
 410  CONTINUE
!
      RETURN
      END  !#!  MAT_SUMTRA  #!#
