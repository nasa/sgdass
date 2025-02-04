#ifdef GEN_PREFIX
#define FUNC_MUL_MM_IT_I  GEN_MUL_MM_IT_I
#else
#define FUNC_MUL_MM_IT_I  OPT_MUL_MM_IT_I
#endif
#include <mk5_preprocessor_directives.inc>
      SUBROUTINE FUNC_MUL_MM_IT_I ( M1, N1, MAT1, M2, N2, MAT2, M3, N3, &
     &                              MATO, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  MUL_MM_IT_I  multiplies matrices:                      *
! *   MATO = MAT1 * MAT2                                                 *
! *                                                                      *
! *  ###  11-Dec-96   MUL_MM_IT_I    v1.0 (c) L. Petrov 06-APR-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i' ! Definnition of DB__MUL_MM_IT_I
      INTEGER*4  M1, N1, M2, N2, M3, N3, IUER
      REAL*8     MAT1(M1,N1), MAT2(M2,N2), MATO(M3, N3)
      CHARACTER  STR*80
      INTEGER*4  J1, J2, J3
!
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      IF ( N1 .NE. N2 ) THEN
           CALL CLRCH ( STR )
           STR = 'N1 .NE. N2  N1='
           CALL INCH ( N1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N2='
           CALL INCH ( N2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_IT_I', STR )
           RETURN
      END IF
!
      IF ( M1 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. M3  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_IT_I', STR )
           RETURN
      END IF
!
      IF ( M2 .NE. N3 ) THEN
           CALL CLRCH ( STR )
           STR ='M2 .NE. N3  M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N3='
           CALL INCH ( N3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 16, IUER, 'MUL_MM_IT_I', STR )
           RETURN
      END IF
#ifdef GEN_PREFIX
!
! --- Genereic version
!
      DO 410 J1=1,N3
         DO 420 J2=1,M3
            MATO(J2,J1) = 0.0D0
            DO 430 J3=1,N1
               MATO(J2,J1) = MATO(J2,J1) + MAT1(J2,J3)*MAT2(J1,J3)
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
      CALL ERR_LOG ( 0, IUER )
      RETURN
#endif
      IF ( N1 .LT. DB__MUL_MM_IT_I ) THEN
!
           DO 510 J1=1,N3
              DO 520 J2=1,M3
                 MATO(J2,J1) = 0.0D0
                 DO 530 J3=1,N1
                    MATO(J2,J1) = MATO(J2,J1) + MAT1(J2,J3)*MAT2(J1,J3)
 530             CONTINUE 
 520          CONTINUE 
 510       CONTINUE 
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
#ifdef HPUX
!
! --- HP case when we have to different optimized library and choose which
! --- to use
!
      IF ( M2 .LE. DB__MUL_MM_IT_I ) THEN
           CALL BLAS_$DGEMM ( 'N', 'T', M1, M2, N2, 1.0D0, MAT1, M1, &
     &                         MAT2, M2, 0.0D0, MATO, M3 )
         ELSE
           CALL DGEMM ( 'N', 'T', M1, M2, N2, 1.0D0, MAT1, M1, &
     &                   MAT2, M2, 0.0D0, MATO, M3 )
      END IF
#else
      CALL DGEMM ( 'N', 'T', M1, M2, N2, 1.0D0, MAT1, M1, MAT2, M2, 0.0D0, &
     &              MATO, M3 )
#endif
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MUL_MM_IT_I  #!#
