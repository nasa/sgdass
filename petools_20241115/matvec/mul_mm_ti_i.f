#ifdef GEN_PREFIX
#define FUNC_MUL_MM_TI_I  GEN_MUL_MM_TI_I
#else
#define FUNC_MUL_MM_TI_I  OPT_MUL_MM_TI_I
#endif
#include <mk5_preprocessor_directives.inc>
      SUBROUTINE FUNC_MUL_MM_TI_I ( M1, N1, MAT1, M2, N2, MAT2, M3, N3, &
     &                              MATO, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  MUL_MM_TI_I  multiplies matrices:                      *
! *   MATO = MAT1 * MAT2                                                 *
! *                                                                      *
! *  ###  12-Dec-96     MUL_MM_TI_I  v2.3 (c) L. Petrov 12-NOV-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i' ! Definnition of DB__MUL_MM_TI_I
      INTEGER*4  M1, N1, M2, N2, M3, N3, IUER
      REAL*8     MAT1(M1,N1), MAT2(M2,N2), MATO(M3,N3)
      CHARACTER  STR*80
      INTEGER*4  J1, J2, J3
!
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      IF ( M1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. M2  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_TI_I', STR )
           RETURN
      END IF
!
      IF ( N2 .NE. N3 ) THEN
           CALL CLRCH ( STR )
           STR ='N2 .NE. N3  N2='
           CALL INCH ( N2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N3='
           CALL INCH ( N3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_TI_I', STR )
           RETURN
      END IF
!
      IF ( M3 .NE. N1 ) THEN
           CALL CLRCH ( STR )
           STR ='M3 .NE. N1  M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N1='
           CALL INCH ( N1, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 16, IUER, 'MUL_MM_TI_I', STR )
           RETURN
      END IF
#ifdef GEN_PREFIX
!
! --- Genereic version
!
      DO 410 J1=1,N3
         DO 420 J2=1,M3
            MATO(J2,J1) = 0.0D0
            DO 430 J3=1,M1
               MATO(J2,J1) = MATO(J2,J1) + MAT1(J3,J2)*MAT2(J3,J1)
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
      CALL ERR_LOG ( 0, IUER )
      RETURN
#endif
!
! --- First try special cases for small dimensions
!
      IF ( M1 .EQ. 2  .AND.  N1 .EQ. 2  .AND.  N2 .EQ. 2 ) THEN
!
! -------- 2x2 mult 2x2
!
           MATO(1,1) = MAT1(1,1)*MAT2(1,1) + MAT1(2,1)*MAT2(2,1)
           MATO(2,1) = MAT1(1,2)*MAT2(1,1) + MAT1(2,2)*MAT2(2,1)
           MATO(1,2) = MAT1(1,1)*MAT2(1,2) + MAT1(2,1)*MAT2(2,2)
           MATO(2,2) = MAT1(1,2)*MAT2(1,2) + MAT1(2,2)*MAT2(2,2)
           CALL ERR_LOG ( 0, IUER )
           RETURN
         ELSE IF ( M1 .EQ. 3  .AND.  N1 .EQ. 3  .AND.  N2 .EQ. 3 ) THEN
!
! -------- 3x3 mult 3x3
!
           MATO(1,1) = MAT1(1,1)*MAT2(1,1) + MAT1(2,1)*MAT2(2,1) + &
     &                                       MAT1(3,1)*MAT2(3,1)
           MATO(2,1) = MAT1(1,2)*MAT2(1,1) + MAT1(2,2)*MAT2(2,1) + &
     &                                       MAT1(3,2)*MAT2(3,1)
           MATO(3,1) = MAT1(1,3)*MAT2(1,1) + MAT1(2,3)*MAT2(2,1) + &
     &                                       MAT1(3,3)*MAT2(3,1)
!
           MATO(1,2) = MAT1(1,1)*MAT2(1,2) + MAT1(2,1)*MAT2(2,2) + &
     &                                       MAT1(3,1)*MAT2(3,2)
           MATO(2,2) = MAT1(1,2)*MAT2(1,2) + MAT1(2,2)*MAT2(2,2) + &
     &                                       MAT1(3,2)*MAT2(3,2)
           MATO(3,2) = MAT1(1,3)*MAT2(1,2) + MAT1(2,3)*MAT2(2,2) + &
     &                                       MAT1(3,3)*MAT2(3,2)
!
           MATO(1,3) = MAT1(1,1)*MAT2(1,3) + MAT1(2,1)*MAT2(2,3) + &
     &                                       MAT1(3,1)*MAT2(3,3)
           MATO(2,3) = MAT1(1,2)*MAT2(1,3) + MAT1(2,2)*MAT2(2,3) + &
     &                                       MAT1(3,2)*MAT2(3,3)
           MATO(3,3) = MAT1(1,3)*MAT2(1,3) + MAT1(2,3)*MAT2(2,3) + &
     &                                       MAT1(3,3)*MAT2(3,3)
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
         ELSE IF ( M1 .EQ. 4  .AND.  N1 .EQ. 4  .AND. N2 .EQ. 4 ) THEN
!
! -------- 4x4 mult 4x4
!
           MATO(1,1) = MAT1(1,1)*MAT2(1,1) + MAT1(2,1)*MAT2(2,1) + &
     &                 MAT1(3,1)*MAT2(3,1) + MAT1(4,1)*MAT2(4,1)
           MATO(2,1) = MAT1(1,2)*MAT2(1,1) + MAT1(2,2)*MAT2(2,1) + &
     &                 MAT1(3,2)*MAT2(3,1) + MAT1(4,2)*MAT2(4,1)
           MATO(3,1) = MAT1(1,3)*MAT2(1,1) + MAT1(2,3)*MAT2(2,1) + &
     &                 MAT1(3,3)*MAT2(3,1) + MAT1(4,3)*MAT2(4,1)
           MATO(4,1) = MAT1(1,4)*MAT2(1,1) + MAT1(2,4)*MAT2(2,1) + &
     &                 MAT1(3,4)*MAT2(3,1) + MAT1(4,4)*MAT2(4,1)
!
           MATO(1,2) = MAT1(1,1)*MAT2(1,2) + MAT1(2,1)*MAT2(2,2) + &
     &                 MAT1(3,1)*MAT2(3,2) + MAT1(4,1)*MAT2(4,2)
           MATO(2,2) = MAT1(1,2)*MAT2(1,2) + MAT1(2,2)*MAT2(2,2) + &
     &                 MAT1(3,2)*MAT2(3,2) + MAT1(4,2)*MAT2(4,2)
           MATO(3,2) = MAT1(1,3)*MAT2(1,2) + MAT1(2,3)*MAT2(2,2) + &
     &                 MAT1(3,3)*MAT2(3,2) + MAT1(4,3)*MAT2(4,2)
           MATO(4,2) = MAT1(1,4)*MAT2(1,2) + MAT1(2,4)*MAT2(2,2) + &
     &                 MAT1(3,4)*MAT2(3,2) + MAT1(4,4)*MAT2(4,2)
!
           MATO(1,3) = MAT1(1,1)*MAT2(1,3) + MAT1(2,1)*MAT2(2,3) + &
     &                 MAT1(3,1)*MAT2(3,3) + MAT1(4,1)*MAT2(4,3)
           MATO(2,3) = MAT1(1,2)*MAT2(1,3) + MAT1(2,2)*MAT2(2,3) + &
     &                 MAT1(3,2)*MAT2(3,3) + MAT1(4,2)*MAT2(4,3)
           MATO(3,3) = MAT1(1,3)*MAT2(1,3) + MAT1(2,3)*MAT2(2,3) + &
     &                 MAT1(3,3)*MAT2(3,3) + MAT1(4,3)*MAT2(4,3)
           MATO(4,3) = MAT1(1,4)*MAT2(1,3) + MAT1(2,4)*MAT2(2,3) + &
     &                 MAT1(3,4)*MAT2(3,3) + MAT1(4,4)*MAT2(4,3)
!
           MATO(1,4) = MAT1(1,1)*MAT2(1,4) + MAT1(2,1)*MAT2(2,4) + &
     &                 MAT1(3,1)*MAT2(3,4) + MAT1(4,1)*MAT2(4,4)
           MATO(2,4) = MAT1(1,2)*MAT2(1,4) + MAT1(2,2)*MAT2(2,4) + &
     &                 MAT1(3,2)*MAT2(3,4) + MAT1(4,2)*MAT2(4,4)
           MATO(3,4) = MAT1(1,3)*MAT2(1,4) + MAT1(2,3)*MAT2(2,4) + &
     &                 MAT1(3,3)*MAT2(3,4) + MAT1(4,3)*MAT2(4,4)
           MATO(4,4) = MAT1(1,4)*MAT2(1,4) + MAT1(2,4)*MAT2(2,4) + &
     &                 MAT1(3,4)*MAT2(3,4) + MAT1(4,4)*MAT2(4,4)
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
      IF ( N1 .LT. DB__MUL_MM_TI_I ) THEN
!
           DO 510 J1=1,N3
              DO 520 J2=1,M3
                 MATO(J2,J1) = 0.0D0
                 DO 530 J3=1,M1
                    MATO(J2,J1) = MATO(J2,J1) + MAT1(J3,J2)*MAT2(J3,J1)
 530             CONTINUE 
 520          CONTINUE 
 510       CONTINUE 
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
#ifdef HPUX
      IF ( N2 .LE. DB__MUL_MM_TI_I ) THEN
           CALL BLAS_$DGEMM ( 'T', 'N', N1, N2, M1, 1.0D0, MAT1, M1, &
     &                         MAT2, M2, 0.0D0, MATO, M3 )
         ELSE
           CALL DGEMM ( 'T', 'N', N1, N2, M1, 1.0D0, MAT1, M1, &
     &                   MAT2, M2, 0.0D0, MATO, M3 )
      END IF
#else
      CALL DGEMM ( 'T', 'N', N1, N2, M1, 1.0D0, MAT1, M1, MAT2, M2, 0.0D0, &
     &              MATO, M3 )
#endif
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MUL_MM_TI_I  #!#
