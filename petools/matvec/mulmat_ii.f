      SUBROUTINE MUL_MM_II_I ( M1, N1, MAT1, M2, N2, MAT2, M3, N3,
     #                         MATO, IUER )
C ************************************************************************
C *                                                                      *
C *   Subroutine  MUL_MM_II_I  multiplies matrices:                      *
C *   MATO = MAT1 * MAT2                                                 *
C *                                                                      *
C *  ###  10-Dec-96   MUL_MM_II_I  v2.1  (c) L. Petrov  05-JUL-2001 ###  *
C *                                                                      *
C ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i' ! Definnition of DB__MUL_MM_II_I
      INTEGER*4  M1, N1, M2, N2, M3, N3, IUER
      REAL*8     MAT1(M1,N1), MAT2(M2,N2), MATO(M3, N3)
      CHARACTER  STR*80
C
      INTEGER*4  I_LEN, ILEN
C
      IF ( N1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='N1 .NE. M2  N1='
           CALL INCH ( N1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_II_I', STR )
           RETURN
      END IF
C
      IF ( M1 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR = 'M1 .NE. M3  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_II_I', STR )
           RETURN
      END IF
C
      IF ( N2 .NE. N3 ) THEN
           CALL CLRCH ( STR )
           STR = 'N2 .NE. N3  N2='
           CALL INCH ( N2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N3='
           CALL INCH ( N3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 16, IUER, 'MUL_MM_II_I', STR )
           RETURN
      END IF
C
C --- First try special cases for small dimensions
C
      IF ( M1 .EQ. 2  .AND.  N1 .EQ. 2  .AND.  N2 .EQ. 2 ) THEN
C
C -------- 2x2 mult 2x2
C
           MATO(1,1) = MAT1(1,1)*MAT2(1,1) + MAT1(1,2)*MAT2(2,1)
           MATO(2,1) = MAT1(2,1)*MAT2(1,1) + MAT1(2,2)*MAT2(2,1)
           MATO(1,2) = MAT1(1,1)*MAT2(1,2) + MAT1(1,2)*MAT2(2,2)
           MATO(2,2) = MAT1(2,1)*MAT2(1,2) + MAT1(2,2)*MAT2(2,2)
           RETURN 
         ELSE IF ( M1 .EQ. 3  .AND.  N1 .EQ. 3  .AND.  N2 .EQ. 3 ) THEN
C
C -------- 3x3 mult 3x3
C
           MATO(1,1) = MAT1(1,1)*MAT2(1,1) + MAT1(1,2)*MAT2(2,1) + 
     #                                       MAT1(1,3)*MAT2(3,1) 
           MATO(2,1) = MAT1(2,1)*MAT2(1,1) + MAT1(2,2)*MAT2(2,1) + 
     #                                       MAT1(2,3)*MAT2(3,1) 
           MATO(3,1) = MAT1(3,1)*MAT2(1,1) + MAT1(3,2)*MAT2(2,1) + 
     #                                       MAT1(3,3)*MAT2(3,1) 
C
           MATO(1,2) = MAT1(1,1)*MAT2(1,2) + MAT1(1,2)*MAT2(2,2) + 
     #                                       MAT1(1,3)*MAT2(3,2) 
           MATO(2,2) = MAT1(2,1)*MAT2(1,2) + MAT1(2,2)*MAT2(2,2) + 
     #                                       MAT1(2,3)*MAT2(3,2) 
           MATO(3,2) = MAT1(3,1)*MAT2(1,2) + MAT1(3,2)*MAT2(2,2) + 
     #                                       MAT1(3,3)*MAT2(3,2) 
C
           MATO(1,3) = MAT1(1,1)*MAT2(1,3) + MAT1(1,2)*MAT2(2,3) + 
     #                                       MAT1(1,3)*MAT2(3,3) 
           MATO(2,3) = MAT1(2,1)*MAT2(1,3) + MAT1(2,2)*MAT2(2,3) + 
     #                                       MAT1(2,3)*MAT2(3,3) 
           MATO(3,3) = MAT1(3,1)*MAT2(1,3) + MAT1(3,2)*MAT2(2,3) + 
     #                                       MAT1(3,3)*MAT2(3,3) 
C
           RETURN 
         ELSE IF ( M1 .EQ. 4  .AND.  N1 .EQ. 4  .AND. N2 .EQ. 4 ) THEN
C
C -------- 4x4 mult 4x4
C
           MATO(1,1) = MAT1(1,1)*MAT2(1,1) + MAT1(1,2)*MAT2(2,1) + 
     #                 MAT1(1,3)*MAT2(3,1) + MAT1(1,4)*MAT2(4,1) 
           MATO(2,1) = MAT1(2,1)*MAT2(1,1) + MAT1(2,2)*MAT2(2,1) + 
     #                 MAT1(2,3)*MAT2(3,1) + MAT1(2,4)*MAT2(4,1) 
           MATO(3,1) = MAT1(3,1)*MAT2(1,1) + MAT1(3,2)*MAT2(2,1) + 
     #                 MAT1(3,3)*MAT2(3,1) + MAT1(3,4)*MAT2(4,1) 
           MATO(4,1) = MAT1(4,1)*MAT2(1,1) + MAT1(4,2)*MAT2(2,1) + 
     #                 MAT1(4,3)*MAT2(3,1) + MAT1(4,4)*MAT2(4,1) 
C
           MATO(1,2) = MAT1(1,1)*MAT2(1,2) + MAT1(1,2)*MAT2(2,2) + 
     #                 MAT1(1,3)*MAT2(3,2) + MAT1(1,4)*MAT2(4,2) 
           MATO(2,2) = MAT1(2,1)*MAT2(1,2) + MAT1(2,2)*MAT2(2,2) + 
     #                 MAT1(2,3)*MAT2(3,2) + MAT1(2,4)*MAT2(4,2) 
           MATO(3,2) = MAT1(3,1)*MAT2(1,2) + MAT1(3,2)*MAT2(2,2) + 
     #                 MAT1(3,3)*MAT2(3,2) + MAT1(3,4)*MAT2(4,2) 
           MATO(4,2) = MAT1(4,1)*MAT2(1,2) + MAT1(4,2)*MAT2(2,2) + 
     #                 MAT1(4,3)*MAT2(3,2) + MAT1(4,4)*MAT2(4,2) 
C
           MATO(1,3) = MAT1(1,1)*MAT2(1,3) + MAT1(1,2)*MAT2(2,3) + 
     #                 MAT1(1,3)*MAT2(3,3) + MAT1(1,4)*MAT2(4,3) 
           MATO(2,3) = MAT1(2,1)*MAT2(1,3) + MAT1(2,2)*MAT2(2,3) + 
     #                 MAT1(2,3)*MAT2(3,3) + MAT1(2,4)*MAT2(4,3) 
           MATO(3,3) = MAT1(3,1)*MAT2(1,3) + MAT1(3,2)*MAT2(2,3) + 
     #                 MAT1(3,3)*MAT2(3,3) + MAT1(3,4)*MAT2(4,3) 
           MATO(4,3) = MAT1(4,1)*MAT2(1,3) + MAT1(4,2)*MAT2(2,3) + 
     #                 MAT1(4,3)*MAT2(3,3) + MAT1(4,4)*MAT2(4,3) 
C
           MATO(1,4) = MAT1(1,1)*MAT2(1,4) + MAT1(1,2)*MAT2(2,4) + 
     #                 MAT1(1,3)*MAT2(3,4) + MAT1(1,4)*MAT2(4,4) 
           MATO(2,4) = MAT1(2,1)*MAT2(1,4) + MAT1(2,2)*MAT2(2,4) + 
     #                 MAT1(2,3)*MAT2(3,4) + MAT1(2,4)*MAT2(4,4) 
           MATO(3,4) = MAT1(3,1)*MAT2(1,4) + MAT1(3,2)*MAT2(2,4) + 
     #                 MAT1(3,3)*MAT2(3,4) + MAT1(3,4)*MAT2(4,4) 
           MATO(4,4) = MAT1(4,1)*MAT2(1,4) + MAT1(4,2)*MAT2(2,4) + 
     #                 MAT1(4,3)*MAT2(3,4) + MAT1(4,4)*MAT2(4,4) 
C
           RETURN 
      END IF
C
      IF ( N2 .LE. DB__MUL_MM_II_I ) THEN
C
C -------- All other case of small dimensions
C
           CALL BLAS_$DGEMM ( 'N', 'N', M1, N2, M2, 1.0D0, MAT1, M1,
     #                         MAT2, M2, 0.0D0, MATO, M3 )
         ELSE 
C
C -------- large dimension
C
           CALL DGEMM ( 'N', 'N', M1, N2, M2, 1.D0, MAT1, M1, MAT2, M2, 0.D0, 
     #                  MATO, M3 )
      END IF
      CALL ERR_LOG ( 0, IUER,, )
      RETURN
      END  !#!  MUL_MM_II_I  #!#
C
C ------------------------------------------------------------------------
C
      SUBROUTINE MUL_MM_IT_I ( M1, N1, MAT1, M2, N2, MAT2, M3, N3,
     #                         MATO, IUER )
C ************************************************************************
C *                                                                      *
C *   Subroutine  MUL_MM_IT_I  multiplies matrices:                      *
C *   MATO = MAT1 * MAT2                                                 *
C *                                                                      *
C *  ###  11-Dec-96   MUL_MM_IT_I    v1.0 (c) L. Petrov 06-APR-2001 ###  *
C *                                                                      *
C ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i' ! Definnition of DB__MUL_MM_IT_I
      INTEGER*4  M1, N1, M2, N2, M3, N3, IUER
      REAL*8     MAT1(M1,N1), MAT2(M2,N2), MATO(M3, N3)
      CHARACTER  STR*80
C
      INTEGER*4  I_LEN, ILEN
C
      IF ( N1 .NE. N2 ) THEN
           CALL CLRCH ( STR )
           STR = 'N1 .NE. N2  N1='
           CALL INCH ( N1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N2='
           CALL INCH ( N2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_IT_I', STR )
           RETURN
      END IF
C
      IF ( M1 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. M3  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_IT_I', STR )
           RETURN
      END IF
C
      IF ( M2 .NE. N3 ) THEN
           CALL CLRCH ( STR )
           STR ='M2 .NE. N3  M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N3='
           CALL INCH ( N3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 16, IUER, 'MUL_MM_IT_I', STR )
           RETURN
      END IF
C
      IF ( M2 .LE. DB__MUL_MM_IT_I ) THEN
           CALL BLAS_$DGEMM ( 'N', 'T', M1, M2, N2, 1.0D0, MAT1, M1,
     #                         MAT2, M2, 0.0D0, MATO, M3 )
         ELSE 
           CALL DGEMM ( 'N', 'T', M1, M2, N2, 1.0D0, MAT1, M1,
     #                   MAT2, M2, 0.0D0, MATO, M3 )
      END IF
C
      CALL ERR_LOG ( 0, IUER,, )
      RETURN
      END  !#!  MUL_MM_IT_I  #!#
C
C ------------------------------------------------------------------------
C
      SUBROUTINE MUL_MM_TI_I ( M1, N1, MAT1, M2, N2, MAT2, M3, N3,
     #                         MATO, IUER )
C ************************************************************************
C *                                                                      *
C *   Subroutine  MUL_MM_TI_I  multiplies matrices:                      *
C *   MATO = MAT1 * MAT2                                                 *
C *                                                                      *
C *  ###  12-Dec-96     MUL_MM_TI_I  v2.0 (c) L. Petrov 06-APR-2001 ###  *
C *                                                                      *
C ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i' ! Definnition of DB__MUL_MM_TI_I
      INTEGER*4  M1, N1, M2, N2, M3, N3, IUER
      REAL*8     MAT1(M1,N1), MAT2(M2,N2), MATO(M3,N3)
      CHARACTER  STR*80
C
      INTEGER*4  I_LEN, ILEN
C
      IF ( M1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. M2  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_TI_I', STR )
           RETURN
      END IF
C
      IF ( N2 .NE. N3 ) THEN
           CALL CLRCH ( STR )
           STR ='N2 .NE. N3  N2='
           CALL INCH ( N2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N3='
           CALL INCH ( N3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_TI_I', STR )
           RETURN
      END IF
C
      IF ( M3 .NE. N1 ) THEN
           CALL CLRCH ( STR )
           STR ='M3 .NE. N1  M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N1='
           CALL INCH ( N1, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 16, IUER, 'MUL_MM_TI_I', STR )
           RETURN
      END IF
C
      IF ( N2 .LE. DB__MUL_MM_TI_I ) THEN
           CALL BLAS_$DGEMM ( 'T', 'N', N1, N2, M1, 1.0D0, MAT1, M1,
     #                         MAT2, M2, 0.0D0, MATO, M3 )
         ELSE 
           CALL DGEMM ( 'T', 'N', N1, N2, M1, 1.0D0, MAT1, M1,
     #                   MAT2, M2, 0.0D0, MATO, M3 )
      END IF
C
      CALL ERR_LOG ( 0, IUER,, )
      RETURN
      END  !#!  MUL_MM_TI_I  #!#
C
C ------------------------------------------------------------------------
C
      SUBROUTINE MUL_MM_TT_I ( M1, N1, MAT1, M2, N2, MAT2, M3, N3,
     #                         MATO, IUER )
C ************************************************************************
C *                                                                      *
C *   Subroutine  MUL_MM_TT_I  multiplies matrices:                      *
C *   MATO = MAT1 * MAT2                                                 *
C *                                                                      *
C *  ###  11-Dec-96   MUL_MM_TT_I  v2.0  (c)  L. Petrov 11-APR-2001 ###  *
C *                                                                      *
C ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i' ! Definnition of DB__MUL_MM_TT_I
      INTEGER*4  M1, N1, M2, N2, M3, N3, IUER
      REAL*8     MAT1(M1,N1), MAT2(M2,N2), MATO(M3,N3)
      CHARACTER  STR*80
C
      INTEGER*4  I_LEN, ILEN
C
      IF ( M1 .NE. N2 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. N2  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N2='
           CALL INCH ( N2, STR(I_LEN(STR)+2:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_TT_I', STR )
           RETURN
      END IF
C
      IF ( N1 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR ='N1 .NE. M3  N1='
           CALL INCH ( N1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_TT_I', STR )
           RETURN
      END IF
C
      IF ( M2 .NE. N3 ) THEN
           CALL CLRCH ( STR )
           STR ='M2 .NE. N3  M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N3='
           CALL INCH ( N3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 16, IUER, 'MUL_MM_TT_I', STR )
           RETURN
      END IF
C
      IF ( M2 .LE. DB__MUL_MM_TT_I ) THEN
           CALL BLAS_$DGEMM ( 'T', 'T', N1, M2, N2, 1.0D0, MAT1, M1,
     #                         MAT2, M2, 0.0D0, MATO, M3 )
         ELSE 
           CALL DGEMM ( 'T', 'T', N1, M2, N2, 1.0D0, MAT1, M1,
     #                   MAT2, M2, 0.0D0, MATO, M3 )
      END IF
C
      CALL ERR_LOG ( 0, IUER,, )
      RETURN
      END  !#!  MUL_MM_TT_I  #!#
