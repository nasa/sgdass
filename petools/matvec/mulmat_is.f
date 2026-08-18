      SUBROUTINE MUL_MM_II_S ( M1, N1, MAT1, M2, N2, MAT2, M3,
     #                         MATO, IUER )
C ************************************************************************
C *                                                                      *
C *   Subroutine  MUL_MM_II_S  multiplies matrices:                      *
C *   MATO = MAT1 * MAT2                                                 *
C *                                                                      *
C *  ###  19-Dec-96   MUL_MM_II_S  v2.0  (c)  L. Petrov 09-APR-2001 ###  *
C *                                                                      *
C ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i' ! Definnition of DB1__MUL_MM_II_S, DB2__MUL_MM_II_S 
      INTEGER*4  M1, N1, M2, N2, M3, IUER
      REAL*8     MAT1(M1,N1), MAT2(M2,N2), MATO(*)
      CHARACTER  STR*80
C
      ADDRESS__TYPE :: MEM_SIZE, MEM_ADR, IADR
      INTEGER*4  J1, J2, J3, LC
      INTEGER*4  I_LEN, ILEN
      REAL*8     VEC_$DDOT_I 
C
      IF ( N1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='N1 .NE. M2  N1='
           CALL INCH ( N1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_II_S', STR )
           RETURN
      END IF
C
      IF ( M1 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR = 'M1 .NE. M3  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_II_S', STR )
           RETURN
      END IF
C
      IF ( N2 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR = 'N2 .NE. N3  N2='
           CALL INCH ( N2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 16, IUER, 'MUL_MM_II_S', STR )
           RETURN
      END IF
CCC
      IF ( M3 .LT. DB1__MUL_MM_II_S   .OR.  M3 .GT. DB2__MUL_MM_II_S  ) THEN
           LC=0
           DO 410 J1=1,M1
              DO 420 J2=1,J1
                 LC=LC+1
                 MATO(LC)= VEC_$DDOT_I ( MAT1(J2,1), M1, MAT2(1,J1), 1, N1 )
 420         CONTINUE
 410       CONTINUE
         ELSE IF ( M3 .LE. DB2__MUL_MM_II_S ) THEN
#ifdef ADR_64BIT
           MEM_SIZE = 8*INT8(M3)*INT8(M3)
           CALL GET_MEM ( MEM_SIZE, MEM_ADR )
#else
           MEM_SIZE = 8*M3*M3
           CALL GET_MEM32 ( MEM_SIZE, MEM_ADR )
#endif
           CALL DGEMM ( 'N', 'N', M1, N2, M2, 1.D0, MAT1, M1, MAT2, M2, 0.D0, 
     #                   %VAL(MEM_ADR), M3 )
           LC=1
           DO 430 J3=1,M3
              IADR = MEM_ADR + 8*(J3-1)*M3
              CALL DCOPY ( J3, %VAL(IADR), 1, MATO(LC), 1 )
              LC = LC + J3
 430       CONTINUE
           CALL FREE ( MEM_ADR )
      END IF
CCC
      CALL ERR_LOG ( 0, IUER,, )
      RETURN
      END  !#!  MUL_MM_II_S  #!#
C
C ------------------------------------------------------------------------
C
      SUBROUTINE MUL_MM_IT_S ( M1, N1, MAT1, M2, N2, MAT2, M3, MATO,
     #                         IUER )
C ************************************************************************
C *                                                                      *
C *   Subroutine  MUL_MM_IT_S  multiplies matrices:                      *
C *   MATO = MAT1 * MAT2                                                 *
C *                                                                      *
C *  ###  12-Dec-96     MUL_MM_IT_S  v2.0 (c) L. Petrov 09-APR-2001 ###  *
C *                                                                      *
C ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i' ! Definnition of DB1__MUL_MM_IT_S, DB2__MUL_MM_IT_S 
      INTEGER*4  M1, N1, M2, N2, M3, IUER
      REAL*8     MAT1(M1,N1), MAT2(M2,N2), MATO(*)
      CHARACTER  STR*80
C
      INTEGER*4  J1, J2, J3, LC
      ADDRESS__TYPE :: MEM_SIZE, MEM_ADR, IADR
      REAL*8     VECN(MAX__DIM), VEC_$DDOT_I
      INTEGER*4  I_LEN, ILEN
C
      IF ( N1 .NE. N2 ) THEN
           CALL CLRCH ( STR )
           STR = 'N1 .NE. N2  N1='
           CALL INCH ( N1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N2='
           CALL INCH ( N2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_IT_S', STR )
           RETURN
      END IF
C
      IF ( M1 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. M3  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_IT_S', STR )
           RETURN
      END IF
C
      IF ( M2 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR ='M2 .NE. N3  M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 16, IUER, 'MUL_MM_IT_S', STR )
           RETURN
      END IF
CCC
      IF ( M3 .LT. DB1__MUL_MM_IT_S  .OR.  M3 .GT. DB2__MUL_MM_IT_S ) THEN
           LC=0
           DO 410 J1=1,M1
              CALL VEC_$DCOPY_I ( MAT2(J1,1), M2, VECN(1), 1, N2 )
              DO 420 J2=1,J1
                 LC=LC+1
                 MATO(LC)= VEC_$DDOT_I ( MAT1(J2,1), M1, VECN, 1, N1 )
 420         CONTINUE
 410       CONTINUE
         ELSE IF ( M3 .LE. DB2__MUL_MM_IT_S ) THEN
#ifdef ADR_64BIT
           MEM_SIZE = 8*INT8(M3)*INT8(M3)
           CALL GET_MEM ( MEM_SIZE, MEM_ADR )
#else
           MEM_SIZE = 8*M3*M3
           CALL GET_MEM32 ( MEM_SIZE, MEM_ADR )
#endif
           CALL GET_MEM32 ( MEM_SIZE, MEM_ADR )
!old           CALL DGEMM ( 'N', 'T', M1, N2, M2, 1.D0, MAT1, M1, MAT2, M2, 0.D0, 
!old     #                   %VAL(MEM_ADR), M3 )
           CALL DGEMM ( 'N', 'T', M1, M2, N2, 1.0D0, MAT1, M1,
     #                   MAT2, M2, 0.0D0, %VAL(MEM_ADR), M3 )
           LC=1
           DO 430 J3=1,M3
              IADR = MEM_ADR + 8*(J3-1)*M3
              CALL DCOPY ( J3, %VAL(IADR), 1, MATO(LC), 1 )
              LC = LC + J3
 430       CONTINUE
           CALL FREE ( MEM_ADR )
      END IF
C
      CALL ERR_LOG ( 0, IUER,, )
      RETURN
      END  !#!  MUL_MM_IT_S  #!#
C
C ------------------------------------------------------------------------
C
      SUBROUTINE MUL_MM_TI_S ( M1, N1, MAT1, M2, N2, MAT2, M3, MATO,
     #                         IUER )
C ************************************************************************
C *                                                                      *
C *   Subroutine  MUL_MM_TI_S  multiplies matrices:                      *
C *   MATO = MAT1 * MAT2                                                 *
C *                                                                      *
C *  ###  09-DEC-96    MUL_MM_TI_S   v2.1 (c) L. Petrov 13-APR-2001 ###  *
C *                                                                      *
C ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i' ! Definnition of DB1__MUL_MM_TI_S, DB2__MUL_MM_TI_S 
      INTEGER*4  M1, N1, M2, N2, M3, IUER
      REAL*8     MAT1(M1,N1), MAT2(M2,N2), MATO(*)
      CHARACTER  STR*80
C
      INTEGER*4  J1, J2, J3, LC
      ADDRESS__TYPE :: MEM_SIZE, MEM_ADR, IADR
      REAL*8     DP_VV_V
      INTEGER*4  I_LEN, ILEN
C
      IF ( M1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. M2  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_TI_S', STR )
           RETURN
      END IF
C
      IF ( N1 .NE. N2 ) THEN
           CALL CLRCH ( STR )
           STR ='N1 .NE. N2  N1='
           CALL INCH ( N1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N2='
           CALL INCH ( N2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_TI_S', STR )
           RETURN
      END IF
C
      IF ( M3 .NE. N1 ) THEN
           CALL CLRCH ( STR )
           STR ='M3 .NE. N1  M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N1='
           CALL INCH ( N1, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 16, IUER, 'MUL_MM_TI_S', STR )
           RETURN
      END IF
CCC
      IF ( M3 .LT. DB1__MUL_MM_TI_S  .OR.  M3 .GT. DB2__MUL_MM_TI_S ) THEN
           LC=0
           DO 410 J1=1,N1
              DO 420 J2=1,J1
                 LC=LC+1
                 MATO(LC)= DP_VV_V ( M1, MAT1(1,J2), MAT2(1,J1) )
 420         CONTINUE
 410      CONTINUE
         ELSE IF ( M3 .LE. DB2__MUL_MM_TI_S ) THEN
#ifdef ADR_64BIT
           MEM_SIZE = 8*INT8(M3)*INT8(M3)
           CALL GET_MEM ( MEM_SIZE, MEM_ADR )
#else
           MEM_SIZE = 8*M3*M3
           CALL GET_MEM32 ( MEM_SIZE, MEM_ADR )
#endif
           CALL DGEMM ( 'T', 'N', N1, N2, M1, 1.0D0, MAT1, M1, MAT2, M2, 0.0D0,
     #                   %VAL(MEM_ADR), M3 )
           LC=1
           DO 430 J3=1,M3
              IADR = MEM_ADR + 8*(J3-1)*M3
              CALL DCOPY ( J3, %VAL(IADR), 1, MATO(LC), 1 )
              LC = LC + J3
 430       CONTINUE
           CALL FREE ( MEM_ADR )
      END IF
C
      CALL ERR_LOG ( 0, IUER,, )
      RETURN
      END  !#!  MUL_MM_TI_S  #!#
