      SUBROUTINE MUL_MM_SI_I ( M1, MAT1, M2, N2, MAT2, M3, N3, MATO, IUER )
C ************************************************************************
C *                                                                      *
C *   Subroutine  MUL_MM_SI_I  multiplies matrices:                      *
C *   MATO = MAT1 * MAT2                                                 *
C *                                                                      *
C *   MAT1 -- symmetric matrix in the upper triangular representation;   *
C *   MAT2 -- rectangular matrix;                                        *
C *   MATO -- rectangular matrix.                                        *
C *                                                                      *
C *  ###  09-DEC-1996  MUL_MM_SI_I  v3.0 (c)  L. Petrov 14-MAR-2002 ###  *
C *                                                                      *
C ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i' ! Definnition of DB1__MUL_MM_SI_I, DB2__MUL_MM_SI_I 
      INTEGER*4  M1, M2, N2, M3, N3, IUER
      REAL*8     MAT1(*), MAT2(M2,N2), MATO(M3,N3)
      CHARACTER  STR*80
C
      INTEGER*4  J1, J2, J3, J4, KB, LC, K_PART, 
      ADDRESS__TYPE :: IADR, MEM_SIZE, MEM_ADR
      INTEGER*4  I_LEN, ILEN, IN(MAX__DIM), IN2(MAX__DIM), IN16(16)
      DATA IN16 / 1, 2, 4, 7, 11, 16, 22, 29, 37, 46, 56, 67, 79, 92, 106, 121 /
      REAL*8     VECN(MAX__DIM), DP_VV_V
C
      IF ( M1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. M2  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_SI_I', STR )
           RETURN
      END IF
C
      IF ( M2 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR ='M2 .NE. M3  M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_SI_I', STR )
           RETURN
      END IF
C
      IF ( N2 .NE. N3 ) THEN
           CALL CLRCH ( STR )
           STR ='N2 .NE. N3  N2='
           CALL INCH ( N2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N3='
           CALL INCH ( N3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 16, IUER, 'MUL_MM_SI_I', STR )
           RETURN
      END IF
CCC
      IF ( M1 .LE. 16 ) THEN
           CALL COPY_I4 ( 16, IN16, IN )
         ELSE 
           IN2(1)=1
           CALL VEC_$IINIT  ( IN,  M1, 1   )
           CALL VEC_$IREC1N ( IN,  M1, IN2 )
           CALL VEC_$IREC1N ( IN2, M1, IN  )
      END IF
C
      IF ( M1 .LT. DB1__MUL_MM_SI_I  ) THEN
C
C -------- Dimensions when libvec routines are more efficient
C
           KB=1
           DO 410 J1=1,M3
              CALL VEC_$DCOPY ( MAT1(KB), VECN, J1 )
              IF ( J1.LT.M1 ) THEN
                   CALL VEC_$DGATHER ( MAT1(J1), IN(J1+1), M1-J1, VECN(J1+1) )
              END IF
              DO 420 J2=1,N3
                 MATO(J1,J2) = DP_VV_V ( M1, VECN, MAT2(1,J2) )
 420          CONTINUE
              KB=KB+J1
 410       CONTINUE
         ELSE IF ( M1 .LE. DB2__MUL_MM_SI_I ) THEN
C
C -------- Dimensions when lblas routines are more efficient
C
#ifdef ADR_64BIT
           MEM_SIZE = 8*INT8(M1)*INT8(M1)
           CALL GET_MEM ( MEM_SIZE, MEM_ADR )
#else
           MEM_SIZE = 8*M1*M1
           CALL GET_MEM32 ( MEM_SIZE, MEM_ADR )
#endif
           IF ( MEM_ADR .LE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( MEM_SIZE, STR )
                CALL ERR_LOG ( 1241, IUER, 'MULMAT_SI_I', 'Error in attempt '//
     #              'to to get '//STR(1:I_LEN(STR))//' bytes dynamic memory' )
                CALL MEMORYMAP ( %VAL(1) ) 
                RETURN 
           END IF
C
           LC=1
           DO 430 J3=1,M1
              IADR = MEM_ADR + 8*(J3-1)*M1
              CALL DCOPY ( J3, MAT1(LC), 1, %VAL(IADR), 1 )
              IF ( J3.LT.M1 ) THEN
                   IADR = IADR + 8*J3
                   CALL VEC_$DGATHER ( MAT1(J3), IN(J3+1), M1-J3, %VAL(IADR) )
              END IF
              LC = LC + J3
 430       CONTINUE
C
           CALL DGEMM ( 'N', 'N', M1, N2, M2, 1.D0, %VAL(MEM_ADR), 
     #                   M1, MAT2, M2, 0.D0, MATO, M3 )
           CALL FREE ( MEM_ADR )
         ELSE IF ( M1 .GT. DB2__MUL_MM_SI_I ) THEN
C
C -------- We use blas routines for part of the matrix
C
           K_PART = DB2__MUL_MM_SI_I*DB2__MUL_MM_SI_I/M1
#ifdef ADR_64BIT
           MEM_SIZE = 8*INT8(M1)*INT8(K_PART)
           CALL GET_MEM ( MEM_SIZE, MEM_ADR )
#else
           MEM_SIZE = 8*M1*K_PART
           CALL GET_MEM32 ( MEM_SIZE, MEM_ADR )
#endif
           IF ( MEM_ADR .LE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( MEM_SIZE, STR )
                CALL ERR_LOG ( 1261, IUER, 'MULMAT_IS_I', 'Error in attempt '//
     #              'to to get '//STR(1:I_LEN(STR))//' bytes dynamic memory' )
                CALL MEMORYMAP ( %VAL(1) ) 
                RETURN 
           END IF
C
           LC = 1
           KB = 0
           DO 440 J4=1,M1
              KB = KB + 1
              IADR = MEM_ADR + 8*(KB-1)*M1
              CALL DCOPY ( J4, MAT1(LC), 1, %VAL(IADR), 1 )
              IF ( J4 .LT. M2 ) THEN
                   IADR = IADR + 8*J4
                   CALL VEC_$DGATHER ( MAT1(J4), IN(J4+1), M1-J4, %VAL(IADR) )
              END IF
              LC = LC + J4
C
              IF ( KB .EQ. K_PART ) THEN
                   CALL DGEMM ( 'T', 'N', KB, N3, M1, 1.0D0, %VAL(MEM_ADR), M1,
     #                          MAT2, M2, 0.0D0, MATO(J4-KB+1,1), M3 )
                   KB = 0
              END IF
 440       CONTINUE
C           
           IF ( KB .GT. 0 ) THEN
                CALL DGEMM ( 'T', 'N', KB, M3, M1, 1.0D0, %VAL(MEM_ADR), M1,
     #                        MAT2, M2, 0.0D0, MATO(M3-KB+1,1), M3 )
           END IF
           CALL FREE ( MEM_ADR )
      END IF
CCC
      CALL ERR_LOG ( 0, IUER,, )
      RETURN
      END  !#!  MUL_MM_SI_I  #!#
C
C ------------------------------------------------------------------------
C
      SUBROUTINE MUL_MM_ST_I ( M1, MAT1, M2, N2, MAT2, M3, N3, MATO,
     #                         IUER )
C ************************************************************************
C *                                                                      *
C *   Subroutine  MUL_MM_ST_I  multiplies matrices:                      *
C *   MATO = MAT1 * MAT2                                                 *
C *                                                                      *
C *   MAT1 -- symmetric matrix in upper triangular representation;       *
C *   MAT2 -- transpose of the rectangular matrix;                       *
C *   MATO -- rectangular matrix.                                        *
C *                                                                      *
C *  ###  11-DEC-96  MUL_MM_ST_I    v2.0 (c)  L. Petrov 09-APR-2001 ###  *
C *                                                                      *
C ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i' ! Definition of DB1__MUL_MM_ST_I, DB2__MUL_MM_ST_I 
      INTEGER*4  M1, M2, N2, M3, N3, IUER
      REAL*8     MAT1(*), MAT2(M2,N2), MATO(M3,N3)
      CHARACTER  STR*80
C
      INTEGER*4  J1, J2, J3, KB, LC
      ADDRESS__TYPE :: IADR, MEM_SIZE, MEM_ADR
      INTEGER*4  I_LEN, ILEN, IN(MAX__DIM), IN2(MAX__DIM), IN16(16)
      DATA IN16 / 1, 2, 4, 7, 11, 16, 22, 29, 37, 46, 56, 67, 79, 92, 106, 121 /
      REAL*8     VECN(MAX__DIM), VEC_$DDOT_I
C
      IF ( M1 .NE. N2 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. N2  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N2='
           CALL INCH ( N2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_ST_I', STR )
           RETURN
      END IF
C
      IF ( N2 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR ='N2 .NE. M3  N2='
           CALL INCH ( N2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_ST_I', STR )
           RETURN
      END IF
C
      IF ( M2 .NE. N3 ) THEN
           CALL CLRCH ( STR )
           STR ='M2 .NE. N3  M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N3='
           CALL INCH ( N3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 16, IUER, 'MUL_MM_ST_I', STR )
           RETURN
      END IF
CCC
      IF ( M1 .LE. 16 ) THEN
           CALL COPY_I4 ( 16, IN16, IN )
         ELSE 
           IN2(1)=1
           CALL VEC_$IINIT  ( IN,  M1, 1   )
           CALL VEC_$IREC1N ( IN,  M1, IN2 )
           CALL VEC_$IREC1N ( IN2, M1, IN  )
      END IF
C
      IF ( M1 .LT. DB1__MUL_MM_ST_I  .OR.  M1 .GT. DB2__MUL_MM_ST_I   ) THEN
           KB=1
           DO 410 J1=1,M3
              CALL VEC_$DCOPY ( MAT1(KB), VECN, J1 )
              IF ( J1.LT.M1 ) THEN
                   CALL VEC_$DGATHER ( MAT1(J1), IN(J1+1), M1-J1, VECN(J1+1) )
              END IF
              DO 420 J2=1,N3
                 MATO(J1,J2) = VEC_$DDOT_I ( VECN, 1, MAT2(J2,1), M2, M1 )
 420          CONTINUE
              KB=KB+J1
 410       CONTINUE 
         ELSE IF ( M1 .LE. DB2__MUL_MM_ST_I ) THEN
#ifdef ADR_64BIT
           MEM_SIZE = 8*INT8(M1)*INT8(M1)
           CALL GET_MEM ( MEM_SIZE, MEM_ADR )
#else
           MEM_SIZE = 8*M1*M1
           CALL GET_MEM32 ( MEM_SIZE, MEM_ADR )
#endif
           IF ( MEM_ADR .LE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( MEM_SIZE, STR )
                CALL ERR_LOG ( 1251, IUER, 'MULMAT_ST_I', 'Error in attempt '//
     #              'to to get '//STR(1:I_LEN(STR))//' bytes dynamic memory' )
                CALL MEMORYMAP ( %VAL(1) ) 
                RETURN 
           END IF
C
           LC=1
           DO 430 J3=1,M1
              IADR = MEM_ADR + 8*(J3-1)*M1
              CALL DCOPY ( J3, MAT1(LC), 1, %VAL(IADR), 1 )
              IF ( J3.LT.M1 ) THEN
                   IADR = IADR + 8*J3
                   CALL VEC_$DGATHER ( MAT1(J3), IN(J3+1), M1-J3, %VAL(IADR) )
              END IF
              LC = LC + J3
 430       CONTINUE
C
           CALL DGEMM ( 'N', 'T', M1, N2, M2, 1.D0, %VAL(MEM_ADR), 
     #                   M1, MAT2, M2, 0.D0, MATO, M3 )
           CALL FREE ( MEM_ADR )
      END IF
C
      CALL ERR_LOG ( 0, IUER,, )
      RETURN
      END  !#!  MUL_MM_ST_I  #!#
C
C ------------------------------------------------------------------------
C
      SUBROUTINE MUL_MM_IS_I ( M1, N1, MAT1, M2, MAT2, M3, N3, MATO,
     #                         IUER )
C ************************************************************************
C *                                                                      *
C *   Subroutine  MUL_MM_IS_I  multiplies matrixes:                      *
C *   MATO = MAT1 * MAT2                                                 *
C *                                                                      *
C *   MAT1 -- rectangular matrix;                                        *
C *   MAT2 -- symmetric matrix in the upper triangular representation;   *
C *   MATO -- rectangular matrix.                                        *
C *                                                                      *
C *  ###  11-Dec-96    MUL_MM_IS_I   v3.0 (c) L. Petrov 14-MAR-2002 ###  *
C *                                                                      *
C ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i' ! Definition of DB1__MUL_MM_IS_I, DB2__MUL_MM_IS_I 
      INTEGER*4  M1, N1, M2, M3, N3, IUER
      REAL*8     MAT1(M1,N1), MAT2(*), MATO(M3,N3)
      CHARACTER  STR*80
C
      INTEGER*4  J1, J2, J3, J4, KB, K_PART, LC
      ADRESS__TYPE :: IADR, MEM_SIZE, MEM_ADR
      INTEGER*4  I_LEN, ILEN, IN(MAX__DIM), IN2(MAX__DIM), IN16(16)
      DATA IN16 / 1, 2, 4, 7, 11, 16, 22, 29, 37, 46, 56, 67, 79, 92, 106, 121 /
      REAL*8     VECN(MAX__DIM), VEC_$DDOT_I
C
      IF ( N1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='N1 .NE. M2  N1='
           CALL INCH ( N1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_IS_I', STR )
           RETURN
      END IF
C
      IF ( M2 .NE. N3 ) THEN
           CALL CLRCH ( STR )
           STR ='M2 .NE. N3  M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N3='
           CALL INCH ( N3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_IS_I', STR )
           RETURN
      END IF
C
      IF ( M1 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. M3  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 16, IUER, 'MUL_MM_IS_I', STR )
           RETURN
      END IF
CCC
      IF ( M2 .LE. 16 ) THEN
           CALL COPY_I4 ( 16, IN16, IN )
         ELSE 
           IN2(1)=1
           CALL VEC_$IINIT  ( IN,  M2, 1   )
           CALL VEC_$IREC1N ( IN,  M2, IN2 )
           CALL VEC_$IREC1N ( IN2, M2, IN  )
      END IF
C
      IF ( M2 .LT. DB1__MUL_MM_IS_I ) THEN
C
C -------- Dimensions when libvec routines are more efficient
C
           DO 410 J1=1,M1
              KB=1
             DO 420 J2=1,N1
                 CALL DCOPY ( J2, MAT2(KB), 1, VECN, 1 )
                 IF ( J2.LT.N1 ) THEN
                      CALL VEC_$DGATHER ( MAT2(J2), IN(J2+1), N1-J2, VECN(J2+1))
                 END IF
                 MATO(J1,J2) = VEC_$DDOT_I ( MAT1(J1,1), M1, VECN, 1, M2 )
                 KB=KB+J2
 420         CONTINUE
 410       CONTINUE
         ELSE IF ( M2 .LE. DB2__MUL_MM_IS_I ) THEN
C
C -------- Dimensions when blas routines are more efficient
C
#ifdef ADR_64BIT
           MEM_SIZE = 8*INT8(M2)*INT8(M2)
           CALL GET_MEM ( MEM_SIZE, MEM_ADR )
#else
           MEM_SIZE = 8*M2*M2
           CALL GET_MEM32 ( MEM_SIZE, MEM_ADR )
#endif
           IF ( MEM_ADR .LE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( MEM_SIZE, STR )
                CALL ERR_LOG ( 1262, IUER, 'MULMAT_IS_I', 'Error in attempt '//
     #              'to to get '//STR(1:I_LEN(STR))//' bytes dynamic memory' )
                CALL MEMORYMAP ( %VAL(1) ) 
                RETURN 
           END IF
C
           LC=1
           DO 430 J3=1,M2
              IADR = MEM_ADR + 8*(J3-1)*M2
              CALL DCOPY ( J3, MAT2(LC), 1, %VAL(IADR), 1 )
              IF ( J3.LT.M2 ) THEN
                   IADR = IADR + 8*J3
                   CALL VEC_$DGATHER ( MAT2(J3), IN(J3+1), M2-J3, %VAL(IADR) )
              END IF
              LC = LC + J3
 430       CONTINUE
C
           CALL DGEMM ( 'N', 'N', M1, N1, M2, 1.D0, MAT1, 
     #                   M1, %VAL(MEM_ADR), M2, 0.D0, MATO, M3 )
           CALL FREE ( MEM_ADR )
        ELSE IF ( M2 .GT. DB2__MUL_MM_IS_I ) THEN
C
C -------- We use blas routines for parts of the matrix
C
           K_PART = DB2__MUL_MM_IS_I*DB2__MUL_MM_IS_I/M2
#ifdef ADR_64BIT
           MEM_SIZE = 8*INT8(M2)*INT8(K_PART)
           CALL GET_MEM ( MEM_SIZE, MEM_ADR )
#else
           MEM_SIZE = 8*M2*K_PART
           CALL GET_MEM32 ( MEM_SIZE, MEM_ADR )
#endif
           IF ( MEM_ADR .LE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( MEM_SIZE, STR )
                CALL ERR_LOG ( 1261, IUER, 'MULMAT_IS_I', 'Error in attempt '//
     #              'to to get '//STR(1:I_LEN(STR))//' bytes dynamic memory' )
                CALL MEMORYMAP ( %VAL(1) ) 
                RETURN 
           END IF
C
           LC = 1
           KB = 0
           DO 440 J4=1,N1
              KB = KB + 1
              IADR = MEM_ADR + 8*(KB-1)*M2
              CALL DCOPY ( J4, MAT2(LC), 1, %VAL(IADR), 1 )
              IF ( J4 .LT. M2 ) THEN
                   IADR = IADR + 8*J4
                   CALL VEC_$DGATHER ( MAT2(J4), IN(J4+1), M2-J4, %VAL(IADR) )
              END IF
              LC = LC + J4
C
              IF ( KB .EQ. K_PART ) THEN
                   CALL DGEMM ( 'N', 'N', M1, KB, N1, 1.0D0, MAT1, M1,
     #                          %VAL(MEM_ADR), M2, 0.0D0, MATO(1,J4-KB+1), M3 )
                   KB = 0
              END IF
 440       CONTINUE
C           
           IF ( KB .GT. 0 ) THEN
                CALL DGEMM ( 'N', 'N', M1, KB, N1, 1.0D0, MAT1, M1,
     #                        %VAL(MEM_ADR), M2, 0.0D0, MATO(1,N1-KB+1), M3 )
           END IF
           CALL FREE ( MEM_ADR )
      END IF
CCC
      CALL ERR_LOG ( 0, IUER,, )
      RETURN
      END  !#!  MUL_MM_IS_I  #!#
C
C ------------------------------------------------------------------------
C
      SUBROUTINE MUL_MM_TS_I ( M1, N1, MAT1, M2, MAT2, M3, N3, MATO,
     #                         IUER )
C ************************************************************************
C *                                                                      *
C *   Subroutine  MUL_MM_TS_I  multiplies matrixes:                      *
C *   MATO = MAT1 * MAT2                                                 *
C *                                                                      *
C *   MAT1 -- transpose of the rectangular matrix;                       *
C *   MAT2 -- symmetric matrix in the upper triangular representation;   *
C *   MATO -- rectangular matrix.                                        *
C *                                                                      *
C *  ###  11-DEC-96  MUL_MM_TS_I    v2.1 (c)  L. Petrov 13-APR-2001 ###  *
C *                                                                      *
C ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i' ! Definition of DB1__MUL_MM_TS_I, DB2__MUL_MM_TS_I 
      INTEGER*4  M1, N1, M2, M3, N3, IUER
      REAL*8     MAT1(M1,N1), MAT2(*), MATO(M3,N3)
      CHARACTER  STR*80
C
      INTEGER*4  J1, J2, J3, KB, LC
      ADDRESS__TYPE :: IADR, MEM_SIZE, MEM_ADR
      INTEGER*4  I_LEN, ILEN, IN(MAX__DIM), IN2(MAX__DIM), IN16(16)
      DATA IN16 / 1, 2, 4, 7, 11, 16, 22, 29, 37, 46, 56, 67, 79, 92, 106, 121 /
      REAL*8     VECN(MAX__DIM), DP_VV_V
C
      IF ( M1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. M2  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_TS_I', STR )
           RETURN
      END IF
C
      IF ( M2 .NE. N3 ) THEN
           CALL CLRCH ( STR )
           STR ='M2 .NE. N3  M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N3='
           CALL INCH ( N3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_TS_I', STR )
           RETURN
      END IF
C
      IF ( N1 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR ='N1 .NE. N3  N1='
           CALL INCH ( N1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N3='
           CALL INCH ( N3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 16, IUER, 'MUL_MM_TS_I', STR )
           RETURN
      END IF
CCC
      IF ( M2 .LE. 16 ) THEN
           CALL COPY_I4 ( 16, IN16, IN )
         ELSE 
           IN2(1)=1
           CALL VEC_$IINIT  ( IN,  M2, 1   )
           CALL VEC_$IREC1N ( IN,  M2, IN2 )
           CALL VEC_$IREC1N ( IN2, M2, IN  )
      END IF
C
      IF ( M2 .LT. DB1__MUL_MM_TS_I  .OR.  M2 .GT. DB2__MUL_MM_TS_I ) THEN
C
C -------- Dimensions when libvec routines are more efficient
C
           DO 410 J1=1,M3
              KB=1
              DO 420 J2=1,N3
                 CALL VEC_$DCOPY ( MAT2(KB), VECN, J2 )
                 IF ( J2.LT.N3 ) THEN
                      CALL VEC_$DGATHER ( MAT2(J2), IN(J2+1), N3-J2, VECN(J2+1))
                 END IF
                 MATO(J1,J2) = DP_VV_V ( M2, MAT1(1,J1), VECN )
                 KB=KB+J2
 420          CONTINUE
 410       CONTINUE
         ELSE IF ( M2 .LE. DB2__MUL_MM_TS_I ) THEN
C
C -------- Dimensions when blas routines are more efficient
C
#ifdef ADR_64BIT
           MEM_SIZE = 8*INT8(M2)*INT8(M2)
           CALL GET_MEM ( MEM_SIZE, MEM_ADR )
#else
           MEM_SIZE = 8*M2*M2
           CALL GET_MEM32 ( MEM_SIZE, MEM_ADR )
#endif
           IF ( MEM_ADR .LE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( MEM_SIZE, STR )
                CALL ERR_LOG ( 1271, IUER, 'MULMAT_TS_I', 'Error in attempt '//
     #              'to to get '//STR(1:I_LEN(STR))//' bytes dynamic memory' )
                CALL MEMORYMAP ( %VAL(1) ) 
                RETURN 
           END IF
C
           LC=1
           DO 430 J3=1,M2
              IADR = MEM_ADR + 8*(J3-1)*M2
              CALL DCOPY ( J3, MAT2(LC), 1, %VAL(IADR), 1 )
              IF ( J3 .LT. M2 ) THEN
                   IADR = IADR + 8*J3
                   CALL VEC_$DGATHER ( MAT2(J3), IN(J3+1), M2-J3, %VAL(IADR) )
              END IF
              LC = LC + J3
 430       CONTINUE
C
           CALL DGEMM ( 'T', 'N', N1, M2, M1, 1.D0, MAT1, M1, 
     #                   %VAL(MEM_ADR), M2, 0.D0, MATO, M3 )
           CALL FREE ( MEM_ADR )
      END IF
CC
      CALL ERR_LOG ( 0, IUER,, )
      RETURN
      END  !#!  MUL_MM_TS_I  #!#
