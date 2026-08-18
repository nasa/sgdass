      SUBROUTINE MUL_MM_SS_I ( M1, MAT1, M2, MAT2, M3, N3, MATO, IUER )
C ************************************************************************
C *                                                                      *
C *   Subroutine  MUL_MM_SS_I  multiplies matrixes:                      *
C *   MATO = MAT1 * MAT2                                                 *
C *                                                                      *
C *  ###  11-DEC-96  MUL_MM_SS_I    v2.0 (c)  L. Petrov 10-APR-2001 ###  *
C *                                                                      *
C ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i' ! Definition of DB1__MUL_MM_SS_I, DB2__MUL_MM_SS_I 
      INTEGER*4  M1, M2, M3, N3, IUER
      REAL*8     MAT1(*), MAT2(*), MATO(M3,N3)
      CHARACTER  STR*80
C
      INTEGER*4  J1, J2, J3, KBL, KBR, LC
      ADDRESS__TYPE :: IADR1, IADR2, MEM_SIZE, MEM_ADR1, MEM_ADR2
      INTEGER*4  I_LEN, ILEN, IN(MAX__DIM), IN2(MAX__DIM), IN16(16)
      DATA IN16 / 1, 2, 4, 7, 11, 16, 22, 29, 37, 46, 56, 67, 79, 92, 106, 121 /
      REAL*8     VECL(MAX__DIM), VECR(MAX__DIM), DP_VV_V
C
      IF ( M1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. M2  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_SS_I', STR )
           RETURN
      END IF
C
      IF ( M2 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR ='M2 .NE. M3  M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_SS_I', STR )
           RETURN
      END IF
C
      IF ( M3 .NE. N3 ) THEN
           CALL CLRCH ( STR )
           STR ='M3 .NE. N3  M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N3='
           CALL INCH ( N3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_SS_I', STR )
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
      IF ( M1 .LT. DB1__MUL_MM_SS_I  .OR.  M1 .GT. DB2__MUL_MM_SS_I ) THEN
C
C -------- Dimensions when libvec routines are more efficient
C
           KBL=1
           DO 410 J1=1,M3
              CALL VEC_$DCOPY ( MAT1(KBL), VECL, J1 )
              IF ( J1.LT.M1 ) THEN
                   CALL VEC_$DGATHER ( MAT1(J1), IN(J1+1), M1-J1, VECL(J1+1) )
              END IF
              KBR=1
              DO 420 J2=1,N3
                 CALL VEC_$DCOPY ( MAT2(KBR), VECR, J2 )
                 IF ( J2.LT.N3 ) THEN
                      CALL VEC_$DGATHER ( MAT2(J2), IN(J2+1), M2-J2, VECR(J2+1))
                 END IF
                 MATO(J1,J2) = DP_VV_V ( M2, VECL, VECR )
                 KBR=KBR+J2
 420          CONTINUE
              KBL=KBL+J1
 410       CONTINUE
         ELSE IF ( M1 .LE. DB2__MUL_MM_SS_I ) THEN
C
C -------- Dimensions when blas routines are more efficient
C
#ifdef ADR_64BIT
           MEM_SIZE = 2*8*INT8(M1)*INT8(M1)
           CALL GET_MEM32 ( MEM_SIZE, MEM_ADR1 )
           IF ( MEM_ADR1 .LE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( MEM_SIZE, STR )
                CALL ERR_LOG ( 1281, IUER, 'MULMAT_SS_I', 'Error in attempt '//
     #              'to to get '//STR(1:I_LEN(STR))//' bytes dynamic memory' )
                CALL MEMORYMAP ( %VAL(1) ) 
                RETURN 
           END IF
           MEM_ADR2 = MEM_ADR1 + 8*INT8(M1)*INT8(M1)
#else
           MEM_SIZE = 2*8*M1*M1
           CALL GET_MEM32 ( MEM_SIZE, MEM_ADR1 )
           IF ( MEM_ADR1 .LE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( MEM_SIZE, STR )
                CALL ERR_LOG ( 1281, IUER, 'MULMAT_SS_I', 'Error in attempt '//
     #              'to to get '//STR(1:I_LEN(STR))//' bytes dynamic memory' )
                CALL MEMORYMAP ( %VAL(1) ) 
                RETURN 
           END IF
           MEM_ADR2 = MEM_ADR1 + 8*M1*M1
#endif
C
           LC=1
           DO 430 J3=1,M1
              IADR1 = MEM_ADR1 + 8*(J3-1)*M1
              IADR2 = MEM_ADR2 + 8*(J3-1)*M1
              CALL DCOPY ( J3, MAT1(LC), 1, %VAL(IADR1), 1 )
              CALL DCOPY ( J3, MAT2(LC), 1, %VAL(IADR2), 1 )
              IF ( J3 .LT. M1 ) THEN
                   IADR1 = IADR1 + 8*J3
                   IADR2 = IADR2 + 8*J3
                   CALL VEC_$DGATHER ( MAT1(J3), IN(J3+1), M1-J3, %VAL(IADR1) )
                   CALL VEC_$DGATHER ( MAT2(J3), IN(J3+1), M1-J3, %VAL(IADR2) )
              END IF
              LC = LC + J3
 430       CONTINUE
C
           CALL DGEMM ( 'N', 'N', M1, M1, M1, 1.D0, %VAL(MEM_ADR1), 
     #                   M1, %VAL(MEM_ADR2), M1, 0.D0, MATO, M1 )
           CALL FREE ( MEM_ADR1 )
      END IF
CCC
      CALL ERR_LOG ( 0, IUER,, )
      RETURN
      END  !#!  MUL_MM_SS_I  #!#
C
C ------------------------------------------------------------------------
C
      SUBROUTINE MUL_MM_SS_S ( M1, MAT1, M2, MAT2, M3, MATO, IUER )
C ************************************************************************
C *                                                                      *
C *   Subroutine  MUL_MM_SS_S  multiplies matrixes:                      *
C *   MATO = MAT1 * MAT2                                                 *
C *                                                                      *
C *  ###  11-Dec-96  MUL_MM_SS_S    v2.0 (c)  L. Petrov 10-APR-2001 ###  *
C *                                                                      *
C ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i' ! Definition of DB1__MUL_MM_SS_S, DB2__MUL_MM_SS_S
      INTEGER*4  M1, M2, M3, IUER
      REAL*8     MAT1(*), MAT2(*), MATO(*)
      CHARACTER  STR*80
C
      INTEGER*4  J1, J2, J3, J4, KBL, KBR, LC
      ADDRESS__TYPE :: IADR1, IADR2, IADR3, MEM_SIZE, MEM_ADR1, MEM_ADR2, MEM_ADR3
      INTEGER*4  I_LEN, ILEN, IN(MAX__DIM), IN2(MAX__DIM), IN16(16)
      DATA IN16 / 1, 2, 4, 7, 11, 16, 22, 29, 37, 46, 56, 67, 79, 92, 106, 121 /
      REAL*8     VECL(MAX__DIM), VECR(MAX__DIM), DP_VV_V
C
      IF ( M1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. M2  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_SS_S', STR )
           RETURN
      END IF
C
      IF ( M2 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR ='M2 .NE. M3  M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_SS_S', STR )
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
      IF ( M1 .LT. DB1__MUL_MM_SS_S  .OR.  M1 .GT. DB2__MUL_MM_SS_S ) THEN
C
C -------- Dimensions when libvec routines are more efficient
C
           LC=0
           KBL=1
           DO 410 J1=1,M3
              CALL VEC_$DCOPY ( MAT1(KBL), VECL, J1 )
              IF ( J1.LT.M1 ) THEN
                   CALL VEC_$DGATHER ( MAT1(J1), IN(J1+1), M1-J1, VECL(J1+1) )
              END IF
              KBR=1
              DO 420 J2=1,J1
                 CALL VEC_$DCOPY ( MAT2(KBR), VECR, J2 )
                 IF ( J2.LT.M2 ) THEN
                      CALL VEC_$DGATHER ( MAT2(J2), IN(J2+1), M2-J2, VECR(J2+1))
                 END IF
                 LC=LC+1
                 MATO(LC) = DP_VV_V ( M2, VECL, VECR )
                 KBR=KBR+J2
 420          CONTINUE
              KBL=KBL+J1
 410       CONTINUE
         ELSE IF ( M1 .LE. DB2__MUL_MM_SS_S ) THEN
C
C -------- Dimensions when blas routines are more efficient
C
#ifdef ADR_64BIT
           MEM_SIZE = 3*8*INT8(M1)*INT8(M1)
           CALL GET_MEM32 ( MEM_SIZE, MEM_ADR1 )
           IF ( MEM_ADR1 .LE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( MEM_SIZE, STR )
                CALL ERR_LOG ( 1292, IUER, 'MULMAT_SS_S', 'Error in attempt '//
     #              'to to get '//STR(1:I_LEN(STR))//' bytes dynamic memory' )
                CALL MEMORYMAP ( %VAL(1) ) 
                RETURN 
           END IF
           MEM_ADR2 = MEM_ADR1 +   8*INT8(M1)*INT8(M1)
           MEM_ADR3 = MEM_ADR1 + 2*8*INT8(M1)*INT8(M1)
#else
           MEM_SIZE = 3*8*M1*M1
           CALL GET_MEM32 ( MEM_SIZE, MEM_ADR1 )
           IF ( MEM_ADR1 .LE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( MEM_SIZE, STR )
                CALL ERR_LOG ( 1292, IUER, 'MULMAT_SS_S', 'Error in attempt '//
     #              'to to get '//STR(1:I_LEN(STR))//' bytes dynamic memory' )
                CALL MEMORYMAP ( %VAL(1) ) 
                RETURN 
           END IF
           MEM_ADR2 = MEM_ADR1 +  8*M1*M1
           MEM_ADR3 = MEM_ADR1 + 16*M1*M1
#endif
C
           LC=1
           DO 430 J3=1,M1
              IADR1 = MEM_ADR1 + 8*(J3-1)*M1
              IADR2 = MEM_ADR2 + 8*(J3-1)*M1
              CALL DCOPY ( J3, MAT1(LC), 1, %VAL(IADR1), 1 )
              CALL DCOPY ( J3, MAT2(LC), 1, %VAL(IADR2), 1 )
              IF ( J3 .LT. M1 ) THEN
                   IADR1 = IADR1 + 8*J3
                   IADR2 = IADR2 + 8*J3
                   CALL VEC_$DGATHER ( MAT1(J3), IN(J3+1), M1-J3, %VAL(IADR1) )
                   CALL VEC_$DGATHER ( MAT2(J3), IN(J3+1), M1-J3, %VAL(IADR2) )
              END IF
              LC = LC + J3
 430       CONTINUE
C
           CALL DGEMM ( 'N', 'N', M1, M1, M1, 1.D0, %VAL(MEM_ADR1), 
     #                   M1, %VAL(MEM_ADR2), M1, 0.D0, %VAL(MEM_ADR3), M1 )
           LC=1
           DO 440 J4=1,M3
              IADR3 = MEM_ADR3 + 8*(J4-1)*M3
              CALL DCOPY ( J4, %VAL(IADR3), 1, MATO(LC), 1 )
              LC = LC + J4
 440       CONTINUE
           CALL FREE ( MEM_ADR1 )
      END IF
C
      CALL ERR_LOG ( 0, IUER,, )
      RETURN
      END  !#!  MUL_MM_SS_S  #!#
