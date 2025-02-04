      SUBROUTINE MUL_MV_IR_V ( M1, N1, MAT, M2, M2I, VECI, IVECI, M3, VECO, &
     &                            IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  MUL_MV_IR_V  multiplies matrix by sparse vector:       *
! *   VECO = MAT * VECI                                                  *
! *                                                                      *
! *   Array IVECI contains indeces of non-zero elements of the vector    *
! *   VECI.                                                              *
! *                                                                      *
! *  ###  26-JAN-98    MUL_MV_IR_V   v2.0  (c) L. Petrov  26-JAN-98 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i'
      INTEGER*4  M1, N1, M2, M2I, M3, IUER, M
      PARAMETER ( M = MAX__DIM )
      INTEGER*4  IVECI(M2I)
      REAL*8     MAT(M1,N1), VECI(M2I), VECO(M3)
      REAL*8     VECW(M)
      INTEGER*4  IN(M), IN2(M), IVECI_MIN, IVECI_MAX, J1
      CHARACTER  STR*80, STR2*80
      REAL*8,    EXTERNAL :: DP_VV_V
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      IF ( N1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='N1 .NE. M2  N1='
           CALL INCH ( N1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MV_IR_V', STR )
           RETURN
      END IF
!
      IF ( M1 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR = 'M1 .NE. M3  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MV_IR_V', STR )
           RETURN
      END IF
!
! --- Find the minimal and maximla element of the array  IVECI
!
      CALL VEC_$IRMIN ( IVECI, M2I, IVECI_MIN )
      CALL VEC_$IRMAX ( IVECI, M2I, IVECI_MAX )
!
! --- Make some test of validity of the indeces of the array IVECI
!
      IF ( IVECI_MIN .LE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IVECI_MIN, STR )
           CALL ERR_LOG ( 22, IUER, 'MUL_MV_IR_V', 'Vector IVECI contains '// &
     &                   'non-valid element: '//STR )
           RETURN
      END IF
      IF ( IVECI_MAX .GT. M2 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IVECI_MAX, STR )
           CALL CLRCH ( STR2 )
           CALL INCH  ( M2, STR2 )
           CALL ERR_LOG ( 24, IUER, 'MUL_MV_IR_V', 'Vector IVECI contains '// &
     &                   'non-valid element: '//STR(1:I_LEN(STR))//'. '// &
     &                   'It is greater than declared dimension of the '// &
     &                   'vector: '//STR2 )
           RETURN
      END IF
!
! --- Calculate the vecot IN which contain indeces of the 1-st row of the
! --- matrix MAT which correspond non-zero elements of the sparse vector VECI
!
      IN2(1) = 1
      CALL VEC_$IINIT   ( IN,  N1,    M1       )
      CALL VEC_$IREC1N  ( IN,  N1,    IN2      )
      CALL VEC_$IGATHER ( IN2, IVECI, M2I, IN  )
!
      DO 410 J1=1,M1
!
! ------ Gather the vector VECW which would contain only elements of
! ------ the J1-th row of the matrix MAT which correspond non-zero elements
! ------ of the sparse vector VECI
!
         CALL VEC_$DGATHER  ( MAT(J1,1), IN, M2I, VECW )
         VECO(J1) = DP_VV_V ( M2I, VECW, VECI )
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MUL_MV_IR_V  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MUL_MV_SR_V ( M1, MAT, M2, MI, VECI, IVECI, M3, VECO, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  MUL_MV_SR_V  multiplies matrix by sparse vector:       *
! *   VECO = MAT * VECI                                                  *
! *                                                                      *
! *   Array IVECI contains indeces of non-zero elements of the vector    *
! *   VECI.                                                              *
! *                                                                      *
! *  ###  26-JAN-98  MUL_MV_SR_V  v1.0 (c) L. Petrov  26-JAN-98  ###     *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i'
      INTEGER*4  M1, M2, MI, IVECI(MI), M3, IUER
      REAL*8     MAT(*), VECI(M2), VECO(M3)
      INTEGER*4   M, INM, IVECI_MIN, IVECI_MAX
      PARAMETER ( M = MAX__DIM, INM = 1 )
      REAL*8     VECN(M), VECW(M)
      INTEGER*4  J1, KB, IN(M), IN2(M)
      CHARACTER  STR*80, STR2*80
      REAL*8,    EXTERNAL :: DP_VV_V
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      IF ( M1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. M2  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MV_SR_V', STR )
           RETURN
      END IF
!
      IF ( M1 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR = 'M1 .NE. M3  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MV_SR_V', STR )
           RETURN
      END IF
!
! --- Find the minimal and maximla element of the array  IVECI
!
      CALL VEC_$IRMIN ( IVECI, MI, IVECI_MIN )
      CALL VEC_$IRMAX ( IVECI, MI, IVECI_MAX )
!
! --- Make some test of validity of the indeces of the array IVECI
!
      IF ( IVECI_MIN .LE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IVECI_MIN, STR )
           CALL ERR_LOG ( 22, IUER, 'MUL_MV_SR_V', 'Vector IVECI contains '// &
     &                   'non-valid element: '//STR )
           RETURN
      END IF
      IF ( IVECI_MAX .GT. M2 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IVECI_MAX, STR )
           CALL CLRCH ( STR2 )
           CALL INCH  ( M2, STR2 )
           CALL ERR_LOG ( 24, IUER, 'MUL_MV_SR_V', 'Vector IVECI contains '// &
     &                   'non-valid element: '//STR(1:I_LEN(STR))//'. '// &
     &                   'It is greater than declared dimension of the '// &
     &                   'vector: '//STR2 )
           RETURN
      END IF
!
! --- Making array IN which would contain the index of the first elements for
! --- each column of square symmetric matrix
!
      IN2(1)=1
      CALL VEC_$IINIT  ( IN,  M1, 1   )
      CALL VEC_$IREC1N ( IN,  M1, IN2 )
      CALL VEC_$IREC1N ( IN2, M1, IN  )
!CCCCC
      KB=1
      DO 410 J1=1,M1
!
! ------ Copying J1 elements of the J1-th column of the matrix MAT
! ------ to the vector VECN
!
         CALL VEC_$DCOPY ( MAT(KB), VECN, J1 )
         IF ( J1.LT.M1 ) THEN
!
! ----------- Gathering remain elements of the J1-th row to the vector VECN
!
              CALL VEC_$DGATHER ( MAT(J1), IN(J1+1), M1-J1, VECN(J1+1) )
         END IF
!
! ------ Gathering elements fo VECN the subvector VECW which correspnds
! ------ non-zeroelements of VECI
!
         CALL VEC_$DGATHER  ( VECN, IVECI, MI, VECW )
!
! ------ And at last -- vector product of non-zero elements
!
         VECO(J1) = DP_VV_V ( MI, VECW, VECI )
         KB=KB+J1
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MUL_MV_SR_V  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MUL_MV_TR_V ( M1, N1, MAT, M2, M2I, VECI, IVECI, &
     &                         M3, VECO, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  MUL_MV_TR_V  multiplies matrix by sparse vector:       *
! *   VECO = MAT * VECI                                                  *
! *                                                                      *
! *   Array IVECI contains indeces of non-zero elements of the vector    *
! *   VECI.                                                              *
! *                                                                      *
! *  ###  26-JAN-98   MUL_MV_TR_V   v1.0 (c)  L. Petrov  26-JAN-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i'
      INTEGER*4  M1, N1, M2, M2I, M3, IVECI(M2I), IUER
      REAL*8     MAT(M1,N1), VECI(M2), VECO(M3)
      INTEGER*4  M, J1, IVECI_MIN, IVECI_MAX
      PARAMETER ( M = MAX__DIM )
      REAL*8     VECW(M)
      CHARACTER  STR*80, STR2*80
      REAL*8,    EXTERNAL :: DP_VV_V
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      IF ( M1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. M2  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MV_TR_V', STR )
           RETURN
      END IF
!
      IF ( N1 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR = 'N1 .NE. M3  N1='
           CALL INCH ( N1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MV_TR_V', STR )
           RETURN
      END IF
!
! --- Find the minimal and maximla element of the array  IVECI
!
      CALL VEC_$IRMIN ( IVECI, M2I, IVECI_MIN )
      CALL VEC_$IRMAX ( IVECI, M2I, IVECI_MAX )
!
! --- Make some test of validity of the indeces of the array IVECI
!
      IF ( IVECI_MIN .LE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IVECI_MIN, STR )
           CALL ERR_LOG ( 22, IUER, 'MUL_MV_TR_V', 'Vector IVECI contains '// &
     &                   'non-valid element: '//STR )
           RETURN
      END IF
      IF ( IVECI_MAX .GT. M2 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IVECI_MAX, STR )
           CALL CLRCH ( STR2 )
           CALL INCH  ( M2, STR2 )
           CALL ERR_LOG ( 24, IUER, 'MUL_MV_TR_V', 'Vector IVECI contains '// &
     &                   'non-valid element: '//STR(1:I_LEN(STR))//'. '// &
     &                   'It is greater than declared dimension of the '// &
     &                   'vector: '//STR2 )
           RETURN
      END IF
!
      DO 410 J1=1,N1
!
! ------ Gather the vector VECW which would contain only elements of
! ------ the J1-th column of the matrix MAT which correspond non-zero elements
! ------ of the sparse vector VECI
!
         CALL VEC_$DGATHER  ( MAT(1,J1), IVECI, M2I, VECW )
         VECO(J1) = DP_VV_V ( M2I, VECW, VECI )
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MUL_MV_TR_V  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MUL_MV_IR_R ( M1, N1, MAT, M2, M2I, VEC2, IVEC2, M3, M3I, &
     &                         VEC3, IVEC3, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  MUL_MV_IR_R  multiplies matrix by sparse vector:       *
! *   VEC3 = MAT * VEC2. It calculates not all elements of the vector    *
! *   VEC3, but only the elements with indeces contained in the array    *
! *   IVEC3.                                                             *
! *                                                                      *
! *   Array IVEC2 contains indeces of non-zero elements of the vector    *
! *   VEC2.                                                              *
! *                                                                      *
! *  ###  27-JAN-98   MUL_MV_IR_R  v1.0  (c)  L. Petrov 27-JAN-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i'
      INTEGER*4  M1, N1, M2, M3, M2I, IVEC2(M2I), M3I, IVEC3(M3I), IUER
      REAL*8     MAT(M1,N1), VEC2(M2I), VEC3(M3I)
      INTEGER*4   M, INM, IVEC2_MIN, IVEC2_MAX, IVEC3_MIN, IVEC3_MAX
      PARAMETER ( M = MAX__DIM, INM = 1 )
      REAL*8     VECW(M)
      INTEGER*4  J1, IR, IN(M), IN2(M)
      CHARACTER  STR*80, STR2*80
      REAL*8,    EXTERNAL :: DP_VV_V
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      IF ( N1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='N1 .NE. M2  N1='
           CALL INCH ( N1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MV_IR_R', STR )
           RETURN
      END IF
!
      IF ( M1 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR = 'M1 .NE. M3  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MV_IR_R', STR )
           RETURN
      END IF
!
! --- Search of the minimal and maximal elements of the array  IVEC2
!
      CALL VEC_$IRMIN ( IVEC2, M2I, IVEC2_MIN )
      CALL VEC_$IRMAX ( IVEC2, M2I, IVEC2_MAX )
!
! --- Make some test of validity of the indeces of the array IVEC2
!
      IF ( IVEC2_MIN .LE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IVEC2_MIN, STR )
           CALL ERR_LOG ( 22, IUER, 'MUL_MV_IR_R', 'Vector IVEC2 contains '// &
     &                   'non-valid element: '//STR )
           RETURN
      END IF
      IF ( IVEC2_MAX .GT. M2 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IVEC2_MAX, STR )
           CALL CLRCH ( STR2 )
           CALL INCH  ( M2, STR2 )
           CALL ERR_LOG ( 24, IUER, 'MUL_MV_IR_R', 'Vector IVEC2 contains '// &
     &                   'non-valid element: '//STR(1:I_LEN(STR))//'. '// &
     &                   'It is greater than declared dimension of the '// &
     &                   'vector: '//STR2 )
           RETURN
      END IF
!
! --- Search of the minimal and maximal element of the array  IVEC3
!
      CALL VEC_$IRMIN ( IVEC3, M3I, IVEC3_MIN )
      CALL VEC_$IRMAX ( IVEC3, M3I, IVEC3_MAX )
!
! --- Make some test of validity of the indeces of the array IVEC3
!
      IF ( IVEC3_MIN .LE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IVEC3_MIN, STR )
           CALL ERR_LOG ( 32, IUER, 'MUL_MV_IR_R', 'Vector IVEC3 contains '// &
     &                   'non-valid element: '//STR )
           RETURN
      END IF
      IF ( IVEC3_MAX .GT. M3 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IVEC3_MAX, STR )
           CALL CLRCH ( STR2 )
           CALL INCH  ( M3, STR2 )
           CALL ERR_LOG ( 34, IUER, 'MUL_MV_IR_R', 'Vector IVEC3 contains '// &
     &                   'non-valid element: '//STR(1:I_LEN(STR))//'. '// &
     &                   'It is greater than declared dimension of the '// &
     &                   'vector: '//STR2 )
           RETURN
      END IF
!
! --- Calculate the vecor IN which contain indeces of the 1-st row of the
! --- matrix MAT which correspond non-zero elements of the sparse vector VECI
!
      IN2(1) = 1
      CALL VEC_$IINIT   ( IN,  N1,    M1       )
      CALL VEC_$IREC1N  ( IN,  N1,    IN2      )
      CALL VEC_$IGATHER ( IN2, IVEC2, M2I, IN  )
!CCCCC
      DO 410 J1=1,M3I
!
! ------ Gather the vector VECW which would contain only elements of
! ------ the J1-th row of the matrix MAT which correspond non-zero elements
! ------ of the sparse vector VECI
!
         IR = IVEC3(J1)
         CALL VEC_$DGATHER  ( MAT(IR,1), IN, M2I, VECW )
         VEC3(J1) = DP_VV_V ( M2I, VECW, VEC2 )
 410  CONTINUE
!
 810  CONTINUE
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MUL_MV_IR_R  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MUL_MV_SR_R ( M1, MAT, M2, M2I, VEC2, IVEC2, M3, M3I, &
     &                         VEC3, IVEC3, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  MUL_MV_SR_R  multiplies matrix by sparse vector:       *
! *   VEC3 = MAT * VEC2. It calculates not all elements of the vector    *
! *   VEC3, but only the elements with indeces contained in the array    *
! *   IVEC3.                                                             *
! *                                                                      *
! *   Array IVEC2 contains indeces of non-zero elements of the vector    *
! *   VEC2.                                                              *
! *                                                                      *
! *  ###  27-JAN-98   MULS_MV_SR_R   v1.0 (c) L. Petrov  27-JAN-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i'
      INTEGER*4  M1, M2, M3, M2I, IVEC2(M2I), M3I, IVEC3(M3I), IUER
      REAL*8     MAT(*), VEC2(M2I), VEC3(M3I)
      INTEGER*4   M, INM, IVEC2_MIN, IVEC2_MAX, IVEC3_MIN, IVEC3_MAX
      PARAMETER ( M = MAX__DIM, INM = 1 )
      REAL*8     VECN(M), VECW(M)
      INTEGER*4  J1, IR, IN(M), IN2(M)
      CHARACTER  STR*80, STR2*80
      REAL*8,    EXTERNAL :: DP_VV_V
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
! --- Test of consistency of the parameters M1, M2, M3
!
      IF ( M1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. M2  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MV_SR_R', STR )
           RETURN
      END IF
!
      IF ( M1 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR = 'M1 .NE. M3  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MV_SR_R', STR )
           RETURN
      END IF
!
! --- Search of the minimal and maximal elements of the array  IVEC2
!
      CALL VEC_$IRMIN ( IVEC2, M2I, IVEC2_MIN )
      CALL VEC_$IRMAX ( IVEC2, M2I, IVEC2_MAX )
!
! --- Make some test of validity of the indeces of the array IVEC2
!
      IF ( IVEC2_MIN .LE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IVEC2_MIN, STR )
           CALL ERR_LOG ( 22, IUER, 'MUL_MV_SR_R', 'Vector IVEC2 contains '// &
     &                   'non-valid element: '//STR )
           RETURN
      END IF
      IF ( IVEC2_MAX .GT. M2 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IVEC2_MAX, STR )
           CALL CLRCH ( STR2 )
           CALL INCH  ( M2, STR2 )
           CALL ERR_LOG ( 24, IUER, 'MUL_MV_SR_R', 'Vector IVEC2 contains '// &
     &                   'non-valid element: '//STR(1:I_LEN(STR))//'. '// &
     &                   'It is greater than declared dimension of the '// &
     &                   'vector: '//STR2 )
           RETURN
      END IF
!
! --- Search of the minimal and maximal element of the array  IVEC3
!
      CALL VEC_$IRMIN ( IVEC3, M3I, IVEC3_MIN )
      CALL VEC_$IRMAX ( IVEC3, M3I, IVEC3_MAX )
!
! --- Make some test of validity of the indeces of the array IVEC3
!
      IF ( IVEC3_MIN .LE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IVEC3_MIN, STR )
           CALL ERR_LOG ( 32, IUER, 'MUL_MV_SR_R', 'Vector IVEC3 contains '// &
     &                   'non-valid element: '//STR )
           RETURN
      END IF
      IF ( IVEC3_MAX .GT. M3 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IVEC3_MAX, STR )
           CALL CLRCH ( STR2 )
           CALL INCH  ( M3, STR2 )
           CALL ERR_LOG ( 34, IUER, 'MUL_MV_SR_R', 'Vector IVEC3 contains '// &
     &                   'non-valid element: '//STR(1:I_LEN(STR))//'. '// &
     &                   'It is greater than declared dimension of the '// &
     &                   'vector: '//STR2 )
           RETURN
      END IF
!
! --- Making array IN which would contain the index of the first elements for
! --- each column of square symmetric matrix
!
      IN2(1)=1
      CALL VEC_$IINIT  ( IN,  M1, 1   )
      CALL VEC_$IREC1N ( IN,  M1, IN2 )
      CALL VEC_$IREC1N ( IN2, M1, IN  )
!CCCCC
      DO 410 J1=1,M3I
!
! ------ Copying IVEC3(J1) elements of the J1-th column of the matrix MAT
! ------ to the vector VECN
!
         IR = IVEC3(J1)
         CALL VEC_$DCOPY ( MAT(IN(IR)), VECN, IR )
         IF ( IR .LT. M1 ) THEN
!
! ----------- Gathering remain elements of the J1-th row to the vector VECN
!
              CALL VEC_$DGATHER ( MAT(IR), IN(IR+1), M1-IR, VECN(IR+1) )
         END IF
!
! ------ Gathering elements fo VECN the subvector VECW which correspnds
! ------ non-zeroelements of VEC2
!
         CALL VEC_$DGATHER  ( VECN, IVEC2, M2I, VECW )
!
! ------ And at last -- vector product of non-zero elements
!
         VEC3(J1) = DP_VV_V ( M2I, VECW, VEC2 )
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MUL_MV_SR_R  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MUL_MV_TR_R ( M1, N1, MAT, M2, M2I, VEC2, IVEC2, M3, M3I, &
     &                         VEC3, IVEC3, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  MUL_MV_TR_R  multiplies matrix by sparse vector:       *
! *   VEC3 = MAT * VEC2. It calculates not all elements of the vector    *
! *   VEC3, but only the elements with indeces contained in the array    *
! *   IVEC3.                                                             *
! *                                                                      *
! *   Array IVEC2 contains indeces of non-zero elements of the vector    *
! *   VEC2.                                                              *
! *                                                                      *
! *  ###  27-JAN-98   MUL_MV_TR_R  v1.0  (c)  L. Petrov 27-JAN-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i'
      INTEGER*4  M1, N1, M2, M3, M2I, IVEC2(M2I), M3I, IVEC3(M3I), IUER
      REAL*8     MAT(M1,N1), VEC2(M2I), VEC3(M3I)
      INTEGER*4   M, IR, IVEC2_MIN, IVEC2_MAX, IVEC3_MIN, IVEC3_MAX
      PARAMETER ( M = MAX__DIM )
      REAL*8     VECW(M)
      INTEGER*4  J1
      CHARACTER  STR*80, STR2*80
      REAL*8,    EXTERNAL :: DP_VV_V
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      IF ( M1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. M2  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MV_TR_R', STR )
           RETURN
      END IF
!
      IF ( N1 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR = 'N1 .NE. M3  N1='
           CALL INCH ( N1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MV_TR_R', STR )
           RETURN
      END IF
!
! --- Search of the minimal and maximal elements of the array  IVEC2
!
      CALL VEC_$IRMIN ( IVEC2, M2I, IVEC2_MIN )
      CALL VEC_$IRMAX ( IVEC2, M2I, IVEC2_MAX )
!
! --- Make some test of validity of the indeces of the array IVEC2
!
      IF ( IVEC2_MIN .LE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IVEC2_MIN, STR )
           CALL ERR_LOG ( 22, IUER, 'MUL_MV_TR_R', 'Vector IVEC2 contains '// &
     &                   'non-valid element: '//STR )
           RETURN
      END IF
      IF ( IVEC2_MAX .GT. M2 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IVEC2_MAX, STR )
           CALL CLRCH ( STR2 )
           CALL INCH  ( M2, STR2 )
           CALL ERR_LOG ( 24, IUER, 'MUL_MV_TR_R', 'Vector IVEC2 contains '// &
     &                   'non-valid element: '//STR(1:I_LEN(STR))//'. '// &
     &                   'It is greater than declared dimension of the '// &
     &                   'vector: '//STR2 )
           RETURN
      END IF
!
! --- Search of the minimal and maximal element of the array  IVEC3
!
      CALL VEC_$IRMIN ( IVEC3, M3I, IVEC3_MIN )
      CALL VEC_$IRMAX ( IVEC3, M3I, IVEC3_MAX )
!
! --- Make some test of validity of the indeces of the array IVEC3
!
      IF ( IVEC3_MIN .LE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IVEC3_MIN, STR )
           CALL ERR_LOG ( 32, IUER, 'MUL_MV_TR_R', 'Vector IVEC3 contains '// &
     &                   'non-valid element: '//STR )
           RETURN
      END IF
      IF ( IVEC3_MAX .GT. M3 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IVEC3_MAX, STR )
           CALL CLRCH ( STR2 )
           CALL INCH  ( M3, STR2 )
           CALL ERR_LOG ( 34, IUER, 'MUL_MV_TR_R', 'Vector IVEC3 contains '// &
     &                   'non-valid element: '//STR(1:I_LEN(STR))//'. '// &
     &                   'It is greater than declared dimension of the '// &
     &                   'vector: '//STR2 )
           RETURN
      END IF
!CCCCC
      DO 410 J1=1,M3I
!
! ------ Gather the vector VECW which would contain only elements of
! ------ the J1-th row of the matrix MAT which correspond non-zero elements
! ------ of the sparse vector VECI
!
         IR = IVEC3(J1)
         CALL VEC_$DGATHER  ( MAT(1,IR), IVEC2, M2I, VECW )
         VEC3(J1) = DP_VV_V ( M2I, VECW, VEC2 )
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MUL_MV_TR_R  #!#
