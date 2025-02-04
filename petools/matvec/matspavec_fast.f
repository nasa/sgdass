      SUBROUTINE MUL_MV_IR_V ( M1, N1, MAT, M2, M2I, VECI, IVECI, M3, VECO, &
     &                         IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  MUL_MV_IR_V  multiplies matrix by sparse vector:       *
! *   VECO = MAT * VECI                                                  *
! *                                                                      *
! *   Array IVECI contains indices of non-zero elements of the vector    *
! *   VECI.                                                              *
! *                                                                      *
! *  ###  26-JAN-1998   MUL_MV_IR_V  v3.1 (c) L. Petrov 19-OCT-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i'
      INTEGER*4  M1, N1, M2, M2I, M3, IUER
       INTEGER*4  IVECI(M2I)
      REAL*8     MAT(M1,N1), VECI(M2I), VECO(M3)
      REAL*8,    ALLOCATABLE :: VECW(:)
      INTEGER*4, ALLOCATABLE :: IN(:)
      INTEGER*4  IVECI_MIN, IVECI_MAX, J1, J2
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
      CALL VEC_IRMIN ( IVECI, M2I, IVECI_MIN )
      CALL VEC_IRMAX ( IVECI, M2I, IVECI_MAX )
!
! --- Make some test of validity of the indices of the array IVECI
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
      ALLOCATE ( VECW(M2I) )
      ALLOCATE ( IN(M2I)   )
!
! --- Calculate the vector IN which contain indices of the 1-st row of the
! --- matrix MAT which correspond non-zero elements of the sparse vector VECI
!
      DO 410 J1=1,M2I
         IN(J1) = M1*IVECI(J1) + (1-M1)
 410  CONTINUE 
!
      DO 420 J2=1,M1
!
! ------ Gather the vector VECW which would contain only elements of
! ------ the J2-th row of the matrix MAT which correspond non-zero elements
! ------ of the sparse vector VECI
!
         CALL DGATHER ( M2I, IN, MAT(J2,1), VECW )
         VECO(J2) = DP_VV_V ( M2I, VECW, VECI )
 420  CONTINUE
      DEALLOCATE ( IN )
      DEALLOCATE ( VECW )
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
! *   Array IVECI contains indices of non-zero elements of the vector    *
! *   VECI.                                                              *
! *                                                                      *
! *  ###  26-JAN-1998   MUL_MV_SR_V  v2.1 (c) L. Petrov  19-OCT-2017 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i'
      INTEGER*4  M1, M2, MI, IVECI(MI), M3, IUER
      REAL*8     MAT(*), VECI(M2), VECO(M3)
      INTEGER*4   M, IVECI_MIN, IVECI_MAX
      REAL*8,    ALLOCATABLE, TARGET :: VEC_ALLOCATED(:)
      REAL*8,    POINTER     :: VECN(:), VECW(:)
      INTEGER*4, ALLOCATABLE :: IN(:)
      INTEGER*4  J1, J2, KB
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
! --- Find the minimal and maximal element of the array  IVECI
!
      CALL VEC_IRMIN ( IVECI, MI, IVECI_MIN )
      CALL VEC_IRMAX ( IVECI, MI, IVECI_MAX )
!
! --- Make some test of validity of the indices of the array IVECI
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
      ALLOCATE ( IN(MI)   )
      ALLOCATE ( VEC_ALLOCATED(2*M1) )
      VECN=>VEC_ALLOCATED(1:M1)
      VECW=>VEC_ALLOCATED(M1+1:2*M1)
!
! --- Build array IN which contains the index of the first elements for
! --- each column of square symmetric matrix
!
      DO 410 J1=1,MI
         IN(J1) = IVECI(J1)*(IVECI(J1)-1)/2 + 1
 410  CONTINUE 
!
      KB=1
      DO 420 J2=1,M1
!
! ------ Copying J2 elements of the J2-th column of the matrix MAT
! ------ to the vector VECN
!
         CALL COPY_V ( J2, MAT(KB), VECN )
         IF ( J2.LT.M1 ) THEN
!
! ----------- Gathering remain elements of the J2-th row to the vector VECN
!
              CALL DGATHER ( M1-J2, IN(J2+1), MAT(J2), VECN(J2+1) )
         END IF
!
! ------ Gathering elements fo VECN the subvector VECW which correspnds
! ------ non-zeroelements of VECI
!
         CALL DGATHER ( MI, IVECI, VECN, VECW )
!
! ------ And at last -- vector product of non-zero elements
!
         VECO(J2) = DP_VV_V ( MI, VECW, VECI )
         KB=KB+J2
 420  CONTINUE
      DEALLOCATE ( VEC_ALLOCATED )
      DEALLOCATE ( IN   )
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
! *   Array IVECI contains indices of non-zero elements of the vector    *
! *   VECI.                                                              *
! *                                                                      *
! *  ###  26-JAN-1998   MUL_MV_TR_V  v1.1 (c)  L. Petrov 19-OCT-2017 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i'
      INTEGER*4  M1, N1, M2, M2I, M3, IVECI(M2I), IUER
      REAL*8     MAT(M1,N1), VECI(M2), VECO(M3)
      INTEGER*4  J1, IVECI_MIN, IVECI_MAX
      REAL*8,    ALLOCATABLE :: VECW(:)
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
      CALL VEC_IRMIN ( IVECI, M2I, IVECI_MIN )
      CALL VEC_IRMAX ( IVECI, M2I, IVECI_MAX )
!
! --- Make some test of validity of the indices of the array IVECI
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
      ALLOCATE ( VECW(M2I) )
      DO 410 J1=1,N1
!
! ------ Gather the vector VECW which would contain only elements of
! ------ the J1-th column of the matrix MAT which correspond non-zero elements
! ------ of the sparse vector VECI
!
         CALL DGATHER ( M2I, IVECI, MAT(1,J1), VECW )
         VECO(J1) = DP_VV_V ( M2I, VECW, VECI )
 410  CONTINUE
      DEALLOCATE ( VECW )
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
! *   VEC3, but only the elements with indices contained in the array    *
! *   IVEC3.                                                             *
! *                                                                      *
! *   Array IVEC2 contains indices of non-zero elements of the vector    *
! *   VEC2.                                                              *
! *                                                                      *
! *  ###  27-JAN-1998   MUL_MV_IR_R  v2.1 (c) L. Petrov 19-OCT-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i'
      INTEGER*4  M1, N1, M2, M3, M2I, IVEC2(M2I), M3I, IVEC3(M3I), IUER
      REAL*8     MAT(M1,N1), VEC2(M2I), VEC3(M3I)
      INTEGER*4   INM, IVEC2_MIN, IVEC2_MAX, IVEC3_MIN, IVEC3_MAX
      REAL*8,    ALLOCATABLE :: VECW(:)
      INTEGER*4, ALLOCATABLE :: IN(:)
      INTEGER*4  J1, J2, IR
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
      CALL VEC_IRMIN ( IVEC2, M2I, IVEC2_MIN )
      CALL VEC_IRMAX ( IVEC2, M2I, IVEC2_MAX )
!
! --- Make some test of validity of the indices of the array IVEC2
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
      CALL VEC_IRMIN ( IVEC3, M3I, IVEC3_MIN )
      CALL VEC_IRMAX ( IVEC3, M3I, IVEC3_MAX )
!
! --- Make some test of validity of the indices of the array IVEC3
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
      ALLOCATE ( VECW(M2I) )
      ALLOCATE ( IN(M2I)   )
!
! --- Calculate the vector IN which contain indices of the 1-st row of the
! --- matrix MAT which correspond non-zero elements of the sparse vector VECI
!
      DO 410 J1=1,M2I
         IN(J1) = M1*IVEC2(J1) + (1-M1)
 410  CONTINUE 
!
      DO 420 J2=1,M3I
!
! ------ Gather the vector VECW which would contain only elements of
! ------ the J2-th row of the matrix MAT which correspond non-zero elements
! ------ of the sparse vector VECI
!
         IR = IVEC3(J2)
         CALL DGATHER ( M2I, IN, MAT(IR,1), VECW )
         VEC3(J2) = DP_VV_V ( M2I, VECW, VEC2 )
 420  CONTINUE
 810  CONTINUE
      DEALLOCATE ( IN   )
      DEALLOCATE ( VECW )
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
! *   VEC3, but only the elements with indices contained in the array    *
! *   IVEC3.                                                             *
! *                                                                      *
! *   Array IVEC2 contains indices of non-zero elements of the vector    *
! *   VEC2.                                                              *
! *                                                                      *
! *  ###  27-JAN-1998  MUL_MV_SR_R   v3.1 (c)  L. Petrov 19-OCT-2017 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i'
      INTEGER*4  M1, M2, M3, M2I, IVEC2(M2I), M3I, IVEC3(M3I), IUER
      REAL*8     MAT(*), VEC2(M2I), VEC3(M3I)
      INTEGER*4  IVEC2_MIN, IVEC2_MAX, IVEC3_MIN, IVEC3_MAX
      REAL*8,    ALLOCATABLE, TARGET :: VEC_ALLOCATED(:)
      REAL*8,    POINTER     :: VECN(:), VECW(:)
      INTEGER*4, ALLOCATABLE :: IN(:)
      INTEGER*4  J1, J2, IR
      CHARACTER  STR*80, STR2*80
!
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
      CALL VEC_IRMIN ( IVEC2, M2I, IVEC2_MIN )
      CALL VEC_IRMAX ( IVEC2, M2I, IVEC2_MAX )
!
! --- Make some test of validity of the indices of the array IVEC2
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
      CALL VEC_IRMIN ( IVEC3, M3I, IVEC3_MIN )
      CALL VEC_IRMAX ( IVEC3, M3I, IVEC3_MAX )
!
! --- Make some test of validity of the indices of the array IVEC3
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
! --- Build array IN which would contain the index of the first elements for
! --- each column of square symmetric matrix
! --- Find array INEND
!
      ALLOCATE ( IN(M1)   )
      ALLOCATE ( VEC_ALLOCATED(2*M1) )
      VECN=>VEC_ALLOCATED(1:M1)
      VECW=>VEC_ALLOCATED(M1+1:2*M1)
!
      IN(1) = 1
      IF ( M1 .GT. 1 ) THEN
           DO 410 J1=2,M1
              IN(J1) = IN(J1-1) + J1-1
 410       CONTINUE 
      END IF
      DO 420 J2=1,M3I
!
! ------ Copying IR elements of the IR-th column of the matrix MAT 
! ------ to the vector VECN
!
         IR = IVEC3(J2)
!
         CALL COPY_V ( IR, MAT(IN(IR)), VECN )
         IF ( IR .LT. M1 ) THEN
!
! ----------- Gathering remaining elements of the IR-th row to the vector VECN
!
              CALL DGATHER ( M1-IR, IN(IR+1), MAT(IR), VECN(IR+1) )
         END IF
!
! ------ Gathering elements fo VECN the subvector VECW which correspnds
! ------ non-zeroelements of VEC2
!
         CALL DGATHER  ( M2I, IVEC2, VECN, VECW )
!
! ------ And at last -- vector product of non-zero elements
!
         VEC3(J2) = DP_VV_V ( M2I, VECW, VEC2 )
 420  CONTINUE
      DEALLOCATE ( VEC_ALLOCATED )
      DEALLOCATE ( IN   )
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
! *   VEC3, but only the elements with indices contained in the array    *
! *   IVEC3.                                                             *
! *                                                                      *
! *   Array IVEC2 contains indices of non-zero elements of the vector    *
! *   VEC2.                                                              *
! *                                                                      *
! *  ###  27-JAN-1998  MUL_MV_TR_R  v1.1  (c) L. Petrov 19-OCT-2017  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i'
      INTEGER*4  M1, N1, M2, M3, M2I, IVEC2(M2I), M3I, IVEC3(M3I), IUER
      REAL*8     MAT(M1,N1), VEC2(M2I), VEC3(M3I)
      INTEGER*4  IR, IVEC2_MIN, IVEC2_MAX, IVEC3_MIN, IVEC3_MAX
      REAL*8,    ALLOCATABLE :: VECW(:)
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
      CALL VEC_IRMIN ( IVEC2, M2I, IVEC2_MIN )
      CALL VEC_IRMAX ( IVEC2, M2I, IVEC2_MAX )
!
! --- Make some test of validity of the indices of the array IVEC2
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
      CALL VEC_IRMIN ( IVEC3, M3I, IVEC3_MIN )
      CALL VEC_IRMAX ( IVEC3, M3I, IVEC3_MAX )
!
! --- Make some test of validity of the indices of the array IVEC3
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
!
      ALLOCATE ( VECW(M2I) )
      DO 410 J1=1,M3I
!
! ------ Gather the vector VECW which would contain only elements of
! ------ the J1-th row of the matrix MAT which correspond non-zero elements
! ------ of the sparse vector VECI
!
         IR = IVEC3(J1)
         CALL DGATHER ( M2I, IVEC2, MAT(1,IR), VECW )
         VEC3(J1) = DP_VV_V ( M2I, VECW, VEC2 )
 410  CONTINUE
      DEALLOCATE ( VECW )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MUL_MV_TR_R  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE VEC_IRMIN ( IVEC, COUNT, IVEC_MIN )
      IMPLICIT   NONE 
      INTEGER*4  COUNT, IVEC(COUNT), IVEC_MIN, J1
!
      IF ( COUNT .LE. 0 ) RETURN 
      IVEC_MIN = IVEC(1)
!
      DO 410 J1=1,COUNT
         IF ( IVEC(J1) .LT. IVEC_MIN ) IVEC_MIN = IVEC(J1)
 410  CONTINUE 
      RETURN
      END  !#!  VEC_IRMIN  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE VEC_IRMAX ( IVEC, COUNT, IVEC_MAX )
      IMPLICIT   NONE 
      INTEGER*4  COUNT, IVEC(COUNT), IVEC_MAX, J1
!
      IF ( COUNT .LE. 0 ) RETURN 
      IVEC_MAX = IVEC(1)
!
      DO 410 J1=1,COUNT
         IF ( IVEC(J1) .GT. IVEC_MAX ) IVEC_MAX = IVEC(J1)
 410  CONTINUE 
      RETURN
      END  !#!  VEC_IRMAX  #!#
