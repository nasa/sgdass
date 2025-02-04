#include <mk5_preprocessor_directives.inc>
      SUBROUTINE RCT_SCATTER ( DIM1, DIM2, M1, M2, IND1, IND2, AC, A )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine RCT_SCATTER extracts the region of the          *
! *   rectangular matrix and puts results in another rectangular matrix. *
! *                                                                      *
! *                                 A A A A A A A A                      *
! *                                                                      *
! *                                 A A A A A A A A                      *
! *                                                                      *
! *                                 A A A A A A A A                      *
! *                                    |-------|                         *
! *    B B B B   -------->          A A|A A A A|A A                      *
! *                                    |       |                         *
! *    B B B B                      A A|A A A A|A A                      *
! *                                    |_______|                         *
! *                                 A A A A A A A A                      *
! *                                                                      *
! *                                 A A A A A A A A                      *
! *                                                                      *
! *                                 A A A A A A A A                      *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *   M1 ( INTEGER*4 ) -- First dimension of the input rectangular       *
! *                       matrix A.                                      *
! *   M2 ( INTEGER*4 ) -- Second dimension of the input rectangular      *
! *                       matrix A.                                      *
! * IND1 ( INTEGER*4 ) -- First index (row index) of the region to be    *
! *                       copied.                                        * 
! * DIM1 ( INTEGER*4 ) -- First dimension (the number of rows) of the    *
! *                       region to be copied.                           *
! * IND2 ( INTEGER*4 ) -- Second index (column index) of the region to   *
! *                       be copied.                                     *
! * DIM2 ( INTEGER*4 ) -- Second dimension (the number of columns) of    *
! *                       the region to be copied.                       *
! *    A ( REAL*8    ) -- Square symmetric matrix in upper triangular    *
! *                       representation of dimension M1.                *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *   AC ( REAL*8    ) -- Rectangular matrix of dimension (DIM1,DIM2)    *
! *                       which is the region of matrix A                *
! *                                                                      *
! *                       A(IND1,IND2)        A(IND1,IND2+DIM2-1)        *
! *                       A(IND1+DIM1-1,IND2) A(IND1+DIM1-1,IND2+DIM2-1) *
! *                                                                      *
! *                                                                      *
! *  ### 28-JUN-2021  RCT_SCATTER  v1.0 (c)  L. Petrov  28-JUN-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  DIM1, DIM2, IND1, IND2, M1, M2
      REAL*8     AC(DIM1,DIM2), A(M1,M2)
      INTEGER*4  J1
!
      DO 410 J1=1,DIM2
         CALL DCOPY ( DIM1, AC(1,J1), 1, A(IND1,IND2+J1-1), 1 )
 410  CONTINUE 
!
      RETURN
      END  !#!  RCT_SCATTER  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE RCT_SCATTER_ADD ( DIM1, DIM2, M1, M2, IND1, IND2, AC, A )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine RCT_SCATTER extracts the region of the          *
! *   rectangular matrix and puts results in another rectangular matrix. *
! *                                                                      *
! *                                 A A A A A A A A                      *
! *                                                                      *
! *                                 A A A A A A A A                      *
! *                                                                      *
! *                                 A A A A A A A A                      *
! *                                    |-------|                         *
! *    B B B B   -------->          A A|A A A A|A A                      *
! *                                    |       |                         *
! *    B B B B                      A A|A A A A|A A                      *
! *                                    |_______|                         *
! *                                 A A A A A A A A                      *
! *                                                                      *
! *                                 A A A A A A A A                      *
! *                                                                      *
! *                                 A A A A A A A A                      *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *   M1 ( INTEGER*4 ) -- First dimension of the input rectangular       *
! *                       matrix A.                                      *
! *   M2 ( INTEGER*4 ) -- Second dimension of the input rectangular      *
! *                       matrix A.                                      *
! * IND1 ( INTEGER*4 ) -- First index (row index) of the region to be    *
! *                       copied.                                        * 
! * DIM1 ( INTEGER*4 ) -- First dimension (the number of rows) of the    *
! *                       region to be copied.                           *
! * IND2 ( INTEGER*4 ) -- Second index (column index) of the region to   *
! *                       be copied.                                     *
! * DIM2 ( INTEGER*4 ) -- Second dimension (the number of columns) of    *
! *                       the region to be copied.                       *
! *    A ( REAL*8    ) -- Square symmetric matrix in upper triangular    *
! *                       representation of dimension M1.                *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *   AC ( REAL*8    ) -- Rectangular matrix of dimension (DIM1,DIM2)    *
! *                       which is the region of matrix A                *
! *                                                                      *
! *                       A(IND1,IND2)        A(IND1,IND2+DIM2-1)        *
! *                       A(IND1+DIM1-1,IND2) A(IND1+DIM1-1,IND2+DIM2-1) *
! *                                                                      *
! *                                                                      *
! *  ### 28-JUN-2021  RCT_SCATTER_ADD  v1.0 (c)  L. Petrov  28-JUN-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  DIM1, DIM2, IND1, IND2, M1, M2
      REAL*8     AC(DIM1,DIM2), A(M1,M2)
      INTEGER*4  J1, J2
!
      DO 410 J1=1,DIM2
         CALL DAXPY ( DIM1, 1.0D0, AC(1,J1), 1, A(IND1,IND2+J1-1), 1 )
 410  CONTINUE 
!
      RETURN
      END  SUBROUTINE  RCT_SCATTER_ADD  !#!#
