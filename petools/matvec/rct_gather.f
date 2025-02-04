#include <mk5_preprocessor_directives.inc>
      SUBROUTINE RCT_GATHER ( M1, M2, IND1, DIM1, IND2, DIM2, A, AC )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine  RCT_GATER extracts the region of the           *
! *   rectangular matrix and puts results in another rectangular matrix. *
! *                                                                      *
! *   A A A A A A A A                                                    *
! *                                                                      *
! *   A A A A A A A A                                                    *
! *                                                                      *
! *   A A A A A A A A                                                    *
! *      |-------|                                                       *
! *   A A|A A A A|A A   -------->   B B B B                              *
! *      |       |                                                       *
! *   A A|A A A A|A A               B B B B                              *
! *      |_______|                                                       *
! *   A A A A A A A A                                                    *
! *                                                                      *
! *   A A A A A A A A                                                    *
! *                                                                      *
! *   A A A A A A A A                                                    *
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
! *  ### 14-SEP-2002   RCT_GATHER  v1.0 (c)  L. Petrov  14-SEP-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  M1, M2, IND1, DIM1, IND2, DIM2
      REAL*8     A(M1,M2), AC(DIM1,DIM2)
      INTEGER*4  J1
!
      DO 410 J1=1,DIM2
         CALL DCOPY ( DIM1, A(IND1,IND2+J1-1), 1, AC(1,J1), 1 )
 410  CONTINUE 
!
      RETURN
      END  !#!  RCT_GATHER  #!#
