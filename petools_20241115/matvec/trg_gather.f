#include <mk5_preprocessor_directives.inc>
      SUBROUTINE TRG_GATHER ( M1, IND1, DIM1, IND2, DIM2, A, AC )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine  TRG_GATHER extracts the region of the square   *
! *   symmetric matrix in upper triangular represinatation and puts      *
! *   results in the rectangular matrix.                                 *
! *                                                                      *
! *   A A A A A A A A                                                    *
! *                                                                      *
! *     A A A A A A A                                                    *
! *                                                                      *
! *       A A A A A A                                                    *
! *      |-------|                                                       *
! *      |  A A A|A A   -------->   B B B B                              *
! *      |       |                                                       *
! *      |    A A|A A               B B B B                              *
! *      |_______|                                                       *
! *             A A A                                                    *
! *                                                                      *
! *               A A                                                    *
! *                                                                      *
! *                 A                                                    *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *   M1 ( INTEGER*4 ) -- Dimension of the square symmetrical matrix A   *
! *                       kept in upper triangular representation.       *
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
! *                       which is the region of matrix A.               *
! *                                                                      *
! *                       A(IND1,IND2)        A(IND1,IND2+DIM2-1)        *
! *                       A(IND1+DIM1-1,IND2) A(IND1+DIM1-1,IND2+DIM2-1) *
! *                                                                      *
! *  ### 14-SEP-2002   TRG_GATHER  v1.2 (c)  L. Petrov  27-JUN-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  M1, IND1, DIM1, IND2, DIM2
      REAL*8     A(*), AC(DIM1,DIM2)
      INTEGER*8  LC
      INTEGER*4  J1, J2, J3
!
      IF ( IND1 .LT. IND2 ) THEN
           LC = INT8(IND2)*INT8(IND2-1)/2 + IND1
           DO 410 J1=1,DIM2
              CALL DCOPY ( DIM1, A(LC), 1, AC(1,J1), 1 )
              LC = LC + (J1-1) + IND2
 410       CONTINUE 
         ELSE IF ( IND1 .EQ. IND2 ) THEN
           LC = INT8(IND2)*INT8(IND2+1)/2 
           DO 420 J2=1,DIM2
              CALL DCOPY ( J2, A(LC), 1, AC(1,J2), 1 )
              IF ( J2 .GT. 1 ) THEN
                   CALL DCOPY ( J2-1, A(LC), 1, AC(J2,1), DIM1 )
              END IF
              LC = LC + IND2 + J2-1
 420       CONTINUE
         ELSE ! imd1 .gt .ind2
           LC = INT8(IND1)*INT8(IND1-1)/2 + IND2
           DO 430 J3=1,DIM1
              CALL DCOPY ( DIM2, A(LC), 1, AC(J3,1), DIM1 )
              LC = LC + (J3-1) + IND1
 430       CONTINUE 
      END IF
!
      RETURN
      END  !#!  TRG_GATHER  #!#
