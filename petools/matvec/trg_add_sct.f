#include <mk5_preprocessor_directives.inc>
      SUBROUTINE TRG_ADD_SCT ( M1, IND1, DIM1, IND2, DIM2, AC, A )
! ************************************************************************
! *                                                                      *
! *   Routine  TRG_ADD_SCT  scatters elements of the rectangular region  *
! *   of the rectangular matrix and adds them to the region of the       *
! *   symmetric matrix in upper triangular prepresentation.              *
! *                                                                      *
! *   A A A A A A A A                                                    *
! *                                                                      *
! *     A A A A A A A                                                    *
! *                                                                      *
! *       A A A A A A                                                    *
! *      |-------|                                                       *
! *      |  A A A|A A   <--------   B B B B                              *
! *      |       |                                                       *
! *      |    A A|A A               B B B B                              *
! *      |_______|                                                       *
! *             A A A                                                    *
! *                                                                      *
! *               A A                                                    *
! *                                                                      *
! *                 A                                                    *
! *                                                                      *
! *   A(k) := A(k) + B(i,j)                                              *
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
! *   AC ( REAL*8    ) -- Rectangular matrix of dimension (DIM1,DIM2)    *
! *                       which is the region of matrix A.               *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *    A ( REAL*8    ) -- Square symmetric matrix in upper triangular    *
! *                       representation of dimension M1.                *
! *                                                                      *
! *                       A(IND1,IND2)        A(IND1,IND2+DIM2-1)        *
! *                       A(IND1+DIM1-1,IND2) A(IND1+DIM1-1,IND2+DIM2-1) *
! *                                                                      *
! *  ### 12-OCT-2002  TRG_ADD_SCT  v1.2 (c)  L. Petrov  27-JUN-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  M1, IND1, DIM1, IND2, DIM2
      REAL*8     AC(DIM1,DIM2), A(*)
      INTEGER*4  NP, J1, J2, J3
      INTEGER*8  LC
!
      IF ( IND1 .LT. IND2 ) THEN
           LC = INT8(IND2)*INT8(IND2-1)/2 + IND1
           DO 410 J1=1,DIM2
              CALL DAXPY ( DIM1, 1.D0, AC(1,J1), 1, A(LC), 1 )
              LC = LC + (J1-1) + IND2
 410       CONTINUE 
         ELSE IF ( IND1 .EQ. IND2 ) THEN
           LC = INT8(IND2)*INT8(IND2+1)/2 
           DO 420 J2=1,DIM2
              CALL DAXPY ( MIN(J2,DIM1), 1.D0, AC(1,J2), 1, A(LC), 1 )
              LC = LC + IND2 + J2-1
 420       CONTINUE
         ELSE ! ind1 .gt .ind2
           LC = INT8(IND1)*INT8(IND1-1)/2 + IND2
           DO 430 J3=1,DIM2
              NP = IND2-1+J3-IND1
              IF ( NP .GE. 1 ) THEN
                   CALL DAXPY ( MIN(NP,DIM1), 1.D0, AC(J3,1), DIM1, A(LC), 1 )
              END IF
              LC = LC + (J3-1) + IND1
 430       CONTINUE 
      END IF
      RETURN
      END  !#!  TRG_ADD_SCT  #!#
