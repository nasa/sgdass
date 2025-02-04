#include <mk5_preprocessor_directives.inc>
      FUNCTION  SPARSE_DOT ( M, VEC1, VEC2, N, IND, VC1, VC2 )
! ************************************************************************
! *                                                                      *
! *   Function  SPARSE_DOT  calculates a dot product of two sparse       *
! *   vectors.                                                           *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *    M ( INTEGER*4  ) -- Total length of the vectors (including zero   *
! *                        elements ).                                   *
! * VEC1 ( REAL*8     ) -- 1-st vectors. Dimension: M.                   *
! * VEC2 ( REAL*8     ) -- 2-nd vectors. Dimension: M.                   *
! *    N ( INTEGER*4  ) -- Number of non-zero elements in VEC1.          *
! *  IND ( INTEGER*4  ) -- Array of indeces of the non-zero elements     *
! *                        VEC1 and VEC2.                                *
! *                                                                      *
! * ________________________ WORKING PARAMETERS: _______________________ *
! *                                                                      *
! *  VC1 ( REAL*8     ) -- Work vector. Dimension: N.                    *
! *  VC2 ( REAL*8     ) -- Work vector. Dimension: N.                    *
! *                                                                      *
! *  ###  03-Jan-97    SPARSE_DOT   v1.1 (c)  L. Petrov 11-JUL-2003 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT  NONE
      INTEGER*4 M, N, IND(N)
      REAL*8    SPARSE_DOT, VEC1(M), VEC2(M), VC1(N), VC2(N)
      REAL*8,   EXTERNAL :: DP_VV_V
!
      CALL DGATHER ( N, IND, VEC1, VC1 )
      CALL DGATHER ( N, IND, VEC2, VC2 )
!
      SPARSE_DOT = DP_VV_V ( N, VC1, VC2 )
!
      RETURN
      END  !#!  SPARSE_DOT  #!#
