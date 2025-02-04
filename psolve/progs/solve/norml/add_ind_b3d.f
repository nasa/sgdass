      SUBROUTINE ADD_IND_B3D ( V, IXREF, NP, WT, CNSTROBJ )
! ************************************************************************
! *                                                                      *
! *   Auxilary routune  ADD_IND_B3D  adds a constraint to the normal     *
! *   equations.                                                         *
! *                                                                      *
! *  ###  ??-NOV-96   ADD_IND_B3D  v1.0  (c)  J. Gipson  ??-NOV-96  ###  *
! *  ###  ADD_IND_B3D  v3.0  (c)  modified by L. Petrov  19-JAN-98  ###  *
! *  ###  ADD_IND_B3D  v4.0  (c)  modified by L. Petrov 07-NOV-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INCLUDE    'cnstr.i'
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      REAL*8     V(*), WT
      INTEGER*4  IXREF(*), NP
      INTEGER*4  I, J
!
      DO I=1,NP
         DO J=1,I
            CALL ADD_CNSTR ( IXREF(I), IXREF(J), V(I)*V(J)*WT, CNSTROBJ )
         ENDDO
      ENDDO
!
      RETURN
      END  !#!  ADD_IND_B3D  #!#
