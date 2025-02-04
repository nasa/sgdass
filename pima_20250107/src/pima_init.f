      SUBROUTINE PIMA_INIT ( PIM )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine initializes PIMA internal data structure.       *
! *                                                                      *
! *  ### 06-JAN-2006   PIMA_INIT   v1.0 (c)  L. Petrov  06-JAN-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
!
      CALL NOUT ( SIZEOF(PIM), PIM )
!
      RETURN
      END  SUBROUTINE  PIMA_INIT  !#!#
