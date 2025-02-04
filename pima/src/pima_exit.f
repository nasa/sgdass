      SUBROUTINE PIMA_EXIT ( PIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_EXIT
! *                                                                      *
! *  ### 09-JAN-2006    PIMA_EXIT   v1.0 (c)  L. Petrov 09-JAN-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE    ) :: PIM
      INTEGER*4  IUER
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END   SUBROUTINE   PIMA_EXIT  !#!#
