      SUBROUTINE BATCH_CLEANUP()
! ************************************************************************
! *                                                                      *
! *   Routine  BACH_CLEANUP makes some operations for cleanup to         *
! *   prevent inadvertent attempt to recover after completion of the     *
! *   solution.                                                          *
! *                                                                      *
! *  ###  06-APR-1999  BATCH_CLEANUP v1.1 (c) L. Petrov 26-SEP-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'glbcm.i'
!
      CALL USE_GLBFIL ( 'OR' )
!
      CALL CLRCH ( INAMCG )
      CALL CLRCH ( ONAMCG )
      CALL CLRCH ( PONAMC )
      IARCNM = -2
      IARCS  = -2
      IGLBLS = -2
      ISLTY2 = '?'
      IIPASS = -2
      PARCNM = -2
      PSLTY2 = '?'
!
      BUILT  = .FALSE.
      COPIED = .FALSE.
      KMORED = .FALSE.
      NARCS  = -1
!
      CALL USE_GLBFIL ( 'WC' )
!
      RETURN
      END  !#!  BATCH_CLEANUP  #!#
